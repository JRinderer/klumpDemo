#include <iostream>
#include <iomanip>
#include "assembler.h"

using namespace std;

const char COLON       =  ':';
const char TAB         = '\t';
const string SEMICOLON =  "; ";

string	getLabel (void)
//	generate a new internal label
//	generate a new internal label
//	internal label format: _L_<label>_
{
    static int Lcount = 0;
    string Scount = to_string(Lcount++);
    string result = "_L_" + Scount + "_";
    return result;
}

void	emitBlankLine (void)
//	generate a blank line of code in the nasm source file
{
    cout << endl;
}

void	emitComment (const string& s)
//	insert a single comment  in the nasm source file
{
    cout << SEMICOLON + s << endl;
}

void	emitDefine (const string& name,const string& value)
//	generate a single DEFINE directive
{
    cout << setw(15) << left << "%define"
         << setw(15) << left << name
         << setw(15) << left << value
         << endl;
}

void	emitEQU (const string& name,const string& value)
//	generate a single EQU directive
{
    cout << setw(15) << left << name
         << setw(15) << left << "equ"
         << setw(15) << left << value
         << endl;
}

void	emitLine (const string& label,const string& opcode,const string& operands,const string& comment)
//	generate a single line of code in the nasm source file
//	code format: <label> <opcode> <operands> [ ; <comment> ]
{
    if (label != "")
        cout << setw(15) << left << label + COLON;
    else
        cout << setw(15) << left << " ";
    if (opcode != "")
        cout << setw(10) << left << opcode;
    else
        cout << setw(10) << left << " ";
    if (operands != "")
        cout << setw(20) << left << operands;
    else
        cout << setw(20) << left << " ";
    if (comment != "")
        cout << TAB + SEMICOLON + comment;
    cout << endl;
}

void	emitDefinitions (void)
//	useful nasm constant definitions
{
    emitBlankLine();
    emitComment("X86 register assignments");
    emitBlankLine();
    emitDefine("IOP1", "rax");
    emitDefine("IOP2", "rbx");
    emitDefine("COUNT","rcx");
    emitDefine("DONE", "rdx");
    emitDefine("MOD",  "rdx");
    emitDefine("DPTR", "rdi");
    emitDefine("SPTR", "rsi");
    emitDefine("FRAME","rbp");
    emitDefine("STACK","rsp");
    emitDefine("INDEX","r14");
    emitDefine("ALIGN","r15");
    emitDefine("ROP1", "xmm0");
    emitDefine("ROP2", "xmm1");
    emitBlankLine();
    emitComment("EQU definitions");
    emitBlankLine();
    emitEQU("LF",         "10");
    emitEQU("NULL",        "0");
    emitEQU("TRUE",        "1");
    emitEQU("FALSE",       "0");
    emitEQU("EXIT_SUCCESS","0");
    emitBlankLine();
}

void	emitProlog (void)
//	insert the introductory code into the nasm source file
{
    emitBlankLine();
    emitDefinitions();
    emitComment("TEXT Section");
    emitBlankLine();
    emitLine("","section",".text");
    emitBlankLine();
    emitLine("","extern","printf");
    emitLine("","extern","scanf");
    emitLine("","extern","getchar");
    emitBlankLine();
    emitLine("","global","main");	// gcc linker
}

void    emitEpilog (const GSTtype& ST,const GTTtype& TT,const GLTtype& LT)
//      insert the concluding code into the nasm source file
{
    emitBlankLine();
// BSS section
    emitComment("BSS section");
    emitBlankLine();
    emitLine("","section",".bss");
    emitBlankLine();
    emitBSSData(ST,TT);
// DATA section
    emitComment("DATA section");
    emitBlankLine();
    emitLine("","section",".data");
    emitBlankLine();
// literal constants
    emitLine("_L_ZERO_  ",    "dq",     "0.0");
    emitLine("_L_EOL_   ",    "db",     "LF,NULL");
    emitDATAData(ST,LT);
// input / output formatting
    emitLine("_L_BOOL_  ",    "db",     "\"%ld\",NULL");
    emitLine("_L_INT_   ",    "db",     "\"%ld\",NULL");
    emitLine("_L_REAL_  ",    "db",     "\"%lf\",NULL");
    emitLine("_L_STRING_",    "db",     "\"%s\",NULL");
    emitBlankLine();
}

void	emitProcProlog (const string& proc,const string& storage)
//	insert the introductory code for each procedure/function
{
    string lbl;
    if (proc == "MAIN")
        lbl = "main";
    else
        lbl = "_ENTRY_" + proc + "_";
    emitLine(lbl,"","","begin proc/func " + proc);
    emitLine("","push","FRAME");
    emitLine("","mov","FRAME,STACK");
    emitLine("","sub","STACK," + storage);
}

void	emitProcEpilog (const string& proc,const string& storage)
//	insert the concluding code for each procedure/function
{
    string lbl;
    if (proc == "MAIN")
        lbl = "_EXIT_main";
    else
        lbl = "_EXIT_" + proc + "_";
    emitLine(lbl,"","","end proc/func " + proc);
    emitLine("","add","STACK," + storage);
    emitLine("","mov","STACK,FRAME");
    emitLine("","pop","FRAME");
    if (proc == "MAIN")
        emitLine("","mov","rax,EXIT_SUCCESS");
    emitLine("","ret","");
}

void	emitRead (const string& a_type)
//	read a variable from the keyboard (basic types only)
{
    string data_format = "[_L_" + a_type + "_]";;
    emitLine("","lea","rdi," + data_format,"emit read (to address)");
    emitLine("","mov","rax,0");
    stackAlign();
    emitLine("","call","scanf");
    stackRestore();
}

void	emitReadLn (void)
//	gobble up remainder of the input line
{
    string loopLabel = getLabel();
    emitLine(loopLabel,"nop","");
    emitLine("","call","getchar","read EOL");
    emitLine("","cmp","rax,LF");
    emitLine("","jne",loopLabel);
}

void	emitWrite (const string& e_type)
//	write the expression on the top of the stack to the display screen
{
    string data_format = "[_L_" + e_type + "_]";
    emitLine("","lea","rdi," + data_format,"emit write (top of stack)");
    emitLine("","xor","rdx,rdx");
    emitLine("","xor","rcx,rcx");
    if ((e_type == "INT") || (e_type == "BOOL") || (e_type == "STRING"))
    {
        emitLine("","pop","rsi");
        emitLine("","mov","rax,0");
    }
    else // (e_type == "REAL")
    {
        emitLine("","xor","rsi,rsi");
        emitLine("","pop","qword [_L_TEMP_]");
        emitLine("","movsd","xmm0,qword [_L_TEMP_]");
        emitLine("","mov","rax,1");
    }
    stackAlign();
    emitLine("","call","printf");
    stackRestore();
}

void	emitWriteLn (void)
//	generate a simplle carriage return
{
    emitLine("","lea","rdi,[_L_STRING_]","write EOL");
    emitLine("","lea","rsi,[_L_EOL_]");
    emitLine("","xor","rdx,rdx");
    emitLine("","xor","rcx,rcx");
    emitLine("","mov","rax,0");
    stackAlign();
    emitLine("","call","printf");
    stackRestore();
}

void    emitNeg (const string& n_type)
//      negate the numerical value (INT or REAL) at the top of the stack
{
    if (n_type == "INT")
        emitLine("","neg","qword [STACK]","emit unary negative");
    else // (n_type == "REAL")
    {
        emitLine("","pop","qword [_L_TEMP_]","emit unary negative");
        emitLine("","movsd","ROP1,qword [_L_ZERO_]");
        emitLine("","movsd","ROP2,qword [_L_TEMP_]");
        emitLine("","subsd","ROP1,ROP2");
        emitLine("","movsd","qword [_L_TEMP_],ROP1");
        emitLine("","push","qword [_L_TEMP_]");
    }
}

void	emitAND (void)
//	AND the logical values (INT) at the top of the stack
{
    emitLine("","pop","IOP2","emit AND");
    emitLine("","pop","IOP1");
    emitLine("","and","IOP1,IOP2");
    emitLine("","push","IOP1");
}

void	emitOR (void)
//	OR the logical values (INT) at the top of the stack
{
    emitLine("","pop","IOP2","emit OR");
    emitLine("","pop","IOP1");
    emitLine("","or","IOP1,IOP2");
    emitLine("","push","IOP1");
}

void	emitNOT (void)
//	NOT the logical value (INT) at the top of the stack
{
    emitLine("","pop","IOP1","emit NOT");
    emitLine("","xor","IOP1,TRUE");
    emitLine("","push","IOP1");
}

void	emitCompop (const string& op,const string& o_type)
//	comparison of two numeric values found on stack
{
    string labelTrue = getLabel();
    string labelDone = getLabel();
    if (o_type == "INT")
    {
        emitLine("","pop","IOP2","emit compare");
        emitLine("","pop","IOP1");
        emitLine("","cmp","IOP1,IOP2");
        if (op == "=")
            emitLine("","je",labelTrue);
        else if (op == "<>")
            emitLine("","jne",labelTrue);
        else if (op == ">")
            emitLine("","jg",labelTrue);
        else if (op == "<")
            emitLine("","jl",labelTrue);
        else if (op == ">=")
            emitLine("","jge",labelTrue);
        else // (op == "<=")
            emitLine("","jle",labelTrue);
    }
    else // (o_type == REAL")
    {
        emitLine("","pop","qword [_L_TEMP_]","emit compare");
        emitLine("","movsd","ROP2,qword [_L_TEMP_]");
        emitLine("","pop","qword [_L_TEMP_]");
        emitLine("","movsd","ROP1,qword [_L_TEMP_]");
        emitLine("","ucomisd","ROP1,ROP2");
        if (op == "=")
            emitLine("","je",labelTrue);
        else if (op == "<>")
            emitLine("","jne",labelTrue);
        else if (op == ">")
            emitLine("","ja",labelTrue);
        else if (op == "<")
            emitLine("","jb",labelTrue);
        else if (op == ">=")
            emitLine("","jae",labelTrue);
        else // (op == "<=")
            emitLine("","jbe",labelTrue);
    }
    emitLine("","push","FALSE","comparison false");
    emitLine("","jmp",labelDone);
    emitLine(labelTrue,"nop","");
    emitLine("","push","TRUE","comparison true");
    emitLine(labelDone,"nop","");
}

void	emitMulop (const string& op,const string& o_type)
//	multiplication or division of two numeric values found on stack
//	AND the two logical values (BOOLs) at the top of the stack
{
    if (o_type == "BOOL")
    {
        emitAND();
    }
    else if (o_type == "INT")
    {
        emitLine("","pop","IOP2","emit mulop");
        emitLine("","pop","IOP1");
        emitLine("","xor","rdx,rdx");
        if (op == "*")
        {
            emitLine("","imul","IOP1,IOP2");
            emitLine("","push","IOP1");
        }
        else if (op == "/")
        {
            emitLine("","idiv","IOP2");
            emitLine("","push","IOP1");
        }
        else if (op == "%")
        {
            emitLine("","idiv","IOP2");
            emitLine("","push","MOD");
        }
    }
    else // (o_type == "REAL")
    {
        emitLine("","pop","qword [_L_TEMP_]","emit mulop");
        emitLine("","movsd","ROP2,qword [_L_TEMP_]");
        emitLine("","pop","qword [_L_TEMP_]");
        emitLine("","movsd","ROP1,qword [_L_TEMP_]");
        if (op == "*")
            emitLine("","mulsd","ROP1,ROP2");
        else // (op == "/")
            emitLine("","divsd","ROP1,ROP2");
        emitLine("","movsd","qword [_L_TEMP_],ROP1");
        emitLine("","push","qword [_L_TEMP_]");
    }
}

void	emitAddop (const string& op,const string& o_type)
//	addition or subtraction of two numeric values found on stack
//	OR the two logical value (BOOLs) at the top of the stack
{
    if (o_type == "BOOL")
    {
        emitOR();
    }
    else if (o_type == "INT")
    {
        emitLine("","pop","IOP2","emit addop");
        emitLine("","pop","IOP1");
        if (op == "+")
            emitLine("","add","IOP1,IOP2");
        else // (op == "-")
            emitLine("","sub","IOP1,IOP2");
        emitLine("","push","IOP1");
    }
    else // (o_type == "REAL")
    {
        emitLine("","pop","qword [_L_TEMP_]","emit addop");
        emitLine("","movsd","ROP2,qword [_L_TEMP_]");
        emitLine("","pop","qword [_L_TEMP_]");
        emitLine("","movsd","ROP1,qword [_L_TEMP_]");
        if (op == "+")
            emitLine("","addsd","ROP1,ROP2");
        else // (op == "-")
            emitLine("","subsd","ROP1,ROP2");
        emitLine("","movsd","qword [_L_TEMP_],ROP1");
        emitLine("","push","qword [_L_TEMP_]");
    }
}

void	emitAssign (void)
//	store the data value at the top of the stack
//	to the address of the receiving variable
{
    emitLine("","pop","IOP1","emit assignment");
    emitLine("","pop","DPTR");
    emitLine("","mov","[DPTR],IOP1");
}

void	emitStructAssign (int nbytes)
{
    int nqwords = nbytes / 8;
    emitLine("","pop","SPTR","emit Struct Assignment");
    emitLine("","pop","DPTR");
    emitLine("","mov","DONE," + to_string(nqwords));
    emitLine("","xor","COUNT,COUNT");
    string more_label = getLabel();
    emitLine(more_label,"mov","IOP1,[SPTR+8*COUNT]");
    emitLine("","mov","[DPTR+8*COUNT],IOP1");
    emitLine("","inc","COUNT");
    emitLine("","cmp","COUNT,DONE");
    emitLine("","jne",more_label);
}

void	emitTest (const string& false_label)
//	is the result of a comparison (top of the stack)
//	   TRUE:  continue with the following statements
//	   FALSE: jump to a specified label
{
    emitLine("","pop","IOP1","test TRUE or FALSE?");
    emitLine("","cmp","IOP1,0");
    emitLine("","je",false_label);
}

void	emitLabel (const string& label)
//	pretty obvious!
{
    emitLine(label,"nop","");
}

void	emitEmpty (void)
//	pretty obvious!
{
    emitLine("","nop","");
}

void	emitGoto (const string& label)
//	pretty obvious!
{
    emitLine("","jmp",label);
}

void	emitCall (const string& proc,int no_params,const string& ret_type)
//	issue CALL to pass control to a callee function/procedure
//	also:
//	   clean up the actual arguments stil on the stack
//	   and save any return value to the top of the stack
{
    emitLine("","call","_ENTRY_" + proc + "_","emit CALL: " + proc);
    string arg_bytes = to_string(no_params*8);
    emitLine("","add","STACK," + arg_bytes,"cleanup actual arguments");
    if (ret_type == "REAL")
    {
        emitLine("","movsd","qword [_L_TEMP_],ROP1","ROP1 to top of stack");
        emitLine("","push","qword [_L_TEMP_]");
    }
    else if (ret_type == "INT")
        emitLine("","push","IOP1","IOP1 to top of stack");
    else // (ret_type == "")
        ;
}

void	emitReturn (const string& proc,const string& etype)
//	save any return value in the appropriate register (RAX or XMM0)
//	JUMP to the epilog to return control back to caller function/procedure
{
    if (etype == "REAL")
    {
        emitLine("","pop","qword [_L_TEMP_]","save return value in ROP1");
        emitLine("","movsd","ROP1,qword [_L_TEMP_]");
    }
    else if (etype == "INT")
        emitLine("","pop","IOP1","save return value in IOP1");
    else // (etype == "")
        ;
    emitLine("","jmp","_EXIT_" + proc + "_","emit RETURN");
}

void	emitCleanup (int no_params)
//	remove obsolete actual arguments from the stack
{
    string arg_bytes = to_string(no_params*8);
    emitLine("","add","STACK," + arg_bytes,"cleanup actual args");
}

void	emitInitCase (void)
//	initial coding for CASE statement
{
    emitLine("","pop","IOP1","emit init CASE");
}

void	emitTestCase (const string& test_value,const string& jmp_label)
//	compare CASE expression with possible CASE value
{
    emitLine("","cmp","IOP1," + test_value,"emit TEST CASE");
    emitLine("","jne",jmp_label);
}

void	emitInitFor (const string& addr,bool ptr)
//	initial coding for FOR statement
{
    emitLval(addr,ptr);
    emitLine("","mov","DPTR,SPTR","emit INIT FOR");
    emitLine("","pop","IOP1");
    emitLine("","mov","qword [DPTR],IOP1");
}

void	emitTestFor (const string& addr,bool ptr,const string& step,const string& labelDone)
//	compare FOR variable with STOP value
{
    emitLval(addr,ptr);
    emitLine("","mov","DPTR,SPTR","emit TEST FOR");
    emitLine("","mov","IOP1,[STACK]");
    emitLine("","cmp","qword [DPTR],IOP1");
    if (step == "TO")
    {
        emitLine("","jg",labelDone);
    }
    else // (step == "DOWNTO")
    {
        emitLine("","jl",labelDone);
    }
}

void	emitDoneFor (const string& labelDone)
{
    emitLabel(labelDone);
    emitLine("","add","STACK,8");
}

void	emitIncrementFor (const string& addr,bool ptr,const string& step,const string& labelRepeat)
//	advance the FOR variable: FOR = FOR + STEP
{
    emitLval(addr,ptr);
    emitLine("","mov","DPTR,SPTR","emit INCREMENT FOR");
    if (step == "TO")
        emitLine("","inc","qword [DPTR]");
    else // (step == "DOWNTO")
        emitLine("","dec","qword [DPTR]");
    emitGoto(labelRepeat);
}

void	emitLiteral (const string& c_label,const string& c_type,const string& c_value)
//	move the constant value to the top of the stack
{
    if (c_type == "INT")
        emitLine("","push","qword [" + c_label + "]","emit INT literal");
    else if (c_type == "REAL")
        emitLine("","push","qword [" + c_label + "]","emit REAL literal");
    else // (c_type == "STRING")
        emitLine("","push",c_label,"emit STRING literal");
}

void	emitLval (const string& addr,bool ptr)
//	move a memory address (Lval) to the SPTR register
{
    if (ptr)
    {
        emitLine("","lea","SPTR,[" + addr + "]","emit Lvalue (call by VAR)");
        emitLine("","mov","SPTR,[SPTR]");
    }
    else
        emitLine("","lea","SPTR,[" + addr + "]","emit Lvalue (default:call by VAL)");
}

void	emitSaveAddr (void)
//	move the memory address (Lval) found in the SPTR register
//	to the top of the stack
{
    emitLine("","push","SPTR","save Lvalue on stack");
}

void	emitRval (const string& vtype)
//	move the data value (Rval)
//	found at the address in the SPTR register
//	to the top of the stack
{
    if (vtype == "STRING")
        emitLine("","push","SPTR","save STRING Lvalue on stack");
    else if (vtype == "STRUCT")
        emitLine("","push","SPTR","save STRUCT Lvalue on stack");
    else
        emitLine("","push","qword [SPTR]","convert Lvalue to Rval");
}

void	emitBSSData (const GSTtype& ST,const GTTtype& TT)
//	reserve space for all the variables found in
//	   the global symbol table
{
    for (int i = 0;i < ST.size();++i)
    {
        string sident = ST[i].getIdentifier();
        string stype = ST[i].getType();
        bool sconst = ST[i].isConstant();
        if (sconst)
            continue;
        int loc = search(TT,stype);
        int ssize = TT[loc].getSize();
        emitLine("_" + sident + "_","resb",to_string(ssize));
    }
    emitBlankLine();
}

void	emitDATAData (const GSTtype& ST,const GLTtype& LT)
//	define space for all the named constants found in
//	   either the global symbol table
//	   or     the literal symbol table
{
    for (int i = 0;i < ST.size();++i)
    {
        string sident = ST[i].getIdentifier();
        string stype = ST[i].getType();
        bool sconst = ST[i].isConstant();
        if (!sconst)
            continue;
        string sliteral = ST[i].getValue();
        string reserve;
        if (stype == "INT")
            reserve = "dq";
        else if (stype == "REAL")
            reserve = "dq";
        else // (stype == "STRING")
            reserve = "db";
        if (stype == "STRING")
            sliteral = sliteral + ",NULL";
        emitLine("_" + sident + "_",reserve,sliteral);
    }
    emitBlankLine();
    for (int i = 0;i < LT.size();++i)
    {
        string clabel = LT[i].getLabel();
        string ctype = LT[i].getType();
        string cvalue = LT[i].getValue();
        string reserve;
        if (ctype == "INT")
            reserve = "dq";
        else if (ctype == "REAL")
            reserve = "dq";
        else // (ctype == "STRING")
            reserve = "db";
        if (ctype == "STRING")
            cvalue = cvalue + ",NULL";
        emitLine(clabel,reserve,cvalue);
    }
    emitBlankLine();
}

void	stackAlign (void)
//	align the stack frame on a 16 byte boundary
//	   if NOT aligned: pad with 8 bytes
//	   if     aligned: pad with 16 bytes
{
    string even_label = getLabel();
    string done_label = getLabel();
    emitLine("","mov","ALIGN,STACK");
    emitLine("","and","ALIGN,0x000000000000000f");
    emitLine("","cmp","ALIGN,0");
    emitLine("","je",even_label);
    emitLine("","push","0");		// # bytes still to remove
    emitLine("","jmp",done_label);
    emitLine(even_label,"","");
    emitLine("","sub","STACK,8");
    emitLine("","push","8");		// # bytes still to remove
    emitLine(done_label,"","");
}

void	stackRestore (void)
//	return the stack frame to its original state
{
    emitLine("","pop","ALIGN");
    emitLine("","add","STACK,ALIGN");
}

bool	promote (const string& from,const string& to,const string& addr)
//	promote a lower data type to a higher data type
{
    bool success = true;
    if (from == to)
        ;
    else if ((from == "BOOL") && (to == "INT"))
        emitLine("","nop","","conversion BOOL --> INT");
    else if ((from == "INT") && (to == "REAL"))
    {
        emitLine("","mov","IOP1," + addr,"conversion INT --> REAL");
        emitLine("","cvtsi2sd","ROP1,IOP1");
        emitLine("","movsd",addr + ",ROP1");
    }
    else
        success = false;;
    return success;
}

bool	demote (const string& from,const string& to,const string& addr)
//	demote a higher data type to a lower data type
{
    bool success = true;
    if (from == to)
        ;
    else if ((from == "INT") && (to == "BOOL"))
    {
        string labelFalse = getLabel();
        string labelDone = getLabel();
        emitLine("","mov","IOP1," + addr,"conversion INT --> BOOL");
        emitLine("","cmp","IOP1,0");
        emitLine("","je",labelFalse);
        emitLine("","mov","qword " + addr + ",1");
        emitLine("","jmp",labelDone);
        emitLine(labelFalse,"nop","");
        emitLine("","mov","qword " + addr + ",0");
        emitLine(labelDone,"nop","");
    }
    else if ((from == "REAL") && (to == "INT"))
    {
        emitLine("","movsd","ROP1," + addr);
        emitLine("","cvttsd2si","IOP1,ROP1");
        emitLine("","mov",addr + ",IOP1");
    }
    else
        success = false;
    return success;
}

