#include <iostream>
#include <cstdlib>
#include <string>
#include <set>
#include <iomanip>
#include "scanner.h"
#include "error.h"
#include "assembler.h"
#include "tables.h"

using namespace std;

//	KLUMP DECLARATIONS  ***************************************************

Lexeme		current;
string		current_proc;

GSTtype		global_symbol_table;
GTTtype		global_type_table;
GLTtype		global_literal_table;
GPTtype		global_proc_table;

GStype		break_stack;
GStype		next_stack;

LSTtype		local_symbol_table;
LLTtype         local_label_table;

const bool	DEBUG		= true;

set<string>     ATOMICTYPE  = { "BOOL","INT","REAL","STRING" };
set<string>	CONSTANTS   = { "NUMBER","DECIMAL","ASCII","CSTRING" };
set<string>	ENDLIST	    = { "ELSE","END" };

set<string>     COMPOP	    = { "=","<>",">","<",">=","<=" };
set<string>     ADDOP	    = { "+","-","OR" };
set<string>     MULOP	    = { "*","/","%","AND" };

//	KLUMP PRODUCTION RULES  ***********************************************

void	parseKlumpProgram	    (void);
void	parseGlobalDefinitions	(void);
void	parseConstDefinitions	(void);
void    parseConstList          (void);
void    parseConst              (string&,string&);
void    parseTypeDefinitions    (void);
void    parseTypeList           (void);
void	parseStructType         (int&,string&,ARRpair&,RECtype&);
void	parseArrayType          (int&,ARRpair&);
void	parseRecordType         (int&,RECtype&);
void    parseFldList            (int&,RECtype&);
int     parseDclDefinitions     (const string&);
int	parseDclList		(const string& = "global");
string	parseDclType	        (void);
void	parseProcDeclarations	(void);
void	parseSignatureList	(void);
void	parseProcSignature	(string&,PLtype&,string&);
PLtype	parseFormalArgs		(void);
PLtype	parseFormalArgList	(void);
PLentry	parseFormalArg		(void);
string	parseCallBy		(void);
string	parseReturnType		(void);
void	parseActualArgs		(const string&);
void	parseActualArgList	(const string&,const PLtype&);
string	parseActualArg		(const string&,const string&);
void	parseProcedureList	(void);
void	parseProcedure		(void);
void	parseProcHead		(int&);
void	parseProcBody		(int);
void	parseStatementList	(void);
void	parseStatement		(void);
void	parseLabel		(string&);
void	parseExecutableStatement(void);
void	parseReadStatement	(void);
void	parseWriteStatement	(void);
void	parseAssignmentStatement(void);
void	parseCallStatement	(void);
void	parseReturnStatement	(void);
void	parseGotoStatement	(void);
void	parseEmptyStatement	(void);
void	parseCompoundStatement	(void);
void	parseIfStatement	(void);
void	parseElseClause		(void);
void	parseWhileStatement	(void);
void	parseCaseStatement	(void);
void	parseCaseList		(const string&);
void	parseForStatement	(void);
void	parseNextStatement	(void);
void	parseBreakStatement	(void);
string	parseExpression		(void);
string	parseComparison		(void);
string	parseSimpleExpression	(void);
string	parseTerm		(void);
string	parseFactor		(void);
bool	parseUnary		(void);
string	parseLval		(string&);
string	parseQualifier		(const string&);
string  parseFuncRef            (int);

//	KLUMP SUPPORT ALGORITHMS  *********************************************

void    mustMatch		(const Lexeme&,const string&,bool = true);
bool    doesMatch		(const Lexeme&,const string&,bool = false);
bool	element			(const string&,const set<string>&);
void	initTables		(void);
bool	isGlobalDcl		(const string&);
bool	isNamedConstant		(const string&);
void	displayGlobalTables	(void);
void	displayLocalTables	(const string&);
void	resetLocalTables	(void);
void    getVarInfo		(const string&,string&,string&,bool&);
string	resolveType		(const string&,const string&);
int	calcAlignedStorage	(int);

//	MAIN PROGRAM  *********************************************************

int main (int argc,char* argv[])
//	main program for the KLUMP compiler
{
    initScanner();
    initTables();
    current = getNext();
    parseKlumpProgram();
    exit(EXIT_SUCCESS);
}

//	PRODUCTION RULES  *****************************************************

void	parseKlumpProgram (void)
// < program > --> < global_definitions > !emit_prolog
//                 < procedure_list > !emit_epilog .
{
    parseGlobalDefinitions();
    if (DEBUG)
        displayGlobalTables();
    emitProlog();
    parseProcedureList();
    emitEpilog(global_symbol_table,global_type_table,global_literal_table);
    if (!doesMatch(current,"."))
        SyntaxError(current.getLineNo(),".",current.getValue());
}

void	parseGlobalDefinitions (void)
// < global_definitions > --> [ GLOBAL
//                            < const_definitions >
//                            < type_definitions >
//                            < dcl_definitions >
//                            < proc_declarations > ]
{
    if (!doesMatch(current,"GLOBAL"))
        return;
    current = getNext();
    parseConstDefinitions();
    parseTypeDefinitions();
    parseDclDefinitions("global");
    parseProcDeclarations();
}

void	parseConstDefinitions (void)
// < const_definitions > --> [ CONST < const_list > ]
{
    if (!doesMatch(current,"CONST"))
        return;
    current = getNext();
    parseConstList();
}

void	parseConstList (void)
// < const_list > --> [ IDENTIFIER : < const > !save_const_item ;
//                    < const_list > ]
{
    bool loop = true;
    while (loop)
    {
        loop = false;
        mustMatch(current,"IDENTIFIER",false); //Are we sending false for a reason?
        string id = current.getValue(); //what is this doing?
        current = getNext();
        mustMatch(current,":");
        string c_type;
        string c_value;
        parseConst(c_type,c_value);
        // !save_const_item
        if (!isGlobalDcl(id))
        {
            GSTentry data(id,c_type,true,c_value);
            insert(global_symbol_table,data);
        }
        else
            SemanticsError(current.getLineNo(),11,id);
        mustMatch(current,";");
        if (doesMatch(current,"IDENTIFIER"))	// for tail recursion
            loop = true;
    }
}

void	parseConst (string& c_type,string& c_value)
// < const > --> NUMBER | DECIMAL | CSTRING
{
    if (element(current.getToken(),CONSTANTS))
    {
        c_type = current.getToken();
        if (c_type == "NUMBER")
            c_type = "INT";
        else if (c_type == "DECIMAL")
            c_type = "REAL";
        else // (c_type == "CSTRING")
            c_type = "STRING";
        c_value = current.getValue();
        current = getNext();
    }
    else
        SyntaxError(current.getLineNo(),"< constant >",current.getToken());
}

void    parseTypeDefinitions (void)
// < type_definitions > --> [ TYPE < type_list > ]
{
    if (!doesMatch(current,"TYPE"))
        return;
    current = getNext();
    parseTypeList();
}

void	parseTypeList (void)
// < type_list > --> [ IDENTIFIER : < struct_type > !save_type ;
//                   < type_list > ]
{
    bool loop = true;
    while (loop)
    {
        loop = false;
        mustMatch(current,"IDENTIFIER",false);
        string ident = current.getValue();
        current = getNext();
        mustMatch(current,":");
        int tsize;
        string tstruct;
        ARRpair ainfo;
        RECtype rinfo;
        parseStructType(tsize,tstruct,ainfo,rinfo);
        // !save_type
        if (!isGlobalDcl(ident))
        {
            GTTentry data(ident,tsize,tstruct,ainfo,rinfo);
            insert(global_type_table,data);
        }
        else
            SemanticsError(current.getLineNo(),11,ident);
        mustMatch(current,";");
        if (doesMatch(current,"IDENTIFIER"))	// for tail recursion
            loop = true;
    }
}

void	parseStructType
        (int& tsize,string& tstruct,ARRpair& ainfo,RECtype& rinfo)
// < struct_type > --> < array_type > | < record_type >
{
    if (doesMatch(current,"ARRAY"))
    {
        tstruct = "ARRAY";
        parseArrayType(tsize,ainfo);
        rinfo = empty_ri;
    }
    else if (doesMatch(current,"RECORD"))
    {
        tstruct = "RECORD";
        parseRecordType(tsize,rinfo);
        ainfo = empty_ai;
    }
    else
        SemanticsError(current.getLineNo(),23,current.getValue());
}

void	parseArrayType (int& tsize,ARRpair& ainfo)
// < array_type > --> ARRAY [ { NUMBER | IDENTIFIER } ] OF < dcl_type >
{
    current = getNext();
    mustMatch(current,"[");
    int no_items;
    if (doesMatch(current,"NUMBER"))
        no_items = stoi(current.getValue());
    else if (doesMatch(current,"IDENTIFIER"))
    {
        string ident = current.getValue();
        if (!isNamedConstant(ident))
            SemanticsError(current.getLineNo(),99,"IDENTIFIER (" + ident + ") must be named constant");
        int loc = search(global_symbol_table,ident);
        string id_type = global_symbol_table[loc].getType();
        if (id_type != "INT")
            SemanticsError(current.getLineNo(),99,"IDENTIFIER (" + ident + ") must be INT");
        no_items = stoi(global_symbol_table[loc].getValue());
    }
    else
        SyntaxError(current.getLineNo(),"NUMBER or IDENTIFIER",current.getToken());
    current = getNext();
    mustMatch(current,"]");
    mustMatch(current,"OF");
    string element_type = parseDclType();;
    ARRpair result(no_items,element_type);
    ainfo = result;
    int loc = search(global_type_table,element_type);
    int esize = global_type_table[loc].getSize();
    tsize = no_items*esize;
}

void	parseRecordType (int& tsize,RECtype& rinfo)
// < record_type > --> record < fld_list > END
{
    current = getNext();
    parseFldList(tsize,rinfo);
    mustMatch(current,"END");
}

void	parseFldList (int& tsize,RECtype& rinfo)
// < fld_list > --> { IDENTIFIER : < dcl_type > ; }+
{
    RECtype result;
    bool loop = true;
    string fname;
    string ftype;
    int foffset = 0;
    while (loop)
    {
        loop = false;
        mustMatch(current,"IDENTIFIER",false);
        fname = current.getValue();
        current = getNext();
        mustMatch(current,":");
        ftype = parseDclType();
        mustMatch(current,";");
        if (doesMatch(current,"IDENTIFIER"))      // for tail recursion
            loop = true;
        RECentry data(fname,ftype,foffset);
        if(!insert(result,data))
            SemanticsError(current.getLineNo(),16,fname);
        int loc = search(global_type_table,ftype);
        int fsize = global_type_table[loc].getSize();
        foffset += fsize;
    }
    rinfo = result;
    tsize = foffset;
}

int	parseDclDefinitions (const string& lorg)
// < dcl_definitions > --> [ DCL < dcl_list > ]
{
    if (!doesMatch(current,"DCL"))
        return 0;
    current = getNext();
    return parseDclList(lorg);
}

int	parseDclList (const string& lorg)
// < dcl_list > --> { IDENTIFIER : < dcl_type > !save_dcl_item ; }+
{
    bool loop = true;
    int offset = 0;
    while (loop)
    {
        loop = false;
        mustMatch(current,"IDENTIFIER",false);
        string id = current.getValue();
        current = getNext();
        mustMatch(current,":");
        string dcl_type;
        dcl_type = parseDclType();
        int tindex = search(global_type_table,dcl_type);
        int tsize;
        if (tindex >= 0)
            tsize = global_type_table[tindex].getSize();
        else
            SemanticsError(current.getLineNo(),24,dcl_type);
        // !save_dcl_item
        if (lorg == "global")
        {
            GSTentry data(id,dcl_type);
            if (!insert(global_symbol_table,data))
                SemanticsError(current.getLineNo(),11,id);
        }
        else // (lorg == "local")
        {
            offset += tsize;		// modify for arrays/records
            string callby = "VAL";
            LSTentry data(id,dcl_type,to_string(-offset),callby);
            if (!insert(local_symbol_table,data))
                SemanticsError(current.getLineNo(),12,id);
        }
        mustMatch(current,";");
        if (doesMatch(current,"IDENTIFIER"))      // for tail recursion
            loop = true;
    }
    return offset;
}

string	parseDclType (void)
// < dcl_type > --> < atomic > | IDENTIFIER
{
    string dcl_type;
    if (element(current.getToken(),ATOMICTYPE))
        dcl_type = current.getToken();
    else if (doesMatch(current,"IDENTIFIER",false))
    {
        dcl_type = current.getValue();
        int loc = search(global_type_table,dcl_type);
        if (loc < 0)
            SemanticsError(current.getLineNo(),24,dcl_type);
    }
    else
        SyntaxError(current.getLineNo(),"< dcl_type >",current.getValue());
    current = getNext();
    return dcl_type;
}

void	parseProcDeclarations (void)
// < proc_declarations > --> [ PROC < signature_list > ]
{
    if (!doesMatch(current,"PROC"))
        return;
    current = getNext();
    parseSignatureList ();
}

void	parseSignatureList (void)
// < signature_list > --> { < proc_signature > !save_signature }*
{
    bool loop = true;
    while (loop)
    {
        loop = false;
        string proc;
        PLtype params;
        string ret_type;
        parseProcSignature(proc,params,ret_type);
        // !save_signature
        if (!isGlobalDcl(proc))
        {
            GPTentry data(proc,params,ret_type);
            insert(global_proc_table,data);
        }
        else
            SemanticsError(current.getLineNo(),15,proc);
        if (doesMatch(current,"IDENTIFIER"))      // for tail recursion
            loop = true;
    }
}

void	parseProcSignature (string& proc,PLtype& params,string& ret_type)
// < proc_signature > --> IDENTIFIER < formal_args > < return_type > ;
{
    mustMatch(current,"IDENTIFIER",false);
    proc = current.getValue();
    current = getNext();
    params = parseFormalArgs();
    ret_type = parseReturnType();
    mustMatch(current,";");
}

PLtype    parseFormalArgs (void)
// < formal_args > --> [ ( < formal_arg_list > ) ]
{
    if (!doesMatch(current,"("))
        return empty_pl;
    current = getNext();
    PLtype params = parseFormalArgList();
    mustMatch(current,")");
    return params;
}

PLtype    parseFormalArgList (void)
// < formal_arg_list > --> < formal_arg > { , < formal_arg > }*
{
    PLtype params;
    bool loop = true;
    while (loop)
    {
        loop = false;
        PLentry param = parseFormalArg();
        if (!insert(params,param))
            SemanticsError(current.getLineNo(),17,param.getIdentifier());
        if (doesMatch(current,","))
        {
            loop = true;
            current = getNext();
        }
    }
    return params;
}

PLentry    parseFormalArg (void)
// < formal_arg > --> < call_by > IDENTIFIER : < dcl_type >
{
    string callby = parseCallBy();
    mustMatch(current,"IDENTIFIER",false);
    string id = current.getValue();
    current = getNext();
    mustMatch(current,":");
    string dcl_type;
    dcl_type = parseDclType();
    PLentry param(id,dcl_type,callby);
    return param;
}

string    parseCallBy (void)
// < call_by > --> [ VAR ]
{
    if (doesMatch(current,"VAR"))
    {
        current = getNext();
        return "VAR";
    }
    else
        return "VAL";
}

string    parseReturnType (void)
// < return_type > --> [ : < atomic > ]
{
    string result = "";
    if (!doesMatch(current,":"))
        return result;
    current = getNext();
    if (element(current.getToken(),ATOMICTYPE))
    {
        result = current.getValue();
        current = getNext();
    }
    else
        SyntaxError (current.getLineNo(),"< atomic >",current.getToken());
    return result;
}

void	parseActualArgs (const string& caller)
// < actual_args > --> [ ( < actual_arg_list > ) ]
{
    if (!doesMatch(current,"("))
        return;
    current = getNext();
    PLtype params;
    if ((caller == "read") || (caller == "write"))
        params = empty_pl;
    else // (caller == "proc")
    {
        int loc = search(global_proc_table,caller);
        if (loc < 0)
            SemanticsError(current.getLineNo(),25,caller);
        GPTentry data = retrieve(global_proc_table,loc);
        params = data.getParameters();
    }
    parseActualArgList(caller,params);
    mustMatch(current,")");
}

void	parseActualArgList (const string& caller,const PLtype& params)
// < actual_arg_list > --> < actual_arg > !emit_read !emit_write
//                         { , < actual_arg > !emit_read !emit_write }*
{
    bool loop = true;
    if ((caller == "read") || (caller == "write"))
    {
        while(loop)
        {
            loop = false;
            if (caller == "read")
            {
                string lident;
                string etype = parseLval(lident);
                if (isNamedConstant(lident))
                    SemanticsError(current.getLineNo(),99,"cannot change named constant (" + lident + ")");
                emitRead(etype);
            }
            else // (caller == "write")
            {
                string etype = parseExpression();
                emitWrite(etype);
            }
            if (doesMatch(current,","))
            {
                loop = true;
                current = getNext();
            }
        }
    }
    else // (caller = "proc")
    {
        int counter;
        for (counter = 0;loop && (counter < params.size());++counter)
        {
            loop = false;
            PLentry param = params[counter];
            string atype = param.getType();
            string acallby = param.getCallby();
            string etype = parseActualArg(atype,acallby);
            if (doesMatch(current,","))
            {
                loop = true;
                current = getNext();
            }
        }
        if (loop == true)
            SemanticsError(current.getLineNo(),36,"> " + to_string(params.size()));
        else if (counter < params.size() - 1)
            SemanticsError(current.getLineNo(),37,"< " + to_string(params.size()));
        else
            ;					// do nothing!
    }
}

string    parseActualArg (const string& atype,const string& acallby)
// < actual_arg > --> < expression >
{
    if (acallby == "VAL")
    {
        string etype = parseExpression();

        if (atype > etype)
        {
            if (!promote(etype,atype,"[STACK]"))
                SemanticsError
                        (current.getLineNo(),41,etype + " --> " + atype);
        }
        else if (atype < etype)
        {
            if (!demote(etype,atype,"[STACK]"))
                SemanticsError
                        (current.getLineNo(),42,etype + " --> " + atype);
        }
        else // (atype = etype)
            ;                                 // do nothing!
        return etype;
    }
    else // (acallby == "VAR")
    {
        string lident;
        string etype = parseLval(lident);
        if (isNamedConstant(lident))
            SemanticsError(current.getLineNo(),99,"can not change named constant (" + lident + ")");
        if (etype != atype)
            SemanticsError(current.getLineNo(),38,etype + " <--> " + atype);
        emitSaveAddr();
        return atype;
    }
}

void	parseProcedureList (void)
// < procedure_list > --> [ < procedure > < procedure_list> ]
{
    parseProcedure();
    while (doesMatch(current,"PROCEDURE"))	// for tail recursion
        parseProcedure();
}

void	parseProcedure (void)
// < procedure > --> < proc_head > < proc_body >
{
    int proc_loc;
    parseProcHead(proc_loc);
    parseProcBody(proc_loc);
}

void	parseProcHead (int& proc_loc)
// < proc_head > --> PROCEDURE IDENTIFIER ;
{
    mustMatch(current,"PROCEDURE");
    mustMatch(current,"IDENTIFIER",false);
    string proc = current.getValue();
    current = getNext();
    proc_loc = search(global_proc_table,proc);
    if (proc_loc < 0)
        SemanticsError(current.getLineNo(),25,proc);
    mustMatch(current,";");
}

void	parseProcBody (int proc_loc)
// < proc_body > --> !reset_local_tables !process_parameters
//                   < dcl_definitions >
//                   BEGIN !emit_proc_prolog
//                   < statement_list >
//                   END !emit_proc_epilog !emit_cleanup
{
    resetLocalTables();
    GPTentry proc_info = retrieve(global_proc_table,proc_loc);
    string name = proc_info.getIdentifier();
    PLtype params = proc_info.getParameters();
    string ret_type = proc_info.getReturnType();
    current_proc = name;
    int no_params = params.size();
    // process parameters
    for (int i = 0;i < no_params;++i)
    {
        PLentry item = params[i];
        string pid = item.getIdentifier();
        string ptype = item.getType();
        string poffset = "+" + to_string((no_params+1-i)*8);
        string pcallby = item.getCallby();
        if (!element(ptype,ATOMICTYPE))
            pcallby = "VAR";
        LSTentry data(pid,ptype,poffset,pcallby);
        if (!insert(local_symbol_table,data))
            SemanticsError(current.getLineNo(),12,pid);
    }
    int offset = parseDclDefinitions("local");
    int aligned_offset = calcAlignedStorage(offset);
    mustMatch(current,"BEGIN");
    emitProcProlog(name,to_string(aligned_offset));
    parseStatementList();
    mustMatch(current,"END");
    emitProcEpilog(name,to_string(aligned_offset));
    current_proc = "";
    if (DEBUG)
        displayLocalTables(name);
}


void    parseStatementList (void)
// < statement_list > --> [ < statement > < statement_list > ]
{
    while (!doesMatch(current,"END"))	// for tail recursion
        parseStatement();
}

void    parseStatement (void)
// < statement > --> < label > !emit_label < executable_statement >
{
    string label;
    parseLabel(label);
    if (label != "")
        emitLabel(label);
    parseExecutableStatement();
}

void	parseLabel (string& label)
// < label > --> [ # NUMBER ]
{
    label = "";
    if (!doesMatch(current,"#"))
        return;
    current = getNext();
    mustMatch(current,"NUMBER",false);
    string number = current.getValue();
    current = getNext();
    int loc = search(local_label_table,number);
    if (loc >= 0)
        label = local_label_table[loc].getLabel();
    else
    {
        label = getLabel();
        LLTentry data(number,label);
        insert(local_label_table,data);
    }
}

void	parseExecutableStatement (void)
{
    if ((doesMatch(current,"READ")) || (doesMatch(current,"READLN")))
        parseReadStatement();
    else if ((doesMatch(current,"WRITE")) || (doesMatch(current,"WRITELN")))
        parseWriteStatement();
    else if (doesMatch(current,"IDENTIFIER"))
        parseAssignmentStatement();
    else if (doesMatch(current,"CALL"))
        parseCallStatement();
    else if (doesMatch(current,"RETURN"))
        parseReturnStatement();
    else if (doesMatch(current,"GOTO"))
        parseGotoStatement();
    else if (doesMatch(current,";"))
        parseEmptyStatement();
    else if (doesMatch(current,"DO"))
        parseCompoundStatement();
    else if (doesMatch(current,"IF"))
        parseIfStatement();
    else if(doesMatch(current,"WHILE"))
        parseWhileStatement();
    else if (doesMatch(current,"CASE"))
        parseCaseStatement();
    else if (doesMatch(current,"FOR"))
        parseForStatement();
    else if (doesMatch(current,"NEXT"))
        parseNextStatement();
    else if (doesMatch(current,"BREAK"))
        parseBreakStatement();
    else
        SyntaxError
                (current.getLineNo(),"<executable_statement>",current.getToken());
}

void    parseReadStatement (void)
// < read_statement > --> { READ | READLN } !set_read_flag
//                        < lvals > !emit_read_ln ;
{
    bool ln_flag;
    if (doesMatch(current,"READ"))
        ln_flag = false;
    else
        ln_flag = true;
    current = getNext();
    parseActualArgs("read");
    if (ln_flag)
        emitReadLn();
    mustMatch(current,";");
}

void    parseWriteStatement (void)
// < write_statement > --> { WRITE | WRITELN } !set_write_flag
//                         < actual_args > !emit_write_ln ;
{
    bool write_flag = true;
    bool ln_flag;
    if (doesMatch(current,"WRITE"))
        ln_flag = false;
    else
        ln_flag = true;
    current = getNext();
    parseActualArgs("write");
    if (ln_flag)
        emitWriteLn();
    mustMatch(current,";");
}

void    parseAssignmentStatement (void)
// < assignment_statement > --> < lval > !emit_save_addr := < expression >
//                              [ !promote | !demote ] !emit_assign ;
{
    string lident;
    string ltype = parseLval(lident);
    if (isNamedConstant(lident))
        SemanticsError(current.getLineNo(),99,"cannot change named constant (" + lident + ")");
    emitSaveAddr();
    mustMatch(current,":=");
    string etype = parseExpression();
    if (ltype > etype)
    {
        if (!promote(etype,ltype,"[STACK]"))
            SemanticsError
                    (current.getLineNo(),41,etype + " --> " + ltype);
    }
    else if (ltype < etype)
    {
        if (!demote(etype,ltype,"[STACK]"))
            SemanticsError
                    (current.getLineNo(),42,etype + " --> " + ltype);
    }
    else // (ltype = etype)
        ;					// do nothing!
    if (element(ltype,ATOMICTYPE))
        emitAssign();
    else
    {
        int loc = search(global_type_table,ltype);
        int nbytes = global_type_table[loc].getSize();
        emitStructAssign(nbytes);
    }
    mustMatch(current,";");
}

void    parseCallStatement (void)
// < call_statement > --> CALL IDENTIFIER < actual_args > !emit_call ;
{
    mustMatch(current,"CALL");
    mustMatch(current,"IDENTIFIER",false);
    string proc = current.getValue();
    current = getNext();
    if (proc == "MAIN")
        SemanticsError(current.getLineNo(),31);
    int loc = search(global_proc_table,proc);
    if (loc < 0)
        SemanticsError(current.getLineNo(),25,proc);
    PLtype params = global_proc_table[loc].getParameters();
    string rtype = global_proc_table[loc].getReturnType();
    if (rtype != "")
        SemanticsError(current.getLineNo(),32,proc);
    parseActualArgs(proc);
    int no_params = params.size();
    emitCall(proc,no_params,"");
    mustMatch(current,";");
}

void	parseReturnStatement (void)
// < return_statement > --> RETURN [ < expression > ] ;
{
    mustMatch(current,"RETURN");
    int loc = search(global_proc_table,current_proc);
    string rtype = global_proc_table[loc].getReturnType();
    if ((doesMatch(current,";")) && (rtype != ""))
        SemanticsError(current.getLineNo(),33,current_proc);
    else if ((!doesMatch(current,";")) && (rtype == ""))
        SemanticsError(current.getLineNo(),35,current_proc);
    else if (!doesMatch(current,";"))
    {
        string etype = parseExpression();
        if (rtype > etype)
        {
            if (!promote(etype,rtype,"[STACK]"))
                SemanticsError
                        (current.getLineNo(),41,etype + " --> " + rtype);
        }
        else if (rtype < etype)
        {
            if (!demote(etype,rtype,"[STACK]"))
                SemanticsError
                        (current.getLineNo(),42,etype + " --> " + rtype);
        }
        else // (rtype = etype)
            ;					// do nothing!
    }
    else
        ;
    emitReturn(current_proc,rtype);
    mustMatch(current,";");
}

void    parseGotoStatement (void)
// < goto_statement > --> GOTO < label > !emit_goto ;
{
    string label;
    mustMatch(current,"GOTO");
    parseLabel(label);
    emitGoto(label);
    mustMatch(current,";");
}

void    parseEmptyStatement (void)
// < empty_statement > --> ;
{
    emitEmpty();
    mustMatch(current,";");
}

void	parseCompoundStatement (void)
// < compound_statement > --> DO ; < statement_list > END ;
{
    mustMatch(current,"DO");
    mustMatch(current,";");
    parseStatementList();
    mustMatch(current,"END");
    mustMatch(current,";");
}

void    parseIfStatement (void)
// < if_statement > --> IF ( < comparison > ) !emit_test
//                      THEN < statement > !emit_goto !emit_label
//                      <else_clause > !emit_label
{
    string ctype;
    string labelElse = getLabel();
    string labelEndif = getLabel();
    mustMatch(current,"IF");
    mustMatch(current,"(");
    ctype = parseComparison();
    if ((ctype != "BOOL") && (!demote(ctype,"BOOL","[STACK]")))
        SemanticsError(current.getLineNo(),42,ctype + " -- > BOOL");
    mustMatch(current,")");
    emitTest(labelElse);
    mustMatch(current,"THEN");
    parseStatement();
    emitGoto(labelEndif);
    emitLabel(labelElse);
    parseElseClause();
    emitLabel(labelEndif);
}

void	parseElseClause (void)
// < else_clause > --> [ ELSE < statement > ]
{
    if (!doesMatch(current,"ELSE"))
        return;
    else
        current = getNext();
    parseStatement();
}

void    parseWhileStatement (void)
// < while_statement > --> WHILE ( !emit_label < comparison > ) !emit_test
//                         < statement > !emit_goto !emit_label
{
    string ctype;
    string labelDone = getLabel();
    string labelRepeat = getLabel();
    break_stack.push(labelDone);
    next_stack.push(labelRepeat);
    mustMatch(current,"WHILE");
    mustMatch(current,"(");
    emitLabel(labelRepeat);
    ctype = parseComparison();
    if ((ctype != "BOOL") && (!demote(ctype,"BOOL","[STACK]")))
        SemanticsError(current.getLineNo(),42,ctype + " -- > BOOL");
    mustMatch(current,")");
    emitTest(labelDone);
    parseStatement();
    emitGoto(labelRepeat);
    emitLabel(labelDone);
    break_stack.pop();
    next_stack.pop();
}

void	parseCaseStatement (void)
// < case_statement > --> CASE ( < expression > ) !emit_init_case < case_list >
{
    string ctype;
    string labelDone = getLabel();
    mustMatch(current,"CASE");
    mustMatch(current,"(");
    ctype = parseExpression();
    if (ctype != "INT")
        SemanticsError(current.getLineNo(),51,ctype);
    mustMatch(current,")");
    emitInitCase();
    parseCaseList(labelDone);
}

void	parseCaseList (const string& labelDone)
// < case_list > --> < unary > NUMBER !emit_test_case : < statement >
//                   !emit_goto !emit_label < case_list > !emit_label
//                   | DEFAULT : < statement > !emit_label
{
    while (!doesMatch(current,"DEFAULT"))
    {
        string labelNext = getLabel();
        bool neg = parseUnary();
        mustMatch(current,"NUMBER",false);
        string testValue = current.getValue();
        current = getNext();
        if (neg)
            testValue = "-" + testValue;
        mustMatch(current,":");
        emitTestCase(testValue,labelNext);
        parseStatement();
        emitGoto(labelDone);
        emitLabel(labelNext);
    }
    mustMatch(current,"DEFAULT");
    {
        mustMatch(current,":");
        parseStatement();
    }
    emitLabel(labelDone);
}

void	parseForStatement (void)
// < for_statement > --> FOR IDENTIFIER := < expression > { TO | DOWNTO } < expression
//                       !emit_init_for !emit_label !emit_test_for
//                       < statement >
//                       !emit_label !emit_increment_for !emit_label
{
    mustMatch(current,"FOR");
    mustMatch(current,"IDENTIFIER",false);
    string var_name = current.getValue();
    current = getNext();
    string var_type;
    string addr;
    bool ptr;
    getVarInfo(var_name,var_type,addr,ptr);
    string step;
    if (var_type == "")
        SemanticsError(current.getLineNo(),21,var_name);
    if (var_type != "INT")
        SemanticsError(current.getLineNo(),52,var_name);
    if (isNamedConstant(var_name))
        SemanticsError(current.getLineNo(),99,"cannot change named constant (" + var_name + ")");
    mustMatch(current,":=");
    string etype = parseExpression();
    if (etype != "INT")
        SemanticsError(current.getLineNo(),53,etype);
    string labelTest = getLabel();
    string labelNext = getLabel();
    string labelDone = getLabel();
    emitInitFor(addr,ptr);
    if ((doesMatch(current,"TO")) || (doesMatch(current,"DOWNTO")))
    {
        step = current.getToken();
        current = getNext();
    }
    else
        SyntaxError(current.getLineNo(),"TO / DOWNTO",current.getToken());
    etype = parseExpression();
    if (etype != "INT")
        SemanticsError(current.getLineNo(),54,etype);
    break_stack.push(labelDone);
    next_stack.push(labelNext);
    emitLabel(labelTest);
    emitTestFor(addr,ptr,step,labelDone);
    parseStatement();
    emitLabel(labelNext);
    emitIncrementFor(addr,ptr,step,labelTest);
    emitDoneFor(labelDone);
    break_stack.pop();
    next_stack.pop();
}

void    parseNextStatement (void)
// < next_statement > --> NEXT !emit_goto ;
{
    mustMatch(current,"NEXT");
    mustMatch(current,";");
    string label = next_stack.top();
    emitGoto(label);
}

void    parseBreakStatement (void)
// < break_statement > --> BREAK !emit_goto ;
{
    mustMatch(current,"BREAK");
    mustMatch(current,";");
    string label = break_stack.top();
    emitGoto(label);
}

string	parseExpression (void)
// < expression > --> < comparison >
{
    string etype = parseComparison();
    return etype;
}

string	parseComparison (void)
// < comparison > --> < simple_expression >
//                    [ < compop > < simple_expression > !emit_compop ]
{
    string atype = parseSimpleExpression();
    if (!element(current.getValue(),COMPOP))
        return atype;
    string op = current.getValue();
    current = getNext();
    string btype = parseSimpleExpression();
    string ctype = resolveType(atype,btype);
    emitCompop(op,ctype);
    return "BOOL";
}

string	parseSimpleExpression (void)
//  < simple_expression > --> < unary > < term > !emit_neg
//                            { < addop > < term > !emit_addop }*
{
    bool neg_flag = parseUnary();
    string atype = parseTerm();
    if (neg_flag)
        emitNeg(atype);
    while (element(current.getValue(),ADDOP))	// for tail recursion
    {
        string op = current.getValue();
        current = getNext();
        string btype = parseTerm();
        string ctype = resolveType(atype,btype);
        emitAddop(op,ctype);
        atype = ctype;
    }
    return atype;
}

string	parseTerm (void)
// < term > --> < factor > [ < mulop > < factor > !emit_mulop }*
{
    string atype = parseFactor();
    while (element(current.getValue(),MULOP))	// for tail recursion
    {
        string op = current.getValue();
        current = getNext();
        string btype = parseFactor();
        string ctype = resolveType(atype,btype);
        emitMulop(op,ctype);
        atype = ctype;
    }
    return atype;
}

string	parseFactor (void)
// < factor > --> < const > !emit_literal | < func_ref >
//                | < lval > !emit_rval | ( < expression > )
//                | NOT < factor > !emit_not
// semantics can resolve differences: < const >, < lval >, < func_ref >
{
    if (doesMatch(current,"IDENTIFIER"))
    {
        string lident;
        // is the IDENTIFIER a local variable?
        int loc = search(local_symbol_table,current.getValue());
        if (loc >= 0)
        {
            string ltype = parseLval(lident);
            if (element(ltype,ATOMICTYPE))
                emitRval(ltype);
            else
                emitRval("STRUCT");
            return ltype;
        }
        // is the IDENTIFIER a global variable?
        loc = search(global_symbol_table,current.getValue());
        if (loc >= 0)
        {
            string ltype = parseLval(lident);
            if (element(ltype,ATOMICTYPE))
                emitRval(ltype);
            else
                emitRval("STRUCT");
            return ltype;
        }
        // is the IDENTIFIER a function reference?
        loc = search(global_proc_table,current.getValue());
        if (loc >= 0)
        {
            string ltype =  parseFuncRef(loc);
            return ltype;
        }
        SemanticsError(current.getLineNo(),99);
    }
    else if (doesMatch(current,"("))
    {
        current = getNext();
        string etype = parseExpression();
        mustMatch(current,")");
        return etype;
    }
    else if (doesMatch(current,"NOT"))
    {
        current = getNext();
        string ftype = parseFactor();
        emitNOT();
        return ftype;
    }
    else
    {
        string c_label;
        string c_type;
        string c_value;
        parseConst(c_type,c_value);
        // !save_const_item
        int loc = search(global_literal_table,c_value);
        if (loc >= 0)
            c_label = global_literal_table[loc].getLabel();
        else
        {
            c_label = getLabel();
            GLTentry data(c_label,c_type,c_value);
            insert(global_literal_table,data);
        }
        emitLiteral(c_label,c_type,c_value);
        return c_type;
    }
}

bool    parseUnary (void)
// < unary > --> [ + | - ]
{
    bool neg = false;
    if (doesMatch(current,"+"))
        current = getNext();
    else if (doesMatch(current,"-"))
    {
        neg = true;
        current = getNext();
    }
    else // no unary sign!
        ;
    return neg;
}

string	parseLval (string& lident)
// < lval > --> IDENTIFIER < qualifier > !emit_lval
{
    mustMatch(current,"IDENTIFIER",false);
    lident = current.getValue();
    current = getNext();
    string ltype;
    string addr;
    bool ptr;
    getVarInfo(lident,ltype,addr,ptr);
    if (ltype == "")
        SemanticsError(current.getLineNo(),21,lident);
    emitLval(addr,ptr);
    string qtype = parseQualifier(ltype);
    return qtype;
}

string    parseQualifier (const string& ltype)
// < qualifier > --> { [ < expression > !emit_array_ref ]
//                   |  . IDENTIFIER !emit_record_ref }*
{
    string qtype = ltype;
    bool loop = false;
    if ((doesMatch(current,"[")) || (doesMatch(current,".")))
        loop = true;
    while (loop)
    {
        loop = false;
        int loc = search(global_type_table,qtype);
        string struct_type = global_type_table[loc].getStruct();
        if (doesMatch(current,"["))
        {
            if (struct_type != "ARRAY")
                break;
            else
                current = getNext();
            ARRpair arrInfo = global_type_table[loc].getArrayInfo();
            string atype = arrInfo.getElementType();
            loc = search(global_type_table,atype);
            int asize = global_type_table[loc].getSize();
            // emit array reference
            emitLine("","push","SPTR","save base address");
            string etype = parseExpression();
            if (etype != "INT")
                SemanticsError(current.getLineNo(),52,etype);
            emitLine("","pop","INDEX","get array index");
            emitLine("","pop","SPTR","recover base address");
            emitLine("","imul","INDEX," + to_string(asize),
                     "multiply by element size");
            emitLine("","lea","SPTR,[SPTR+INDEX]","array element");
            mustMatch(current,"]");
            qtype = atype;
        }
        else // (doesMatch(current,"."))
        {
            if (struct_type != "RECORD")
                break;
            else
                current = getNext();
            string fname;
            RECtype recInfo = global_type_table[loc].getRecordInfo();
            mustMatch(current,"IDENTIFIER",false);
            fname = current.getValue();
            current = getNext();
            loc = search(recInfo,fname);
            RECentry finfo = retrieve(recInfo,loc);
            string ftype = finfo.getType();
            int foffset = finfo.getOffset();
            string address = "[SPTR+" + to_string(foffset) + "]";
            // emit record reference
            emitLine("","lea","SPTR," + address,"record item");
            qtype = ftype;
        }
        if ((doesMatch(current,"[",false)) || (doesMatch(current,".",false)))
            loop = true;
    }
    return qtype;
}

string  parseFuncRef (int loc)
//  < func_ref > --> IDENTIFIER < actual_args >
{
    current = getNext();
    GPTentry item = retrieve(global_proc_table,loc);
    string proc = item.getIdentifier();
    string ret_type = item.getReturnType();
    if (ret_type == "")
        SemanticsError(current.getLineNo(),43,proc);
    PLtype parameters = item.getParameters();
    int no_params = parameters.size();
    parseActualArgs(proc);
    emitCall(proc,no_params,ret_type);
    return ret_type;
}

//	SUPPORT ALGORITHMS  ***************************************************

void    mustMatch (const Lexeme& L,const string& s,bool advance)
{
    if (matchLexeme(L,s))
    {
        if (advance)
            current = getNext();
    }
    else
        SyntaxError(L.getLineNo(),s,L.getToken());
}

bool    doesMatch (const Lexeme& L,const string& s,bool advance)
{
    if (matchLexeme(L,s))
    {
        if (advance)
            current = getNext();
        return true;
    }
    else
        return false;
}

bool	element (const string& x,const set<string>& S)
{
    return S.find(x) != S.end();
}

void	initTables (void)
{
    GSTentry stemp("L_TEMP","REAL");
    global_symbol_table.push_back(stemp);

    GTTentry data1("BOOL",8,"ATOM");
    GTTentry data2("INT",8,"ATOM");
    GTTentry data3("REAL",8,"ATOM");
    GTTentry data4("STRING",-1,"ATOM");
    global_type_table.push_back(data1);
    global_type_table.push_back(data2);
    global_type_table.push_back(data3);
    global_type_table.push_back(data4);

    GPTentry pmain("MAIN",empty_pl,"INT");
    global_proc_table.push_back(pmain);
}

bool	isGlobalDcl (const string& ident)
{
    if (search(global_symbol_table,ident) >= 0)
        return true;
    else if (search(global_type_table,ident) >= 0)
        return true;
    else if (search(global_proc_table,ident) >= 0)
        return true;
    else
        return false;
}

bool	isNamedConstant (const string& ident)
{
    int loc = search(global_symbol_table,ident);
    if (loc < 0)
        return false;
    else
        return global_symbol_table[loc].isConstant();
}

void	displayGlobalTables (void)
{
    int sz = global_symbol_table.size();
    if (sz > 0)
    {
        cout << endl;
        cout << "; GLOBAL SYMBOL  TABLE" << endl;
    }
    for (int i = 0;i < sz;++i)
        display(global_symbol_table[i]);

    sz = global_type_table.size();
    if (sz > 0)
    {
        cout << endl;
        cout << "; GLOBAL TYPE    TABLE" << endl;
    }
    for (int i = 0;i < sz;++i)
        display(global_type_table[i]);

    sz = global_proc_table.size();
    if (sz > 0)
    {
        cout << endl;
        cout << "; GLOBAL PROC    TABLE" << endl;
    }
    for (int i = 0; i < sz;++i)
        display(global_proc_table[i]);

    sz = global_literal_table.size();
    if (sz > 0)
    {
        cout << endl;
        cout << "; GLOBAL LITERAL TABLE" << endl;
    }
    for (int i = 0;i < sz;++i)
        display(global_literal_table[i]);
}

void	displayLocalTables (const string& proc)
{
    int sz = local_symbol_table.size();
    if (sz > 0)
    {
        cout << endl;
        cout << "; LOCAL SYMBOL TABLE: " + proc << endl;
    }
    for (int i = 0;i < sz;++i)
        display(local_symbol_table[i]);

    sz = local_label_table.size();
    if (sz > 0)
    {
        cout << endl;
        cout << "; LOCAL LABEL  TABLE" << endl;
    }
    for (int i = 0;i < sz;++i)
        display(local_label_table[i]);

    cout << endl;
}

void	resetLocalTables (void)
{
    local_symbol_table.clear();
    local_label_table.clear();
}

void    getVarInfo (const string& lident,string& ltype,string& addr,bool& ptr)
{
    int loc;
    loc = search(local_symbol_table,lident);
    if (loc >= 0)
    {
        LSTentry data = local_symbol_table[loc];
        ltype = data.getType();
        addr = "FRAME" + data.getOffset();
        ptr = (data.getCallby() == "VAR");
        return;
    }
    loc = search(global_symbol_table,lident);
    if (loc >= 0)
    {
        GSTentry data = global_symbol_table[loc];
        ltype = data.getType();
        addr = "_" + lident + "_";
        ptr = false;
        return;
    }
    ltype = "";
    addr = "";
    ptr = false;
    return;
}

string	resolveType(const string& atype,const string& btype)
{
    if (atype == btype)
        return atype;
    else if ((!element(atype,ATOMICTYPE)) || (!element(btype,ATOMICTYPE)))
        SemanticsError(current.getLineNo(),43,atype + " <--> " + btype);
    else if (atype > btype)
        if (promote(btype,atype,"[STACK]"))
            return atype;
        else
            SemanticsError(current.getLineNo(),41,btype + " --> " + atype);
    else // (atype < btype)
    if (promote(atype,btype,"[STACK+8]"))
        return btype;
    else
        SemanticsError(current.getLineNo(),41,atype + " --> " + btype);
}

int	calcAlignedStorage(int actual_size)
{
    int q = actual_size / 16;
    int r = actual_size % 16;
    if (r > 0)
        ++q;
    int aligned_size = 16*q;
    return aligned_size;
}
