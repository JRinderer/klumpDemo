#include <iostream>
#include <cstdlib>
#include <vector>
#include <utility>
#include <string>

#include "error.h"

using namespace std;

typedef pair<int,string>	SemErrorType;
vector<SemErrorType>		SemErrors =
        {
                {11,"duplicate global dcl"},
                {12,"duplicate local dcl"},
                {13,"duplicate const"},
                {14,"duplicate type"},
                {15,"duplicate proc"},
                {16,"duplicate field name"},
                {17,"duplicate parameter name"},

                {21,"undeclared identifier"},
                {23,"structure type expected"},
                {24,"undeclared type"},
                {25,"undeclared proc"},
                {26,"undeclared field name"},
                {27,"undeclared parameter name"},

                {31,"procedure MAIN is automatically CALLed"},
                {32,"FUNCTIONs are not CALLed"},
                {33,"FUNCTIONs do have RETURN value"},
                {34,"PROCEDUREs are CALLed"},
                {35,"PROCEDUREs do not have RETURN value"},
                {36,"too many actual arguments"},
                {37,"too few actual arguments"},
                {38,"type mismatch in CALL BY VAR"},

                {41,"cannot promote"},
                {42,"cannot demote"},
                {43,"types not NAME equivalent"},

                {51,"CASE expression is not INT"},
                {52,"INDEX variable must be INT"},
                {53,"START expression must be INT"},
                {54,"STOP expression must be INT"},

                {99,"not sure what you did!!"}
        };

string	SemErr (int en)
{
    for (int i = 0;i < SemErrors.size();++i)
        if (SemErrors[i].first == en)
            return SemErrors[i].second;
    return "undefined errorno!";
}

void	Abort (const string& s)
{
    cerr << s << endl;
    exit(EXIT_FAILURE);
}

void	SyntaxError (int ln,const string& exp,const string& fnd)
{
    Abort
            ("Syntax Error (line " + to_string(ln) + "): expected ( " + exp + " )"
             + " but found ( " + fnd + " )");
}

void	SemanticsError (int ln,int en,const string& s)
{
    Abort
            ("Semantics Error (~line " + to_string(ln) + "): "
             + SemErr(en) + " [ " + s + " ]");
}
