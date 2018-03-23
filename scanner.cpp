#include <iostream>
#include <cctype>
#include <cstdlib>
#include "scanner.h"

using namespace std;

char	Look;
int	old_lineno = 1;
int	lineno = 1;
bool	flag = false;

vector<string>  KeyWords =
        {
                "AND",     "ARRAY",   "ASCII",  "BEGIN",     "BREAK",     "CALL",
                "CASE",    "CSTRING", "CONST",  "DCL",       "DECIMAL",   "DEFAULT",
                "DO",      "DOWNTO",  "ELSE",   "END",       "FOR",       "GLOBAL",
                "GOTO",    "IF",      "INT",    "NEXT",      "NOT",       "NUMBER",
                "OF",      "OR",      "PROC",   "PROCEDURE", "READ",      "READLN",
                "REAL",    "RECORD",  "RETURN", "STRING",    "THEN",      "TO",
                "TYPE",    "VAR",     "WHILE",  "WRITE",     "WRITELN"
        };

char	getChar (void)
//	get next symbol from the input stream
{
    return cin.get();
}

void	skipWhite (void)
//	skip over any leading whitespace
//	however, keep track of carriage returns!
{
    while (isspace(Look))
    {
        if (Look == '\n')
        {
            ++lineno; flag = true;
        }
        Look = getChar();
    }
}

void	initScanner (void)
//	find the first non-whitespace character in the file
{
    Look = ' '; skipWhite();
}

bool	isKeyWord (const string& s)
//	check if a potential IDENTIFIER is a keyword in the language
{
    for (int i = 0;i < KeyWords.size();++i)
        if (KeyWords[i] == s)
            return true;
    return false;
}

Lexeme	getNext (void)
//	determine the next Lexeme triple
{
    string token;
    string value;

//	a mulligan jumps here
//	to continue after a comment string
    mulligan:

    if (isalpha(Look))					// identifiers
    {
        value = toupper(Look); Look = getChar();
        while (isalpha(Look) || isdigit(Look))
        {
            value += toupper(Look); Look = getChar();
        }
        if (isKeyWord(value))				// keywords
            token = value;
        else
            token = "IDENTIFIER";
        skipWhite();
    }

    else if (isdigit(Look))				// numbers
    {
        bool isdec = false;
        value = Look; Look = getChar();
        while (isdigit(Look))
        {
            value += Look; Look = getChar();
        }
        if (Look == '.')
        {
            isdec = true;
            value += Look; Look = getChar();
            while (isdigit(Look))
            {
                value += Look; Look = getChar();
            }
        }
        if (isdec)
            token = "DECIMAL";
        else
            token = "NUMBER";
        skipWhite();
    }

    else if (Look == '\'')				// ASCII constants
    {
        value = Look; Look = getChar();
        bool Done = false;
        while (!Done)
        {
            if (Look != '\'')
            {
                value += Look; Look = getChar();
            }
            else
            {
                value += Look; Look = getChar();
                Done = true;
                if (Look == '\'')
                {
                    value += Look; Look = getChar();
                    Done = false;
                }
            }
        }
        if (value.length() == 3)
            token = "ASCII";
        else
            token = "CSTRING";
        skipWhite();
    }

    else if (Look == '{')				// comments
    {
        bool Done = false;
        while (!Done)
        {
            Look = getChar();
            if (Look == '}')
                Done = true;
            if (Look == '\n')
            {
                ++lineno; flag = true;
            }
        }
        token = "COMMENT"; value = "";
        Look = getChar(); skipWhite();
        goto mulligan;
    }

    else if (Look == '.')				// periods
    {
        Look = getChar();
        value = ".";
        token = value; skipWhite();
    }

    else if (Look == ':')				// colons
    {
        value = Look; Look = getChar();
        if (Look == '=')					// assignments
        {
            value += Look; Look = getChar();
        }
        token = value; skipWhite();
    }

    else if ((Look == '=') || (Look == ';') || (Look == ',')
             || (Look == '#') || (Look == '+') || (Look == '-')
             || (Look == '*') || (Look == '/') || (Look == '%')
             || (Look == '(') || (Look == ')') || (Look == '[')
             || (Look == ']'))
    {
        value = Look; token = Look;			// single symbols
        Look = getChar(); skipWhite();
    }

    else if ((Look == '<') || (Look == '>'))		// comparisons
    {
        value = Look; Look = getChar();
        if (Look == '=')
        {
            value += Look; Look = getChar();
        }
        else if ((value == "<") && (Look == '>'))
        {
            value+= Look; Look = getChar();
        }
        token = value; skipWhite();
    }

    else
    {
        cerr << " ... unrecognized symbol encountered (" << Look << ")"
             << "  while scanning\n";
        exit(EXIT_FAILURE);
    }

    int ln;
    if (flag)
    {
        ln = old_lineno; old_lineno = lineno;
        flag = false;
    }
    else
        ln = lineno;
    Lexeme result(ln,token,value);
    return result;
}

//	test to check whether current lexeme's token is "tok"
bool	matchLexeme (const Lexeme& L,const string& tok)
{
    return (L.getToken() == tok);
}