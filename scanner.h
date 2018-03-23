//
// Created by jrinder on 3/14/18.
//

#ifndef KLUMPDEMO_SCANNER_H
#define KLUMPDEMO_SCANNER_H

#include <string>
#include <vector>

using namespace std;

char	getChar		(void);
void	skipWhite	(void);
void	initScanner	(void);
bool	isKeyWord	(const string&);

// definition of a lexeme -- a token - value pair,
// augmented with a line number (useful in error messages)

class Lexeme
{
private:
    int		_lineno;
    string		_token;
    string		_value;
public:
    Lexeme (int lineno = 0,const string& tok = "",const string& val = "")
            : _lineno(lineno),_token(tok),_value(val)
    {};

    int     getLineNo (void) const
    { return _lineno; };

    string	getToken (void) const
    { return _token; };

    string	getValue (void) const
    { return _value; };
};

// scanner support algorithms

Lexeme	getNext (void);

bool	matchLexeme (const Lexeme&,const string&);

typedef	vector<Lexeme> LexList;


#endif //KLUMPDEMO_SCANNER_H
