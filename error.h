//
// Created by jrinder on 3/14/18.
//

#ifndef KLUMPDEMO_ERROR_H
#define KLUMPDEMO_ERROR_H

#include <string>

using namespace std;

void	Abort (const string&);

void	SyntaxError (int,const string&,const string&);

void	SemanticsError (int,int,const string& ="");


#endif //KLUMPDEMO_ERROR_H
