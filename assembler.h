//
// Created by jrinder on 3/14/18.
//

#ifndef KLUMPDEMO_ASSEMBLER_H
#define KLUMPDEMO_ASSEMBLER_H

#include <string>
#include "tables.h"

using namespace std;

string	getLabel		(void);

void	emitBlankLine   	(void);
void	emitComment		(const string&);

void	emitDefine		(const string&,const string&);
void	emitEQU			(const string&,const string&);

void    emitLine                (const string&,const string&,
                                 const string&,const string& = "");

void	emitDefinitions		(void);

void	emitProlog		(void);
void	emitEpilog		(const GSTtype&,const GTTtype&,const GLTtype&);

void	emitProcProlog		(const string&,const string&);
void	emitProcEpilog		(const string&,const string&);

void	emitRead		(const string&);
void	emitReadLn		(void);
void	emitWrite		(const string&);
void	emitWriteLn		(void);

void	emitNeg			(const string&);

void	emitAND			(void);
void	emitOR			(void);
void	emitNOT			(void);

void	emitCompop		(const string&,const string&);
void	emitMulop		(const string&,const string&);
void	emitAddop		(const string&,const string&);

void	emitAssign		(void);
void	emitStructAssign	(int);

void	emitTest		(const string&);
void	emitLabel		(const string&);
void	emitEmpty		(void);

void	emitGoto		(const string&);
void	emitCall		(const string&,int,const string&);
void	emitReturn		(const string&,const string&);

void	emitInitCase		(void);
void	emitTestCase		(const string&,const string&);

void	emitInitFor		(const string&,bool);
void	emitTestFor		(const string&,bool,const string&,const string&);
void	emitIncrementFor	(const string&,bool,const string&,const string&);
void	emitDoneFor		(const string&);

void	emitLiteral		(const string&,const string&,const string&);
void	emitLval		(const string&,bool);
void	emitSaveAddr		(void);
void	emitRval		(const string&);

void	emitBSSData		(const GSTtype&,const GTTtype&);
void	emitDATAData		(const GSTtype&,const GLTtype&);

void	stackAlign		(void);
void	stackRestore		(void);

bool	promote			(const string&,const string&,const string&);
bool	demote			(const string&,const string&,const string&);


#endif //KLUMPDEMO_ASSEMBLER_H
