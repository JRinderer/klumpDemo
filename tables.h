//
// Created by jrinder on 3/14/18.
//

#ifndef KLUMPDEMO_TABLES_H
#define KLUMPDEMO_TABLES_H

#include <string>
#include <vector>
#include <stack>
#include <utility>

using namespace std;

/* ------------------------ Table Declarations -----------------------*/

// definition of entries into various global tables
class	GSTentry;
class	GTTentry;
class	GLTentry;
class	GPTentry;

// definition of specialized entries into type tables
class	ARRpair;				// array information
class	RECentry;
typedef vector<RECentry>	RECtype;	// record information

// definition of specialized entries into proc tables
class	PLentry;
typedef	vector<PLentry>		PLtype;		// parameter list
const	PLtype			empty_pl;

// definition of entries into various local tables
class	LSTentry;
class	LLTentry;

// definition of various global tables
typedef	vector<GSTentry>	GSTtype;	// global symbol table
typedef vector<GTTentry>	GTTtype;	// global type   table
typedef vector<GLTentry>	GLTtype;	// global label  table
typedef vector<GPTentry>	GPTtype;	// global proc   table

// definition of various local tables
typedef vector<LSTentry>        LSTtype;        // local  symbol table
typedef vector<LLTentry>        LLTtype;        // local  label  table

// definition of global stack: for implementation of BREAK and NEXT
typedef	stack<string>		GStype;		// global stack


// define the class GSTentry for data to be stored in global symbol table

class	GSTentry
{
private:
    string		_sident;	// ID
    string		_stype;		// atomic/named/PROC/ARRAY/RECORD
    bool		_sconst;	// named constant (T/F)
    string		_svalue;	// named constant value
public:
    GSTentry (const string& id,const string& typ,bool cnst = false,
              const string& val = "")
            : _sident(id),_stype(typ),_sconst(cnst),_svalue(val)
    {  }

    string		getIdentifier (void) const
    { return _sident; }

    string		getType (void) const
    { return _stype; }

    bool		isConstant (void) const
    { return _sconst; }

    string		getValue (void) const
    { return _svalue; }
};


// define the class ARRpair for array information
// to be stored in global type table

class	ARRpair
{
private:
    int		_ub;		// index range [ 0 .. (ub-1) ]
    string		_etype;		// element type

public:
    ARRpair (int ub = 0,const string& et = "")
            : _ub(ub),_etype(et)
    {  }

    int	getUpperBound (void) const
    { return _ub; }

    string	getElementType (void) const
    { return _etype; }
};

const	ARRpair		empty_ai;


// define the class RECentry for record information
// to be stored in global type table

class	RECentry
{
private:
    string		_fname;		// field name
    string		_ftype;		// field type
    int		_foffset;	// field offset

public:
    RECentry (const string& name,const string& typ,int off)
            : _fname(name),_ftype(typ),_foffset(off)
    {  }

    string	getName (void) const
    { return _fname; }

    string	getType (void) const
    { return _ftype; }

    int	getOffset (void) const
    { return _foffset; }
};

const	RECtype		empty_ri;


// define the class GTTentry for data to be stored in global type table

class	GTTentry
{
private:
    string		_tident;	// ID
    int		_tsize;		// type size in bytes
    string		_tstruct;	// atom / array / record
    ARRpair		_arrinfo;	// array information
    RECtype		_recinfo;	// record information

public:
    GTTentry (const string& id,int tsz,const string& str = "ATOM",
              const ARRpair& ai = empty_ai,const RECtype& ri = empty_ri)
            : _tident(id),_tsize(tsz),_tstruct(str),_arrinfo(ai),_recinfo(ri)
    {  }

    string		getIdentifier (void) const
    { return _tident; }

    int		getSize (void) const
    { return _tsize; }

    string		getStruct (void) const
    { return _tstruct; }

    ARRpair		getArrayInfo (void) const
    { return _arrinfo; }

    RECtype		getRecordInfo (void) const
    { return _recinfo; }
};


// define the class GLTentry for data to be stored in global literal table

class	GLTentry
{
private:
    string          _clabel;	// literal internal label
    string		_ctype;		// literal data type
    string		_cvalue;	// literal data value

public:
    GLTentry (const string& lbl,const string& typ,const string& val)
            : _clabel(lbl),_ctype(typ),_cvalue(val)
    {  }

    string          getLabel (void) const
    { return _clabel; }

    string          getType (void) const
    { return _ctype; }

    string          getValue (void) const
    { return _cvalue; }
};


// define the class GPTentry for data to be stored in global proc table

class	GPTentry
{
private:
    string		_pident;	// proc identifier
    PLtype		_pparams;	// proc parameters
    string		_pret_type;	// proc return type (if any)

public:
    GPTentry (const string& id,const PLtype& pl = empty_pl,
              const string& rt = "")
            : _pident(id),_pparams(pl),_pret_type(rt)
    {  }

    string		getIdentifier (void) const
    { return _pident; }

    PLtype		getParameters (void) const
    { return _pparams; }

    string		getReturnType (void) const
    { return _pret_type; }
};



// define the class LSTentry for data to be stored in local symbol table

class	LSTentry
{
private:
    string          _sident;        // ID
    string          _stype;         // atomic/named/PROC/ARRAY/RECORD
    string          _soffset;       // offset from RBP
    string          _scallby;       // call by: VAR or VAL
public:
    LSTentry (const string& id,const string& typ,const string& off,
              const string& cb = "VAL")
            : _sident(id),_stype(typ),_soffset(off),_scallby(cb)
    {  }

    string          getIdentifier (void) const
    { return _sident; }

    string          getType (void) const
    { return _stype; }

    string          getOffset (void) const
    { return _soffset; }

    string          getCallby (void) const
    { return _scallby; }
};


// define the class LLTentry for data to be stored in local label table

class	LLTentry
{
private:
    string          _lnumber;       // numeric label
    string          _llabel;        // internal label

public:
    LLTentry (const string& num,const string& lbl)
            : _lnumber(num),_llabel(lbl)
    {  }

    string  getNumber (void) const
    { return _lnumber; }

    string  getLabel (void) const
    { return _llabel; }
};


// define the class PLentry for data to be stored in parameter list

class	PLentry
{
private:
    string		_pident;	// parameter identifier
    string		_ptype;		// parameter type
    string		_pcallby;	// call by VAL or VAR

public:
    PLentry (const string& id,const string& typ,const string& cb = "VAL")
            : _pident(id),_ptype(typ),_pcallby(cb)
    {  }

    string	getIdentifier (void) const
    { return _pident; }

    string	getType (void) const
    { return _ptype; }

    string	getCallby (void) const
    { return _pcallby; }
};


/* ------------------------ Table Subroutines ---------------------- */

int		search (const GSTtype&,const string&);
int		search (const GTTtype&,const string&);
int		search (const GLTtype&,const string&);
int		search (const GPTtype&,const string&);
int             search (const LSTtype&,const string&);
int             search (const LLTtype&,const string&);
int		search (const PLtype&,const string&);
int		search (const RECtype&,const string&);

bool		insert (GSTtype&,const GSTentry&);
bool		insert (GTTtype&,const GTTentry&);
bool		insert (GLTtype&,const GLTentry&);
bool		insert (GPTtype&,const GPTentry&);
bool		insert (LSTtype&,const LSTentry&);
bool		insert (LLTtype&,const LLTentry&);
bool		insert (PLtype&,const PLentry&);
bool		insert (RECtype&,const RECentry&);

GSTentry	retrieve (const GSTtype&,int);
GTTentry	retrieve (const GTTtype&,int);
GLTentry	retrieve (const GLTtype&,int);
GPTentry	retrieve (const GPTtype&,int);
LSTentry	retrieve (const LSTtype&,int);
LLTentry	retrieve (const LLTtype&,int);
PLentry		retrieve (const PLtype&,int);
RECentry	retrieve (const RECtype&,int);

void		display (const GSTentry&);
void		display (const GTTentry&);
void		display (const GLTentry&);
void		display (const GPTentry&);
void		display (const LSTentry&);
void		display (const LLTentry&);
void		display (const PLtype&);
void		display (const ARRpair&);
void		display (const RECtype&);


#endif //KLUMPDEMO_TABLES_H
