//
// Created by jrinder on 3/11/18.
//

#include <iostream>
#include <iomanip>
#include "tables.h"

using namespace std;

// search algorithms

int	search (const LSTtype& lst,const string& id)
{
    for (int i = 0;i < lst.size();++i)
        if (lst[i].getIdentifier() == id)
            return i;
    return -1;
}
int	search (const LLTtype& llt,const string& num)
{
    for (int i = 0;i < llt.size();++i)
        if (llt[i].getNumber() == num)
            return i;
    return -1;
}

int	search (const GSTtype& gst,const string& id)
{
    for (int i = 0;i < gst.size();++i)
        if (gst[i].getIdentifier() == id)
            return i;
    return -1;
}

int	search (const GTTtype& gtt,const string& id)
{
    for (int i = 0;i < gtt.size();++i)
        if (gtt[i].getIdentifier() == id)
            return i;
    return -1;
}

int	search (const GLTtype& glt,const string& lit)
{
    for (int i = 0;i < glt.size();++i)
        if (glt[i].getValue() == lit)
            return i;
    return -1;
}

int	search (const GPTtype& gpt,const string& id)
{
    for (int i = 0;i < gpt.size();++i)
        if (gpt[i].getIdentifier() == id)
            return i;
    return -1;
}

int	search (const PLtype& pl,const string& id)
{
    for (int i = 0;i < pl.size();++i)
        if (pl[i].getIdentifier() == id)
            return i;
    return -1;
}

int	search (const RECtype& fl,const string& id)
{
    for (int i = 0;i < fl.size();++i)
        if (fl[i].getName() == id)
            return i;
    return -1;
}

// insert algorithms

bool	insert (LSTtype& lst,const LSTentry& item)
{
    int loc = search(lst,item.getIdentifier());
    if (loc < 0)
    {
        lst.push_back(item);
        return true;
    }
    else
        return false;
}

bool	insert (LLTtype& llt,const LLTentry& item)
{
    int loc = search(llt,item.getNumber());
    if (loc < 0)
    {
        llt.push_back(item);
        return true;
    }
    else
        return false;
}

bool	insert (GSTtype& gst,const GSTentry& item)
{
    int loc = search(gst,item.getIdentifier());
    if (loc < 0)
    {
        gst.push_back(item);
        return true;
    }
    else
        return false;
}

bool	insert (GTTtype& gtt,const GTTentry& item)
{
    int loc = search(gtt,item.getIdentifier());
    if (loc < 0)
    {
        gtt.push_back(item);
        return true;
    }
    else
        return false;
}

bool	insert (GLTtype& glt,const GLTentry& item)
{
    int loc = search(glt,item.getValue());
    if (loc < 0)
    {
        glt.push_back(item);
        return true;
    }
    else
        return false;
}

bool	insert (GPTtype& gpt,const GPTentry& item)
{
    int loc = search(gpt,item.getIdentifier());
    if (loc < 0)
    {
        gpt.push_back(item);
        return true;
    }
    else
        return false;
}

bool	insert (PLtype& pl,const PLentry& item)
{
    int loc = search(pl,item.getIdentifier());
    if (loc < 0)
    {
        pl.push_back(item);
        return true;
    }
    else
        return false;
}

bool	insert (RECtype& fl,const RECentry& item)
{
    int loc = search(fl,item.getName());
    if (loc < 0)
    {
        fl.push_back(item);
        return true;
    }
    else
        return false;
}

// retrieve algorithms

LSTentry	retrieve (const LSTtype& lst,int loc)
{
    return lst[loc];
}

LLTentry	retrieve (const LLTtype& llt,int loc)
{
    return llt[loc];
}

GSTentry	retrieve (const GSTtype& gst,int loc)
{
    return gst[loc];
}

GTTentry	retrieve (const GTTtype& gtt,int loc)
{
    return gtt[loc];
}

GLTentry	retrieve (const GLTtype& glt,int loc)
{
    return glt[loc];
}

GPTentry	retrieve (const GPTtype& gpt,int loc)
{
    return gpt[loc];
}

PLentry		retrieve (const PLtype& pl,int loc)
{
    return pl[loc];
}

RECentry	retrieve (const RECtype& fl,int loc)
{
    return fl[loc];
}

// retrieve algorithms

void	display (const LSTentry& item)
{
    cout << setw(05) << left << ";"
         << setw(10) << left << item.getIdentifier()
         << setw(10) << left << item.getType()
         << setw(10) << left << item.getOffset()
         << setw(10) << left << item.getCallby()
         << endl;
}

void	display (const LLTentry& item)
{
    cout << setw(05) << left << ";"
         << setw(10) << left << item.getNumber()
         << setw(10) << left << item.getLabel()
         << endl;
}

void	display (const GSTentry& item)
{
    cout << setw(05) << left << ";"
         << setw(10) << left << item.getIdentifier()
         << setw(10) << left << item.getType()
         << setw(05) << left << item.isConstant()
         << setw(15) << left << "| " + item.getValue() + " |"
         << endl;
}

void	display (const GTTentry& item)
{
    string tstruct = item.getStruct();
    cout << setw(05) << left << ";"
         << setw(10) << left << item.getIdentifier()
         << setw(05) << left << item.getSize()
         << setw(10) << left << tstruct
         << endl;
    if (tstruct == "ARRAY")
        display (item.getArrayInfo());
    else if (tstruct == "RECORD")
        display (item.getRecordInfo());
    else // (tstruct == "ATOM")
        ;
}

void	display (const GLTentry& item)
{
    cout << setw(05) << left << ";"
         << setw(10) << left << item.getLabel()
         << setw(10) << left << item.getType()
         << setw(10) << left << item.getValue()
         << endl;
}

void	display (const GPTentry& item)
{
    PLtype params = item.getParameters();
    int no_params = params.size();
    cout << setw(05) << left << ";"
         << setw(10) << left << item.getIdentifier()
         << setw(25) << left << no_params
         << setw(10) << left << item.getReturnType()
         << endl;
    if (no_params > 0)
        display (params);
}

void	display (const PLtype& params)
{
    for (int i = 0;i < params.size();++i)
        cout << setw(10) << left << ";"
             << setw(10) << left << params[i].getIdentifier()
             << setw(10) << left << params[i].getType()
             << setw(10) << left << params[i].getCallby()
             << endl;
}

void	display (const ARRpair& info)
{
    cout << setw(10) << left << ";"
         << setw(10) << left << info.getUpperBound()
         << setw(10) << left << info.getElementType()
         << endl;
}

void	display (const RECtype& info)
{
    for (int i = 0;i < info.size();++i)
    {
        RECentry field = info[i];
        cout << setw(10) << left << ";"
             << setw(10) << left << field.getName()
             << setw(10) << left << field.getType()
             << setw(10) << left << field.getOffset()
             << endl;
    }
}


