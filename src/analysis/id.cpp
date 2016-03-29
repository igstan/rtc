#include <stdio.h>
#include <string.h>

#include "id.h"
#include "ao.h"

//------------------------------------------------------
//- ID

ID::ID(char * n, char sc, bool isl)
: key(0), name(0), ao(0), islib(isl),
  stclass((sc=='A')?AUTO:((sc=='H')?HEAP:STATIC))
{
  name = new char[strlen(n)+1];
  strcpy(name,n);
}

ID::~ID()
{
  if(name) delete [] name;
  if(ao) delete ao;
}

AOId& ID::get_AOId(bool mark_zeroed)
{
  if(ao){
    if(ao->getKind() != AO::aoId){
      fprintf(stderr, "ERROR(ID::get_AOId): expecting AOId, but found ");
      ao->debug_dump(stderr);
      fprintf(stderr, "\n");
    }
    if(mark_zeroed){
      ao->is_zeroed = true;
    }
    return *(AOId *)ao;
  } else {
    ao = new AOId(*this, islib);
    if(mark_zeroed){
      ao->is_zeroed = true;
    }
    return *(AOId *)ao;
  }
}

AOStringLit& ID::get_AOStringLit()
{
  if(ao){
    if(ao->getKind() != AO::aoStringLit){
      fprintf(stderr, "ERROR(ID::get_AOStringLit): expecting AOStringLit, but found ");
      ao->debug_dump(stderr);
      fprintf(stderr, "\n");
    }
    return *(AOStringLit *)ao;
  } else {
    ao = new AOStringLit(*this, islib);
    return *(AOStringLit *)ao;
  }
}

AOMalloc& ID::get_AOMalloc(bool is_alloca, bool mark_zeroed)
{
  if(ao){
    if(ao->getKind() != AO::aoMalloc){
      fprintf(stderr, "ERROR(ID::get_AOMalloc): expecting aoMalloc, but found ");
      ao->debug_dump(stderr);
      fprintf(stderr, "\n");
    }
    ((AOMalloc *)ao)->is_alloca = is_alloca;
    if(mark_zeroed){
      ao->is_zeroed = true;
    }
    return *(AOMalloc *)ao;
  } else {
    ao = new AOMalloc(*this, islib);
    ((AOMalloc *)ao)->is_alloca = is_alloca;
    if(mark_zeroed){
      ao->is_zeroed = true;
    }
    return *(AOMalloc *)ao;
  }
}


//------------------------------------------------------
//- IDmap

IDmap::IDmap(unsigned int s)
: table(new (ID*)[s]), size(s)
{
  for(unsigned int i = 0; i < s; ++i) // zero-initialize
    table[i] = 0;
}

IDmap::~IDmap()
{
  delete [] table;
}

void IDmap::debug_dump(FILE * os)
{
  for(unsigned int i = 0; i < size; ++i)
    if(table[i])
      fprintf(os, "map(%d):[%s]\n", i, table[i]->getname());
}

void IDmap::map(unsigned int key, ID& id)
{
  if(key >= size) grow(key + 10);
  table[key] = &id;
}

ID * IDmap::lookup(unsigned int key)
{
  if(key >= size) return 0;
  else return table[key];
}

void IDmap::grow(unsigned int upto)
{
  unsigned int newsize;
  for(newsize = size; newsize <= upto; newsize *= 2);

  ID ** oldtab = table;
  table = new (ID*)[newsize];

  unsigned int i;
  for(i = 0; i < size; ++i) table[i] = oldtab[i];
  for(; i < newsize; ++i) table[i] = 0;

  size = newsize;
  delete [] oldtab;
}

//------------------------------------------------------
//- IDlist

IDlist::IDlist(unsigned int s)
: table(new (node*)[s]), size(s)
{
  for(unsigned int i = 0; i < size; ++i) // zero-initialize
    table[i] = 0;
}

IDlist::~IDlist()
{
  for(unsigned int i = 0; i < size; ++i){
    node * nptr = table[i];
    while(nptr){
      node * del = nptr;
      nptr = nptr->next;
      delete &del->id;
      delete del;
    }
  }
  delete [] table;
}

void IDlist::debug_dump(FILE * os)
{
  for(unsigned int i = 0; i < size; ++i)
    for(node * nptr = table[i]; nptr; nptr = nptr->next)
      fprintf(os, "ID(%d):[%s]\n", nptr->id.getkey(), nptr->id.getname());
}

void IDlist::traverseAOs(void (*fp)(AO& ao))
{
  for(unsigned int i = 0; i < size; ++i)
    for(node * nptr = table[i]; nptr; nptr = nptr->next)
      if(nptr->id.ao) nptr->id.ao->traverseAOs(fp);
}

void IDlist::traverseIDs(void (*fp)(ID&))
{
  for(unsigned int i = 0; i < size; ++i)
    for(node * nptr = table[i]; nptr; nptr = nptr->next)
      fp(nptr->id);
}

void IDlist::output_aliases(FILE * os, char prefix)
{
  for(unsigned int i = 0; i < size; ++i)
    for(node * nptr = table[i]; nptr; nptr = nptr->next)
      fprintf(os, "%c %d %s\n", prefix, nptr->id.getkey(), nptr->id.getname());
}

ID * IDlist::lookupByName(const char * str)
{
  node * nptr;
  for(nptr = table[hash(str)]; nptr; nptr = nptr->next)
    if(!strcmp(str, nptr->id.name))
      return &nptr->id;
  return 0;
}

ID& IDlist::getID(char * str, char sc)
{
  //-- first, check for library function prefix
  bool islib = false;
  if(!strncmp(str, "._rtclib_", 9)){
    islib = true;
    str += 8;
    *str = '.';
  }
  node ** nptr;
  for(nptr = &table[hash(str)];
      *nptr;
      nptr = &(*nptr)->next){
    int cmp = strcmp(str, (*nptr)->id.name);
    if(cmp == 0){
      if(islib) (*nptr)->id.islib = true;
      return (*nptr)->id;
    } else if(cmp < 0) break;
  }
  ID * newid = new ID(str, sc, islib);
  *nptr = new node(*newid, *nptr);
  return *newid;
}

unsigned int IDlist::assignUIDs(unsigned int start)
{
  for(unsigned int i = 0; i < size; ++i)
    for(node * nptr = table[i]; nptr; nptr = nptr->next)
      nptr->id.setkey(start++);
  return start;
}

unsigned int IDlist::hash(const char * str)
{
  unsigned int h = 0;
  for(const char * c = str; *c; ++c)
    h = 5*h + *c;
  return h % size;
}

//------------------------------------------------------
