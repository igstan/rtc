#ifndef TC_ID_H /* { */
#define TC_ID_H

#include <stdio.h>

//------------------------------------------------------

class AO;
class AOId;
class AOStringLit;
class AOMalloc;
class TCtype;

class ID
{
  public:
    friend class IDlist;
    enum stClass { AUTO, HEAP, STATIC };

    unsigned int getkey() const { return key; }
    const char * getname() { return name; }
    enum stClass getSC() const { return stclass; }
    AOId& get_AOId(bool mark_zeroed = false);
    AOStringLit& get_AOStringLit();
    AOMalloc& get_AOMalloc(bool is_alloca = false, bool mark_zeroed = false);

  private:
    ID();
    ID(char * n, char sc, bool isl = false);
    ~ID();
    void setkey(unsigned int k) { key = k; }
    unsigned int key;
    char * name;
    AO * ao;
    bool islib;
    enum stClass stclass;
};

//------------------------------------------------------

class IDmap
{
  public:
    IDmap(unsigned int size = 1024);
    ~IDmap();
    void debug_dump(FILE * os);
    void map(unsigned int key, ID& id);
    ID * lookup(unsigned int key);

  private:
    void grow(unsigned int upto);
    ID ** table;
    unsigned int size;
};

//------------------------------------------------------

class IDlist
{
  public:
    IDlist(unsigned int size = 1023);
    ~IDlist();
    void debug_dump(FILE * os);
    void output_aliases(FILE * os, char prefix);
    ID& getID(char * str, char sc);
    unsigned int assignUIDs(unsigned int start);
    void traverseAOs(void (*fp)(AO& ao));
    ID * lookupByName(const char * str);
    void traverseIDs(void (*fp)(ID&));

  private:
    unsigned int hash(const char * str);
    class node {
      public:
        node(ID& i, node * n):id(i), next(n){}
        ID& id;
        node * next;
    };
    node ** table;
    unsigned int size;
};

//------------------------------------------------------

#endif /* } ifdef TC_ID_H */
