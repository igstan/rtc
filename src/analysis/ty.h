#ifndef TC_TY_H /* { */
#define TC_TY_H

#include <stdio.h>

#include "suco.h"

//------------------------------------------------------

class TCtype
{
  public:
    enum tcKind {
	 tcVoid, tcZero, tcInt, tcChar, tcShort, tcLong, tcLongLong,
	 tcFloat, tcDouble, tcLongDouble, tcPointer,
	 tcStruct, tcUnion, tcArray,
	 tcBitField, tcFunction
	};
    static TCtype * stringToTy(char * str, char ** nptr); //- return should be cleaned up by calling deleteTy
    static void deleteTy(TCtype& ty);

    const char * kindString();
    virtual void debug_dump(FILE * os);
    char kindCode();
    tcKind getKind() const { return kind; }
    virtual void write_string_rep(FILE * os);
    bool equals(TCtype &t) const; //- equals: must be exactly equal
    bool equiv(TCtype &t) const; //- equiv: ignores pointer dest type, and empty functions match all

    virtual int countInstancesOf(TCtype& ty) const;	//- counts occurrences of "ty" (equiv or subtype) in this object
    virtual int SizeOf() const;				//- return sizeof for a "common" scheme

    //- list operations
    static suco_llist<TCtype *>& stringToTyList(char * str, char ** nptr); //- return should be cleaned up by calling deleteTyList
    static void deleteTyList(suco_llist<TCtype *>& ty);
    static void write_list_string_rep(suco_llist<TCtype *>& tl, FILE * os);
    static void debug_dump_list(suco_llist<TCtype *>& tl, FILE * os);
    static bool listEquals(suco_llist<TCtype *>& l1, suco_llist<TCtype *>& l2);
    static bool listEquiv(suco_llist<TCtype *>& l1, suco_llist<TCtype *>& l2);
    static TCtype * listEquivPrefix(suco_llist<TCtype *>& prefix_list, suco_llist<TCtype *>& full_list);

    static TCtype tcVoidType;
    static TCtype tcZeroType;
    static TCtype tcIntType;
    static TCtype tcCharType;
    static TCtype tcShortType;
    static TCtype tcLongType;
    static TCtype tcLongLongType;
    static TCtype tcFloatType;
    static TCtype tcDoubleType;
    static TCtype tcLongDoubleType;

  protected:
    TCtype(enum tcKind k) : kind(k) {}
    virtual bool cleanup() const { return false; } // indicates if class needs to be destroyed
    virtual ~TCtype() {}
  private:
    TCtype();
    enum tcKind kind;
};

//------------------------------------------------------

//- for { tcStruct, tcUnion }
class TCstructUnionType : public TCtype
{
  public:
    friend class TCtype;
    void debug_dump(FILE * os);
    void write_string_rep(FILE * os);
    virtual int countInstancesOf(TCtype& ty) const;	//- counts occurrences of "ty" (equiv or subtype) in this object
    virtual int SizeOf() const;				//- return sizeof for a "common" scheme
    suco_llist<TCtype *>& getTypeList() const { return tylist; }
  private:
    TCstructUnionType();
    ~TCstructUnionType() { delete &tylist; }
    bool cleanup() const { return true; }
    TCstructUnionType(enum tcKind k, suco_llist<TCtype *>& l) : TCtype(k), tylist(l) {}
    suco_llist<TCtype *>& tylist;
};

class TCfunctionType : public TCtype
{
  public:
    friend class TCtype;
    void debug_dump(FILE * os);
    void write_string_rep(FILE * os);
    virtual int countInstancesOf(TCtype& ty) const;
    virtual int SizeOf() const { return 0; }

    bool isEmpty() const { return (rty.getKind() == tcInt) && tylist.IsEmpty(); }
    TCtype& getReturnType() const { return rty; }
  private:
    TCfunctionType();
    ~TCfunctionType() { deleteTy(rty); delete &tylist; }
    TCfunctionType(TCtype &t, suco_llist<TCtype *>& l) : TCtype(tcFunction), rty(t), tylist(l) {}
    bool cleanup() const { return true; }
    TCtype &rty;
    suco_llist<TCtype *>& tylist;
};

class TCpointerType : public TCtype
{
  public:
    friend class TCtype;
    void debug_dump(FILE * os);
    void write_string_rep(FILE * os);
    virtual int countInstancesOf(TCtype& ty) const;
    // virtual int SizeOf() const;	//- use default version

    static TCpointerType tcVoidPtrType;

  private:
    TCpointerType();
    ~TCpointerType() { deleteTy(ty); }
    bool cleanup() const { return (this != &tcVoidPtrType); }
    TCpointerType(TCtype * t = 0) : TCtype(tcPointer), ty(t?(*t):tcVoidType) {}
    TCtype &ty;	//- NOTE: currently, typed pointers not supported
};

//-- for { tcArray, tcBitField }
class TCnumType : public TCtype
{
  public:
    friend class TCtype;
    void debug_dump(FILE * os);
    void write_string_rep(FILE * os);
    virtual int countInstancesOf(TCtype& ty) const;	//- counts occurrences of "ty" (equiv or subtype) in this object
    virtual int SizeOf() const;				//- return sizeof for a "common" scheme

    TCtype& getBaseType() const { return ty; }
    unsigned int getSize() const { return size; }
  private:
    TCnumType();
    ~TCnumType() { deleteTy(ty); }
    bool cleanup() const { return true; }
    TCnumType(enum tcKind k, unsigned int i, TCtype &t) : TCtype(k), size(i), ty(t) {}
    unsigned int size;
    TCtype &ty;
};

//------------------------------------------------------

class TClatType
{ //- "enriched" lattice type
  public:
    friend class TClatRootSet;

    enum latKind {    // encoding used in meet/join()!
				tclBottom=0,

	tclChar=101, tclShort=111, tclInt=121, tclLong=122, tclLongLong=131,
		tclFloat=141, tclDouble=151, tclLongDouble=161,
				tclPointer=171,

				tclZero=200,
				tclTop=300
	};
    TClatType(latKind k = tclTop) : kind(k) {}
    TClatType(TCtype& t) : kind(getLatKindFor(t)),
			   nodesLEthis() {}

    void debug_dump(FILE * os);

    //-- static functions to manipulate latKind values
    static latKind getLatKindFor(TCtype& t);

    static latKind Join(latKind k1, latKind k2);
    static latKind Meet(latKind k1, latKind k2);
    static bool LE(latKind k1, latKind k2) { return ((k1 == k2) || ((k1/100) < (k2/100))); }
    static bool GE(latKind k1, latKind k2) { return ((k1 == k2) || ((k1/100) > (k2/100))); }
    static void Debug_dump(latKind k, FILE * os);

    //-- basic operations
    void join(latKind k);
    void meet(latKind k);

    void setKind(latKind k) { kind = k; }
    latKind getKind() const { return kind; }

    //-- constraint solving stuff: designed for moving downward in lattice
    void constrainLE(TClatType& gl); // constrain this <= gl

  private:
    void setZero();

    latKind kind;
    suco_set<TClatType *> nodesLEthis; // constaints: nodes <= this
};

//------------------------------------------------------

class TClatRootSet
{
  public:

    enum lrIndex {
		lrBottom,
		lrChar,
		lrShort,
		lrInt,
		lrLong,
		lrLongLong,
		lrFloat,
		lrDouble,
		lrLongDouble,
		lrPointer,
		lrZero,
		lrTop,
		lrMax
	};

    TClatRootSet();

    void debug_dump(FILE * os);

    TClatType& getLatType(TCtype& t);
    TClatType& getLatType(lrIndex i) { return roots[i]; }

    //-- solves constraints (meeting along LE constraints)
    void solveMeet();

  private:
    TClatType roots[lrMax];

    //- helper for solveMeet()
    void traverseMeetWith(TClatType& node, TClatType::latKind lty);

};

//------------------------------------------------------

#endif /* } ifndef TC_TY_H */
