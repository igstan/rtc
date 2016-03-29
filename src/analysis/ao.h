#ifndef TC_AO_H /* { */
#define TC_AO_H

#include <stdio.h>

#include "ty.h"
#include "id.h"
#include "edge.h"

#include "suco.h"

//------------------------------------------------------

class AOlist
{
  public:
    friend class AO;

    AOlist() : head(0) {}
    ~AOlist();

    //- These used in cataloguing AOs
    void debug_dump(FILE * os);
    void foreachAO(void (*fp)(AO& o)); //- only walks this list
    void traverseAOs(void (*fp)(AO& o)); //- recursively apply to AO lists

    AO& get_AOValue(TCtype& t); //- delete t if found

  private:
    class node {
      public:
	node(AO& o, node * n = 0) : ao(o), next(n) {}
	AO& ao;
	node * next;
      private:
	node();
    };
    node * head;
};

//------------------------------------------------------
class ExposedStatus
{
  private:
    enum exp_sta { esAll, esSome, esNone, esNA } status;
    ExposedStatus(enum exp_sta es = esNA): status(es) {}

  public:
    static const ExposedStatus All;
    static const ExposedStatus Some;
    static const ExposedStatus None;
    static const ExposedStatus NA;

    bool isAll() const { return status == esAll; }
    bool isSome() const { return status == esSome; }
    bool isNone() const { return status == esNone; }
    bool isNA() const { return status == esNA; }

    char exposedCode();
    void combine(ExposedStatus es);
};

//------------------------------------------------------
class AO
{
  public:
    enum aoKind {
	 aoId, aoMalloc, aoSDot, aoUDot,
	 aoReturn, aoArg, aoStar, aoValue,
	 aoStringLit, aoOp, aoExt, aoAddrOf,
	 aoFunction
	};

    //-------------------------
    //-- old type-safety levels
    enum ts_level { TS_NONE, TS_SAFE, TS_TRACKED, TS_UNSAFE };

    void setTS_SAFE()    { tsl = TS_SAFE; }
    void setTS_TRACKED() { tsl = TS_TRACKED; }
    void setTS_UNSAFE(); //- sets unsafe, and propagates along assign edges 
    char getTScode(); //- returns 'S'|'T'|'U'|0

    static void writeTSlevel(AO& ao);

    //-----------------------------
    //-- new type-safety categories
    enum ts_categ { TSC_POSS_INVALID = 0, // numbers used for comparison
		    TSC_BADLY_TYPED = 1,
		    TSC_INFLUENTIAL = 2,
		    TSC_EXPOSED = 3,
		    TSC_SAFE = 4 };

    void setTSC(ts_categ c);
    ts_categ getTSC() const { return tsc; }
    static ts_categ getSetTSC(suco_set<AO *>& set);
    char getTSCcode(); //- returns 'P'|'B'|'I'|'E'|'S'|0
    static void writeTScateg(AO& ao);
    static void writeUntouchedExposed(AO& ao);
    static void writeVulnerable(AO& ao);

    //-----------------------------

    friend class ECR; //- to play with ECR
    friend class ID;  //- to mark is_zeroed

    static FILE * aoWriteStream;
    static void writeECR(AO& ao);

    void debug_dump(FILE * os);
    virtual void dump_descr(FILE * os) = 0;
    virtual void write_string_rep(FILE * os, bool readable);
    static void write_list_string_rep(FILE * os, suco_llist<AO *>& set, bool readable);
    aoKind getKind() const { return kind; }

    static void assignEcrNosAndWriteToFile(AO& ao);

    //-NOTE: this function should only be called after pt-analysis and ECRs have been finalized
    ECR& getECR();

    static AO * stringToAO(char * str, char ** nptr,
				IDmap& aidmap, IDmap& pidmap, AOlist& valueAOs);

    static suco_llist<AO *>& stringToAOlist(bool do_set, char * str, char ** nptr,
					IDmap& aidmap, IDmap& pidmap, AOlist& valueAOs);

    AO * find(aoKind k);
    AO& get_AOAddrOf();
    AO& get_AOFunction();
    AO& get_AOStar();
    AO& get_AOExt(TCtype& t, TCtype& f); //- delete t,f if found
    AO& get_AOOp(TCtype& t); //- delete t if found
    AO& get_AOReturn();	//- note: function return node is R F I <fnid>, while callsite return node is R D I <fnid>
    AO * get_AOArg(int n);
    AO * get_AOSDot(suco_llist<TCtype *>& l);
    AO * get_AOUDot(TCtype& t);

    virtual AO& normalize() { return *this; }	//- normalize union members, and struct first members
							//- TODO: incorporate into PTA?
    void traverseAOs(void (*fp)(AO& o));
    AOlist& getAOlist() { return aolist; }

    TCassignEdge * assignTo(AO& tgt, TCtype& ty); //- returns edge if not already there
    suco_set<TCassignEdge *>& getOutgoingAssignEdges() { return assignsTo; }
    suco_set<TCassignEdge *>& getIncomingAssignEdges() { return assignsFrom; }

    bool isVal() const { return isValAO; } //- is AO a descendent of an AOValue object?
    bool isRef() const { return isRefAO; } //- is AO a descendent of a dereference object?
    bool isLib() const { return isLibAO; } //- is AO from a library function?
    bool isLoc() const { return isLocAO; } //- is AO a location object (x, x.i, malloc, strlit)?
    bool isLocArgRet() const { return (isLocAO || kind == aoArg || kind == aoReturn); }
    bool isAssigned() const { return isAssignedAO; }
    bool isVulnerableLoc() const { return is_vuln_loc; }
    bool isZeroed() const { return is_zeroed; }
    bool isDirectArrayAccess();

    void markVulnerableLocAndPropagate(const char * vuln_fn_name = 0);

    virtual AO& getEnclosingStruct() { return *this; } //- return containing struct/union
    virtual AO& getRootAO() { return *this; } //- get the root AO object

    virtual AO * derefOneLevel() { return 0; }

    ExposedStatus pointsToExposed(bool limit_malloc = false, bool do_touched = false, bool do_vuln = false);

    void setStaticType(TCtype& ty);	//- must consume ty
    void setVerifyPtrType(TCtype& ty);	//- must consume ty
    void setRequiredType(TCtype& ty);

    TClatType::latKind getRequiredType() { return reqdType; }
    virtual TCtype * getStaticType() { return staticType; }

    bool dfa_relevant; //- this tag is for use by dataflow analyses to filter out
		       //  unnecesssary AOs from dataflow facts (clients include: MBU, RAN)
  protected:
    AO(enum aoKind k, bool iv, bool ir, bool il, bool ic)
	: dfa_relevant(true),
	  kind(k),
	  isValAO(iv),
	  isRefAO(ir),
	  isLibAO(il),
	  isLocAO(ic),
	  isAssignedAO(false),
	  is_vuln_loc(false),
	  is_zeroed(false),
	  aolist(/*true*/), //- true -> on delete, AOs will be deleted
	  tsl(TS_NONE),
	  tsc(TSC_SAFE),
	  assignsTo(TCassignEdge::compare),
	  assignsFrom(TCassignEdge::compare),
	  ecr(0),
	  staticType(0),
	  verifyPtrType(0),
	  reqdType(TClatType::tclBottom) {}

    AO& get_or_create_AOArg(int n);
    AO& get_or_create_AOSDot(suco_llist<TCtype *>& l, bool delty = true); //- delete l if found
    AO& get_or_create_AOUDot(TCtype& t, bool delty = true); //- delete t if found
    void instantiateStructUnionMembers();	//- called by setStaticType

  private:
    AO();
    AOlist::node ** findAO(AOlist::node ** n, aoKind k);
    enum aoKind kind;
    bool isValAO; //- is this AO a descendent of an AOValue abstract object?
    bool isRefAO; //- is this AO a descendent of a dereference abstract object?
    bool isLibAO; //- is this AO in a library function (descended from a library pid/aid)?
    bool isLocAO; //- is this AO a location object (x, x.i, malloc, strlit)?
		  //  NOTE: for now, arg/ret are excluded; see isLocArgRet()
    bool isAssignedAO; //- is this AO the target of an assignment?
    bool is_vuln_loc; //- identify vulnerable locations
    bool is_zeroed;
    AOlist aolist;
    ts_level tsl;
    ts_categ tsc;
    suco_set<TCassignEdge *> assignsTo; //- TODO: have destructor delete edges?
    suco_set<TCassignEdge *> assignsFrom; //- TODO: have destructor delete edges?
    ECR * ecr;
    TCtype * staticType;
    TCtype * verifyPtrType;
    TClatType::latKind reqdType;
};

//------------------------------------------------------

class AOAddrOf : public AO
{
  public:  AOAddrOf(AO& o)
		: AO(aoAddrOf, o.isVal(), false, o.isLib(), false),
		  ao(o) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           // virtual AO * derefOneLevel() { return 0; }

	   AO& getTarget() const { return ao; }
  private: AOAddrOf();
	   AO& ao;
};

class AOFunction : public AO
{
  public:  AOFunction(AO& o)
		: AO(aoFunction, o.isVal(), false, o.isLib(), false),
		  ao(o), largno(0), isvarg(false) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           // virtual AO * derefOneLevel() { return 0; }

	   AO& getTarget() const { return ao; }
	   void setAttributes(int larg_no, bool is_varg);
	   int getLargNo() const { return largno; }
	   bool isVarg() const { return isvarg; }
  private: AOFunction();
	   AO& ao;
	   int largno; //- first unnamed argument; zero indicates undefined function
	   bool isvarg; //- is var-arg function (with explicit ellipsis)
};

class AOStar : public AO
{
  public:  AOStar(AO& o)
		: AO(aoStar, o.isVal(), true, o.isLib(), false),
		  ao(o) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           virtual AO * derefOneLevel() { return &this->getTarget(); }

	   AO& getTarget() const { return ao; }
  private: AOStar();
	   AO& ao;
};

class AOExt : public AO
{
  public:  AOExt(AO& o, TCtype& t, TCtype& f)
		: AO(aoExt, o.isVal(), o.isRef(), o.isLib(), false),
		  ao(o), tty(t), fty(f) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           virtual AO * derefOneLevel() { return this->getTarget().derefOneLevel(); }

	   AO& getTarget() const { return ao; }
	   TCtype& getTty() const { return tty; }
	   TCtype& getFty() const { return fty; }
  private: AOExt();
	   AO& ao;
	   TCtype& tty;
	   TCtype& fty;
};

class AOArg : public AO
{
  public:  AOArg(AO& o, int n)
		: AO(aoArg, o.isVal(), o.isRef(), o.isLib(), false),
		  no(n), ao(o) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           // virtual AO * derefOneLevel() { return 0; }

	   AO& getParent() const { return ao; }
	   int argNo() const { return no; }
  private: AOArg();
	   int no;
	   AO& ao;
};

class AOId : public AO
{
  public:  AOId(ID& p, bool islib = false)
		: AO(aoId, false, false, islib, true),
		  pid(p) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           // virtual AO& getRootAO() { return *this; }
           // virtual AO * derefOneLevel() { return 0; }

	   ID& getPid() const { return pid; }
  private: AOId();
	   ID& pid;
};

class AOStringLit : public AO
{
  public:  AOStringLit(ID& a, bool islib = false)
		: AO(aoStringLit, false, false, islib, true),
		  aid(a) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           // virtual AO& getRootAO() { return *this; }
           // virtual AO * derefOneLevel() { return 0; }

  private: AOStringLit();
	   ID& aid;
};

class AOMalloc : public AO
{
  public:  friend class ID;	//- to set is_alloca (yes, hackish)
	   AOMalloc(ID& a, bool islib = false)
		: AO(aoMalloc, false, false, islib, true),
		  aid(a), mtype(0), msize(0), is_alloca(false) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           // virtual AO& getRootAO() { return *this; }
           // virtual AO * derefOneLevel() { return 0; }

	   void meetMallocStaticType(TCtype * ty, unsigned int size);
	   TCtype * getMallocType() const { return mtype; }
	   unsigned int getMallocSize() const { return msize; }
	   bool isAlloca() const { return is_alloca; }

  private: AOMalloc();
	   ID& aid;
	   TCtype * mtype;
	   unsigned msize;
	   bool is_alloca;
};

class AOOp : public AO
{
  public:  AOOp(AO& o, TCtype& t)
		: AO(aoOp, o.isVal(), o.isRef(), o.isLib(), false),
		  ao(o), ty(t) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           virtual AO * derefOneLevel() { return this->getTarget().derefOneLevel(); }

	   AO& getTarget() const { return ao; }
	   TCtype& getTy() const { return ty; }
  private: AOOp();
	   AO& ao;
	   TCtype& ty;
};

class AOReturn : public AO
{
  public:  AOReturn(AO& o)
		: AO(aoReturn, o.isVal(), o.isRef(), o.isLib(), false),
		  ao(o) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           // virtual AO * derefOneLevel() { return 0; }

	   virtual TCtype * getStaticType();

	   AO& getParent() const { return ao; }
  private: AOReturn();
	   AO& ao;
};

class AOSDot : public AO
{
  public:  AOSDot(AO& o, suco_llist<TCtype *>& l)
		: AO(aoSDot, o.isVal(), o.isRef(), o.isLib(), o.isLoc()),
		  ao(o), tylist(l) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getEnclosingStruct() { return this->ao.getEnclosingStruct(); }
           virtual AO& getRootAO() { return ao.getRootAO(); }
           virtual AO * derefOneLevel() { return this->getParent().derefOneLevel(); }

	   TCtype * getStaticType() { return getTyList().Last(); }

	   AO& normalize()		//- normalize struct first member
		{ return (tylist.IsEmpty()) ? ao.normalize() : *this; }
	   suco_llist<TCtype *>& getTyList() const { return tylist; }
	   AO& getParent() const { return ao; }
  private: AOSDot();
	   AO& ao;
	   suco_llist<TCtype *>& tylist;
};

class AOUDot : public AO
{
  public:  AOUDot(AO& o, TCtype& t)
		: AO(aoUDot, o.isVal(), o.isRef(), o.isLib(), o.isLoc()),
		  ao(o), ty(t) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           virtual AO& getRootAO() { return ao.getRootAO(); }
           virtual AO& getEnclosingStruct() { return this->ao.getEnclosingStruct(); }
	   TCtype * getStaticType() { return &getTy(); }
           virtual AO * derefOneLevel() { return this->getParent().derefOneLevel(); }

	   AO& normalize() { return ao.normalize(); }	//- normalize union members
	   TCtype& getTy() const { return ty; }
	   AO& getParent() const { return ao; }
  private: AOUDot();
	   AO& ao;
	   TCtype& ty;
};

class AOValue : public AO
{
  public:  AOValue(TCtype& t)
		: AO(aoValue, true, false, false, false),
		  ty(t) {}
	   void dump_descr(FILE * os);
	   void write_string_rep(FILE * os, bool readable);
           // virtual AO& getRootAO() { return *this; }
           // virtual AO * derefOneLevel() { return 0; }

	   TCtype& getTy() const { return ty; }
  private: AOValue();
	   TCtype& ty;
};

//------------------------------------------------------

#endif /* } ifdef TC_AO_H */
