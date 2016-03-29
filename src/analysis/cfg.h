#ifndef TC_CFG_H /* { */
#define TC_CFG_H

#include "suco.h"
#include "ao.h"
#include "id.h"

//----------------------------------

#define mPfx         '!'  // flow-sensitive prefix
#define  mStart      '{'
#define  mNext       ';'
#define  mEnd        '}'
#define   mParallel  'p'
#define   mBranch    'b'
#define   mWhile     'w'
#define   mDoWhile   'd'
#define   mFor       'f'
#define   mIf        'i'
#define   mSwitch    's'
#define   mNode      'n'
#define   mFunction  'x'
#define  mLabel      ':'
#define  mJump       '>'
#define   mGoto      'g'
#define   mBreak     'b'
#define   mContinue  'c'
#define   mReturn    'r'
#define   mVReturn   'v'
#define   mCase      's'
#define   mDefault   'd'
#define  mCall       'c'
#define  mAssign     'a'
#define  mPredicate  'p'
#define   mPrAnd     'a'
#define   mPrOr      'o'
#define   mPrQC      'q'
#define   mPrStmt    's'
#define   mPrSwitch  'w'
#define  mFormal     'f'
#define  mLocalDecl  'l'
#define  mStaticDecl 's'
#define  mMallocDecl 'm'
#define  mVerify     'v'
#define   mVTag      't'
#define   mVTagPtr   'u'
#define   mVTagPtrW  'v'
#define   mVRhs      'r'
#define   mVRhsPtr   's'
#define   mVPtr      'p'
#define   mVPtrW     'w'

//----------------------------------

class AID
{
  public:
    AID(int fsid = 0, int aidno = 0) : filestem_id(fsid), aid(aidno) {}

    int filestem_id;
    int aid;

    bool isZero() const { return (this->filestem_id == 0) && (this->aid == 0); }
    const char * lookupFileStem();

    static void writeFileStemMap(FILE * outf);

    static int compare(AID * l, AID * r)
	{ return (l->filestem_id-r->filestem_id)?(l->filestem_id-r->filestem_id):(l->aid-r->aid); }

    static AID zero;

    static suco_llist<const char *> filestemlist;
};

//----------------------------------

class Dependency
{
  public:
    Dependency(AO& k) : key(k), aoset() {}
    AO& getKey() { return key; }
    suco_set<AO *>& getAOs() { return aoset; }

    static int compare(Dependency * ldep, Dependency * rdep);

  private:
    AO& key;
    suco_set<AO *> aoset;
};

class DependencyMap
{
  public:
    DependencyMap() : deps(Dependency::compare),
		      aosets(DependencyMap::aoset_compare) {}

    void addDirectedDependency(suco_set<AO *>& keyset, suco_set<AO *>& depset); //- consumes depset
    void addDependencySet(suco_set<AO *>& depset); //- consumes depset
    void debug_dump(FILE * outf);

    suco_set<Dependency *>& getDirectedDependencies() { return deps; }
    suco_set<suco_set<AO *> *>& getDependencySets() { return aosets; }

    static int aoset_compare(suco_set<AO *> * laos, suco_set<AO *> * raos);

  private:
    suco_set<Dependency *> deps;
    suco_set<suco_set<AO *> *> aosets;
};

//----------------------------------

class ExpDescr
{
  public:
    ExpDescr(suco_llist<AO *>& el, char * es, suco_set<AO *>& s)
	: aos(s), estr_aos(el), estr(es), affecrs(0), aliasecrs(0) {}

    ~ExpDescr();

    suco_set<AO *>& getAOs() const { return aos; }
    suco_llist<AO *>& getEstrAOs() const { return estr_aos; }
    char * getEstr() const { return estr; }

    AO * getSingletonLoc(); //-- return AO if this is single location, 0 otherwise

    //- "afflocs": locations that affect the value of this expression,
    //  which includes estr_aos and aos deref'ed one level
    //  It isn't stored as a set, but is computed when needed via these
    //  helpers.
    bool affLocsIntersects(suco_set<AO *>& aoset);
    bool affLocsIntersects(suco_set<ECR *>& ecrset);

    suco_set<ECR *>& getAliasECRs();
    void collectAffectingAliasLocECRs(suco_set<ECR *>& eset);
    void collectDependencies(suco_set<AO *>& dep, bool uncaptured);

    void debug_dump(FILE * outf, int indent = 0);

    static char * stringToEstr(char * str, char ** nptr);
    static int compare(ExpDescr * lex, ExpDescr * rex);

  private:
    ExpDescr();

    suco_set<AO *>& aos;
    suco_llist<AO *>& estr_aos;
    char * estr; //-- TODO: should destruct?

    suco_set<ECR *> * affecrs; // cache-on-demand
    suco_set<ECR *> * aliasecrs; // cache-on-demand

    void instantiateAffEcrs(); //- called by affLocsIntersects
};

//----------------------------------
// Represents a set of location
// Implementation choices include storing set of:
//  - AOs
//  - AOs, one per struct
//  - ECRs
//  - ECRs mapped from a loc-ao
// This set is used to store GMOD, GREF, (GFree?),
// and backedge filters.

class BaseLocSet
{
  public:
    virtual int numAOs() = 0;
    virtual int Size() = 0;
    virtual void Union(BaseLocSet& lset) = 0;
    virtual void AbsorbConsume(suco_set<ECR *>& ecrset) = 0;
    virtual void Clear() = 0;
    virtual bool Insert(AO& ao) = 0;
    virtual void addToEcrSet(suco_set<ECR *>& ecrset) = 0;
    virtual void addToAOset(suco_set<AO *>& aoset) = 0;
    virtual bool interferesWithAffLocs(ExpDescr& edesc) = 0;
    virtual bool Contains(AO& ao) = 0;
    virtual bool Intersects(suco_set<AO *>& aoset) = 0;
    virtual bool Intersects(suco_set<ECR *>& ecrset) = 0;
    virtual void writeIntersectingAOs(BaseLocSet& lset, suco_set<AO *>& aoset) = 0;
    virtual void debug_dump(FILE * outf) = 0;
};

class ECRlocSet : public BaseLocSet
{
  public:
    ECRlocSet() : ecrset() {}

    int numAOs();
    int Size() { return this->ecrset.Length(); }
    void Union(BaseLocSet& lset) { this->ecrset.Union(((ECRlocSet&)lset).ecrset); }
    void AbsorbConsume(suco_set<ECR *>& ecrset);
    void Clear() { this->ecrset.Clear(); }
    bool Insert(AO& ao);
    void addToEcrSet(suco_set<ECR *>& ecrset) { ecrset.Union(this->ecrset); }
    void addToAOset(suco_set<AO *>& aoset);
    bool interferesWithAffLocs(ExpDescr& edesc) { return edesc.affLocsIntersects(this->ecrset); }
    bool Contains(AO& ao);
    bool Intersects(suco_set<AO *>& aoset);
    bool Intersects(suco_set<ECR *>& ecrset) { return this->ecrset.Intersects(ecrset); }
    void writeIntersectingAOs(BaseLocSet& lset, suco_set<AO *>& aoset);

    void debug_dump(FILE * outf);

  private:
    suco_set<ECR *> ecrset;
};

class AOlocSet : public BaseLocSet
{
  public:
    AOlocSet() : aoset() {}

    int numAOs() { return this->aoset.Length(); }
    int Size() { return this->aoset.Length(); }
    void Union(BaseLocSet& lset) { this->aoset.Union(((AOlocSet&)lset).aoset); }
    void AbsorbConsume(suco_set<ECR *>& ecrset);
    void Clear() { this->aoset.Clear(); }
    bool Insert(AO& ao) { return this->aoset.Insert(&ao); }
    void addToEcrSet(suco_set<ECR *>& ecrset);
    void addToAOset(suco_set<AO *>& aoset) { aoset.Union(this->aoset); }
    bool interferesWithAffLocs(ExpDescr& edesc) { return edesc.affLocsIntersects(this->aoset); }
    bool Contains(AO& ao) { return this->aoset.Contains(&ao); }
    bool Intersects(suco_set<AO *>& aoset) { return this->aoset.Intersects(aoset); }
    bool Intersects(suco_set<ECR *>& ecrset);
    void writeIntersectingAOs(BaseLocSet& lset, suco_set<AO *>& aoset);

    void debug_dump(FILE * outf) { AO::write_list_string_rep(outf, this->aoset, true); }

  private:
    suco_set<AO *> aoset;
};

//typedef AOlocSet LocSet;
typedef ECRlocSet LocSet;

//----------------------------------

#include "dfa.h"
#include "mbu.h"
#include "rda.h"
#include "rdmb.h"
#include "ran.h"
#include "red.h"

//----------------------------------

class InputState;
class PgmStmt;

class PgmExpr
{
  public:
    friend class CFG;

    enum Kind { fCall, fAssign, fPredicate, fDecl, fVerify,
		fParallel, fBranch };

    static PgmExpr * read(InputState& is, char * buf, PgmStmt * parent);
    static void readlist(InputState& is, char * buf, suco_llist<PgmExpr *>& nodelist, PgmStmt * parent);
    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false) = 0;

    const char * getKindString() const;

    PgmStmt * getParentNode() const { return parent; }

    bool traverseRootNodes(bool (*fp)(PgmExpr& dn));

    enum Kind getKind() const { return kind; }

    bool isLib(); //- is this part of library function?

    bool MBUisUseless() const { return mbu_is_useless; }
    void MBUsetUseless() { mbu_is_useless = true; }

    bool RANisUseless() const { return ran_is_useless; }
    void RANsetUseless() { ran_is_useless = true; }

    bool RDAisUseless() const { return rda_is_useless; }
    void RDAsetUseless() { rda_is_useless = true; }

    bool REDisUseless() const { return red_is_useless; }
    void REDsetUseless() { red_is_useless = true; }

    static bool notFunctionCall(PgmExpr& dn);

  protected:
    PgmExpr(enum Kind k, PgmStmt * par)
	: kind(k), parent(par),
	  mbu_is_useless(false),
	  ran_is_useless(false),
	  rda_is_useless(false),
	  red_is_useless(false) {}

  private:
    PgmExpr();
    enum Kind kind;
    PgmStmt * parent;

    //-- DFA analysis stuff
    bool mbu_is_useless;
    bool ran_is_useless;
    bool rda_is_useless;
    bool red_is_useless;

};

class CFGfunction;

class PExprArg
{
  public:
    PExprArg(AID a, suco_set<AO *>& aos, suco_llist<AO *>& estr_aos, char * estr)
	: aid(a), edesc(estr_aos, estr, aos) {}

    AID& getAid() { return aid; }
    ExpDescr& getDesc() { return edesc; }

  private:
    PExprArg();
    AID aid;
    ExpDescr edesc;
};

class PExprCall : public PgmExpr
{
  public:
    friend class PgmExpr;
    friend class CFG;
    friend class CFGnode; //- to initialize widen_aos

    PExprCall(AID a, suco_set<AO *>& f, int n, PgmStmt * par)
	: PgmExpr(fCall, par),
	  debug_trigger_counter(0),
	  aid(a), faos(f), nargs(n), args(n?(new PExprArg*[n]):0),
	  targetfns(), undef_tgtfns(), widen_locs(0),
	  ran_fact(), rda_fact(), rda_local(), mbu_fact(),
	  rdmb_fact(rda_fact,mbu_fact), red_fact()
	  {}

    AID& getAid() { return aid; }
    suco_set<AO *>& getFaos() { return faos; }
    PExprArg * getArg(int argno) const //- argno is one-based!
	{ return (argno <= nargs)?(args[argno-1]):((PExprArg *)0); }
    suco_set<CFGfunction *>& getTargetFns() { return targetfns; }
    suco_set<AOId *>& getUndefTargetFns() { return undef_tgtfns; }
    LocSet * getWidenLocs() const { return widen_locs; }
    bool callsFree(); //- does this callnode call free()?
    RANfact& getRANfact() { return ran_fact; }
    RDAfact& getRDAfact() { return rda_fact; }
    RDAfact& getRDAlocal() { return rda_local; }
    MBUfact& getMBUfact() { return mbu_fact; }
    RDMBfact& getRDMBfact() { return rdmb_fact; }
    REDfactPair& getREDfactPair() { return red_fact; }

    ID * isDirectCall();
    bool isRecursiveCall();

    int debug_trigger_counter;

    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false);

  private:
    AID aid;
    suco_set<AO *>& faos;
    int nargs;
    PExprArg ** args;
    suco_set<CFGfunction *> targetfns; //- defined target functions
    suco_set<AOId *> undef_tgtfns; //- undefined (library) target function ids
    LocSet * widen_locs; //- widening points (recursive call): 0 means don't widen, ALL_AOS means don't filter, else filter

    //-- dataflow analysis facts
    RANfact ran_fact; //- RAN "POST-call" fact cache
    RDAfact rda_fact; //- RDA "POST-call" fact cache
    RDAfact rda_local; //- RDA GMOD-filtered fact cache
    MBUfact mbu_fact; //- MBU "POST-call" fact cache
    RDMBfact rdmb_fact; //- MBU collection only
    REDfactPair red_fact; //- RED: actually not used! because intra-procedural
};

class PExprAssign : public PgmExpr
{
  public:
    PExprAssign(AID a, suco_set<AO *>& e1l, suco_llist<AO *>& e1al, char * e1s,
		suco_set<AO *>& e2l, suco_llist<AO *>& e2al, char * e2s,
		PgmStmt * par)
	: PgmExpr(fAssign, par), aid(a),
	  e1desc(e1al, e1s, e1l), e2desc(e2al, e2s, e2l) {}

    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    AID& getAid() { return aid; }
    ExpDescr& getLHS() { return e1desc; }
    ExpDescr& getRHS() { return e2desc; }

  private:
    AID aid;
    ExpDescr e1desc;
    ExpDescr e2desc;
};

class PExprPredicate : public PgmExpr
{
  public:
    enum prKind { prAnd, prOr, prQC, prStmt, prSwitch };

    PExprPredicate(AID a, suco_llist<AO *>& afflocs, char * estr, prKind k, PgmStmt * par)
	: PgmExpr(fPredicate, par), aid(a), edesc(afflocs, estr, empty_aos), pkind(k) {}

    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    AID& getAid() { return aid; }
    ExpDescr& getDesc() { return edesc; }

    const char * getPrKindString() const;
    bool isSwitch() const { return pkind == prSwitch; }
    bool isStmtOrQC() const { return ((pkind == prStmt) || (pkind == prQC)); }

  private:
    AID aid;
    ExpDescr edesc;
    enum prKind pkind;

    static suco_set<AO *> empty_aos; //- static placeholder
};

class PExprDecl : public PgmExpr
{
  public:
    enum dKind { dFormal, dLocal, dStatic, dMalloc };

    PExprDecl(AO& o, int fsid, enum dKind k, PgmStmt * par, bool isz, int arg_no = 0, ExpDescr * msize = 0)
	: PgmExpr(fDecl, par), ao(o), filestem_id(fsid), dkind(k),
	  argno(arg_no), mallocsize(msize), iszeroed(isz) {}

    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    bool isFormal() const { return (dkind == dFormal); }
    bool isLocal() const { return (dkind == dLocal); }
    bool isStatic() const { return (dkind == dStatic); }
    bool isMalloc() const { return (dkind == dMalloc); }
    bool isAlloca() const { return (dkind == dMalloc) && (ao.getKind() == AO::aoMalloc) && ((AOMalloc&)ao).isAlloca(); }
    AO& getAO() const { return ao; }
    int getFSid() const { return filestem_id; }
    int getArgNo() const { return argno; }
    ExpDescr * getMallocSize() const { return mallocsize; }
    bool isZeroed() const { return iszeroed; }

    const char * getDkindString() const;

  private:
    AO& ao;
    int filestem_id;
    enum dKind dkind;
    int argno; //- argno is one-based!!
    ExpDescr * mallocsize;
    bool iszeroed;
};

class PExprVerify : public PgmExpr
{
  public:
    enum vtKind { vtTag, vtRhs, vtNone};
    enum vpKind { vpPtr, vpPtrW, vpNone};

    PExprVerify(enum vtKind vtk, enum vpKind vpk, AID a,
		suco_set<AO *>& s, suco_llist<AO *>& al,
		char * str, PgmStmt * par)
	: PgmExpr(fVerify, par), vtkind(vtk), vpkind(vpk), aid(a),
	  desc(al, str, s) {}

    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    enum vtKind getVtKind() const { return vtkind; }
    enum vpKind getVpKind() const { return vpkind; }

    const char * getVtKindString() const;
    const char * getVpKindString() const;

    suco_set<AO *>& getAOs() { return desc.getAOs(); }
    ExpDescr& getDesc() { return desc; }

    AID& getAid() { return aid; }

  private:
    PExprVerify();
    enum vtKind vtkind;
    enum vpKind vpkind;
    AID aid;
    ExpDescr desc;
};

class PExprParallel : public PgmExpr
{
  public:
    friend class PgmExpr;
    PExprParallel(PgmStmt * par)
		: PgmExpr(fParallel, par), nodelists(),
		  ran_fact(), rda_fact(), mbu_fact(), rdmb_fact(rda_fact,mbu_fact),
		  red_fact() {}

    static PExprParallel& read(InputState& is, char * buf, PgmStmt * parent);
    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    suco_llist<suco_llist<PgmExpr *> *>& getNodeLists() { return nodelists; }

    RANfact& getRANfact() { return ran_fact; }
    RDAfact& getRDAfact() { return rda_fact; }
    MBUfact& getMBUfact() { return mbu_fact; }
    RDMBfact& getRDMBfact() { return rdmb_fact; }
    REDfactPair& getREDfactPair() { return red_fact; }

  private:
    suco_llist<suco_llist<PgmExpr *> *> nodelists;

    //-- dataflow analysis facts
    RANfact ran_fact; //- RAN POST-fact
    RDAfact rda_fact; //- RDA POST-fact
    MBUfact mbu_fact; //- MBU POST-fact
    RDMBfact rdmb_fact; //- MBU collection only
    REDfactPair red_fact; //- RED POST-fact
};

class PExprBranch : public PgmExpr
{
  public:
    friend class PgmExpr;
    PExprBranch(PgmStmt * par) : PgmExpr(fBranch, par), prednode(0), tnodelist(), fnodelist() {}

    static PExprBranch& read(InputState& is, char * buf, PgmStmt * parent);
    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    PgmExpr * getPredNode() { return prednode; }
    suco_llist<PgmExpr *>& getTrueNodes() { return tnodelist; }
    suco_llist<PgmExpr *>& getFalseNodes() { return fnodelist; }

  private:
    PExprPredicate * prednode;
    suco_llist<PgmExpr *> tnodelist, fnodelist;
};

//----------------------------------

class CFGnode
{
  public:
    friend class CFG; //- one instance of addSuccessor(); updateBackEdge(); findRemainingBackedges()
    friend class CFGbblock; //- access to predecessors/successors, for bblock construction
    friend class WorkList; //- access to worklist_flag

    CFGfunction& getParentFunction() const { return parent; }
    bool isEntryNode() const;
    virtual bool isExitNode() = 0;

    virtual bool traverseRootNodes(bool (*fp)(PgmExpr& dn)) = 0;
    virtual suco_llist<PgmStmt *>& getStmtList() = 0;
    virtual void debug_dump(FILE * outf, int indent = 0, bool brief = false) = 0;

    int getId() const { return traverse_id; }

    LocSet * getBackEdge() const;
    LocSet * getBackEdge(bool tf) const;
    static LocSet * const ALL_AOS;  // = (LocSet *)1;

    DFAfactPair& getRANfactPair() { return ran_fact_pair; }
    RDAfact& getRDAfact() { return rda_fact; }
    MBUfact& getMBUfact() { return mbu_fact; }
    RDMBfact& getRDMBfact() { return rdmb_fact; }
    REDfactPair& getREDfactPair() { return red_fact; }

    bool MBUisUseless() const { return mbu_is_useless; }
    void MBUsetUseless() { mbu_is_useless = true; }

    bool RANisUseless() const { return ran_is_useless; }
    void RANsetUseless() { ran_is_useless = true; }

    bool RDAisUseless() const { return rda_is_useless; }
    void RDAsetUseless() { rda_is_useless = true; }

    bool REDisUseless() const { return red_is_useless; }
    void REDsetUseless() { red_is_useless = true; }

    CFGnode * getSucc(int i) const { return (i < nsuccs)?succs[i]:0; }
    int getNsuccs() const { return nsuccs; }
    CFGnode * getPred(int i) const { return (i < npreds)?preds[i]:0; }
    int getNpreds() const { return npreds; }

    int debug_trigger_counter;

  protected:
    CFGnode(CFGfunction& par)
		: debug_trigger_counter(0),
		  succs(0), nsuccs(0),
		  preds(0), npreds(0),
		  parent(par),
		  traverse_id(0),
		  worklist_flag(false),
		  ran_fact_pair(ran_fact),
		  rda_fact(),
		  mbu_fact(),
		  rdmb_fact(rda_fact,mbu_fact),
		  red_fact(),
		  ran_is_useless(false),
		  rda_is_useless(false),
		  mbu_is_useless(false),
		  red_is_useless(false),
		  find_remaining_backedges_touched(0),
		  hascall(false),
		  ran_fact()
		  {
		    backedge_locs[0] = 0;
		    backedge_locs[1] = 0;
		  }

    CFGnode ** succs; //-- array of nsuccs+1 elements, except when 0
    int nsuccs;
    CFGnode ** preds; //-- array of npreds+1 elements, except when 0
    int npreds;

    CFGfunction& parent;

    //-- dataflow analysis stuff
    int traverse_id;
    bool worklist_flag;
    LocSet * backedge_locs[2]; //- 0 means not backedge, ALL_AOS means don't filter, else filter

    DFAfactPair ran_fact_pair; //- RAN POST fact
    RDAfact rda_fact; //- RDA POST fact
    MBUfact mbu_fact; //- MBU POST fact
    RDMBfact rdmb_fact; //- MBU collection only
    REDfactPair red_fact; //- RED POST fact

    bool ran_is_useless;
    bool rda_is_useless;
    bool mbu_is_useless;
    bool red_is_useless;

    //--- pred/succ stuff
    void addSuccessor(CFGnode& n);
    void initSuccsCount(int n);
    void initPredsCount(int n);
    int countPreds() const;
    int countSuccs() const;

    void updateBackEdgeIfSet(CFGnode& tgtnode, LocSet& lset);
    static bool computeWidenAOs(PgmExpr& pe); //- called in CFG initialization cycle

    void findRemainingBackedges(CFGfunction * entryfn = 0); //- called in CFG initialization cycle
    static bool findRemainingCallBackedges(PgmExpr& pe);
    char find_remaining_backedges_touched; //- used by CFGnode::findRemainingBackedges

    bool hascall;

  private:
    CFGnode();

    RANfact ran_fact; //- RAN primary fact: access through ran_fact_pair
};

//----------------------------------

class PgmStmt : public CFGnode
{
  public:
    friend class CFG;
    friend class CFGnode;
    friend class CFGbblock; //- to assign to bblock only
    friend class CFGfunction; //- read, readentry, etc?
    friend class DFA;	//- access to exprlist, parent
    friend class BBA;	//- access to exprlist, parent

    bool isExitNode();

    bool traverseRootNodes(bool (*fp)(PgmExpr& dn));
    PgmStmt * getTailStmt() { return this; }
    suco_llist<PgmStmt *>& getStmtList() { return stmtlist; }

    void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    bool isReturn() const { return (annot.kind == Annot::aReturn); }
    bool isGoto() const { return (annot.kind == Annot::aGoto); }
    const char * getGotoLabel() const { return isGoto()?annot.u.string:0; }
    ExpDescr * getReturnDesc() const
	{ return (annot.kind == Annot::aReturn)?(annot.u.edesc):((ExpDescr *)0); }

    AID& getAid() { return aid; }

    bool hasLabel(const char * s = 0) const;

    CFGbblock * getBBlock() const { return bblock; }
    CFGnode * getCFGactiveNode();	//- returns bblock or this, depending on flag_use_bblocks

    PgmStmt * getSuccStmt(int i) const { return (PgmStmt *) getSucc(i); }
    PgmStmt * getPredStmt(int i) const { return (PgmStmt *) getPred(i); }

  private:
    PgmStmt();

    //-- construct only by read functions
    PgmStmt(CFGfunction& par, AID a = AID::zero)
	: CFGnode(par), exprlist(), stmtlist(),
	  aid(a), labels(0), nlabels(0),
	  bblock(0), annot()
	{ stmtlist.Insert(this); }

    //-- input functions, called by friend CFGfunction
    static PgmStmt& read(InputState& is, CFGfunction& par, char * buf);
    static bool readentry(InputState& is, CFGfunction& par, char * buf, PgmStmt *& current, suco_llist<PgmStmt *>& nodelist);
    static void readlist(InputState& is, CFGfunction& par, char * buf, PgmStmt *& current, suco_llist<PgmStmt *>& nodelist);

    //- called by CFGbblock constructor; sets this->hascall
    bool hasFunctionCalls();

    //-- helper for read
    bool hasSwitchPred() const;

    //--- data
    suco_llist<PgmExpr *> exprlist;
    suco_llist<PgmStmt *> stmtlist;	//- singleton list containing this

    AID aid;
    char ** labels;
    int nlabels;

    CFGbblock * bblock;

    class Annot {
      public:
        friend class CFG;
        friend class PgmStmt;
        friend class CFGfunction;
        enum Kind { aNone = 0,
		    aGoto, aBreak, aContinue, aReturn,
		    aWhile, aDoWhile, aFor, aIf, aSwitch,
		    aCaseLabel, aDefaultLabel,
		    aOutNode
		  };

	Annot() : kind(aNone) { u.string = 0; }
      private:
	enum Kind kind;
	union {
	  const char * string;	//-- for Goto
	  int integer;		//-- for CaseLabel
          ExpDescr * edesc;	//-- for Return
	} u;
    } annot;
};

//----------------------------------

class CFGbblock : public CFGnode
{
  public:
    friend class DFA; //- to access stmtlist
    friend class BBA; //- to access stmtlist
    friend class CFGfunction; //- for constructing basic blocks

    CFGbblock(PgmStmt& headnode, suco_set<PgmStmt *>& pendingnodes);

    void debug_dump(FILE * outf, int indent = 0, bool brief = false);

    bool isExitNode();
    bool traverseRootNodes(bool (*fp)(PgmExpr& dn));
    suco_llist<PgmStmt *>& getStmtList() { return stmtlist; }

  private:
    CFGbblock();

    suco_llist<PgmStmt *> stmtlist;
};

//----------------------------------

class CFGfunction
{
  public:
    friend class CFG; //- callsites, callgraph, etc

    CFGfunction(AOId& ao, AID a) :
		cfglist(), entrystmt(0), exitnodes(),
		idao(ao), aid(a),
		callsites(), localvars(), gfreestack(),
		gmod(), gref(), iref(), gfreeheap_ecrs(),
		bblocks(), entryblock(0),
		callgraph(), callgraph_id(0), recursion_id(0) {}

    static CFGfunction& read(InputState& is, char * buf, int filestem_id);
    void debug_dump(FILE * outf, bool brief = false);
    void writeCallgraphDotEntry(FILE * outf);
    void write_descr(FILE * outf, const char * prefix, int indent);
    void bottomUpWriteCalltrace(FILE * outf, int indent);
    void topDownWriteCalltree(FILE * outf, int indent);

    void collectLocalsAndIMODREF();

    CFGnode * getEntryNode() const;

    suco_llist<PgmStmt *>& getStmtList() { return cfglist; }
    suco_llist<CFGbblock *>& getBBlist() { return bblocks; }
    suco_llist<PgmStmt *>& getExitNodes() { return exitnodes; }
    AOId& getId() const { return idao; }
    int getFileStemId() const { return aid.filestem_id; }
    suco_llist<PExprCall *>& getCallSites() { return callsites; }
    suco_set<AOId *>& getLocalVars() { return localvars; }
    LocSet& getGREF() { return gref; }
    LocSet& getIREF() { return iref; }
    LocSet& getGMOD() { return gmod; }
    suco_set<ECR *>& getGFreeHeap_ecrs() { return gfreeheap_ecrs; }
    suco_set<AO *>& getGFreeStack() { return gfreestack; }
    int getCallGraphId() const { return callgraph_id; }
    int getRecursionId() const { return recursion_id; }

    bool GFreeHeapAffects(AO& ao);

    static int compareByCallgraphId(CFGfunction * fn1, CFGfunction * fn2);
    static void collectCallers(CFGfunction& fn, suco_set<CFGfunction *>& fns); //- used by RDMBfact

  private:
    suco_llist<PgmStmt *> cfglist;
    PgmStmt * entrystmt;
    suco_llist<PgmStmt *> exitnodes;	//-- TODO: convert to bblocks?
    AOId& idao; // function pid
    AID aid; // verification aid
    suco_llist<PExprCall *> callsites;
    suco_set<AOId *> localvars;
    LocSet gmod;
    LocSet gref;
    LocSet iref;
    suco_set<ECR *> gfreeheap_ecrs;
    suco_set<AO *> gfreestack;

    suco_llist<CFGbblock *> bblocks;
    CFGbblock * entryblock;

    //-- callgraph stuff
    suco_set<CFGfunction *> callgraph;
    int callgraph_id;  //- -1=active, 0=inactive, positive=visited
    int recursion_id;  //- 0=nonrecursive
    int traverseCallGraph(int recursing);

    //-- construct basic blocks -- called by friend CFG
    void constructBBlocks();

    //-- static stuff to collect local decls recursively
    // - NOTE: clie_imod_ecrs and collectIMODREF are also used by CFG backedge filter stuff
    static suco_set<AOId *> * clie_curlvars;
    static suco_set<AO *> * clie_ifree_stack;
    static suco_set<ECR *> clie_ifree_heap_ecrs;
    static suco_set<ECR *> clie_imod_ecrs;
    static suco_set<ECR *> clie_iref_ecrs;
    static suco_set<AO *> clie_callsite_retaos;
    static void collectIMODREF(suco_set<AO *>& aoset, bool iref);
    static bool collectLocalsAndIMODREFexpr(PgmExpr& df);
};

//----------------------------------

class CFG
{
  public:
    CFG() : fnlist(), globnodes(), entry(0), mainfn(0), prepared(false), assignIds_counter(0) {}

    void read(InputState& is, char * buf, const char * filestem);
    void debug_dump(FILE * outf);

    void writeCallgraph(FILE * outf, bool dot, char * trace_fn);
    void prepareCFG(bool prepare_widen);

    suco_llist<CFGfunction *>& getFunctionList() { return fnlist; }
    CFGnode * getEntryNode() const { return entry; }

    bool traverseRootNodes(bool (*fp)(PgmExpr& dn));

  private:
    suco_llist<CFGfunction *> fnlist;
    suco_llist<PgmExpr *> globnodes;	//- these will be absorbed into entry node
    CFGnode * entry;
    CFGfunction * mainfn;
    bool prepared; //- has this CFG been "prepared" (callgraph,modref)

    //-- cfg initialization methods
    void collectLocalsAndIMODREF();
    void prepareSuperGraph();
    void connectSuperGraph();
    void remapRecursionId();
    void constructBBlocks();
    void collectGMODREF();

    void assignId(CFGnode& cnode);
    void assignIds(); //- also marks backedges for widen/narrow
    int assignIds_counter;

    //-- collecting backedge filters
    // - NOTE these make use of CFGfunction::clie_imod_ecrs and CFGfunction::collectIMODREF
    void computeBackedgeFilters();
    static bool collectModSetUntil(PgmStmt * exitnode, suco_iterator<PgmStmt *>& si, LocSet& modlocs);
    static bool collectMODecrs(PgmExpr& pe);

    //-- static helpers to connect call/fndefns recursively
    static PExprCall * act_caller_node;
    static suco_llist<CFGfunction *> * act_fnlist;
    static bool addAOtoCallTarget(AO& ao);
    static bool assignCallTargets(PgmExpr& dn);
};

//----------------------------------

#endif /* } ifndef TC_CFG_H */
