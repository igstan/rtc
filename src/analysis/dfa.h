#ifndef TC_DFA_H /* { */
#define TC_DFA_H

#include "suco.h"
#include "ao.h"
#include "cfg.h"

//----------------------------------
// WORKLIST

class CFGnode;

class WorkList
{
  public:
    friend class MBU; //-- for print_debug_stats

    WorkList() : list(), set(leaf_first_compare) {}

    void Insert(CFGnode& cn);

    bool InitialInsert(CFGnode& cn); //- true if inserted
    void FinishedInitialInserts(); //- "cleanup" initialization

    CFGnode * Remove();
    void Clear();
    int Size();
    bool IsEmpty();

    int ListLength() { return list.Length(); } //- (for debugging)
    int SetLength() { return set.Length(); } //- (for debugging)

    void debug_dump(FILE * outf);

  private:
    //- fifo/lifo modes: only use list
    //- leaf-first mode: use list for initial traversal, then use set
    suco_llist<CFGnode *> list;
    suco_set<CFGnode *> set;

    static int leaf_first_compare(CFGnode * n1, CFGnode * n2);
};

//----------------------------------
// ABSTRACT DFA and DFAfact classes

class PExprCall;
class PExprParallel;
class PExprArg;
class PExprDecl;
class PgmExpr;
class PExprVerify;
class PgmStmt;
class CFGfunction;

class DFAfact
{
  public:
    //-- fact constructors/destructor
    virtual DFAfact& newClone(bool preserve = true) = 0;

    //-- lattice operations
    // - preserve is an optimization flag: if false, then the
    //   function is free to modify (consume) df, which should
    //   often be more efficient
    // - NOTE: currently join is not called from the DFA level
    virtual void setTop() = 0;
    virtual void setBottom() = 0;
    virtual bool meet(DFAfact& df, bool preserve = true, bool warnNotLE = false) = 0;
    virtual void join(DFAfact& df, bool preserve = true) = 0;
    virtual void meetFiltered(DFAfact& df, CFGfunction& tgtfn);

    //-- other
    virtual void debug_dump(FILE * outf, bool brief = true) = 0;

    virtual ~DFAfact() {}
};

//----------------------------------

class DFAfactHandler;

class DFAfactPair
{
  public:
    DFAfactPair(DFAfact& f1, DFAfact * f2 = 0): fact1(f1), fact2(f2) {}

    DFAfact& getFact1() { return fact1; }
    DFAfact * getFact2() { return fact2; }
    void setFact2(DFAfact& f2) { fact2 = &f2; }
    void resetFact2() { fact2 = 0; }

    void flipFacts(DFAfactHandler& dfh);
    DFAfact& meetIfPair(DFAfactHandler& dfh);
    void splitIfSingle();

  private:
    DFAfactPair();

    DFAfact& fact1;
    DFAfact * fact2;
};

//----------------------------------

class CFGfunction;
class PExprAssign;
class PExprPredicate;

class DFAfactHandler
{
  public:
    virtual DFAfact& newTopFact() = 0;
    virtual void deleteFact(DFAfact& df) = 0;

    //-- filter callsite facts by GMOD/GREF, if needed - dual to reconstituteFilteredFacts
    virtual void filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local) = 0;
    //-- reincorporate df_local back into df; default version does a (destructive) meet
    virtual void reconstituteFilteredFacts(DFAfact& df, DFAfact& df_local);
    //-- prepare df to do retval-meet, default is noop, currently used by RAN only
    virtual void interProcPrepareReturnCollector(DFAfact& df, PExprCall& dc) {}

    //-- as currently written, lookupNodeFact needs to return
    //   a handle to the actual, modifiable fact object
    //--     if tfSucc, then return fact in pair corresponding to tfSucc
    //-- *** if tfSucc = 0, then MEET the pair and return the single fact! ***
    virtual DFAfact& lookupNodeFact(CFGnode& cn, CFGnode * tfSucc = 0) = 0;
    virtual DFAfactPair& lookupNodeFactPair(CFGnode& cn);
    virtual DFAfact& lookupNodeFact(PExprCall& dc) = 0;
    virtual DFAfact& lookupNodeFact(PExprParallel& dp) = 0;

    //-- dataflow analysis transfer functions
    // - return true means "useless" (NOP) for a given DFA
    virtual bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
				    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0) = 0;
    virtual bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode) = 0;
    virtual bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc) = 0;
    virtual bool handleDecl(DFAfact& df, PExprDecl& dd) = 0;
    virtual bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc) = 0;
    virtual bool handleVerify(DFAfact& df, PExprVerify& dv) = 0;
    virtual bool handlePredicate(DFAfactPair& dfp, PExprPredicate& dp) = 0;

    virtual void handleFreeCall(DFAfact& df, PExprCall& dc) = 0;

    virtual void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn) = 0;
    virtual void interProcFilterEntryFact(DFAfact& df, CFGnode& cn) = 0;
    virtual void intraProcHandleCall(DFAfact& df, PExprCall& dc) = 0;
    virtual void interProcHandleCallArgs(DFAfact& df, PExprCall& dc) = 0;
    virtual void interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc) = 0;
};

//----------------------------------

class CFG;

class BBA
{
  public:

    void collectResults(CFG& cfg, bool skip_libfns = false);

  protected:
    enum parallel_mode { PM_DEFAULT, PM_RED, PM_RED_UNSAFE };

    BBA(DFAfactHandler& h, bool inter, enum parallel_mode pm = PM_DEFAULT)
	: dfh(h),
	  do_interprocedural(inter),
	  pmode(pm)
	  {}

    //-- data
    DFAfactHandler& dfh;
    bool do_interprocedural;
    enum parallel_mode pmode;

    bool PgmExprPropagate(PgmExpr& dn, DFAfactPair& dfp, PgmStmt * parent);
    bool PgmStmtPropagate(PgmStmt& cn, DFAfactPair& dfp);
    void processCFGnode(CFGnode& cn);

    //-- used by DFA to add successors to worklist
    // - default BBA (collector) versions do nothing
    virtual void checkCallProgress(PExprCall& dc, DFAfact& df) {}
    virtual void checkNodeProgress(CFGnode& cn, DFAfactPair& dfp) {}
    virtual bool checkParallelProgress(PExprParallel& dp, DFAfact& df, bool looping_back = true) { return false; }

    //-- default BBA (collector) versions: no node is useless
    virtual bool isUselessNode(CFGnode& cn) { return false; }
    virtual bool isUselessNode(PgmExpr& dn) { return false; }
    virtual bool markUselessNode(CFGnode& cn) { return false; }
    virtual bool markUselessNode(PgmExpr& dn) { return false; }

    virtual bool isCollector() const { return true; } //- BBA = "collect" mode (as opposed to DFA mode)

  private:
    BBA();
};

class DFA : public BBA
{
  public:
    void collectWorkList(CFGnode& cn);
    void doAnalysis(CFG& cn, bool intra_skip_libfns = false);

    WorkList worklist; //-- should be private; is protected now for MBU::print_debug_stats

  protected:
    DFA(DFAfactHandler& h, bool inter, bool widen = false, enum BBA::parallel_mode pm = BBA::PM_DEFAULT)
	: BBA(h,inter,pm),
	  worklist() {}

    virtual bool absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter = 0);

    virtual void print_debug_stats(CFG& cfg, FILE * outf) = 0;

  protected: //- inherited stuff
    virtual bool isUselessNode(CFGnode& cn) = 0;
    virtual bool isUselessNode(PgmExpr& dn) = 0;
    virtual bool markUselessNode(CFGnode& cn) = 0;
    virtual bool markUselessNode(PgmExpr& dn) = 0;

    virtual bool isCollector() const { return false; } //- BBA = "collect" mode (as opposed to DFA mode)

    void checkCallProgress(PExprCall& dc, DFAfact& df);
    void checkNodeProgress(CFGnode& cn, DFAfactPair& dfp);
    bool checkParallelProgress(PExprParallel& dp, DFAfact& df, bool looping_back = true);

  private:
    DFA();

    static DFA * act_dfa;
    static bool addCallTargetsToWorkList(PgmExpr& dn);

    void collectWorkListDepthFirst(CFGnode& cn);

    //- helper for doAnalysis
    void processWorklist(CFG& cfg);
};

//----------------------------------

#endif /* } ifndef TC_DFA_H */

