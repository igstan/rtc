#ifndef TC_MBU_H /* { */
#define TC_MBU_H

#include "dfa.h"
#include "rda.h"
#include "flags.h"

//----------------------------------
//- MBU: may-be-uninit analysis

class MBUfact : public DFAfact
{
  //-- inherited interface
  public:
    friend class MBUfactHandler; //- to access aoset
    friend class pMBUfactHandler; //- to access aoset

    DFAfact& newClone(bool preserve = true);

    void setTop() { is_bot = false; aoset.Clear(); }
    void setBottom() { is_bot = true; aoset.Clear(); }
    bool meet(DFAfact& df, bool preserve = true, bool warnNotLE = false);
    void join(DFAfact& df, bool preserve = true);

    void debug_dump(FILE * outf, bool brief = true);

  //-- mbu-specific stuff
  public:
    MBUfact() : aoset(), is_bot(false) {}
    ~MBUfact() { aoset.Clear(); }

    void addRelevantAliasLocs(suco_set<AO *>& laoset);

    bool intersectsAliases(suco_set<AO *>& raoset);

    void insertComponentAOs(AO& ao);
    void removeComponentAOs(AO& ao);

    bool contains(AO& ao) { return aoset.Contains(&ao); }
    int length() { return aoset.Length(); }

  private:
    MBUfact(MBUfact& mf); //-- disable copy constructor

    //-- data --
    suco_set<AO *> aoset;
    bool is_bot; //- mark bottom

    //-- static helpers for addUnsafeAliasLocs
    static MBUfact * aual_fact;
    static bool add_locs_to_aual_fact(AO& ao);

    //-- static helpers for intersectsAliases
    static suco_set<ECR *> ia_ecrset;
    static bool is_ia_ecrset(ECR& ecr);
    static suco_set<ECR *> cfr_ecrset;
    static bool collectFnReturns(AO& ao);

    //-- static helper for [insert/remove]ComponentAOs()
    static MBUfact * nsao_mbf;
    static void collectNonStructAO(AO& ao);
    static void removeAO(AO& ao);
};

//----------------------------------
//- MBU: may-be-uninit analysis

class MBUfactHandler : public DFAfactHandler
{
  public:
    DFAfact& newTopFact() { return *new MBUfact(); }
    void deleteFact(DFAfact& df) { delete (MBUfact *) &df; }

    void filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local);

    DFAfact& lookupNodeFact(CFGnode& cn, CFGnode * tfSucc = 0);
    DFAfact& lookupNodeFact(PExprCall& dc);
    DFAfact& lookupNodeFact(PExprParallel& dp);

    virtual bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    virtual bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    virtual bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
    virtual bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc);
    virtual bool handleDecl(DFAfact& df, PExprDecl& dd);
    virtual bool handleVerify(DFAfact& df, PExprVerify& dv);
    virtual bool handlePredicate(DFAfactPair& dfp, PExprPredicate& dp) { return true; } //- MBU: do nothing

    virtual void handleFreeCall(DFAfact& df, PExprCall& dc) {} //- MBU: do nothing

    virtual void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn);
    virtual void interProcFilterEntryFact(DFAfact& df, CFGnode& cn);
    virtual void intraProcHandleCall(DFAfact& df, PExprCall& dc);
    virtual void interProcHandleCallArgs(DFAfact& df, PExprCall& dc);
    virtual void interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc);

  public:
    static MBUfactHandler handler;
};

class MBUc1factHandler : public MBUfactHandler
{
  public:
    bool handleVerify(DFAfact& df, PExprVerify& dv);

  public:
    MBUc1factHandler(RDAfact& res) : MBUfactHandler(), results(res) {}

  private:
    MBUc1factHandler();

    RDAfact& results;

    //-- helpers for handleVerify
    static MBUfact * cf_mf;
    static RDAfact * cf_res;
    static bool collectFact(AO& ao);

};

class MBUc2factHandler : public MBUfactHandler
{
  public:
    bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
    bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc);
    bool handleDecl(DFAfact& df, PExprDecl& dd);
    // bool handleVerify(DFAfact& df, PExprVerify& dv); //- MBUc2: do nothing

  public:
    MBUc2factHandler(RDAfact& res) : MBUfactHandler(), results(res) {}

  private:
    MBUc2factHandler();

    RDAfact& results;
};

//----------------------------------
//- MBU: may-be-uninit analysis

class MBU : public DFA
{
  public:
    MBU(bool inter, DFAfactHandler& dfh = MBUfactHandler::handler)
	: DFA(dfh, inter, false, (flag_vtfix)?BBA::PM_RED_UNSAFE:BBA::PM_DEFAULT) {}

    bool isUselessNode(CFGnode& cn);
    bool isUselessNode(PgmExpr& dn);
    bool markUselessNode(CFGnode& cn);
    bool markUselessNode(PgmExpr& dn);

    bool absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter = 0);

    void print_debug_stats(CFG& cfg, FILE * outf);
};

//----------------------------------
// MBUc: collect may-be-uninit results
// A. by rda/mbu - use RDMBfactHandler
// B. by mbu delta, use, in succession:
//    MBUc1factHandler - identify only MBU locations
//    MBUc2factHandler - for these locations, find deltas

class MBUc : public BBA
{
  public:
    MBUc(DFAfactHandler& dfh, bool inter) : BBA(dfh,inter,(flag_vtfix)?PM_RED_UNSAFE:PM_DEFAULT) {}

  private:
    MBUc();
};

//----------------------------------

#endif /* } ifndef TC_MBU_H */

