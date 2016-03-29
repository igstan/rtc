#ifndef TC_RDMB_H /* { */
#define TC_RDMB_H

#include "dfa.h"
#include "mbu.h"
#include "rda.h"

//----------------------------------
//- RDMB: RDA/MBU combo, for collecting may-be-uninit results

class RDMBfact : public DFAfact
{
    friend class RDMBfactHandler;

  public:

    RDMBfact(RDAfact& rf, MBUfact& mf) : rda_fact(rf), mbu_fact(mf) {}
    ~RDMBfact() {}

  private: //-- inherited interface
    DFAfact& newClone(bool preserve = true);

    void setTop();
    void setBottom();
    bool meet(DFAfact& df, bool preserve = true, bool warnNotLE = false);
    void join(DFAfact& df, bool preserve = true);

    void debug_dump(FILE * outf, bool brief = true);

  private:

    RDAfact& rda_fact;
    MBUfact& mbu_fact;

    static RDAfact * results;

    void addFacts(suco_set<AO *>& aoset, PgmExpr& dnode, PgmStmt * cnode = 0, PExprArg * arg = 0);
    //-- static helper for addFacts
    static AO * cf_ao;
    static RDAfact * cf_rda_fact;
    static PgmStmt * cf_parent_node;
    static bool collectFacts(AO& ao);

};

//----------------------------------
//- RDMB: RDA/MBU combo, for collecting may-be-uninit results

class RDMBfactHandler : public DFAfactHandler
{
  public:

    DFAfact& newTopFact();
    void deleteFact(DFAfact& df);

    void filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local);

    DFAfact& lookupNodeFact(CFGnode& cn, CFGnode * tfSucc = 0);
    DFAfact& lookupNodeFact(PExprCall& dc);
    DFAfact& lookupNodeFact(PExprParallel& dp);

    bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
    bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc);
    bool handleDecl(DFAfact& df, PExprDecl& dd);
    bool handleVerify(DFAfact& df, PExprVerify& dv);
    bool handlePredicate(DFAfactPair& dfp, PExprPredicate& dp) { return true; } //- RDMB: do nothing

    void handleFreeCall(DFAfact& df, PExprCall& dc);

    void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn);
    void interProcFilterEntryFact(DFAfact& df, CFGnode& cn);
    void intraProcHandleCall(DFAfact& df, PExprCall& dc);
    void interProcHandleCallArgs(DFAfact& df, PExprCall& dc);
    void interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc);

  public:
    RDMBfactHandler(RDAfact& res) : DFAfactHandler(), results(res)
	{ RDMBfact::results = &results; }

  private:
    RDMBfactHandler();

    RDAfact& results;
};

//----------------------------------

#endif /* } ifndef TC_RDMB_H */

