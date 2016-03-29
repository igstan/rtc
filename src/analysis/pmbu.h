#ifndef TC_PMBU_H /* { */
#define TC_PMBU_H

#include "edge.h"
#include "mbu.h"

//----------------------------------
//- pMBU: vp may-be-uninit analysis

class pMBUfactHandler : public MBUfactHandler
{
  public:
    bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
    bool handleDecl(DFAfact& df, PExprDecl& dd);
    bool handleVerify(DFAfact& df, PExprVerify& dv);
    void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn);
    void intraProcHandleCall(DFAfact& df, PExprCall& dc);

  public:
    pMBUfactHandler() {}

    static void doInsensitiveAnalysis(CFG& cfg);
    static void writeResults(CFG& cfg, FILE * outf);

    //- public, so they can be accessed by TCAstate
    static suco_set<AO *> results;
    static suco_set<AO *> unsafe;
    static suco_set<AO *> tracked;
    static bool readwrite;	//- used by collectPMBUtrackedUnsafe
    static void collectUnsafeTracked(AO& ao);

  private:
    static bool collectStartingPoints(PgmExpr& pe); //- helper for doInsensitiveAnalysis
    static suco_set<ECR *> relevant_ecrs; //- used by collectSatrtingPoints

    void moveToResults(suco_set<AO *>& uninit_aos, ExpDescr& use_exp, bool deref);
    void moveToResults(suco_set<AO *>& uninit_aos, LocSet& use_locs);
    static bool check_pmbu(ExpDescr& ed, suco_set<AO *>& pmbu_set);

};

//----------------------------------

#endif /* } ifndef TC_PMBU_H */

