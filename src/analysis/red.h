#ifndef TC_RED_H /* { */
#define TC_RED_H

#include "cfg.h"
#include "dfa.h"
#include "rda.h"

class PExprVerify;
class PExprAssign;

class REDresults
{
  public:
    REDresults()
	: vp_all(),
	  vp_write(),
	  vt_verify(),
	  vt_assign(),
	  stat_num_vp(0),
	  stat_num_vp_red(0),
	  stat_num_vpw(0),
	  stat_num_vpw_red(0),
	  stat_num_vtv(0),
	  stat_num_vtv_red(0),
	  stat_num_vta(0),
	  stat_num_vta_red(0)
	  {}

    suco_set<PExprVerify *> vp_all;
    suco_set<PExprVerify *> vp_write;
    suco_set<PExprVerify *> vt_verify;
    suco_set<PExprAssign *> vt_assign;

    void debug_dump(FILE * outf);
    void writeResults(FILE * outf);

    int stat_num_vp;
    int stat_num_vp_red;
    int stat_num_vpw;
    int stat_num_vpw_red;
    int stat_num_vtv;
    int stat_num_vtv_red;
    int stat_num_vta;
    int stat_num_vta_red;
};

//----------------------------------
//- RED: redundant analysis

class REDfact : public DFAfact
{
  //-- inherited interface
  public:
    DFAfact& newClone(bool preserve = true);

    void setTop() { is_top = true; edescset.Clear(); }
    void setBottom() { is_top = false; edescset.Clear(); }
    bool meet(DFAfact& df, bool preserve = true, bool warnNotLE = false);
    void join(DFAfact& df, bool preserve = true);

    void debug_dump(FILE * outf, bool brief = true);

  //-- mbu-specific stuff
  public:
    REDfact() : edescset(ExpDescr::compare), is_top(true) {}
    ~REDfact() { edescset.Clear(); }

    suco_set<ExpDescr *>& getEDescSet() { return edescset; }

  private:
    REDfact(REDfact& mf); //-- disable copy constructor

    suco_set<ExpDescr *> edescset;
    bool is_top;
};

class REDfactPair : public DFAfact
{
  //-- inherited interface
  public:

    DFAfact& newClone(bool preserve = true);

    void setTop() { vpfact.setTop(); vtfact.setTop(); }
    void setBottom() { vpfact.setBottom(); vtfact.setBottom(); }
    bool meet(DFAfact& df, bool preserve = true, bool warnNotLE = false);
    void join(DFAfact& df, bool preserve = true);

    void debug_dump(FILE * outf, bool brief = true);

  //-- mbu-specific stuff
  public:
    REDfactPair() : vpfact(), vtfact() {}
    ~REDfactPair() {}

    REDfact& getVPfact() { return vpfact; }
    REDfact& getVTfact() { return vtfact; }

  private:
    REDfactPair(REDfactPair& mf); //-- disable copy constructor

    REDfact vpfact; //- used in ptr, ptrw, and tc modes
    REDfact vtfact; //- used in tc mode only
};

//----------------------------------
//- REDfactHandler: abstract parent for [REDp/REDpw/REDt/REDtp]FactHandler

class REDfactHandler : public DFAfactHandler
{
  public:
    DFAfact& newTopFact() { return *new REDfactPair(); }
    void deleteFact(DFAfact& df) { delete (REDfactPair *) &df; }

    //- inter-proc: never called
    void filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local) {}
    virtual void interProcHandleCallArgs(DFAfact& df, PExprCall& dc) {}
    virtual void interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc) {}

    DFAfact& lookupNodeFact(CFGnode& cn, CFGnode * tfSucc = 0);
    DFAfact& lookupNodeFact(PExprCall& dc);
    DFAfact& lookupNodeFact(PExprParallel& dp);

    virtual bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
	{ return true; } //- intra-proc: do nothing
    virtual bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc)
	{ return true; } //- intra-proc: do nothing
    virtual bool handleDecl(DFAfact& df, PExprDecl& dd)
	{ return true; } //- default: do nothing
    virtual bool handlePredicate(DFAfactPair& dfp, PExprPredicate& dp)
	{ return true; } //- RED: do nothing

  protected:
    REDfactHandler() : DFAfactHandler() {}
};

//----------------------------------
//- REDp: redundant analysis: -ptr mode

class REDpFactHandler : public REDfactHandler
{
  public:
    virtual bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    virtual bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    virtual bool handleVerify(DFAfact& df, PExprVerify& dv);

    virtual void handleFreeCall(DFAfact& df, PExprCall& dc);

    virtual void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn);
    virtual void interProcFilterEntryFact(DFAfact& df, CFGnode& cn);
    virtual void intraProcHandleCall(DFAfact& df, PExprCall& dc);

  public:
    static REDpFactHandler handler;
};

//----------------------------------
//- REDpw: redundant analysis: -ptrw mode

class REDpwFactHandler : public REDpFactHandler
{
  public:
    virtual bool handleVerify(DFAfact& df, PExprVerify& dv);

  public:
    static REDpwFactHandler handler;
};

//----------------------------------
//- REDt: redundant analysis: tc types

class REDtFactHandler : public REDfactHandler
{
  public:
//    bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
//    bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc);
//    bool handleDecl(DFAfact& df, PExprDecl& dd);

    virtual bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    virtual bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    virtual bool handleVerify(DFAfact& df, PExprVerify& dv);

    void handleFreeCall(DFAfact& df, PExprCall& dc);

    void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn);
    void interProcFilterEntryFact(DFAfact& df, CFGnode& cn);
    void intraProcHandleCall(DFAfact& df, PExprCall& dc);

  public:
    static REDtFactHandler handler;
};

//----------------------------------
//- REDtp: redundant analysis: tc types + ptr

class REDtpFactHandler : public REDfactHandler
{
  public:
    bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
    bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc);
    bool handleDecl(DFAfact& df, PExprDecl& dd);

    virtual bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    virtual bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    virtual bool handleVerify(DFAfact& df, PExprVerify& dv);

    void handleFreeCall(DFAfact& df, PExprCall& dc);

    void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn);
    void interProcFilterEntryFact(DFAfact& df, CFGnode& cn);
    void intraProcHandleCall(DFAfact& df, PExprCall& dc);

  public:
    static REDtpFactHandler handler;
};

//----------------------------------
//- RED: redundant analysis

class RED : public DFA
{
  public:
    RED(REDfactHandler& rfh, bool paral2r) : DFA(rfh, false, false, paral2r?BBA::PM_RED_UNSAFE:BBA::PM_RED) {}

    bool isUselessNode(CFGnode& cn);
    bool isUselessNode(PgmExpr& dn);
    bool markUselessNode(CFGnode& cn);
    bool markUselessNode(PgmExpr& dn);

    bool absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter = 0);

    void print_debug_stats(CFG& cfg, FILE * outf);

  private:
    RED();
};

//----------------------------------
// REDpc: collect redundant results: -ptr/ptrw mode

class REDpcFactHandler : public REDpFactHandler
{
  public:
    bool handleVerify(DFAfact& df, PExprVerify& dv);

  public:
    REDpcFactHandler(REDresults& res) : REDpFactHandler(), results(res) {}

  private:
    REDpcFactHandler();

    REDresults& results;
};

//----------------------------------
// REDpwc: collect redundant results: -ptr/ptrw mode

class REDpwcFactHandler : public REDpwFactHandler
{
  public:
    bool handleVerify(DFAfact& df, PExprVerify& dv);

  public:
    REDpwcFactHandler(REDresults& res) : REDpwFactHandler(), results(res) {}

  private:
    REDpwcFactHandler();

    REDresults& results;
};

//----------------------------------
// REDtc: collect redundant results: tc types

class REDtcFactHandler : public REDtFactHandler
{
  public:
    bool handleVerify(DFAfact& df, PExprVerify& dv);

  public:
    bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);

    REDtcFactHandler(REDresults& res) : REDtFactHandler(), results(res) {}

  private:
    REDtcFactHandler();

    REDresults& results;
};

//----------------------------------
// REDtpc: collect redundant results: tc types + ptr

class REDtpcFactHandler : public REDtpFactHandler
{
  public:
    bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    bool handleVerify(DFAfact& df, PExprVerify& dv);

  public:
    REDtpcFactHandler(REDresults& res) : REDtpFactHandler(), pcfh(res), tcfh(res) {}

  private:
    REDtpcFactHandler();

    REDpcFactHandler pcfh;
    REDtcFactHandler tcfh;
};

//----------------------------------
// REDc: collect redundant results

class REDc : public BBA
{
  public:
    REDc(REDfactHandler& rcfh) : BBA(rcfh, false, PM_RED) {}

  private:
    REDc();
};

//----------------------------------

#endif /* } ifndef TC_RED_H */

