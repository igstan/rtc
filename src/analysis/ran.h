#ifndef TC_RAN_H /* { */
#define TC_RAN_H

#include "cfg.h"
#include "dfa.h"
#include "red.h"
#include "interval.h"

//----------------------------------
//- RANfactHandler: range analysis handler

class RANfactHandler : public DFAfactHandler
{
  public: // inherited interface

    DFAfact& newTopFact();
    void deleteFact(DFAfact& df);

    void filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local);
    void reconstituteFilteredFacts(DFAfact& df, DFAfact& df_local);
    void interProcPrepareReturnCollector(DFAfact& df, PExprCall& dc);

    DFAfact& lookupNodeFact(CFGnode& cn, CFGnode * tfSucc = 0);
    DFAfactPair& lookupNodeFactPair(CFGnode& cn);
    DFAfact& lookupNodeFact(PExprCall& dc);
    DFAfact& lookupNodeFact(PExprParallel& dp);

    bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode);
    bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
    bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc);
    bool handleDecl(DFAfact& df, PExprDecl& dd);
    bool handleVerify(DFAfact& df, PExprVerify& dv);
    bool handlePredicate(DFAfactPair& dfp, PExprPredicate& dp);

    void handleFreeCall(DFAfact& df, PExprCall& dc);

    void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn);
    void interProcFilterEntryFact(DFAfact& df, CFGnode& cn);
    void intraProcHandleCall(DFAfact& df, PExprCall& dc);
    void interProcHandleCallArgs(DFAfact& df, PExprCall& dc);
    void interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc);

  public:
    static RANfactHandler handler;

    virtual void reportKnownPredicate(PExprPredicate& dp) {} //- to report known predicates (for RANcFactHandler to collect data)

  private:
};

//----------------------------------
//- RAN: range analysis

class RAN : public DFA
{
  public:
    enum wn_mode {
      WN_MEET = 0,
      WN_WIDEN,
      WN_NARROW
    };

    RAN(RANfactHandler& rah, bool inter, enum wn_mode wn, bool always)
	: DFA((DFAfactHandler&)rah, inter, true), wnmode(wn), wn_always(always) {}

    bool isUselessNode(CFGnode& cn);
    bool isUselessNode(PgmExpr& dn);
    bool markUselessNode(CFGnode& cn);
    bool markUselessNode(PgmExpr& dn);

    virtual bool absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter = 0);

    void print_debug_stats(CFG& cfg, FILE * outf);

  private:
    RAN();
    enum wn_mode wnmode;
    bool wn_always;
};

//----------------------------------
//- RAN: range analysis

class RANfact : public DFAfact
{
  public:

    RANfact(): is_top(true), map(0) {}
    ~RANfact();

  public: //-- inherited interface
    DFAfact& newClone(bool preserve = true);

    void setTop();
    void setBottom();
    bool meet(DFAfact& df, bool preserve = true, bool warnNotLE = false);
    void join(DFAfact& df, bool preserve = true);
    void debug_dump(FILE * outf, bool brief = true);
    void meetFiltered(DFAfact& df, CFGfunction& tgtfn);

  public:
    //- wnmode: { WN_MEET, WN_WIDEN, WN_NARROW }
    bool extended_meet(RANfact& df, bool preserve, bool warnNotLE,
			enum RAN::wn_mode wn, LocSet * backedge_filter = 0);
    bool isTop() const { return is_top; }

    Interval getDerefRangeFor(ExpDescr& ed);
    Interval evalExpr(ExpDescr& ed);
    void evalMallocSize(char * s, suco_iterator<AO *>& aoi, unsigned int& r_size, TCtype *& r_type); //- r_size,r_type are return values
    void updateInterval(AO& ao, Interval iv, TCtype * aoty = 0);
    void meetIntervals(suco_set<ECR *>& ecrset, Interval iv);
    void removeIntervalsPointingTo(suco_set<ECR *>& tgt_ecrset);
    bool disableIntervalsWithTarget(AO& ao); //- return true if any disabled
    Interval getInterval(AO& ao);
    void reconstitute(RANfact& rf_local);
    void handleStructAssign(AO& lhs, TCtype& lhsty, AO * rhs, bool strong);
    void createTopMappings(suco_set<AO *>& aoset); //- called by RANfactHandler::interProcPrepareReturnCollector
    void collectAOset(suco_set<AO *>& aoset); //- collect the AOs present in this fact

    //- helper cleanup functions
    void filterGrefMayFreeInto(CFGfunction& fn, RANfact * in_fact);
    void removeRetIntervalsExcept(AO * fnao);

    int countIntervals() const;

    enum cfmode {
      m_eqne = 0,
      m_ltge = 1,
      m_gtle = 2
    };
    bool evalPredExpr(char * s, suco_iterator<AO *>& aoi, enum cfmode mode, Interval iv, DFAfactPair& dfp);

  private:
    bool is_top;
    class interval_node
    {
      public:
        interval_node(AO& o, Interval iv, interval_node * nx)
		: ao(o), interval(iv), next(nx) {}
        AO& ao;
        Interval interval;
        interval_node * next;
      private:
        interval_node();
    } * map;

    interval_node ** lookupIntervalPosn(AO& ao);
    void clearMap();
    void deleteInterval(AO& ao);

    Interval evalSubexpr(char *& s, suco_iterator<AO *>& aoi, bool skipahead = false); //- helper for evalExpr
    Interval evalAddrRange(char *& s, suco_iterator<AO *>& aoi); // used by getDerefRangeFor, evalExpr
    TCtype * evalSizeOfExpr(char *& s); // used by evalMallocSize

    static void skipArgs(char *& s, suco_iterator<AO *>& aoi); //- helper for evalSubexpr, evalAddrRange
    static void consume(char *& s, char c);

    static enum cfmode flipmode(enum cfmode mode)
	{ return (mode == m_ltge) ? m_gtle : ((mode == m_gtle) ? m_ltge : mode); }
};

//----------------------------------
// RANc: collect redundant results: -ptr/ptrw mode

class RANcFactHandler : public RANfactHandler
{
  public:
    bool handleVerify(DFAfact& df, PExprVerify& dv);
    virtual void reportKnownPredicate(PExprPredicate& dp);

  public:
    RANcFactHandler(suco_set<PExprVerify *>& res, bool rw)
	: RANfactHandler(),
	  readwrite(rw),
	  inbounds_vps(res),
	  all_vps(),
	  finite_vps(),
	  half_finite_vps(),
	  known_preds(),
	  num_inb_sdotarrows(0)
	{}
    
    int countAllVPs(bool limit_array) { return countVPs(all_vps, limit_array); }
    int countFiniteVPs(bool limit_array) { return countVPs(finite_vps, limit_array); }
    int countHalfFiniteVPs(bool limit_array) { return countVPs(half_finite_vps, limit_array); }
    int countInboundsVPs(bool limit_array) { return countVPs(inbounds_vps, limit_array); }
    int countInboundsSdotArrows() { return num_inb_sdotarrows; }
    int countKnownPreds() { return known_preds.Length(); }

  private:
    RANcFactHandler();
    
    bool readwrite;

    suco_set<PExprVerify *>& inbounds_vps;
    suco_set<PExprVerify *> all_vps;
    suco_set<PExprVerify *> finite_vps;
    suco_set<PExprVerify *> half_finite_vps;
    suco_set<PExprPredicate *> known_preds;
    int num_inb_sdotarrows;

    static int countVPs(suco_set<PExprVerify *>& vps, bool limit_array);
};  

//----------------------------------
// RANc: collect redundant results

class RANc : public BBA
{
  public:
    RANc(RANcFactHandler& rcfh, bool inter) : BBA(rcfh, inter) {}

  private:
    RANc();
};

//----------------------------------

#endif /* } ifndef TC_RAN_H */

