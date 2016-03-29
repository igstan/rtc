#ifndef TC_RDA_H /* { */
#define TC_RDA_H

#include "dfa.h"

//----------------------------------
//- RDA: reaching-defs analysis

class RDAfact : public DFAfact
{
  //-- inherited interface
  public:
    DFAfact& newClone(bool preserve = true);

    void setTop() { is_bot = false; clear(); }
    void setBottom() { is_bot = true; clear(); }
    bool meet(DFAfact& df, bool preserve = true, bool warnNotLE = false);
    void join(DFAfact& df, bool preserve = true);

    void debug_dump(FILE * outf, bool brief = true);

  //-- mbu-specific stuff
  public:
    RDAfact() : head(0), is_bot(false) {}
    virtual ~RDAfact() { clear(); }

    void filterGmodInto(CFGfunction& fn, RDAfact& ffact);
    void addFact(AO& ao, PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    void copyFacts(AO& idx_ao, AO& ao, RDAfact& mf);
    void removeFacts(AO& ao);
    bool hasKey(AO& ao);

    void markAOsExposed(); //TODO: migrate to BBA?

    static int freelistLength();

    void writeResults(FILE * outf);

  private:
    RDAfact(RDAfact& mf); //-- disable copy constructor

    //-- data --
    class LocAidNode
    {
      public:
        //-- constructor/destructor wrappers
        static LocAidNode * acquire(AO& o, LocAidNode * nx = 0);
        static LocAidNode * acquire(LocAidNode& n, LocAidNode * nx = 0);
        static void dispose(LocAidNode * n);

        AO& getAO() const { return *ao; }
        //-- three different types of meta-facts are collected
        suco_set<PgmExpr *> nodes;	//- assigns, calls, formal decls
        suco_set<PExprArg *> args;	//- call args
        suco_set<PgmStmt *> rets;	//- return nodes

        LocAidNode * next;

        static LocAidNode * freelist;

      private:
        LocAidNode();
        LocAidNode(AO& o, LocAidNode * nx)
		: nodes(), args(), rets(),
		  next(nx), ao(&o) {}
        ~LocAidNode() {}
        AO * ao;
    } * head;

    bool is_bot; //- mark bottom

    //-- methods --
    void clear();
    LocAidNode * getFactNode(AO& ao); //- helper for addFact
};

//----------------------------------
//- RDA: reaching-defs analysis

class RDAfactHandler : public DFAfactHandler
{
  public:
    DFAfact& newTopFact() { return *new RDAfact(); }
    void deleteFact(DFAfact& df) { delete (RDAfact *) &df; }

    void filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local);

    DFAfact& lookupNodeFact(CFGnode& cn, CFGnode * tfSucc = 0);
    DFAfact& lookupNodeFact(PExprCall& dc);
    DFAfact& lookupNodeFact(PExprParallel& dp);

    bool handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& /*ignored*/,
			    PgmExpr * dnode = 0, PgmStmt * cnode = 0, PExprArg * arg = 0);
    bool handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& /*ignored*/, PExprAssign& dnode);
    bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc);
    bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc);
    bool handleDecl(DFAfact& df, PExprDecl& dd);
    bool handleVerify(DFAfact& df, PExprVerify& dv) { return true; } //- RDA: do nothing
    bool handlePredicate(DFAfactPair& dfp, PExprPredicate& dp) { return true; } //- RDA: do nothing

    void handleFreeCall(DFAfact& df, PExprCall& dc) {} //- RDA: do nothing

    void intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn) {} //- RDA: no intraProc
    void interProcFilterEntryFact(DFAfact& df, CFGnode& cn);
    void intraProcHandleCall(DFAfact& df, PExprCall& dc) {} //- RDA: no intraProc
    void interProcHandleCallArgs(DFAfact& df, PExprCall& dc);
    void interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc);

  public:
    static RDAfactHandler handler;

  private:
    //-- static helpers for handleWeakAssign
    static RDAfact * aual_fact;
    static PgmExpr * aual_assign_node;
    static bool add_locs_to_aual_fact(AO& ao);
};

//----------------------------------
//- RDA: reaching-defs analysis

class RDA : public DFA
{
  public:
    RDA() : DFA(RDAfactHandler::handler, true) {}

    bool isUselessNode(CFGnode& cn);
    bool isUselessNode(PgmExpr& dn);
    bool markUselessNode(CFGnode& cn);
    bool markUselessNode(PgmExpr& dn);

    // bool absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter = 0); //-- use base version

    void print_debug_stats(CFG& cfg, FILE * outf) {}

};

//----------------------------------

#endif /* } ifndef TC_RDA_H */

