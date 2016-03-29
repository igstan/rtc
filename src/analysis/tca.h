#ifndef TC_TCA_H /* { */
#define TC_TCA_H

#include "id.h"
#include "edge.h"
#include "ao.h"
#include "ecr.h"
#include "cfg.h"

//------------------------------------------------------
class InputState
{
  public:
    InputState(FILE * f, IDmap& a, IDmap& p, AOlist& v)
	: inf(f), aidmap(a), pidmap(p), values(v) {}
    FILE * inf;
    IDmap& aidmap;
    IDmap& pidmap;
    AOlist& values;
};

//------------------------------------------------------

class TCAstate
{
  public:
    typedef enum { relaos_MBU, relaos_RAN } relaos_mode;	//- for computing relevant aos

    TCAstate() : aidtab(), pidtab(), values(), cfg(),
		 mbu_results(), red_results(), ran_results(),
		 ran_num_all_vps(0), ran_num_finite_vps(0), ran_num_half_finite_vps(0),
		  ran_num_inbounds_vps(0), ran_num_inbounds_sdotarrows(0),
		 ran_num_array_all_vps(0), ran_num_array_finite_vps(0),
		  ran_num_array_half_finite_vps(0), ran_num_array_inbounds_vps(0),
		 ran_num_known_preds(0),
		 assigns(), arg_ret_assigns() {}

    static bool isVulnFunction(const char * cp);

    void processFile(char *);

    void traverseAOs(void (*fp)(AO&));

    void writeTSoutput(FILE *, bool tsl);
    void writeAliases(FILE *);
    void writeCallgraph(FILE *, bool dot, char * trace_fn);

    void addrAssignAnalysis(bool ptrptrw);
    void ptAnalysis();
    void supplementalPTanalysis();
    void possTypeAnalysis();
    void tscAnalysis();
    void invalidPtrAnalysis();
    void allDerefUnsafeAnalysis();
    void tcFlowSensitiveAnalyses();
    void ptrFlowSensitiveAnalyses();
    void markRelevantLocs(bool readwrite, relaos_mode relmode);
    void computeVulnerable();
    void writeVulnerable(FILE * outf);
    void outputLibfnTrackStatus(FILE * outf);

    void debug_dump(FILE *);
    void summary_dump(FILE *);

  private:
    IDlist aidtab, pidtab;
    AOlist values;

    //- flow-sensitive analysis stuff
    CFG cfg;
    RDAfact mbu_results;
    REDresults red_results;
    suco_set<PExprVerify *> ran_results;

    //- some RAN stats
    int ran_num_all_vps;
    int ran_num_finite_vps;
    int ran_num_half_finite_vps;
    int ran_num_inbounds_vps;
    int ran_num_inbounds_sdotarrows;

    int ran_num_array_all_vps;
    int ran_num_array_finite_vps;
    int ran_num_array_half_finite_vps;
    int ran_num_array_inbounds_vps;

    int ran_num_known_preds;

    suco_llist<TCassignEdge *> assigns;
    suco_llist<TCassignEdge *> arg_ret_assigns;

    static suco_llist<TCassignEdge *> * static_arg_ret_assigns; // used by insertArgRetAssignEdges
    static suco_set<ECRargRet *> collectInclToArgRets_ECRs; // used by collectInclToArgRets
    static suco_set<ECRargRet *> insertArgRetAssignEdges_doneset; // used by insertArgRetAssignEdges;

    static void printDerefAliases(AO& ao); // helper for writeAliases
    static void setAddrofDerefUDotUnsafe(AO& ao); // helper for addrAssignAnalysis
    static void markDerefUnsafeAddrofTracked(AO& ao); // helper for addrAssignAnalysis
    static void initPossTypeConstraintsAO(AO& ao); // helper for possTypeAnalysis
    static void initPossTypeConstraintsInclusion(ECR& ecr); // helper for possTypeAnalysis
    static void initPossTypeConstraintsAssign(TCassignEdge& edge); // helper for possTypeAnalysis
    static void addReturnNodeAliases(AO& ao); // called by supplementalPTanalysis
    static bool collectInclToArgRets(ECR& e); // helper for insertArgRetAssignEdges
    static void addAssignEdges(ECR * lecr, ECR * recr, AO::aoKind kind); // helper for insertArgRetAssignEdges
    static void insertArgRetAssignEdges(AO& ao); // helper for supplementalPTanalysis
    static void markInvalidNode(AO& ao); // helper for supplementalPTanalysis
    static void markInvalidNodeNonNull(AO& ao); // helper for invalidPtrAnalysis
    static void markInvalidLocAO(AO& ao); // helper for markAutoInvalidId / invalidPtrAnalysis
    static void markAutoInvalidId(ID& id); // helper for invalidPtrAnalysis
    static void propagateInfluential(ECR& ecr); // helper for assignTSC
    static void propagateExposed(ECR& ecr); // helper for assignTSC
    static void assignTSC(AO& ao); // helper for tscAnalysis
    static void assignTSCderef(AO& ao); // helper for invalidPtrAnalysis
    static void assignTSCderefWrite(AO& ao); // helper for invalidPtrAnalysis
    static void assignAllUnsafeRW(AO& ao); // helper for allDerefUnsafeAnalysis
    static void assignAllUnsafeW(AO& ao); // helper for allDerefUnsafeAnalysis

    static bool markStartingPoints(PgmExpr& pe); // helper for markRelevantLocs
    static relaos_mode msp_relaos_mode; // used by markRelevantLocs/markStartingPoints
    static DependencyMap msp_misc_dependencies; // used by markRelevantLocs/markStartingPoints
    static suco_set<ECR *> msp_starting_ecrs; // used by markRelevantLocs/markStartingPoints

    static void TCAstate::markRelevantAndCollectInclFromECRs(suco_set<AO *>& aoset, suco_set<ECR *>& ecrset); // helper for markRelevantLocs
    static void resetRelevantTag(AO& ao); // helper for -relevant-aos
    static void clearRelevantTag(AO& ao); // helper for markRelevantLocs
    static void countRelevantAOs(AO& ao); // helper for markRelevantLocs
    static int crao_total_aos; // used by countRelevantAOs
    static int crao_marked_aos; // used by countRelevantAOs

    static bool collectVulnerable(PgmExpr& expr); //- helper for computeVulnerable
    static bool writeLibfnTrackStatus(PgmExpr& expr); //- helper for outputLibfnTrackStatus
    static const char * interestingLibFnSignature(ID * fnid); //- helper for writeLibfnTrackStatus

    static TClatRootSet possTypeConstraintRoots; // used for constraint solving

    //- used for verbose output, when generating possible-type constraints
    static int poss_type_init_counter;
    static int poss_type_assign_counter;
    static int poss_type_incl_counter;
};

#define TCA_BUF_SIZE 65535	// Empirically: mesa has one line with 35848 chars; SHOULD WRITE MORE ROBUSTLY!

//------------------------------------------------------

#endif /* } ifdef TC_TCA_H */
