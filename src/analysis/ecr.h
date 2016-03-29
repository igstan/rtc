#ifndef TC_ECR_H /* { */
#define TC_ECR_H

#include <stdio.h>

#include "ao.h"
#include "edge.h"
#include "suco.h"

//------------------------------------------------------

class ECRargRet
{ //-TODO: add mechanism to garbage collect (currently, leaked)
  public:
    friend class ECR;
    ECRargRet(ECR& mp): main_parent(&mp), fwdptr(0),
			ret(0), args(0), argsize(0), nargs(max_nargs)
			{}

    void addArg(int argno, ECR& ecr); // remember, argno is one-based
    void addRet(ECR& ecr);

    ECR& getMainParent();

    static void findMaxNargs(AO& ao);
    static int getMaxNargs() { return max_nargs; }

    ECR * get_arg(int argno) const; // remember, argno is one-based
    ECR * get_ret() const { return ret; }

    int getArgSize() const { return argsize; }

  private:
    ECRargRet();

    ECRargRet& followFwdPtr();

    ECR *& getArgPtr(int argno); // remember, argno is one-based
    ECR& getArg(int argno); // remember, argno is one-based
    ECR& getRet();

    void fixECRs(); // called by ECR::fixECR()

    void absorbArgRet(int argno, ECR ** args, ECR * ret); // remember, argno is one-based
    void absorb(ECRargRet * ar);

    static int max_nargs;

    ECR * main_parent;

    ECRargRet * fwdptr;

    ECR * ret;
    ECR ** args;
    int argsize;
    int nargs;
    int refcount;
};

//------------------------------------------------------

class ECR
{
  public:
    friend class AO;
    friend class ECRargRet;
    friend class TCAstate; //-- give access to traverseCounter, exposed, touched

    static ECR& new_ECR();

    void debug_dump(FILE * outf);
    static void debug_dump_list(suco_llist<ECR *>& elist, FILE * outf);

    static void debug_traceInclude(ECR& e);

    suco_set<AO *>& getAOset() { return aoset; }

    ECR& getPointsTo();

    void pointsTo(ECR& ecr);
    void includesTo(ECR& ecr);

    //- follows inclusion edges, while true; if false, stop and return false
    bool traverseAliases(bool (*aofp)(AO& o), bool (*ecrfp)(ECR& e) = 0);
    static bool traverseSetAliases(suco_set<ECR *>& eset, bool (*aofp)(AO& o), bool (*ecrfp)(ECR& e) = 0);

    void collectAliasECRs(suco_set<ECR *>& eset, bool affecting = false);
    void collectInclFromECRs(suco_set<ECR *>& eset); //- called by TCAstate::collectRelevantLocs

    static void filterLocArgRet(suco_set<ECR *>& eset);

    suco_set<ECR *>& getAliasECRs();	// WARNING: this function caches its lookup result,
					// so it should only be called after the points-to graph
					// has quiesced, which is to say after completion of
					// all points-to analysis phases.  In the meantime,
					// use collectAliasECRs.

    void markInvalid(bool propDeref); // marks invalid, and propagate
    bool isInvalid() const { return invalid; }

    ECR& followECR();

    //- functions to manipulate ECRs in AOs
    static void fixECR(AO& ao);
    static ECR& getECR(AO& ao);
    static void unifyECRs(AO& ao1, AO& ao2);

    ECRargRet& getArgRet();

    ECRargRet * argRet(); //- follows forward pointer; returns null if none

    TClatType& possType() { return poss_type; }
    TClatType& writeType() { return write_type; }

    bool poss_type_incl_visited; //- used by TCAstate::initPossTypeConstraintsInclusion
    suco_set<ECR *>& inclToECRs() { return inclTo; }

    void propagateInfluential();
    void propagateExposed();

    void touchExposedAndPropagate();

    void markVulnerableDerefAndPropagate();

    int getEcrNo(FILE * outf = 0); //- if outf, then output inclTo edges if unassigned

  private:
    ECR() : poss_type_incl_visited(false),
	    aoset(),
	    fwdptr(0),
	    ptsFrom(),
	    ptsTo(0),
	    inclFrom(),
	    inclTo(),
	    alias_ecrset(0),
	    argret(0),
	    invalid(false),
	    poss_type(TClatType::tclTop),
	    write_type(TClatType::tclTop),
	    influential(false),
	    exposed(false),
	    touched(false),
	    is_vuln_deref(false),
	    ecrno(0),
	    traverseTag(traverseCounter)
	    {
		poss_type.constrainLE(write_type);
	    }

    //---------------
    //-- PTA members
    suco_set<AO *> aoset;
    ECR * fwdptr;
    suco_set<ECR *> ptsFrom;
    ECR * ptsTo;
    suco_set<ECR *> inclFrom;
    suco_set<ECR *> inclTo;

    suco_set<ECR *> * alias_ecrset;	//- cache of this ECR's alias set

    ECRargRet * argret;

    //--------------------
    //-- post-PTA members
    bool invalid; //- indicates if ECR represents "invalid dereference"

    TClatType poss_type;
    TClatType write_type; //- extra node to make constraint solving more efficient

    bool influential; //- used to propagate influential/exposedness
    bool exposed;     //  (one forward, one backward, along incl edges)

    bool touched;	//- used by range analysis (and other flow-sensitive analyses?)
			//  to indicate an exposed location is referenced
    bool is_vuln_deref; //- identify vulnerable dereferences

    int ecrno; //- unique ID# for this ecr, assigned late, before output

    //-------------------------
    //-- miscellaneous members
    //- used to mark visited ECRs while traversing
    //  - used in: followInclEdges (from traverseAliases,traverseSetAliases)
    //		   findInclToCycle (from includesTo)	** I _think_ findInclToCycle and findPtsToCycle
    //		   findPtsToCycle (from pointsTo)	** don't clash, but am not sure
    //		   collectArgRetFrontier (from includesTo)
    //		   inclToExposed (from AO::pointsToExposed)
    //             finalizeECR/garbageCollect (from TCAstate::supplementalPTanalysis)
    //  - must make sure there are no clashes!
    static int traverseCounter;
    int traverseTag;

    void finalizeECR();
    static void finalizeECRset(suco_set<ECR *>& eset);
    static void garbageCollect(); //- called by TCAstate (friend)

    static suco_llist<ECR *> ecr_pool; //- for garbage collection

    //-- private functions
    ECRargRet * detachArgRet(); //- also adjusts ref count

    void collectArgRetFrontier(suco_set<ECR *>& frontier, ECRargRet& ar);
    static void simulateAssign(int argno, ECRargRet& largret, ECRargRet& rargret); // remember, argno is one-based
    void processArgRetFlow(ECRargRet& largret, ECRargRet& rargret);

    bool findPtsToCycle(ECR& dest, suco_llist<ECR *>& cyEcrs);
    bool findInclToCycle(ECR& dest, suco_llist<ECR *>& cyEcrs);

    ExposedStatus inclToExposed(bool limit_malloc, bool do_touched, bool do_vuln); // called by AO::pointsToExposed

    void linkAO(AO&);
    void absorb(ECR& ecr);

    //- follows inclusion edges, while true; if either fp returns false, stop and return false
    bool followInclEdges(bool (*fp)(AO& o), bool (*ecrfp)(ECR& e)); //- helper for traverseAliases
};

//------------------------------------------------------

#endif /* } ifdef TC_ECR_H */
