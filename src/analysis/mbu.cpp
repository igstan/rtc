#include <stdio.h>
#include <stdlib.h> // for qsort, used in print_debug_stats
#include "ao.h"
#include "ecr.h"
#include "cfg.h"
#include "mbu.h"
#include "flags.h" // for flag_verbose, flag_may_be_uninit
#include "diag.h" // for TCstats

//----------------------------------
// MAY-BE-UNINIT ANALYSIS
//----------------------------------

DFAfact& MBUfact::newClone(bool preserve)
{
  MBUfact& nmf = *new MBUfact;
  nmf.meet(*this, preserve);
  return nmf;
}

//-- set union
// - preserve=false means we are free to tamper with df
// - warnNotLE=true: warn if this has elements not in df
//   --> in this version, this warning is not reported! (except when bottom)
// - returns true if df has elements not in this
bool MBUfact::meet(DFAfact& df, bool preserve, bool warnNotLE)
{
  MBUfact& mf = (MBUfact&) df;

  if(mf.is_bot){ //- meet with bot: set to bot

    bool ret = !this->is_bot;

    this->is_bot = true;
    this->aoset.Clear();

    return ret;

  } else if(this->is_bot) { //- already bot: do nothing

    if(warnNotLE) {
      fprintf(stderr, "Meet: Higher Than Bottom!: ");
      mf.debug_dump(stderr);
    }

    return false;

  } else { //- non-bot meet with non-bot

    int precount = this->length();

    if(preserve){
      this->aoset.Union(mf.aoset);
    } else {
      this->aoset.UnionConsume(mf.aoset);
    }

    int postcount = this->length();

    return (postcount > precount);
  }
}

//-- set intersection
// - preserve=false means we are free to tamper with df
void MBUfact::join(DFAfact& df, bool preserve)
{
  MBUfact& mf = (MBUfact&) df;

  if(mf.is_bot) { //- join with bot: do nothing

    //- nop

  } else if(this->is_bot) { //- this is bot: set to mf

    this->is_bot = false;
    this->aoset.Clear(); //-- redundant
    this->meet(mf, preserve);

  } else { //- join non-bot with non-bot

    this->aoset.Intersect(mf.aoset);

  }
}

void MBUfact::debug_dump(FILE * outf, bool brief)
{
  if(is_bot){
    fprintf(outf, "bot\n");
  } else if(brief){
    fprintf(outf, " %d aos\n", this->length());
  } else {

    fprintf(outf, "\n");

    suco_iterator<AO *> aoi(aoset);
    while(aoi.Iterate()){
      fprintf(outf, "\t[ ");
      aoi.Current()->write_string_rep(outf, true);
      fprintf(outf, " ]\n");
    }
  }
}

//----------------------------------

//-- static helpers for addRelevantAliasLocs
MBUfact * MBUfact::aual_fact = 0;
bool MBUfact::add_locs_to_aual_fact(AO& ao)
{
  if(MBUfact::aual_fact && ao.isLoc() && ao.dfa_relevant)
    aual_fact->insertComponentAOs(ao);
  return true;
}

//-- traverse aliases of aoset, add safe and exposed locs
//   to this.
void MBUfact::addRelevantAliasLocs(suco_set<AO *>& laoset)
{
  //-- collect aoset's ecrset
  suco_set<ECR *> ecrset;
  suco_iterator<AO *> aoi(laoset);
  while(aoi.Iterate())
    ecrset.Insert(&aoi.Current()->getECR());

  //-- set aual_fact
  MBUfact::aual_fact = this;

  //-- do traversal
  ECR::traverseSetAliases(ecrset, MBUfact::add_locs_to_aual_fact);
}

//-- static helpers for intersectsAliases --
suco_set<ECR *> MBUfact::ia_ecrset;

bool MBUfact::is_ia_ecrset(ECR& ecr)
{
  return !MBUfact::ia_ecrset.Contains(&ecr);
}

suco_set<ECR *> MBUfact::cfr_ecrset;

bool MBUfact::collectFnReturns(AO& ao)
{
  if(ao.getKind() == AO::aoFunction){
    AO& ret = ((AOFunction &)ao).getTarget().get_AOReturn();
    MBUfact::cfr_ecrset.Insert(&ret.getECR());
  }
  return true;
}

//-- check to see if this intersects with the alias set of aoset.
//   We'll do this by collecting this' ecrset, and do a traversal
//   of aoset's ecrs to see if they reach any of this' ecrset.
// - OPTIMIZATION: can skip ecrs for value AOs
bool MBUfact::intersectsAliases(suco_set<AO *>& raoset)
{
  //-- collect this's aos' ecrset
  MBUfact::ia_ecrset.Clear();
  suco_iterator<AO *> taoi(this->aoset);
  while(taoi.Iterate()){
    MBUfact::ia_ecrset.Insert(&taoi.Current()->getECR());
  }

  //-- collect raoset's ecrset -- exclude values! (optimization)
  //				- and special handling for return nodes
  MBUfact::cfr_ecrset.Clear();
  suco_iterator<AO *> raoi(raoset);
  while(raoi.Iterate()){
    AO& rao = *raoi.Current();
    if(!rao.isVal()){
      MBUfact::cfr_ecrset.Insert(&rao.getECR());
    }
  }

  //-- do traversal
  return !ECR::traverseSetAliases(MBUfact::cfr_ecrset, 0, MBUfact::is_ia_ecrset);
}

MBUfact * MBUfact::nsao_mbf = 0;

void MBUfact::collectNonStructAO(AO& ao)
{
  if(ao.isLocArgRet() && ao.getStaticType()
	&& ao.getStaticType()->getKind() != TCtype::tcStruct
	&& ao.getStaticType()->getKind() != TCtype::tcUnion){
    MBUfact::nsao_mbf->aoset.Insert(&ao);
  }
}

void MBUfact::removeAO(AO& ao)
{
  if(ao.isLocArgRet()){
    MBUfact::nsao_mbf->aoset.Remove(&ao);
  }
}

//- (if AO is a struct/union) insert scalar components of AO
//  - optimize: unless root ao is a malloc, then it's pointless
void MBUfact::insertComponentAOs(AO& ao)
{
  if(ao.isLocArgRet() && (ao.getEnclosingStruct().getKind() != AO::aoMalloc)
	&& ao.getStaticType()
	&& (ao.getStaticType()->getKind() == TCtype::tcStruct
	   || ao.getStaticType()->getKind() == TCtype::tcUnion)){
    MBUfact::nsao_mbf = this;
    ao.traverseAOs(MBUfact::collectNonStructAO);
    MBUfact::nsao_mbf = 0;
  } else {
    this->aoset.Insert(&ao);
  }
}

//- (if AO is a struct/union) remove scalar components of AO
void MBUfact::removeComponentAOs(AO& ao)
{
//TODO: if ao is a struct, remove its component scalars
  if(ao.isLocArgRet() && ao.getStaticType() &&
	(ao.getStaticType()->getKind() == TCtype::tcStruct
	|| ao.getStaticType()->getKind() == TCtype::tcUnion)){
    MBUfact::nsao_mbf = this;
    ao.traverseAOs(MBUfact::removeAO);
    MBUfact::nsao_mbf = 0;
  } else {
    this->aoset.Remove(&ao);
  }
}

//----------------------------
// MBUfactHandler

MBUfactHandler MBUfactHandler::handler;

void MBUfactHandler::filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local)
{
  MBUfact& mbf = (MBUfact&) df;
  MBUfact& mbf_local = (MBUfact&) df_local;

  //- 1. move all facts to mbf_local
  mbf_local.meet(mbf, false);

  //- 2. filter GREF elements back into mbf
//TODO: filter also MayFreed pointers?
  suco_iterator<CFGfunction *> tfi(dc.getTargetFns());
  while(tfi.Iterate()){
    suco_iterator<AO *> aoi(mbf_local.aoset);
    while(aoi.Iterate()){
      AO * tao = aoi.Current();
      if(tfi.Current()->getGREF().Contains(*tao)){
        aoi.DeleteCurrent();
        mbf.aoset.Insert(tao);
      }
    }
  }

  //- 3. specially handle call arguments
//  for(int i = 1; dc.getArg(i); ++i)
//    mbf_local.filterInto(dc.getArg(i)->getDesc().getAOs(), mbf);
//TODO: migrate to interProcHandleCallArgs below
}

void MBUfactHandler::interProcHandleCallArgs(DFAfact& df, PExprCall& dc)
{
//TODO-- replace "specially handle call arguments" code above?
}

void MBUfactHandler::interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc)
{
//TODO
}

DFAfact& MBUfactHandler::lookupNodeFact(CFGnode& cn, CFGnode * tfSucc)
{
  return cn.getMBUfact();
}

DFAfact& MBUfactHandler::lookupNodeFact(PExprCall& dc)
{
  return dc.getMBUfact();
}

DFAfact& MBUfactHandler::lookupNodeFact(PExprParallel& dp)
{
  return dp.getMBUfact();
}

bool MBUfactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
				 PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  MBUfact& mf = (MBUfact&) df;
  if(lhs.dfa_relevant){
    if(flag_may_be_uninit < 4 && mf.intersectsAliases(rhs.getAOs())){
      mf.insertComponentAOs(lhs);
    } else {
      mf.removeComponentAOs(lhs);
    }
    return false;
  } else return true; //- useless
}

bool MBUfactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  MBUfact& mf = (MBUfact&) df;
  if(flag_may_be_uninit < 4){
    if(AO::getSetTSC(lhs.getAOs()) >= AO::TSC_EXPOSED){
      if(mf.intersectsAliases(rhs.getAOs())){
        mf.addRelevantAliasLocs(lhs.getAOs());
      } //-- else cannot kill anything
      return false;
    } else return true; //- useless
  } else return true;
}

bool MBUfactHandler::handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
{
  if(interproc){ //- interprocedural
    MBUfact& mf = (MBUfact&) df;
    if(flag_may_be_uninit < 3){
      if(dd.getAO().dfa_relevant){ //- skip if not safe (optimize)
        suco_iterator<PExprCall *> csi(parfn.getCallSites());
        while(csi.Iterate()){
          PExprArg * arg = csi.Current()->getArg(dd.getArgNo());
          if(arg){
            //-- process "weak" assignment [dd.ao] = argaos
            if(mf.intersectsAliases(arg->getDesc().getAOs())){
              mf.insertComponentAOs(dd.getAO());
            }
          } else { //-- this arg not included in callsite: set to uninit!?
                   // - note: may want to ignore -- assume PTA imprecision?
            mf.insertComponentAOs(dd.getAO());
          }
        }
        return false;
      } else return true; //- useless
    } else return true;
  } else { //- if intraprocedural, treat as declaration
    return handleDecl(df, dd);
  }
}

bool MBUfactHandler::handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc)
{
  bool is_useless = true;
  if(interproc){ //- interprocedural
    CFGfunction& parfn = retnode.getParentFunction();
    if(flag_may_be_uninit < 2 && retedesc){
      is_useless &= handleStrongAssign(df, parfn.getId().get_AOFunction().get_AOReturn(), *retedesc, 0, &retnode);
    }
    //-- filter out local vars, only if non-recursive
//TODO: think about this
    if(!parfn.getRecursionId()){
      is_useless = false;
      MBUfact& mf = (MBUfact&) df;
      suco_iterator<AOId *> lvi(parfn.getLocalVars());
      while(lvi.Iterate())
        mf.removeComponentAOs(*lvi.Current());
    }
  }
  return is_useless;
}

bool MBUfactHandler::handleDecl(DFAfact& df, PExprDecl& dd)
{
    
  MBUfact& mf = (MBUfact&) df;
  if((!dd.isZeroed()) &&
	(dd.isLocal() || dd.isMalloc()
	 || (flag_may_be_uninit < 3 && dd.isFormal()))){
    AO& ao = dd.getAO();
    if(ao.dfa_relevant){
      mf.insertComponentAOs(ao);
    }
    return false;
  } else return true; //- useless
}

bool MBUfactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  if(flag_vtfix){
    if(dv.getVtKind() == PExprVerify::vtTag){
      MBUfact& mf = (MBUfact&) df;
      AO * ao = dv.getDesc().getSingletonLoc();
      if(ao && ao->dfa_relevant){
        mf.removeComponentAOs(*ao);
      }
      return false; //- not useless
    }
  }
  return true; //- useless
}

void MBUfactHandler::interProcFilterEntryFact(DFAfact& df, CFGnode& cn) 
{
//NOP for now
}

//- for intra-procedural analysis: initialize fact df for entry node cn
//  - MBU: fact will be IMOD or GREF set, depending on flag_compute_iref
//    (i.e., conservatively assume all IREF/GREF variables may be uninitialized)
//  - NOTE: in IREF mode, it is unsafe at a callsite to correctly check
//    a called function's GREF against the current fact, which may yield
//    significantly-improved results --- doesn't seem to happen in practice.
//    See intraProcHandleCall().
void MBUfactHandler::intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn)
{
  MBUfact& mf = (MBUfact&) df;

  if(flag_compute_iref){
    cn.getParentFunction().getIREF().addToAOset(mf.aoset);
  } else {
    cn.getParentFunction().getGREF().addToAOset(mf.aoset);
  }
  if(flag_gmodref_skip_locals){
    mf.aoset.Union((suco_set<AO *>&)cn.getParentFunction().getLocalVars());
  }

  //-optimization: keep only "relevant" aos:
  suco_iterator<AO *> aoi(mf.aoset);
  while(aoi.Iterate()){
    AO& ao = *aoi.Current();
    if(!ao.dfa_relevant
	|| ((flag_may_be_uninit == 4) && (ao.getEnclosingStruct().isZeroed())) //- if not handling assignments, filter out zeroed aos
	|| (ao.getStaticType()
	    && (ao.getStaticType()->getKind() == TCtype::tcStruct
		|| ao.getStaticType()->getKind() == TCtype::tcUnion))){
//TODO: filter out structs
      aoi.DeleteCurrent();
    }
  }
}

//- for each target function: 
//  - if target GREF doesn't intersect DF, then add nothing
//    (count how frequently this occurs!)
//  - else (common case?) add target GMOD to DF (filtered by local GREF)
//- also: add target's retnode (TODO)
void MBUfactHandler::intraProcHandleCall(DFAfact& df, PExprCall& dc)
{
  MBUfact& mf = (MBUfact&) df;

  if(flag_may_be_uninit < 4){ //- skip if not handling assignments

    suco_iterator<CFGfunction *> fni(dc.getTargetFns());
    while(fni.Iterate()){
      if(flag_compute_iref){ //- iref mode: don't try to "optimize"
        if(dc.getParentNode()){
	  dc.getParentNode()->getParentFunction().getIREF().writeIntersectingAOs(fni.Current()->getGMOD(), mf.aoset);
        }
      } else { //- gref mode: if fact and gmod don't intersect, can skip (don't think this happens ever!)
        if(fni.Current()->getGREF().Intersects(mf.aoset)){
          fni.Current()->getGMOD().addToAOset(mf.aoset);
//TODO: add struct fields? / filter out structs?
        } else if(flag_verbose) {
          //-- function doesn't access anything in mf - add nothing, but report
          fprintf(stderr, "NOTE: intra-proc skipped call to %s (MF size = %d, GREF size = ??)\n",
			fni.Current()->getId().getPid().getname(),
			mf.aoset.Length());
        }
      }
    }

    //- add { R D dc.Faos }  (actually, picking just one representative should be enough?
    suco_iterator<AO *> faoi(dc.getFaos());
    while(faoi.Iterate())
      mf.insertComponentAOs(faoi.Current()->get_AOStar().get_AOReturn());
  }
}

//----------------------------------
// MBUc1

//-- helpers for handleVerify/collectFact
MBUfact * MBUc1factHandler::cf_mf = 0;
RDAfact * MBUc1factHandler::cf_res = 0;

//-- helper for handleVerify
bool MBUc1factHandler::collectFact(AO& ao)
{
  // assert cf_mf?
  if(MBUc1factHandler::cf_mf->contains(ao)){
    MBUc1factHandler::cf_res->addFact(ao);
  }
  return true;
}

bool MBUc1factHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  MBUfact& mf = (MBUfact&) df;

  //- diagnostic data
  {
    int mfsize = mf.length();
    TCstats::mbuc_verify_visit_factsizes += mfsize;
    if(TCstats::mbuc_verify_visit_max_factsize < mfsize)
       TCstats::mbuc_verify_visit_max_factsize = mfsize;
    TCstats::mbuc_verify_visits++;
  }

  switch(dv.getVtKind()){
    case PExprVerify::vtTag:
    case PExprVerify::vtRhs: {
        if(mf.intersectsAliases(dv.getAOs())){
          //-- include in results
          // - A. each AO in dv (so their aliases can be marked exposed)
          // - B. each AO in mf that intersects with dv's AOs (so their delta will be examined)

          // - A. collect dv's AOs
          suco_iterator<AO *> aoi(dv.getAOs());
          while(aoi.Iterate()){
            AO& ao = *aoi.Current();
            if(ao.dfa_relevant && ao.getTSC() >= AO::TSC_EXPOSED){
              this->results.addFact(ao, &dv);
            }
          }

          // - B. collect AOs in mf that intersect with dv's AOs
          //  - B1. collect dv ECRs
          suco_set<ECR *> dv_ecrs;
          suco_iterator<AO *> dvaoi(dv.getAOs());
          while(dvaoi.Iterate()){
            AO& dvao = *dvaoi.Current();
            if(!dvao.isVal()){
              dv_ecrs.Insert(&dvao.getECR());
            }
          }

          //  - B2. traverse dv ECRs; helper collectFact will do the work
          MBUc1factHandler::cf_mf = &mf;
          MBUc1factHandler::cf_res = &this->results;
          ECR::traverseSetAliases(dv_ecrs, MBUc1factHandler::collectFact);
        }
      } break;
    case PExprVerify::vtNone:
    default:
      break;
  }

//  MBUfactHandler::handler.handleVerify(mf, dv); // NOP anyways

  return false; //- not useless
}

//----------------------------------
// MBUc2

bool MBUc2factHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
                            PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  bool inpre = ((MBUfact&)df).contains(lhs);
  MBUfactHandler::handler.handleStrongAssign(df, lhs, rhs, dnode, cnode, arg);
  bool inpost = ((MBUfact&)df).contains(lhs);
  if((inpre || inpost) && this->results.hasKey(lhs)){
    this->results.addFact(lhs, dnode, cnode, arg);
  }
  return false; //- not useless
}

//- case 1: rhs may-be-uninit
//		*p = rhs	// F = F U aliases(*p)
//	    -> must instrument
//- case 2: rhs not uninit
//		*p = rhs	// F = F
//	    -> if (aliases(*p)|results) intersects F
//	       then instrument
bool MBUc2factHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{ 
  MBUfact& mf = (MBUfact&)df;
  int numpre = mf.length();
  MBUfactHandler::handler.handleWeakAssign(df, lhs, rhs, dnode); 
  int numpost = mf.length();
  if(numpre != numpost || mf.intersectsAliases(lhs.getAOs())){ //TODO: should further filter by this->results!
    suco_iterator<AO *> laoi(lhs.getAOs());
    while(laoi.Iterate()){
      AO& lao = *laoi.Current();
      if(!lao.isVal() && (lao.getTSC() >= AO::TSC_EXPOSED)){
        this->results.addFact(lao, &dnode);
      }
    }
  }
  return false; //- not useless
}

bool MBUc2factHandler::handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
{
  MBUfactHandler::handler.handleFormal(df, dd, parfn, interproc);
  if(((MBUfact&)df).contains(dd.getAO()) && this->results.hasKey(dd.getAO())){
    this->results.addFact(dd.getAO(), &dd, 0, 0);
    suco_iterator<PExprCall *> csi(parfn.getCallSites());
    while(csi.Iterate()){
      PExprArg * arg = csi.Current()->getArg(dd.getArgNo());
      if(arg) this->results.addFact(dd.getAO(), 0, 0, arg);
    }
  }
  return false; //- not useless
}

bool MBUc2factHandler::handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc)
{
  MBUfactHandler::handler.handleReturnStmt(df, retnode, retedesc, interproc);
  AO& fnret_ao = retnode.getParentFunction().getId().get_AOFunction().get_AOReturn();
  if(((MBUfact&)df).contains(fnret_ao) && this->results.hasKey(fnret_ao)){
    this->results.addFact(fnret_ao, 0, &retnode, 0);
  }
  return false;
}

//- with delta approach, all non-statics must be initially uninit!
bool MBUc2factHandler::handleDecl(DFAfact& df, PExprDecl& dd)
{
  MBUfactHandler::handler.handleDecl(df, dd);
  if(this->results.hasKey(dd.getAO()) && (dd.isLocal() || dd.isMalloc())){ //TODO: include malloc?
    this->results.addFact(dd.getAO(), &dd, 0, 0);
  }
  return false; //- not useless
}

//----------------------------------
// MBU

bool MBU::isUselessNode(PgmExpr& dn)
{
  return dn.MBUisUseless();
}

bool MBU::isUselessNode(CFGnode& cn)
{
  return cn.MBUisUseless();
}

bool MBU::markUselessNode(PgmExpr& dn)
{
  dn.MBUsetUseless();
  return true;
}

bool MBU::markUselessNode(CFGnode& cn)
{
  cn.MBUsetUseless();
  return true;
}

//- compare df1 with df2: if df2 is lower-than df1, return true
//  then absorb (meet) df2 into df1
//- (warn=true): if df2 is not <= df1, print warning message
bool MBU::absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter/*ignored*/)
{
  if(flag_mbu_lowerthan_by_size &&
    (((MBUfact &)df1).length() == ((MBUfact &)df2).length())){

      return false;

  } else {

    return df1.meet(df2, false, true);

  }
}

//-- used in print_debug_stats
struct fmapentry {
  CFGfunction * fptr;
  int count;
  int oldcount;
};

static int fme_compare(const void * fe1, const void * fe2)
{
  return (((const struct fmapentry *)fe2)->count - ((const struct fmapentry *)fe1)->count);
}

//-- print number of CFG nodes, max and average fact size
void MBU::print_debug_stats(CFG& cfg, FILE * outf)
{
  int num_nodes = 0;
  int tot_facts = 0;
  int max_facts = 0;

  suco_iterator<CFGfunction *> fli(cfg.getFunctionList());
  while(fli.Iterate()){
    if(flag_use_bblocks){
      suco_iterator<CFGbblock *> bbi(fli.Current()->getBBlist());
      while(bbi.Iterate()){
        CFGbblock& bb = *bbi.Current();
        //-- count facts
        MBUfact& mf = bb.getMBUfact();
        int len = mf.length();
        num_nodes++;
        tot_facts += len;
        if(max_facts < len){
          max_facts = len;
        }
      }
    } else {
      suco_iterator<PgmStmt *> sti(fli.Current()->getStmtList());
      while(sti.Iterate()){
        PgmStmt& st = *sti.Current();
        //-- count facts
        MBUfact& mf = st.getMBUfact();
        int len = mf.length();
        num_nodes++;
        tot_facts += len;
        if(max_facts < len){
          max_facts = len;
        }
      }
    }
  }

  if(flag_verbose == 4){
    static fmapentry * func_map = 0;
    static int num_funcs = 0;
    int i;

    if(!func_map){
      //-- one-time initialization of func_map
      num_funcs = cfg.getFunctionList().Length();
      func_map = new struct fmapentry[num_funcs];
      for(i = 0; i < num_funcs; ++i){
        func_map[i].fptr = 0;
        func_map[i].count = 0;
        func_map[i].oldcount = 0;
      }
    }
    for(i = 0; i < num_funcs; ++i){
      func_map[i].oldcount = func_map[i].count;
      func_map[i].count = 0;
    }

    suco_iterator<CFGnode *> wli(this->worklist.list);
    while(wli.Iterate()){
      CFGfunction * fptr = &wli.Current()->getParentFunction();
      for(i = 0; i < num_funcs; ++i){
        if(!func_map[i].fptr){
          func_map[i].fptr = fptr;
          func_map[i].count = 1;
          break;
        } else if(func_map[i].fptr == fptr){
          func_map[i].count++;
          break;
        }
      }
    }

    qsort(func_map, num_funcs, sizeof(struct fmapentry), fme_compare);

    int mode = 0;
    fprintf(outf, "Worklist content: top 5 functions:\n\t");
    for(i = 0; i < num_funcs && func_map[i].count; ++i){
      if(i == 5){ // switch mode to active delta
        mode = 1;
        fprintf(outf, "\nRemaining active with deltas:");
      }
      if(mode == 0){
        fprintf(outf, " %s(%d/%d)", func_map[i].fptr->getId().getPid().getname(),
				  func_map[i].count, func_map[i].count - func_map[i].oldcount);
      } else if(func_map[i].count - func_map[i].oldcount){
        if(!((mode-1) % 5)) fprintf(outf, "\n\t");
        mode++;
        fprintf(outf, " %s(%d/%d)", func_map[i].fptr->getId().getPid().getname(),
				  func_map[i].count, func_map[i].count - func_map[i].oldcount);
      }
    }
    fprintf(outf, "\n");
  }

  fprintf(outf, "Facts: total %d, per %s %.2f, max = %d\n",
		tot_facts, flag_use_bblocks?"bblock":"stmt",
		(num_nodes)?((float)tot_facts/(float)num_nodes):((float)tot_facts),
		max_facts);
}

//----------------------------------
// END
//----------------------------------

