#include "cfg.h"
#include "ecr.h"
#include "flags.h" //- for flag_vtfix
#include "red.h"

//--------------------------------
// REDresults

void REDresults::debug_dump(FILE * outf)
{
  //- write vp results
  fprintf(outf, "--RED-VP-all results:\n");
  suco_iterator<PExprVerify *> pai(this->vp_all);
  while(pai.Iterate())
    pai.Current()->debug_dump(outf, 3, true);

  fprintf(outf, "--RED-VP-write results:\n");
  suco_iterator<PExprVerify *> pwi(this->vp_write);
  while(pwi.Iterate())
    pwi.Current()->debug_dump(outf, 3, true);

  //- write vt results
  fprintf(outf, "--RED-VT results:\n");
  suco_iterator<PExprVerify *> tvi(this->vt_verify);
  while(tvi.Iterate())
    tvi.Current()->debug_dump(outf, 3, true);
  suco_iterator<PExprAssign *> tgi(this->vt_assign);
  while(tgi.Iterate())
    tgi.Current()->debug_dump(outf, 3, true);
}

void REDresults::writeResults(FILE * outf)
{
  fprintf(outf, "# REDSTAT-vp_rdwr(red/all): %d/%d\n", stat_num_vp_red, stat_num_vp);
  fprintf(outf, "# REDSTAT-vp_write(red/all): %d/%d\n", stat_num_vpw_red, stat_num_vpw);
  fprintf(outf, "# REDSTAT-vt_ver(red/all): %d/%d\n", stat_num_vtv_red, stat_num_vtv);
  fprintf(outf, "# REDSTAT-vt_asg(red/all): %d/%d\n", stat_num_vta_red, stat_num_vta);

  //- write vp-all results: ! <N> r p a <int>
  suco_iterator<PExprVerify *> pai(this->vp_all);
  while(pai.Iterate()){
    AID& aid = pai.Current()->getAid();
    fprintf(outf, "! %d r p a %d\n", aid.filestem_id, aid.aid);
  }

  //- write vp-write results: ! <N> r p w <int>
  suco_iterator<PExprVerify *> pwi(this->vp_write);
  while(pwi.Iterate()){
    AID& aid = pwi.Current()->getAid();
    fprintf(outf, "! %d r p w %d\n", aid.filestem_id, aid.aid);
  }

  //- write vt verify results: ! <N> r t v <int>
  suco_iterator<PExprVerify *> tvi(this->vt_verify);
  while(tvi.Iterate()){
    AID& aid = tvi.Current()->getAid();
    fprintf(outf, "! %d r t v %d\n", aid.filestem_id, aid.aid);
  }
  //- write vt assign results: ! <N> r t g <int>
  suco_iterator<PExprAssign *> tgi(this->vt_assign);
  while(tgi.Iterate()){
    AID& aid = tgi.Current()->getAid();
    fprintf(outf, "! %d r t g %d\n", aid.filestem_id, aid.aid);
  }
}

//--------------------------------
// REDfact

DFAfact& REDfact::newClone(bool preserve)
{
  REDfact& nrf = *new REDfact;
  nrf.meet(*this, preserve);
  return nrf;
}

//-- set intersection
// - preserve=false means we are free to tamper with df
// - warnNotLE=true: warn if df has elements not in this
//   --> this warning is not reported in this version!
// - returns true if meet shrinks the size of this
bool REDfact::meet(DFAfact& df, bool preserve, bool warnNotLE)
{
  REDfact& rf = (REDfact&) df;

  if(rf.is_top){ //- meet with top: do nothing

    return false;

  } else if(this->is_top){ //- this is top, rf is not: set to rf

    this->is_top = false;
    this->edescset.Clear(); //- should be redundant
    this->join(rf, preserve);

    return true;

  } else { //- meet non-bot with non-bot

    int precount = this->edescset.Length();
    this->edescset.Intersect(rf.edescset);
    int postcount = this->edescset.Length();

    return (postcount < precount);
  }
}

//-- set union
// - preserve=false means we are free to tamper with df
void REDfact::join(DFAfact& df, bool preserve)
{
  REDfact& rf = (REDfact&) df;

  if(this->is_top){ //- already top: do nothing
    //- nop
  } else if(rf.is_top){ //- join with top: set to top

    this->is_top = true;
    this->edescset.Clear();

  } else { //- non-top meet non-top

    if(preserve){
      this->edescset.Union(rf.edescset);
    } else {
      this->edescset.UnionConsume(rf.edescset);
    }
  }
}

void REDfact::debug_dump(FILE * outf, bool brief)
{
  if(is_top){
    fprintf(outf, "top\n");
  } else if(brief){
    fprintf(outf, " %d exprs\n", this->edescset.Length());
  } else {

    fprintf(outf, "\n");

    suco_iterator<ExpDescr *> edi(edescset);
    while(edi.Iterate()){
      fprintf(outf, "  ExpDescr:\n");
      edi.Current()->debug_dump(outf, 4);
    }
  }
}

//--------------------------------
// REDfactPair

DFAfact& REDfactPair::newClone(bool preserve)
{
  REDfactPair& nrfp = *new REDfactPair;
  nrfp.meet(*this, preserve);
  return nrfp;
}

bool REDfactPair::meet(DFAfact& df, bool preserve, bool warnNotLE)
{
  REDfactPair& rfp = (REDfactPair&) df;
  bool triggered = false;
  triggered |= this->getVPfact().meet(rfp.getVPfact(), preserve, warnNotLE);
  triggered |= this->getVTfact().meet(rfp.getVTfact(), preserve, warnNotLE);
  return triggered;
}

void REDfactPair::join(DFAfact& df, bool preserve)
{
  REDfactPair& rfp = (REDfactPair&) df;
  this->getVPfact().join(rfp.getVPfact(), preserve);
  this->getVTfact().join(rfp.getVTfact(), preserve);
}

void REDfactPair::debug_dump(FILE * outf, bool brief)
{
  fprintf(outf, "\n\t\tVP-Fact: ");
  this->getVPfact().debug_dump(outf, brief);
  fprintf(outf, "\t\tVT-Fact: ");
  this->getVTfact().debug_dump(outf, brief);
}

//--------------------------------
// REDfactHandler

DFAfact& REDfactHandler::lookupNodeFact(CFGnode& cn, CFGnode * tfSucc)
{
  return cn.getREDfactPair();
}

DFAfact& REDfactHandler::lookupNodeFact(PExprCall& dc)
{
  return dc.getREDfactPair(); //- actually useless(?) because intraprocedural!
}

DFAfact& REDfactHandler::lookupNodeFact(PExprParallel& dp)
{
  return dp.getREDfactPair();
}

//--------------------------------
// REDpFactHandler

REDpFactHandler REDpFactHandler::handler;

bool REDpFactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
                            PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  REDfact& fact_vp = ((REDfactPair&) df).getVPfact();

  { //- do VP fact: kill {e | lhs in afflocs(e)}
    //- create singleton set
    suco_set<AO *> lhsset;
    lhsset.Insert(&lhs);

    suco_iterator<ExpDescr *> edi(fact_vp.getEDescSet());
    while(edi.Iterate()){
      if(edi.Current()->affLocsIntersects(lhsset)){
        edi.DeleteCurrent(); //- kill
      }
    }
  }

  return false; //- not useless
}

bool REDpFactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  REDfact& fact_vp = ((REDfactPair&) df).getVPfact();

  //- collect lhs ecrset == aliases(lhs)
  suco_set<ECR *> lecrset;
  { //- collect lhs ecrs
    suco_iterator<AO *> aoi(lhs.getAOs());
    while(aoi.Iterate()){
      AO& lao = *aoi.Current();
      if(!lao.isVal()){
        if(lao.isLoc()) lecrset.Insert(&lao.getECR());
        else lecrset.Union(lao.getECR().getAliasECRs());
      }
    }
  }

  { // - i. kill { e | aliases(lhs) intersects afflocs(e) }
    suco_iterator<ExpDescr *> edi(fact_vp.getEDescSet());
    while(edi.Iterate()){
      if(edi.Current()->affLocsIntersects(lecrset)){
        edi.DeleteCurrent(); //- kill
      }
    }

    // - ii. gen lhs
    fact_vp.getEDescSet().Insert(&lhs);
  }

  return false; //- not useless
}

bool REDpFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  REDfact& fact_vp = ((REDfactPair&) df).getVPfact();

  { //- do VP-all: verify(e) ==> gen e
    if(dv.getVpKind() == PExprVerify::vpPtr)
      fact_vp.getEDescSet().Insert(&dv.getDesc());

    //- NOTE: ignore vpPtrW -- that GEN is handled
    //        in handleWeakAssign (because must kill first)
    //  --> bad design choice to separate vpPtrW from assign nodes!
  }

  return false; //- not useless
}

void REDpFactHandler::handleFreeCall(DFAfact& df, PExprCall& dc)
{
  REDfact& fact_vp = ((REDfactPair&) df).getVPfact();

  PExprArg * arg1 = dc.getArg(1);
  if(arg1){
    //- collect arg1 ecrset == points-to(arg1)
    suco_set<ECR *> a1ecrset;
    { //- collect points-to(arg1) ecrs
      suco_iterator<AO *> aoi(arg1->getDesc().getAOs());
      while(aoi.Iterate()){
        AO& lao = *aoi.Current();
        if(!lao.isVal()){
          a1ecrset.Union(lao.getECR().getPointsTo().getAliasECRs());
        }
      }
    }

    { //- do VP fact
      // - kill { e | points-to(arg1) intersects afflocs(e) }
      suco_iterator<ExpDescr *> edi(fact_vp.getEDescSet());
      while(edi.Iterate()){
        if(edi.Current()->affLocsIntersects(a1ecrset)){
          edi.DeleteCurrent(); //- kill
        }
      }
    }
  } //- else: no argument supplied to free: error?
}

void REDpFactHandler::interProcFilterEntryFact(DFAfact& df, CFGnode& cn)
{
//-NOP for now
}

void REDpFactHandler::intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn)
{
  ((REDfactPair&) df).getVPfact().setBottom(); // empty set
}

void REDpFactHandler::intraProcHandleCall(DFAfact& df, PExprCall& dc)
{
  REDfact& fact_vp = ((REDfactPair&) df).getVPfact();

  suco_iterator<CFGfunction *> fni(dc.getTargetFns());
  while(fni.Iterate()){

    //- do VP fact: kill 2 things:
    //  1. {e | GMOD(fni) intersects afflocs(e)}
    //  2. {e | GFreeHeap(fni) intersects aliases(e) }

    suco_set<ECR *>& freeheap_ecrs = fni.Current()->getGFreeHeap_ecrs();
    suco_iterator<ExpDescr *> edi(fact_vp.getEDescSet());
    while(edi.Iterate()){
      if(fni.Current()->getGMOD().interferesWithAffLocs(*edi.Current())
	|| edi.Current()->getAliasECRs().Intersects(fni.Current()->getGFreeHeap_ecrs())){
        edi.DeleteCurrent(); //- kill
      }
    }
  }
}

//--------------------------------
// REDpcFactHandler

bool REDpcFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  REDfact& fact_vp = ((REDfactPair&) df).getVPfact();

  {
    //- do VP-all
    if(dv.getVpKind() == PExprVerify::vpPtr
	|| dv.getVpKind() == PExprVerify::vpPtrW){
      if(!dv.isLib()) this->results.stat_num_vp++;
      if(fact_vp.getEDescSet().Contains(&dv.getDesc())){
        if(!dv.isLib()) this->results.stat_num_vp_red++;
        this->results.vp_all.Insert(&dv);
      }
    }
  }

  REDpFactHandler::handler.handleVerify(df, dv);
  return false; //- not useless
}

//--------------------------------
// REDpwFactHandler

REDpwFactHandler REDpwFactHandler::handler;

bool REDpwFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  return true; //- useless in -ptrw mode
}

//--------------------------------
// REDpwcFactHandler

bool REDpwcFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  REDfact& fact_vpw = ((REDfactPair&) df).getVPfact();

  {
    //- do VP-write
    if(dv.getVpKind() == PExprVerify::vpPtrW){
      if(!dv.isLib()) this->results.stat_num_vpw++;
      if(fact_vpw.getEDescSet().Contains(&dv.getDesc())){
        if(!dv.isLib()) this->results.stat_num_vpw_red++;
        this->results.vp_write.Insert(&dv);
      }
    }
  }

  // REDpwFactHandler::handler.handleVerify(df, dv); //- NOP anyways
  return false; //- not useless
}

//--------------------------------
// REDtFactHandler

REDtFactHandler REDtFactHandler::handler;

//bool handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc) { }

//bool handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc) { }

//bool REDtFactHandler::handleDecl(DFAfact& df, PExprDecl& dd) { }

bool REDtFactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
                            PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  REDfact& fact_vt = ((REDfactPair&) df).getVTfact();

  //- pre-compte/pre-create useful data items:
  //  - rhs tsc
  AO::ts_categ rhs_tsc = AO::getSetTSC(rhs.getAOs());

  //  - create singleton aoset for lhs
  suco_set<AO *> lhs_aoset;
  lhs_aoset.Insert(&lhs);

  //- loop over fact set
  suco_iterator<ExpDescr *> edi(fact_vt.getEDescSet());
  while(edi.Iterate()){
    ExpDescr& ed = *edi.Current();
    AO * sing_ao = ed.getAOs().GetSingleton();
    if(sing_ao && sing_ao->isLoc()){ //- ed is singleton loc ao
//----//-- a. kill lhs iff rhs badly typed
      if(sing_ao == &lhs && rhs_tsc <= AO::TSC_BADLY_TYPED)
        edi.DeleteCurrent();
      //(else ok: special optimization for singletons)
      //TODO: note this is more precise than &&-ing the ifs, but may be unsafe wrt unions?

    } else { //- ed is multiple or deref ao
//----//-- b. kill {e | lhs in afflocs(e)}
      if(ed.affLocsIntersects(lhs_aoset))
        edi.DeleteCurrent();

//------//-- c. kill {e | lhs in aliases(e)} if rhs badly typed
      if(rhs_tsc <= AO::TSC_BADLY_TYPED
	  && ed.getAliasECRs().Contains(&lhs.getECR())){
        edi.DeleteCurrent();
      }
    }
  }

  return false;
}

bool REDtFactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  REDfact& fact_vt = ((REDfactPair&) df).getVTfact();

  //- pre-compte rhs tsc
  AO::ts_categ rhs_tsc = AO::getSetTSC(rhs.getAOs());

  //- loop over fact set
  {
    suco_iterator<ExpDescr *> edi(fact_vt.getEDescSet());
    while(edi.Iterate()){
      ExpDescr& ed = *edi.Current();

//----//-- a. kill lhs iff rhs badly typed
      if(!ExpDescr::compare(&ed, &lhs)){ //- lhs == ed
	if(rhs_tsc <= AO::TSC_BADLY_TYPED) //- rhs badly typed
          edi.DeleteCurrent();
        //(else don't kill <- key optimization)

      } else {
//------//-- b. kill {e | aliases(lhs) intersects afflocs(e)}
	if(ed.affLocsIntersects(lhs.getAliasECRs()))
          edi.DeleteCurrent();

//------//-- c. kill {e | aliases(lhs) intersects aliases(e)} if rhs badly typed
        if(rhs_tsc <= AO::TSC_BADLY_TYPED &&
	    ed.getAliasECRs().Intersects(lhs.getAliasECRs()))
          edi.DeleteCurrent();
      }
    }
  }

  return false;
}

bool REDtFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  REDfact& fact_vt = ((REDfactPair&) df).getVTfact();

  if(flag_vtfix){ //- do VT: verifyTag(e) ==> gen e
    if(dv.getVtKind() == PExprVerify::vtTag){
      fact_vt.getEDescSet().Insert(&dv.getDesc());
      return false; //- not useless
    }
  }
  return true; //- useless
}

void REDtFactHandler::handleFreeCall(DFAfact& df, PExprCall& dc)
{
//TODO: NOP for now, since our free(p) doesn't change p's tag (can't implement via fncall)
}

void REDtFactHandler::interProcFilterEntryFact(DFAfact& df, CFGnode& cn)
{
//-NOP for now
}

void REDtFactHandler::intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn)
{
  ((REDfactPair&) df).getVTfact().setBottom(); // empty set
}

void REDtFactHandler::intraProcHandleCall(DFAfact& df, PExprCall& dc)
{
  REDfact& fact_vt = ((REDfactPair&) df).getVTfact();

  suco_iterator<CFGfunction *> fni(dc.getTargetFns());
  while(fni.Iterate()){
    //-- do VT facts: kill {e | GMOD(fni.Current) intersects aliases(e)}

    suco_iterator<ExpDescr *> edi(fact_vt.getEDescSet());
    while(edi.Iterate()){
      if(fni.Current()->getGMOD().Intersects(edi.Current()->getAliasECRs())){
        edi.DeleteCurrent(); //- kill
      }
    }
  }
}

//--------------------------------
// REDtcFactHandler

bool REDtcFactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
                            PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  REDtFactHandler::handleStrongAssign(df, lhs, rhs, dnode, cnode, arg); // propagate first, to kill unsafe facts

  if(dnode && dnode->getKind() == PgmExpr::fAssign){

    if(!dnode->isLib()) this->results.stat_num_vta++;

    PExprAssign& pe = *(PExprAssign *) dnode;
    REDfact& fact_vt = ((REDfactPair&) df).getVTfact();

    if(fact_vt.getEDescSet().Contains(&pe.getLHS())){
      if(!dnode->isLib()) this->results.stat_num_vta_red++;
      this->results.vt_assign.Insert(&pe);
    }
  }
  return false; //- collect more: never useless
}

bool REDtcFactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  REDtFactHandler::handleWeakAssign(df, lhs, rhs, dnode); // propagate first, to kill unsafe facts

  REDfact& fact_vt = ((REDfactPair&) df).getVTfact();
  {
    if(!dnode.isLib()) this->results.stat_num_vta++;
    //- do VT-Assign
    if(fact_vt.getEDescSet().Contains(&lhs)){	//- redundancy: lhs should equal dnode.getLHS()
      if(!dnode.isLib()) this->results.stat_num_vta_red++;
      this->results.vt_assign.Insert(&dnode);
    }
  }
  return false; //- not useless
}

bool REDtcFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  REDfact& fact_vt = ((REDfactPair&) df).getVTfact();

  {
    //- do VT
    if(dv.getVtKind() != PExprVerify::vtNone){ //- either verifyTag or verifyRhs
      if(!dv.isLib()) this->results.stat_num_vtv++;
      if(fact_vt.getEDescSet().Contains(&dv.getDesc())){
        if(!dv.isLib()) this->results.stat_num_vtv_red++;
        this->results.vt_verify.Insert(&dv);
      }
    }
  }

  REDtFactHandler::handler.handleVerify(df, dv);
  return false; //- not useless
}

//--------------------------------
// REDtpFactHandler

REDtpFactHandler REDtpFactHandler::handler;

bool REDtpFactHandler::handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
{
  bool is_useless = true;
  is_useless &= REDpFactHandler::handler.handleFormal(df, dd, parfn, interproc);
  is_useless &= REDtFactHandler::handler.handleFormal(df, dd, parfn, interproc);
  return is_useless;
}

bool REDtpFactHandler::handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc)
{
  bool is_useless = true;
  is_useless &= REDpFactHandler::handler.handleReturnStmt(df, retnode, retedesc, interproc);
  is_useless &= REDtFactHandler::handler.handleReturnStmt(df, retnode, retedesc, interproc);
  return is_useless;
}

bool REDtpFactHandler::handleDecl(DFAfact& df, PExprDecl& dd)
{
  bool is_useless = true;
  is_useless &= REDpFactHandler::handler.handleDecl(df, dd);
  is_useless &= REDtFactHandler::handler.handleDecl(df, dd);
  return is_useless;
}

bool REDtpFactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
                            PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  bool is_useless = true;
  is_useless &= REDpFactHandler::handler.handleStrongAssign(df, lhs, rhs, dnode, cnode, arg);
  is_useless &= REDtFactHandler::handler.handleStrongAssign(df, lhs, rhs, dnode, cnode, arg);
  return is_useless;
}

bool REDtpFactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  bool is_useless = true;
  is_useless &= REDpFactHandler::handler.handleWeakAssign(df, lhs, rhs, dnode);
  is_useless &= REDtFactHandler::handler.handleWeakAssign(df, lhs, rhs, dnode);
  return is_useless;
}

bool REDtpFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  bool is_useless = true;
  is_useless &= REDpFactHandler::handler.handleVerify(df, dv);
  is_useless &= REDtFactHandler::handler.handleVerify(df, dv);
  return is_useless;
}

void REDtpFactHandler::handleFreeCall(DFAfact& df, PExprCall& dc)
{
  REDpFactHandler::handler.handleFreeCall(df, dc);
  REDtFactHandler::handler.handleFreeCall(df, dc);
}

void REDtpFactHandler::interProcFilterEntryFact(DFAfact& df, CFGnode& cn)
{
  REDpFactHandler::handler.interProcFilterEntryFact(df, cn);
  REDtFactHandler::handler.interProcFilterEntryFact(df, cn);
}

void REDtpFactHandler::intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn)
{
  REDpFactHandler::handler.intraProcInitializeEntryFact(df, cn);
  REDtFactHandler::handler.intraProcInitializeEntryFact(df, cn);
}

void REDtpFactHandler::intraProcHandleCall(DFAfact& df, PExprCall& dc)
{
  REDpFactHandler::handler.intraProcHandleCall(df, dc);
  REDtFactHandler::handler.intraProcHandleCall(df, dc);
}

//--------------------------------
// REDtpcFactHandler

bool REDtpcFactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
                            PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  bool is_useless = true;
  is_useless &= pcfh.handleStrongAssign(df, lhs, rhs, dnode, cnode, arg);
  is_useless &= tcfh.handleStrongAssign(df, lhs, rhs, dnode, cnode, arg);
  return is_useless;
}

bool REDtpcFactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  bool is_useless = true;
  is_useless &= pcfh.handleWeakAssign(df, lhs, rhs, dnode);
  is_useless &= tcfh.handleWeakAssign(df, lhs, rhs, dnode);
  return is_useless;
}

bool REDtpcFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  bool is_useless = true;
  is_useless &= pcfh.handleVerify(df, dv);
  is_useless &= tcfh.handleVerify(df, dv);
  return is_useless;
}

//--------------------------------
// RED

bool RED::isUselessNode(CFGnode& cn)
{
  return cn.REDisUseless();
}

bool RED::isUselessNode(PgmExpr& dn)
{
  return dn.REDisUseless();
}

bool RED::markUselessNode(CFGnode& cn)
{
  cn.REDsetUseless();
  return true;
}

bool RED::markUselessNode(PgmExpr& dn)
{
  dn.REDsetUseless();
  return true;
}

//- compare df1 with df2: if df2 is lower-than df1, return true
//  then absorb (meet) df2 into df1
bool RED::absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter /*ignored*/)
{
  return df1.meet(df2, false, true); //- (warn=true): if df2 is not <= df1, print warning message
}

void RED::print_debug_stats(CFG& cfg, FILE * outf)
{
//TODO?
}

