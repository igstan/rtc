#include <stdio.h>
#include "ecr.h"
#include "cfg.h"
#include "rdmb.h"
#include "flags.h"

//--------------------------------
// collecting MBU facts

DFAfact& RDMBfact::newClone(bool preserve)
{
  return *new RDMBfact( (RDAfact&) this->rda_fact.newClone(preserve),
			(MBUfact&) this->mbu_fact.newClone(preserve));
}

void RDMBfact::setTop()
{
  this->rda_fact.setTop();
  this->mbu_fact.setTop();
}

void RDMBfact::setBottom()
{
  this->rda_fact.setBottom();
  this->mbu_fact.setBottom();
}

bool RDMBfact::meet(DFAfact& df, bool preserve, bool warnNotLE)
{
  bool r = this->rda_fact.meet(((RDMBfact&)df).rda_fact, preserve, warnNotLE);
  bool m = this->mbu_fact.meet(((RDMBfact&)df).mbu_fact, preserve, warnNotLE);
  return (m || r);
}

void RDMBfact::join(DFAfact& df, bool preserve)
{
  this->rda_fact.join(((RDMBfact&)df).rda_fact, preserve);
  this->mbu_fact.join(((RDMBfact&)df).mbu_fact, preserve);
}

void RDMBfact::debug_dump(FILE * outf, bool brief)
{
  fprintf(outf, "--RDMBfact::debug_dump:\n");
  fprintf(outf, "----RDA:");
  this->rda_fact.debug_dump(outf, brief);
  fprintf(outf, "----MBU:");
  this->mbu_fact.debug_dump(outf, brief);
  fprintf(outf, "--end RDMBfact::debug_dump\n");
}

//------------------------------

RDAfact * RDMBfact::results = 0;

//------------------------------

AO * RDMBfact::cf_ao = 0;
RDAfact * RDMBfact::cf_rda_fact = 0;
PgmStmt * RDMBfact::cf_parent_node = 0;

bool RDMBfact::collectFacts(AO& ao)
{
  //-- add reaching-defs to results -- including cached grefs
  if(cf_ao && cf_rda_fact){ //- assert
    RDMBfact::results->copyFacts(*cf_ao, ao, *cf_rda_fact);

    // - traverse calling context - add cached GMOD-filtered facts
    if(cf_parent_node){
      //- collect caller functions (recursively)
      suco_set<CFGfunction *> fns;
      CFGfunction& cfn = cf_parent_node->getParentFunction();
      CFGfunction::collectCallers(cfn, fns);
      //- now walk caller functions
      suco_iterator<CFGfunction *> fni(fns);
      while(fni.Iterate()){
        suco_iterator<PExprCall *> csi(fni.Current()->getCallSites());
        while(csi.Iterate()){
          RDMBfact::results->copyFacts(*cf_ao, ao, csi.Current()->getRDAlocal());
        }
      }
    }

  } else fprintf(stderr, "collectFacts: cf_ao or cf_rda_fact is null!\n");

  return true;
}

void RDMBfact::addFacts(suco_set<AO *>& aoset, PgmExpr& dnode, PgmStmt * cnode, PExprArg * arg)
{
  //-- collect MBU results
  if(RDMBfact::results){ //- assert
    //-- for each AO...
    suco_iterator<AO *> aoi(aoset);
    while(aoi.Iterate()){
      AO& ao = *aoi.Current();
      if(ao.getTSC() >= AO::TSC_EXPOSED){
//future TODO?: modify to have one LocAidNode per uninit instance?
        //-- add this verifyTag to results
        RDMBfact::results->addFact(ao, &dnode, cnode, arg);

        //-- add reaching-defs of ao's aliases to results, including cached grefs
        RDMBfact::cf_ao = &ao;
        RDMBfact::cf_rda_fact = &this->rda_fact;
        RDMBfact::cf_parent_node = dnode.getParentNode();
        ao.getECR().traverseAliases(RDMBfact::collectFacts);
      }
    }
  } else fprintf(stderr, "RDMBfact::addFacts: results not set!\n");
}

//--------------------------------
// collecting MBU results

DFAfact& RDMBfactHandler::newTopFact()
{
  return *new RDMBfact(*new RDAfact, *new MBUfact);
}

void RDMBfactHandler::deleteFact(DFAfact& df)
{
  RDMBfact& cmf = (RDMBfact&) df;
  delete &cmf.rda_fact;
  delete &cmf.mbu_fact;
  delete &cmf;
}

void RDMBfactHandler::filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local)
{
  RDMBfact& cmbf = (RDMBfact&) df;
  RDMBfact& cmbf_local = (RDMBfact&) df_local;

  //- 1. move all facts to cmbf_local
  cmbf_local.meet(cmbf, false);

  //- 2. filter elements back into cmbf
  //~~ RDA portion: don't call RDA version, because it caches its results (may not matter?)
  suco_iterator<CFGfunction *> tfi(dc.getTargetFns());
  while(tfi.Iterate()){
    cmbf_local.rda_fact.filterGmodInto(*tfi.Current(), cmbf.rda_fact);
  }
  //~~ MBU portion
  MBUfactHandler::handler.filterCallsiteFacts(dc, cmbf.mbu_fact, cmbf_local.mbu_fact);

  //- 3. specially handle call arguments
  //  - not necessary for collecting phase
}

void RDMBfactHandler::interProcHandleCallArgs(DFAfact& df, PExprCall& dc)
{
// TODO?  or NOP?
}

void RDMBfactHandler::interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc)
{
// TODO?  or NOP?
}

DFAfact& RDMBfactHandler::lookupNodeFact(CFGnode& cn, CFGnode * tfSucc)
{
  return cn.getRDMBfact();
}

DFAfact& RDMBfactHandler::lookupNodeFact(PExprCall& dc)
{
  return dc.getRDMBfact();
}

DFAfact& RDMBfactHandler::lookupNodeFact(PExprParallel& dp)
{
  return dp.getRDMBfact();
}

bool RDMBfactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs, PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.handleStrongAssign(cmf.rda_fact, lhs, rhs, dnode, cnode, arg);
  MBUfactHandler::handler.handleStrongAssign(cmf.mbu_fact, lhs, rhs, dnode, cnode, arg);
  return false; //- not useless
}

bool RDMBfactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.handleWeakAssign(cmf.rda_fact, lhs, rhs, dnode);
  MBUfactHandler::handler.handleWeakAssign(cmf.mbu_fact, lhs, rhs, dnode);
  return false; //- not useless
}

bool RDMBfactHandler::handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.handleFormal(cmf.rda_fact, dd, parfn, interproc);
  MBUfactHandler::handler.handleFormal(cmf.mbu_fact, dd, parfn, interproc);
  return false; //- not useless
}

bool RDMBfactHandler::handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.handleReturnStmt(cmf.rda_fact, retnode, retedesc, interproc);
  MBUfactHandler::handler.handleReturnStmt(cmf.mbu_fact, retnode, retedesc, interproc);
  return false;
}

bool RDMBfactHandler::handleDecl(DFAfact& df, PExprDecl& dd)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.handleDecl(cmf.rda_fact, dd);
  MBUfactHandler::handler.handleDecl(cmf.mbu_fact, dd);
  return false; //- not useless
}

bool RDMBfactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  RDMBfact& cmf = (RDMBfact&) df;
  switch(dv.getVtKind()){
    case PExprVerify::vtTag:
    case PExprVerify::vtRhs: {
        //-- collect MBU results
        if(cmf.mbu_fact.intersectsAliases(dv.getAOs())){
          cmf.addFacts(dv.getAOs(), dv);
        }
      } break;
    case PExprVerify::vtNone:
    default:
      break;
  }

  //-- process fact
  RDAfactHandler::handler.handleVerify(cmf.rda_fact, dv);
  MBUfactHandler::handler.handleVerify(cmf.mbu_fact, dv);
  return false; //- not useless
}

void RDMBfactHandler::handleFreeCall(DFAfact& df, PExprCall& dc)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.handleFreeCall(cmf.rda_fact, dc);
  MBUfactHandler::handler.handleFreeCall(cmf.mbu_fact, dc);
}

void RDMBfactHandler::interProcFilterEntryFact(DFAfact& df, CFGnode& cn)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.interProcFilterEntryFact(cmf.rda_fact, cn);
  MBUfactHandler::handler.interProcFilterEntryFact(cmf.mbu_fact, cn);
}

void RDMBfactHandler::intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.intraProcInitializeEntryFact(cmf.rda_fact, cn);
  MBUfactHandler::handler.intraProcInitializeEntryFact(cmf.mbu_fact, cn);
}

void RDMBfactHandler::intraProcHandleCall(DFAfact& df, PExprCall& dc)
{
  RDMBfact& cmf = (RDMBfact&) df;
  RDAfactHandler::handler.intraProcHandleCall(cmf.rda_fact, dc);
  MBUfactHandler::handler.intraProcHandleCall(cmf.mbu_fact, dc);
}


