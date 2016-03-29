#include "cfg.h"
#include "ecr.h"
#include "flags.h" //- for flag_readable_output
#include "pmbu.h"

//----------------------------------
// vp MAY-BE-UNINIT ANALYSIS
//----------------------------------

suco_set<ECR *> pMBUfactHandler::relevant_ecrs;

bool pMBUfactHandler::collectStartingPoints(PgmExpr& pe)
{
  if(pe.getKind() == PgmExpr::fVerify){
    PExprVerify& pv = (PExprVerify &)pe;
    if((pv.getVpKind() == PExprVerify::vpPtrW) ||
	(pMBUfactHandler::readwrite && (pv.getVpKind() == PExprVerify::vpPtr))){
      suco_iterator<AO *> aoi(pv.getDesc().getAOs());
      while(aoi.Iterate()){
        if(aoi.Current()->getTSC() > AO::TSC_POSS_INVALID){
          AO * ao = aoi.Current()->derefOneLevel();
          if(ao && !ao->isVal()){
            pMBUfactHandler::relevant_ecrs.Union(ao->getECR().getAliasECRs());
          }
        }
      }
    }
  }

  return true; //- continue traversal
}

void pMBUfactHandler::doInsensitiveAnalysis(CFG& cfg)
{
  //- 1. collect starting points (into relevant_ecrs)
  cfg.traverseRootNodes(pMBUfactHandler::collectStartingPoints);

  //- 2. follow assignment edges backwards
  suco_set<ECR *> worklist;
  worklist.Union(pMBUfactHandler::relevant_ecrs);
  while(!worklist.IsEmpty()){
    ECR& ecr = *worklist.RemoveHead();
    //- for each ecr in worklist: traverse its aos
    suco_iterator<AO *> aoi(ecr.getAOset());
    while(aoi.Iterate()){
      //  1. collect locs in results
      if(aoi.Current()->isLocArgRet()){
        pMBUfactHandler::results.Insert(aoi.Current());
      }
      //  2. follow incl-edges backwards
      //NOTE: technically, only want to follow "copy" edges,
      // and skip assignments from a (+) node; but the existence
      // of such a node would've rendered the starting point
      // "unsafe", so it should never be encountered!?
      suco_iterator<TCassignEdge *> edi(aoi.Current()->getIncomingAssignEdges());
      while(edi.Iterate()){
        ECR& fecr = edi.Current()->getFrom().getECR();
        if(!pMBUfactHandler::relevant_ecrs.Contains(&fecr)){
          suco_set<ECR *> fecr_aliases;
          fecr.collectAliasECRs(fecr_aliases);	//- using this because getAliasECRs filters loc/arg/ret
//          fecr_aliases.Subtract(pMBUfactHandler::relevant_ecrs);	//-TODO: avoid repetition
          pMBUfactHandler::relevant_ecrs.Union(fecr_aliases);
          worklist.UnionConsume(fecr_aliases);
        }
      }
    }
  }
}

void pMBUfactHandler::writeResults(CFG& cfg, FILE * outf)
{
  { //- write p-MBU results
    suco_iterator<AO *> aoi(pMBUfactHandler::results);
    while(aoi.Iterate()){
      fprintf(outf, "M ");
      aoi.Current()->write_string_rep(outf, flag_readable_output);
      fprintf(outf, "\n");
    }
  }
  { //- write p-MBU unsafe aos
    suco_iterator<AO *> aoi(pMBUfactHandler::unsafe);
    while(aoi.Iterate()){
      fprintf(outf, "U ");
      aoi.Current()->write_string_rep(outf, flag_readable_output);
      fprintf(outf, "\n");
    }
  }
  { //- write p-MBU tracked aos
    suco_iterator<AO *> aoi(pMBUfactHandler::tracked);
    while(aoi.Iterate()){
      fprintf(outf, "T ");
      aoi.Current()->write_string_rep(outf, flag_readable_output);
      fprintf(outf, "\n");
    }
  }
}

//----------------------------------

suco_set<AO *> pMBUfactHandler::results;
suco_set<AO *> pMBUfactHandler::unsafe;
suco_set<AO *> pMBUfactHandler::tracked;
bool pMBUfactHandler::readwrite = false;

//- collect PMBU unsafe and tracked aos:
//  unsafe = all (safe) deref aos affected by something in pmbu_results
//  tracked = all (untracked) locs aliased by a (newly-)unsafe ao
void pMBUfactHandler::collectUnsafeTracked(AO& ao)
{

  if(ao.isRef() && (ao.getTSC() > AO::TSC_POSS_INVALID)
	&& (pMBUfactHandler::readwrite || ao.isAssigned())){
    suco_set<ECR *> affecrs;
    ao.getECR().collectAliasECRs(affecrs, true);	//- using this, because getAliasECRs filters loc/argret
    suco_iterator<ECR *> afecri(affecrs);
    while(afecri.Iterate()){
      if(afecri.Current()->getAOset().Intersects(pMBUfactHandler::results)){
        //- found pmbu-unsafe ao
        pMBUfactHandler::unsafe.Insert(&ao);
        //- collect pmbu-tracked = loc aliases
        suco_iterator<ECR *> alecri(ao.getECR().getAliasECRs());
        while(alecri.Iterate()){
          suco_iterator<AO *> aoi(alecri.Current()->getAOset());
          while(aoi.Iterate()){
            if(aoi.Current()->isLocArgRet() && aoi.Current()->getTSC() == AO::TSC_SAFE){
              pMBUfactHandler::tracked.Insert(aoi.Current());
            }
          }
        }
        return;
      }
    }
  }
}

//----------------------------------

void pMBUfactHandler::intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn)
{
  MBUfact& mf = (MBUfact&) df;
  mf.aoset.Clear();
}

bool pMBUfactHandler::handleDecl(DFAfact& df, PExprDecl& dd)
{
  MBUfact& mf = (MBUfact&) df;
  if((!dd.isZeroed()) &&
	(dd.isLocal() || dd.isFormal() || dd.isMalloc())){
//Future change: collect sdot/udot members also?
    AO& ao = dd.getAO();
    if(ao.dfa_relevant){
      mf.aoset.Insert(&ao);
    }
    return false; //- not useless
  } else return true; //- useless
}

bool pMBUfactHandler::handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
{
//TODO: check callsites
  return handleDecl(df, dd);
}

bool pMBUfactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs, PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  MBUfact& mf = (MBUfact&) df;
  if(pMBUfactHandler::check_pmbu(rhs, mf.aoset)){ //- rhs may be uninit
    mf.aoset.Insert(&lhs);	//-TODO: beware structs and sdot/udot
  } else { //- rhs definitely init
    mf.aoset.Remove(&lhs);	//-TODO: beware structs and sdot/udot
  }
  return false; //- not useless
}

bool pMBUfactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  MBUfact& mf = (MBUfact&) df;
  if(pMBUfactHandler::check_pmbu(rhs, mf.aoset)){ //- rhs may be uninit
    mf.addRelevantAliasLocs(lhs.getAOs());	//-TODO: beware structs and sdot/udot
  } // else cannot kill (weak assignment)
  return false; //- not useless
}

void pMBUfactHandler::intraProcHandleCall(DFAfact& df, PExprCall& dc)
{
  MBUfact& mf = (MBUfact&) df;
  suco_iterator<CFGfunction *> fni(dc.getTargetFns());
  while(fni.Iterate()){
    CFGfunction& fn = *fni.Current();
    if(fn.getGREF().Intersects(mf.aoset)){	//-TODO: beware structs and sdot/udot
      //- if function references anything in pmbu
      //  add gmod set to pmbu
      fni.Current()->getGMOD().addToAOset(mf.aoset);	//-TODO: filter out strlit?/sdot/udot
    } // else (if function doesn't reference anything in pmbu) no change
  }
}

bool pMBUfactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  MBUfact& mf = (MBUfact&) df;
  if((dv.getVpKind() == PExprVerify::vpPtrW) ||
        (pMBUfactHandler::readwrite && (dv.getVpKind() == PExprVerify::vpPtr))){
    this->moveToResults(mf.aoset, dv.getDesc(), true);
  }

  return false; //- not useless
}

//- for each (relevant-loc)AO in uninit_aos,
//  if it affects use_exp, move it to results
//  (move = delete from uninit_aos, add to results)
void pMBUfactHandler::moveToResults(suco_set<AO *>& uninit_aos, ExpDescr& use_exp, bool deref)
{
//TODO: handle deref=true case: deref use_exp one level?
  if(!uninit_aos.IsEmpty()){
    suco_set<ECR *> affecrs;
    use_exp.collectAffectingAliasLocECRs(affecrs);
    suco_iterator<AO *> ui(uninit_aos);
    while(ui.Iterate()){
      if(affecrs.Contains(&ui.Current()->getECR())){
        this->results.Insert(ui.Current());
        ui.DeleteCurrent();
      }
    }
  }
}

void pMBUfactHandler::moveToResults(suco_set<AO *>& uninit_aos, LocSet& use_locs)
{
  if(!uninit_aos.IsEmpty()){
    suco_set<ECR *> affecrs;
    use_locs.addToEcrSet(affecrs);
    suco_iterator<AO *> ui(uninit_aos);
    while(ui.Iterate()){
      if(affecrs.Contains(&ui.Current()->getECR())){
        this->results.Insert(ui.Current());
        ui.DeleteCurrent();
      }
    }
  }
}

//- Check ed to see if it may be uninitialized
bool pMBUfactHandler::check_pmbu(ExpDescr& ed, suco_set<AO *>& pmbu_set)
{
//TODO
  return true;
}

