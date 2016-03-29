#include <stdio.h>
#include "ao.h"
#include "ecr.h"
#include "cfg.h"
#include "rda.h"
#include "flags.h" // for flag_verbose, flag_readable_output
#include "diag.h" // for TCstats

//----------------------------------
// REACHING-DEFS ANALYSIS
//----------------------------------

DFAfact& RDAfact::newClone(bool preserve)
{
  RDAfact& nmf = *new RDAfact;
  nmf.meet(*this, preserve);
  return nmf;
}

//-- filter this, placing into ffact all elements that are part of fn->gmod
// - i.e. this = this \ fn->gmod
// -      ffact = this intersect fn->gmod
void RDAfact::filterGmodInto(CFGfunction& fn, RDAfact& ffact)
{
  LocAidNode ** tnpp = &this->head;
  LocAidNode ** rnpp = &ffact.head;

  while(*tnpp){
    AO * tao = &(*tnpp)->getAO();
    if(!fn.getGMOD().Contains(*tao)){
      tnpp = &(*tnpp)->next;
    } else {
      //-- advance rnpp
      while(*rnpp && (&(*rnpp)->getAO() < tao))
        rnpp = &(*rnpp)->next;

      //-- move node from *tnpp to *rnpp
      LocAidNode * tmp = *tnpp;
      *tnpp = tmp->next;
      tmp->next = *rnpp;
      *rnpp = tmp;
    }
  }
}

//-- set union
// - preserve=false means we are free to tamper with df
// - warnNotLE=true: warn if this has elements not in df
// - returns true if df has elements not in this
bool RDAfact::meet(DFAfact& df, bool preserve, bool warnNotLE)
{
  RDAfact& mf = (RDAfact&) df;

  if(mf.is_bot){ //- meet with bot: set to bot

    bool ret = !this->is_bot;

    this->is_bot = true;
    this->clear();

    return ret;

  } else if(this->is_bot) { //- already bot: do nothing

    if(warnNotLE) {
      fprintf(stderr, "RDA Meet: Higher Than Bottom!: ");
      mf.debug_dump(stderr);
    }

    return false;

  } else { //- non-bot meet with non-bot

    //-- copy elements of mf into this
    LocAidNode ** tnpp = &this->head;
    LocAidNode * mnp = mf.head;

    bool mf_has_extra = false;

    while(mnp){
      if(!*tnpp){ //- copy over all remaining nodes in mf

        mf_has_extra = true;

        if(preserve){ //- copy over
          do {
            *tnpp = LocAidNode::acquire(*mnp);
            mnp = mnp->next;
            tnpp = &(*tnpp)->next;
          } while(mnp);
        } else { //- move over
          *tnpp = mf.head;
          mnp = mf.head = 0;
        }

      } else if(&mnp->getAO() < &(*tnpp)->getAO()){ //- copy over

        mf_has_extra = true;

        if(preserve){ //- copy over
          *tnpp = LocAidNode::acquire(*mnp, *tnpp);
          mnp = mnp->next;
          tnpp = &(*tnpp)->next;
        } else { //- move node(s) over
          mf.head = mnp->next;
          mnp->next = *tnpp;
          *tnpp = mnp;
          tnpp = &(mnp)->next;
          mnp = mf.head;
        }

      } else if(&mnp->getAO() == &(*tnpp)->getAO()){ //- add aids

        LocAidNode * tnp = *tnpp;

        int pre_nelems =  tnp->nodes.Length()
			+ tnp->args.Length()
			+ tnp->rets.Length();

        if(preserve){ //- add aids, increment mnp
          tnp->nodes.Union(mnp->nodes);
          tnp->args.Union(mnp->args);
          tnp->rets.Union(mnp->rets);
          mnp = mnp->next;
          tnpp = &tnp->next;
        } else { //- consume aids, delete mnp, reset mf.head
          tnp->nodes.UnionConsume(mnp->nodes);
          tnp->args.UnionConsume(mnp->args);
          tnp->rets.UnionConsume(mnp->rets);
          mf.head = mnp->next;
          LocAidNode::dispose(mnp);
          mnp = mf.head;
          tnpp = &tnp->next;
        }

        int post_nelems = tnp->nodes.Length()
			+ tnp->args.Length()
			+ tnp->rets.Length();
   
        mf_has_extra |= (post_nelems > pre_nelems);

      } else { //- continue

        if(warnNotLE){
          fprintf(stderr, "RDA Meet: RDAfact Not <=: this has ");
          (*tnpp)->getAO().dump_descr(stderr);
          fprintf(stderr, "\n");
        }

        tnpp = &(*tnpp)->next;

      }
    }

    return mf_has_extra;
  }
}

//-- set intersection
// - preserve=false means we are free to tamper with df
void RDAfact::join(DFAfact& df, bool preserve)
{
  RDAfact& mf = (RDAfact&) df;

  if(mf.is_bot) { //- join with bot: do nothing

    //- nop

  } else if(this->is_bot) { //- this is bot: set to mf

    this->is_bot = false;
    this->clear(); //-- redundant
    this->meet(mf, preserve);

  } else { //- join non-bot with non-bot

    LocAidNode ** tnpp = &this->head;
    LocAidNode * mnp = mf.head;
    while(*tnpp){
      if((!mnp) || (&(*tnpp)->getAO() < &mnp->getAO())){ //- not in mf, delete
        LocAidNode * del = *tnpp;
        *tnpp = (*tnpp)->next;
        LocAidNode::dispose(del);
      } else if(&(*tnpp)->getAO() == &mnp->getAO()){ //- add aids
        if(preserve){ //- add aids
          (*tnpp)->nodes.Union(mnp->nodes);
          (*tnpp)->args.Union(mnp->args);
          (*tnpp)->rets.Union(mnp->rets);
        } else { //- union consume
          (*tnpp)->nodes.UnionConsume(mnp->nodes);
          (*tnpp)->args.UnionConsume(mnp->args);
          (*tnpp)->rets.UnionConsume(mnp->rets);
        }
        mnp = mnp->next;
        tnpp = &(*tnpp)->next;
      } else { //- continue
        mnp = mnp->next;
      }
    }
  }
}

void RDAfact::debug_dump(FILE * outf, bool brief)
{
  if(is_bot){
    fprintf(outf, "bot\n");
  } else if(brief){
    int numaos = 0; 
    int numnodes = 0;
    int numrets = 0;
    int numargs = 0;
    for(LocAidNode * np = this->head; np; np = np->next){
      numaos++;
      numnodes += np->nodes.Length();
      numrets += np->rets.Length();
      numargs += np->args.Length();
    }
    fprintf(outf, "%d aos, %d nodes, %d rets, %d args\n",
		numaos, numnodes, numrets, numargs);
  } else {
    LocAidNode * np;
    fprintf(outf, "\n");
    for(np = this->head; np; np = np->next){
      fprintf(outf, "\t[ ");
      np->getAO().write_string_rep(outf, true);
      fprintf(outf, ":");
      suco_iterator<PgmExpr *> ni(np->nodes);
      while(ni.Iterate()){
        switch(ni.Current()->getKind()){
/*
          case PgmExpr::fCall: {
              AID& aid = ((PExprCall *)ni.Current())->getAid();
              fprintf(outf, " call<%d,%d>", aid.filestem_id, aid.aid);
            } break;
*/
          case PgmExpr::fAssign: {
              AID& aid = ((PExprAssign *)ni.Current())->getAid();
              fprintf(outf, " assign<%d,%d>", aid.filestem_id, aid.aid);
            } break;
          case PgmExpr::fVerify: {
              PExprVerify& pv = *(PExprVerify *)ni.Current();
              AID& aid = pv.getAid();
              fprintf(outf, " verify%s%s<%d,%d>",
			pv.getVtKindString(), pv.getVpKindString(),
			aid.filestem_id, aid.aid);
            } break;
          case PgmExpr::fDecl: {
              fprintf(outf, " decl(");
              ((PExprDecl *)ni.Current())->getAO().write_string_rep(outf, true);
              fprintf(outf, ")");
            } break;
          default:
            fprintf(outf, "Invalid PgmExpr recorded in RDAfact:\n");
            ni.Current()->debug_dump(outf, 2);
        }
      }
      suco_iterator<PExprArg *> ai(np->args);
      while(ai.Iterate()){
        AID& aid = ((PExprArg *)ai.Current())->getAid();
        fprintf(outf, " arg<%d,%d>", aid.filestem_id, aid.aid);
      }
      suco_iterator<PgmStmt *> ri(np->rets);
      while(ri.Iterate()){
        AID& aid = ((PgmStmt *)ri.Current())->getAid();
        fprintf(outf, " ret<%d,%d>", aid.filestem_id, aid.aid);
      }
      fprintf(outf, " ]\n");
    }
  }
}

//----------------------
// RDAfactHandler

RDAfactHandler RDAfactHandler::handler;

//- filter out GMOD, then cache local fact!
void RDAfactHandler::filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local)
{
  RDAfact& rdf = (RDAfact&) df;
  RDAfact& rdf_local = (RDAfact&) df_local;

  //- 1. move all facts to rdf_local
  rdf_local.meet(rdf, false);

  //- 2. filter GMOD elements back into rdf
  suco_iterator<CFGfunction *> tfi(dc.getTargetFns());
  while(tfi.Iterate())
    rdf_local.filterGmodInto(*tfi.Current(), rdf);

  //- 3. specially handle call arguments? --no need for RDA

  //- 4. cache local set
  dc.getRDAlocal().meet(rdf_local);
}

void RDAfactHandler::interProcFilterEntryFact(DFAfact& df, CFGnode& cn) 
{
//-NOP for now
}

void RDAfactHandler::interProcHandleCallArgs(DFAfact& df, PExprCall& dc)
{
//-TODO? or NOP?
}

void RDAfactHandler::interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc)
{
//-TODO? or NOP?
}

DFAfact& RDAfactHandler::lookupNodeFact(CFGnode& cn, CFGnode * tfSucc)
{
  return cn.getRDAfact();
}

DFAfact& RDAfactHandler::lookupNodeFact(PExprCall& dc)
{
  return dc.getRDAfact();
}

DFAfact& RDAfactHandler::lookupNodeFact(PExprParallel& dp)
{
  return dp.getRDAfact();
}

bool RDAfactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& /*ignored*/,
				 PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  RDAfact& rf = (RDAfact&) df;
  if(lhs.getTSC() >= AO::TSC_EXPOSED){
    rf.removeFacts(lhs);
    rf.addFact(lhs, dnode, cnode, arg);
    return false;
  } else return true; //- useless
}

bool RDAfactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& /*ignored*/, PExprAssign& dnode)
{
  RDAfact& rf = (RDAfact&) df;
  //-- collect lhs's ecrset
  suco_set<ECR *> ecrset;
  suco_iterator<AO *> aoi(lhs.getAOs());
  while(aoi.Iterate())
    ecrset.Insert(&aoi.Current()->getECR());

  //-- set aual_fact, aual_assign_node
  RDAfactHandler::aual_fact = &rf;
  RDAfactHandler::aual_assign_node = &dnode;

  //-- do traversal
  ECR::traverseSetAliases(ecrset, RDAfactHandler::add_locs_to_aual_fact);
  return false; //- not useless
}

bool RDAfactHandler::handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
{
  RDAfact& rf = (RDAfact&) df;
  if(dd.getAO().getTSC() >= AO::TSC_EXPOSED){ //- skip if not safe (optimize)
    rf.removeFacts(dd.getAO()); //- treat as strong assign
    rf.addFact(dd.getAO(), &dd, 0, 0);
    suco_iterator<PExprCall *> csi(parfn.getCallSites());
    while(csi.Iterate()){
      PExprArg * arg = csi.Current()->getArg(dd.getArgNo());
      if(arg) rf.addFact(dd.getAO(), 0, 0, arg);
    }
    return false; //- not useless
  } else return true; //- useless
}

bool RDAfactHandler::handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc)
{
  RDAfact& rf = (RDAfact&) df;
  CFGfunction& parfn = retnode.getParentFunction();
  AO& retao = parfn.getId().get_AOFunction().get_AOReturn();

  //- treat as strong assign: Return(Fnid) = retaos
  rf.removeFacts(retao);
  rf.addFact(retao, 0, &retnode, 0);

  //- filter out local vars, only if non-recursive
  if(!parfn.getRecursionId()){
    suco_iterator<AOId *> lvi(parfn.getLocalVars());
    while(lvi.Iterate())
      rf.removeFacts(*lvi.Current());
  }
  return false;
}

bool RDAfactHandler::handleDecl(DFAfact& df, PExprDecl& dd)
{
  RDAfact& rf = (RDAfact&) df;
  if(dd.getAO().getTSC() >= AO::TSC_EXPOSED){  //-- filter safe and tracked AOs
    rf.removeFacts(dd.getAO()); //-- should not already be there -- assert?
    rf.addFact(dd.getAO(), &dd);
    return false; //- not useless
  } else return true; //- useless
}

//-- static helpers for handleWeakAssign
RDAfact * RDAfactHandler::aual_fact = 0;
PgmExpr * RDAfactHandler::aual_assign_node = 0; //- should never be zero when used
bool RDAfactHandler::add_locs_to_aual_fact(AO& ao)
{
  if(RDAfactHandler::aual_fact && ao.isLoc() && (ao.getTSC() >= AO::TSC_EXPOSED))
    RDAfactHandler::aual_fact->addFact(ao, RDAfactHandler::aual_assign_node);
  return true;
}

//----------------------------------

void RDAfact::clear()
{
  LocAidNode * np = this->head;
  while(np){
    LocAidNode * del = np;
    np = np->next;
    LocAidNode::dispose(del);
  }
  this->head = 0;
}

//- return 0 indicates bottom
RDAfact::LocAidNode * RDAfact::getFactNode(AO& ao)
{
  if(this->is_bot)
    return 0;

  LocAidNode ** np;
  for(np = &this->head; *np && (&(*np)->getAO() < &ao); np = &(*np)->next);
  if(!(*np) || (&(*np)->getAO() != &ao)){ //-- node not found, add new
    *np = LocAidNode::acquire(ao, *np);
  }
  return *np;
}

void RDAfact::addFact(AO& ao, PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  LocAidNode * n = getFactNode(ao);
  if(n){
    if(dnode) n->nodes.Insert(dnode);
    if(cnode) n->rets.Insert(cnode);
    if(arg)   n->args.Insert(arg);
  }
}

void RDAfact::copyFacts(AO& idx_ao, AO& ao, RDAfact& mf)
{
  LocAidNode * tn = this->getFactNode(idx_ao);
  LocAidNode * mfn = mf.getFactNode(ao);
  if(tn && mfn){
    tn->nodes.Union(mfn->nodes);
    tn->rets.Union(mfn->rets);
    tn->args.Union(mfn->args);
  }
}

void RDAfact::removeFacts(AO& ao)
{
  LocAidNode ** np;
  for(np = &head; *np && (&(*np)->getAO() < &ao); np = &(*np)->next);
  if(*np && (&(*np)->getAO() == &ao)){ //-- node found, remove
    LocAidNode * del = *np;
    *np = (*np)->next;
    LocAidNode::dispose(del);
  }
}

bool RDAfact::hasKey(AO& ao)
{
  if(this->is_bot)
    return true;

  LocAidNode * np;
  for(np = this->head; np && (&np->getAO() < &ao); np = np->next);
  return (np && (&np->getAO() == &ao));
}

void RDAfact::markAOsExposed()
{
  for(LocAidNode * np = this->head; np; np = np->next){
    np->getAO().getECR().propagateExposed();

    //- find weak assignments; mark their lhs exposed
    suco_iterator<PgmExpr *> ni(np->nodes);
    while(ni.Iterate()){
      if(ni.Current()->getKind() == PgmExpr::fAssign){
        suco_iterator<AO *> laoi(((PExprAssign *)ni.Current())->getLHS().getAOs());
        while(laoi.Iterate()){
          if(!laoi.Current()->isVal()){ //- filter out values
            laoi.Current()->getECR().propagateExposed();
          }
        }
      }
    }
  }
}

int RDAfact::freelistLength()
{
  int i = 0;
  for(LocAidNode * n = LocAidNode::freelist; n; n = n->next)
    i++;
  return i;
}

void RDAfact::writeResults(FILE * outf)
{
  LocAidNode * np;
  for(np = this->head; np; np = np->next){
    //-- do nodes
    suco_iterator<PgmExpr *> ni(np->nodes);
    while(ni.Iterate()){
      switch(ni.Current()->getKind()){
/*
        case PgmExpr::fCall: {
            //-- mbu calls: ! <N> u c <int>
            AID& aid = ((PExprCall *)ni.Current())->getAid();
            fprintf(outf, "! %d u c %d\n", aid.filestem_id, aid.aid);
          } break;
*/
        case PgmExpr::fAssign: {
            //-- mbu assigns: ! <N> u g <int>
            AID& aid = ((PExprAssign *)ni.Current())->getAid();
            fprintf(outf, "! %d u g %d\n", aid.filestem_id, aid.aid);
          } break;
        case PgmExpr::fVerify: {
            //-- mbu verify: ! <N> u v <int> # vtTag | vtRhs
            PExprVerify& pv = *(PExprVerify *)ni.Current();
            AID& aid = pv.getAid();
            fprintf(outf, "! %d u v %d #%s\n", aid.filestem_id, aid.aid, pv.getVtKindString());
          } break;
        case PgmExpr::fDecl: {
            //-- mbu decls (including formals): ! <N> u d <AO>
            PExprDecl& dd = *(PExprDecl *)ni.Current();
            fprintf(outf, "! %d u d ", dd.getFSid());
            dd.getAO().write_string_rep(outf, flag_readable_output);
            fprintf(outf, "\n");
          } break;
        default:
          fprintf(stderr, "Invalid PgmExpr recorded in RDAfact:\n");
          ni.Current()->debug_dump(stderr, 2);
      }
    }
    //-- do args
    //-- mbu args: ! <N> u a <int>
    suco_iterator<PExprArg *> ai(np->args);
    while(ai.Iterate()){
      AID& aid = ai.Current()->getAid();
      fprintf(outf, "! %d u a %d\n", aid.filestem_id, aid.aid);
    }
    //-- do rets
    //-- mbu rets: ! <N> u r <int>
    suco_iterator<PgmStmt *> ri(np->rets);
    while(ri.Iterate()){
      AID& aid = ri.Current()->getAid();
      fprintf(outf, "! %d u r %d\n", aid.filestem_id, aid.aid);
    }
  }
}

//----------------------------------
// RDAfact::LocAidNode

RDAfact::LocAidNode * RDAfact::LocAidNode::freelist = 0;

RDAfact::LocAidNode * RDAfact::LocAidNode::acquire(AO& o, LocAidNode * nx)
{
  LocAidNode * nn;

  if(freelist){
    TCstats::recy_la_nodes++;
    nn = freelist;
    freelist = freelist->next;
    nn->ao = &o;
    nn->next = nx;
  } else {
    TCstats::new_la_nodes++;
    nn = new LocAidNode(o, nx);
  }

  return nn;
}

RDAfact::LocAidNode * RDAfact::LocAidNode::acquire(LocAidNode& n, LocAidNode * nx)
{
  LocAidNode * nn = acquire(n.getAO(), nx);
  //- deep copy
  nn->nodes.Copy(n.nodes);
  nn->args.Copy(n.args);
  nn->rets.Copy(n.rets);
  return nn;
}

void RDAfact::LocAidNode::dispose(LocAidNode * n)
{
  if(flag_recycle){
    n->ao = 0;
    n->nodes.Clear();
    n->args.Clear();
    n->rets.Clear();
    n->next = freelist;
    freelist = n;
  } else {
    delete n;
  }
}

//----------------------------------
// RDA

bool RDA::isUselessNode(PgmExpr& dn)
{
  return dn.RDAisUseless();
}

bool RDA::isUselessNode(CFGnode& cn)
{
  return cn.RDAisUseless();
}

bool RDA::markUselessNode(PgmExpr& dn)
{
  dn.RDAsetUseless();
  return true;
}

bool RDA::markUselessNode(CFGnode& cn)
{
  cn.RDAsetUseless();
  return true;
}

//----------------------------------
// END
//----------------------------------

