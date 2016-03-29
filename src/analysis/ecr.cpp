#include <stdio.h>

#include "diag.h"
#include "ecr.h"
#include "flags.h"

//------------------------------------------------------
//------------------------------------------------------

int ECRargRet::max_nargs = 0;

ECRargRet& ECRargRet::followFwdPtr()
{
  ECRargRet * tail = this;
  while(tail->fwdptr)
    tail = tail->fwdptr;
  if(tail != this)
    this->fwdptr = tail; //- update fwdptr
  return *tail;
}

void ECRargRet::findMaxNargs(AO& ao)
{
  if(ao.getKind() == AO::aoArg
     && max_nargs < ((AOArg&) ao).argNo())
    max_nargs = ((AOArg&) ao).argNo();
}

ECR& ECRargRet::getMainParent()
{
  main_parent = &main_parent->followECR();
  return *main_parent;
}

ECR& ECRargRet::getRet()
{
  if(!ret) ret = &ECR::new_ECR();
  else ret = &ret->followECR();
  return *ret;
}

ECR * ECRargRet::get_arg(int argno) const
{
  if(argno > this->argsize) return 0;
  else return this->args[argno-1];
}

ECR& ECRargRet::getArg(int argno)
{
  ECR *& ar = getArgPtr(argno);

  if(!ar) ar = &ECR::new_ECR();
  else ar = &ar->followECR();
  return *ar;
}

ECR *& ECRargRet::getArgPtr(int argno) // argno is 1-based!
{
  if(argno > this->argsize){ // reallocate
    ECR ** oldargs = this->args;
    int oldargsize = this->argsize;

    if(this->argsize < max_nargs)
      this->argsize = this->nargs;
    if(this->argsize < argno){
      this->argsize = argno * 2;
      if(this->argsize < 10) this->argsize = 10; // minimum size
    }

    this->args = new ECR*[this->argsize];

    int i = 0;
    //- copy old args
    for(; i < oldargsize; ++i)
      this->args[i] = oldargs[i];
    //- initialize new entries
    for(; i < this->argsize; ++i)
      this->args[i] = 0;

    //- free oldargs
    if(oldargs){
      delete [] oldargs;
    }
  }
  if(this->nargs < argno) // adjust nargs
    this->nargs = argno;
  return this->args[argno-1]; // adjust for 1-based argno!
}

void ECRargRet::addArg(int argno, ECR& ecr) //- assume ecr normalized
{
  ECR *& ar = getArgPtr(argno);
  //- absorb ecr
  if(ar){
    ar->absorb(ecr);
  } else {
    ar = &ecr;
  }
}

void ECRargRet::addRet(ECR& ecr) //- assume ecr normalized
{
  if(ret){
    ret->absorb(ecr);
  } else {
    ret = &ecr;
  }
}

// - 2. forall i: tmp[i]       <-- this->arg[i]
//                this->arg[i] <-- null
// - 3. forall i: targ[i] absorb tmp[i]
//                if this->arg[i] = null
//                then this->arg[i] <-- targ[i]
//                     targ[i] = null
//                else go back to step 2
// Note: use recursion for tmp[]

//- absorb args and ret into this; assume this has followed fwdptr
//  again, use recursion to store temps
void ECRargRet::absorbArgRet(int argno, ECR ** args, ECR * ret)
{
  if(argno){ //- doing arg
    //- remember ECRs
    ECR * larg = this->getArgPtr(argno); //-- do this once to instantiate args, if not already allocated
    this->args[argno-1] = 0;

    //- recurse
    absorbArgRet(argno-1, args, ret);

    //- larg absorb rarg (if rarg non null)
    ECR * rarg = args?args[argno-1]:0;	//-- NOT SAFE: argno may go OOB?
    if(larg && rarg) larg->followECR().absorb(rarg->followECR());
    else if(!larg) larg = rarg;

    if(larg){
      while(this->args[argno-1]){
        ECR * tmp = this->args[argno-1];
        this->args[argno-1] = 0;
        larg->followECR().absorb(tmp->followECR());
      }
      this->args[argno-1] = &larg->followECR();
    } // else do nothing

  } else { //- doing ret; also terminates recursion

    if(ret){
      while(this->ret){
        ECR * tmp = this->ret;
        this->ret = 0;
        ret->followECR().absorb(tmp->followECR());
      }
      this->ret = &ret->followECR();
    } //- else do nothing

  }
}

void ECRargRet::absorb(ECRargRet * ar) //- ar will be cleared
{
  // - 1. this  <-- larger nargs
  //      ar    <-- null
  //      targs <-- smaller nargs
  //      ar --[fwdptr]--> this
  ECR ** targs;
  int tnargs;
  if(ar->nargs > this->nargs){
    targs = this->args;
    tnargs = this->nargs;
    this->args = ar->args;
    this->argsize = ar->argsize;
    this->nargs = ar->nargs;
    ar->args = 0;
    ar->argsize = 0;
    ar->nargs = 0;
    ar->fwdptr = this;
  } else {
    targs = ar->args;
    tnargs = ar->nargs;
    ar->args = 0;
    ar->argsize = 0;
    ar->nargs = 0;
    ar->fwdptr = this;
  }
  ECR * tret = ar->ret;
  ar->ret = 0;

  // - 2. forall i: tmp[i]       <-- this->arg[i]
  //                this->arg[i] <-- null
  // - 3. forall i: targ[i] absorb tmp[i]
  //                if this->arg[i] = null
  //                then this->arg[i] <-- targ[i]
  //                     targ[i] = null
  //                else go back to step 2
  // Note: use recursion for tmp[]

  absorbArgRet(tnargs, targs, tret);

  delete [] targs;
}

//------------------------------------------------------

int ECR::traverseCounter = 0;
suco_llist<ECR *> ECR::ecr_pool;

ECR& ECR::new_ECR()
{
  ECR * ecr = new ECR;
  ECR::ecr_pool.Insert(ecr);
  return *ecr;
}

ECR& ECR::followECR()
{
  ECR * tail = this;
  while(tail->fwdptr)
    tail = tail->fwdptr;
  if(tail != this)
    this->fwdptr = tail; //- update fwdptr
  return *tail;
}

void ECR::linkAO(AO& ao)
{
  if(ao.ecr != this){
    if(ao.ecr){
      //- unlink ao.ecr
      ao.ecr->aoset.Remove(&ao);

      //- optional: call garbage-collector here? to clean up ao.ecr? 
      //            just beware that ecr may still be referred to by
      //            other ECRs, as well as edges and other AO
      //            (aoset and edge lists will have been carried
      //            forward to the tail?)
    }
    ao.ecr = this;
    this->aoset.Insert(&ao);
  }
}

void ECR::garbageCollect()
{
  int numecrs = 0;
  int deleted = 0;
  suco_iterator<ECR *> ei(ECR::ecr_pool);
  while(ei.Iterate()){
    numecrs++;
    if(ei.Current()->traverseTag != ECR::traverseCounter){
      ECR * tmp = ei.Current();
      ei.DeleteCurrent();
      deleted++;
//--debug: assert all fields are empty
      if(   (tmp->ptsTo)
	 || (!tmp->ptsFrom.IsEmpty())
	 || (!tmp->inclFrom.IsEmpty())
	 || (!tmp->inclTo.IsEmpty())){
        fprintf(stderr, "WARNING(ECR::garbageCollect): ecr has some non-empty fields\n");
      }
//--end assert
      delete tmp;
    }
  }
  fprintf(stderr, "ECR::garbageCollection: deleted %d out of %d\n", deleted, numecrs);
}

void ECR::finalizeECRset(suco_set<ECR *>& eset)
{
  suco_iterator<ECR *> pfi(eset);
  suco_set<ECR *> newset;
  while(pfi.Iterate()){
    ECR * curr = pfi.Current();
    ECR * tail = &curr->followECR();
    if(curr != tail){
      pfi.DeleteCurrent();
      newset.Insert(tail);
    }
    tail->finalizeECR();
  }
  eset.UnionConsume(newset);
}

void ECRargRet::fixECRs()
{
  //- main parent: probably useless
  this->main_parent = &this->main_parent->followECR();
  this->main_parent->finalizeECR();
  //- ret
  if(this->ret){
    this->ret = &this->ret->followECR();
    this->ret->finalizeECR();
  }
  //- args
  if(this->args && this->argsize){
    for(int i = 0; i < this->argsize; ++i){
      if(this->args[i]){
        this->args[i] = &this->args[i]->followECR();
        this->args[i]->finalizeECR();
      }
    }
  }
}

void ECR::finalizeECR()
{
  if(this->fwdptr)
    fprintf(stderr, "ERROR(finalizeECR): ecr non-null fwdptr!\n");

  if(this->traverseTag == ECR::traverseCounter) // visited; return
    return;
  this->traverseTag = traverseCounter; // mark this ECR as "visited"

  //- fix all ECR pointers:
  // - ptsFrom
  ECR::finalizeECRset(this->ptsFrom);
  // - ptsTo
  if(this->ptsTo){
    this->ptsTo = &this->ptsTo->followECR();
    this->ptsTo->finalizeECR();
  }
  // - inclFrom
  ECR::finalizeECRset(this->inclFrom);
  // - inclTo
  ECR::finalizeECRset(this->inclTo);
  // - alias_ecrset (just clear)
  if(this->alias_ecrset){
    fprintf(stderr, "WARNING(finalizeECR): alias_ecrset is non-null\n");
  }
  // - argret
  if(this->argRet()){
    this->argRet()->fixECRs();
  }
}

void ECR::fixECR(AO& ao) //- if no ECR, creates a new one
{
  if(!ao.ecr)
    new_ECR().linkAO(ao);
  else {
    //- follow forward pointer
    ECR& tail = ao.ecr->followECR();
    if(ao.ecr != &tail)
      tail.linkAO(ao);
  }
  ao.ecr->finalizeECR();
}

ECR& ECR::getECR(AO& ao) //- if no ECR, creates a new one
{
  if(!ao.ecr)
    new_ECR().linkAO(ao);
  else {
    //- follow forward pointer
    ECR& tail = ao.ecr->followECR();
    if(ao.ecr != &tail)
      tail.linkAO(ao);
  }
  return *ao.ecr;
}

void ECR::absorb(ECR& ecr) //- assume both this and ecr are normalized
{
  if(this != &ecr){ //- different ecrs
    //-- absorb ecr into this
    // - transfer aoset
    this->aoset.UnionConsume(ecr.aoset); // should be distinct sets anyway
    // - set fwd pointer
    ecr.fwdptr = this;
    // - transfer ptsFrom
    this->ptsFrom.UnionConsume(ecr.ptsFrom);
    // - transfer inclFrom,inclTo
    this->inclFrom.UnionConsume(ecr.inclFrom);
    this->inclTo.UnionConsume(ecr.inclTo);
    // - transfer ptsTo - triggers further absorbs, so be careful of non-termination
    if(ecr.ptsTo){
      //- first, dismantle ptsTo connection
      ECR &tgtecr = ecr.getPointsTo();
      tgtecr.ptsFrom.Remove(&ecr);
      ecr.ptsTo = 0;
      //- now, we're safe to call the pointsTo function
      this->pointsTo(tgtecr);
    }
    //- unify arg and returns
    if(ecr.argRet()){
      if(ecr.argRet() == this->followECR().argRet()){
        ecr.detachArgRet();
      } else if(this->followECR().argRet()){
        ECRargRet * eargret = ecr.detachArgRet(); //- ecr.argret already followed fwdptr in outer if stmt
        this->followECR().argRet()->absorb(eargret); //- eargret is cleared by absorb
      } else {
        this->followECR().argret = ecr.argRet();
        ecr.argret = 0;
      }
    } // else - nothing to absorb
  }
}

void ECR::unifyECRs(AO& ao1, AO& ao2)
{
  if(!ao1.ecr)
    getECR(ao2).linkAO(ao1);
  else if(!ao2.ecr)
    getECR(ao1).linkAO(ao2);
  else //- both ecrs allocated
    getECR(ao1).absorb(getECR(ao2));
}

ECRargRet * ECR::argRet() //- follows fwdptr
{
  if(this->argret)
    this->argret = &argret->followFwdPtr();
  return this->argret;
}

ECRargRet& ECR::getArgRet()
{
  if(!argret){
    argret = new ECRargRet(*this);
  }
  return *argRet();
}

ECRargRet * ECR::detachArgRet() //- does not follow fwdptr
{
  ECRargRet * ret = this->argRet();
  if(ret)
    this->argret = 0;
  return ret;
}

ECR& ECR::getPointsTo()
{
  if(!this->ptsTo){
    this->ptsTo = &new_ECR();
  } else {
    //- follow forward pointer
    this->ptsTo = &this->ptsTo->followECR();
  }
  return *this->ptsTo;
}

void ECR::pointsTo(ECR& ecr) // assume this and ecr normalized
{
  if(flag_collapse_ptsto_cycle && (this != &ecr)){
    suco_set<ECR *> cyEcrs;
    ECR::traverseCounter++;
    if(ecr.findPtsToCycle(*this, cyEcrs)){
      //- cycle found: collapse
      TCstats::cde_pt_num_collapses++;
      TCstats::cde_pt_nodes_collapsed += cyEcrs.Length();

/**/if(this->ptsTo && (&this->ptsTo->followECR() == &ecr)) fprintf(stderr, "already points-to, still collapsing! (%d)\n", cyEcrs.Length());

      suco_iterator<ECR *> cei(cyEcrs);
      while(cei.Iterate())
        ecr.followECR().absorb(cei.Current()->followECR());

      return;	//- break out
    }
  }
  if(!this->ptsTo){
    this->ptsTo = &ecr;
    ecr.ptsFrom.Insert(this);
  } else {
    ecr.absorb(this->getPointsTo());
  }
}

//- simulate larg --ASSIGN--> rarg
//       and lret <--ASSIGN-- rret (when argno = 0)
//  trick: use recursion to remember ECRs to simulate
void ECR::simulateAssign(int argno, ECRargRet& largret, ECRargRet& rargret) //- assume largret, rargret have followed fwdptr
{
  if(argno){ //- doing arg
    //- remember ECRs
    ECR& larg = largret.getArg(argno);
    ECR& rarg = rargret.getArg(argno);

    //- recurse
    simulateAssign(argno-1, largret, rargret);

    //- do arg : simulate larg --ASSIGN--> rarg
    rarg.followECR().getPointsTo().includesTo(larg.followECR().getPointsTo());

  } else { //- doing ret; also terminates recursion

    //- do ret : simulate lret <--ASSIGN-- rret
    largret.getRet().getPointsTo().includesTo(rargret.getRet().getPointsTo());

  }
}

void ECR::processArgRetFlow(ECRargRet& largret, ECRargRet& rargret) //- assume largret, rargret have followed fwdptr
{
  int minLRnargs = (largret.nargs < rargret.nargs) ? largret.nargs : rargret.nargs;
  int maxLRnargs = (largret.nargs > rargret.nargs) ? largret.nargs : rargret.nargs;
  simulateAssign(maxLRnargs, largret, rargret);

  if(minLRnargs != maxLRnargs){
//TODO: propagate
    fprintf(stderr, "WARNING! Different LR nargs (%d,%d); for robustness, run with -fixnargs\n", minLRnargs, maxLRnargs);

  }
}

// assume this and ecr normalized
//- NOTE: since the current analysis has out-degree = 1 for
//-       points-to edges, we don't need this recursive
//-       traversal.  However, I'll leave this implementation
//-       to accommodate possible future extensions to higher
//-       out degrees.
bool ECR::findPtsToCycle(ECR& dest, suco_llist<ECR *>& cyEcrs)
{
  if(this->traverseTag == ECR::traverseCounter) // visited node
    return cyEcrs.Contains(this); //- return true if previously-identified cycle
  this->traverseTag = ECR::traverseCounter; // mark this ECR as "visited"

  if(this == &dest){ // cycle detected
    cyEcrs.Insert(this);
    return true;
  }

  //- follow points-to target
  if(this->ptsTo &&
	this->ptsTo->followECR().findPtsToCycle(dest, cyEcrs)){
    //- part of cycle - collect results
    cyEcrs.Insert(this);
    return true;
  } else return false;
}

// assume this and ecr normalized
bool ECR::findInclToCycle(ECR& dest, suco_llist<ECR *>& cyEcrs)
{
  if(this->traverseTag == ECR::traverseCounter) // visited node
    return cyEcrs.Contains(this); //- return true if previously-identified cycle
  this->traverseTag = ECR::traverseCounter; // mark this ECR as "visited"

  if(this == &dest){ // cycle detected
    cyEcrs.Insert(this);
    return true;
  }

  //- traverse incl-to targets depth-first
  bool ret = false;
  suco_iterator<ECR *> ei(this->inclTo);
  while(ei.Iterate())
    ret |= ei.Current()->followECR().findInclToCycle(dest, cyEcrs);

  if(ret){ //- part of cycle - collect results
    cyEcrs.Insert(this);
    return true;
  } else return false;
}
    
// assume this and ecr normalized
// return true if new edge added
void ECR::includesTo(ECR& ecr)
{
  //- first, detect cycles introduced by this new edge
  if(flag_collapse_inclto_cycle && this != &ecr){
    suco_set<ECR *> cyEcrs;
    ECR::traverseCounter++;
    if(ecr.findInclToCycle(*this, cyEcrs)){
      //- cycle found: collapse: absorb all into ecr
      TCstats::cde_incl_num_collapses++;
      TCstats::cde_incl_nodes_collapsed += cyEcrs.Length();

      suco_iterator<ECR *> cei(cyEcrs);
      while(cei.Iterate())
        ecr.followECR().absorb(cei.Current()->followECR());

      //- delete redundant inclTo/inclFrom entries
      {
        ECR& ecrn = ecr.followECR();
        suco_iterator<ECR *> tei(ecrn.inclTo);
        while(tei.Iterate())
          if(&tei.Current()->followECR() == &ecrn)
            tei.DeleteCurrent();
        suco_iterator<ECR *> fei(ecrn.inclFrom);
        while(fei.Iterate())
          if(&fei.Current()->followECR() == &ecrn)
            fei.DeleteCurrent();
      }

//TODO: assert that &this->followECR() == ecr.followECR()
      return;	//- must break out, so "this and ecr are normalized"
    }		//  invariant holds for following if test
  }

  if(this != &ecr && // don't include self
     this->inclTo.Insert(&ecr)){ // true means new incl target
    ecr.inclFrom.Insert(this);
    //- unify points-to targets
    if(this->ptsTo)
      ecr.pointsTo(this->getPointsTo());
    else
      this->pointsTo(ecr.getPointsTo());

    //- do arg-ret assignment simulations
    // a. if lhs has argret, but not rhs, set rhs.argret to point to lhs.argret
    //    AND follow incl forward to link all reachable w/no argret
    //        IF reaches another argret, then must perform "virtual assignment" (step d. below)
    // b. if rhs has argret but not lhs, DON'T NEED to go backwards (enough to do in one
    //    direction) --> do nothing
    // c. if both have no argret, do nothing: defer to steps a. and b. later if needed
    //    (optimizing for common case?)
    // d. if both have argret, perform the effect of the assignments
    //      lhs.arg  -ASSIGN-> rhs*.arg; i.e. rhs*.ret.pointsTo -includesTo-> lhs.arg.pointsTo
    //      lhs.ret <-ASSIGN-  rhs*.ret; i.e. lhs.ret.pointsTo  -includesTo-> rhs*.ret.pointsTo
    //    BUT:
    //    if lhs.nargs > rhs.nargs, must search forward, to propagate to other reachable argrets
    //    if rhs.nargs > lhs.nargs, same but search backwards
    ECR& lhs = this->followECR(); //- must recompute, since
    ECR& rhs = ecr.followECR();  //-  unification may have occurred

    ECRargRet * largret = lhs.argRet();
    ECRargRet * rargret = rhs.argRet();
    if(largret == rargret){
      // already share argret -- do nothing
      // or neither has argrets (step c.) - do nothing (defer)
    } else if(largret){
      if(rargret){ // both have argrets (step d.)

        processArgRetFlow(*largret, *rargret);

      } else { // lhs has argrets, but not rhs (step a.) - search forward

        //- traverse include edges, collect frontier ECRs
        suco_set<ECR *> frontier;
        traverseCounter++;
        rhs.collectArgRetFrontier(frontier, *largret);

        //- for each frontier ECR, simulate assignment
        suco_iterator<ECR *> ei(frontier); // follow inclusion edges
        while(ei.Iterate()){
          processArgRetFlow(lhs.followECR().getArgRet(), ei.Current()->followECR().getArgRet());
        }
      }
    } // rhs has argrets, but not lhs (step b.) - do nothing
  }
}

//- traverses include edges forward until nodes with
//  non-0 argrets (that's not equal to ar);
//  collect these ecrs in frontier
void ECR::collectArgRetFrontier(suco_set<ECR *> & frontier, ECRargRet& ar) // assume ar already followed fwdptr
{
  if(this->traverseTag == traverseCounter) // cycle; return
    return;
  this->traverseTag = traverseCounter; // mark this ECR as "visited"

  if(!this->argRet()){ //- copy, and continue search
    this->argret = &ar;

    suco_iterator<ECR *>  ei(inclTo); // follow inclusion edges
    while(ei.Iterate())
      ei.Current()->followECR().collectArgRetFrontier(frontier, ar);
  } else if(this->argRet() != &ar){ //- add to frontier, and stop search
      frontier.Insert(this);
  } //- else this->argret == &ar, do nothing
}

//- follows inclusion edges, while true
//  if either fp returns false, stop and return false
bool ECR::followInclEdges(bool (*aofp)(AO& o), bool (*ecrfp)(ECR& e))
{
  if(this->traverseTag == traverseCounter) // cycle; return
    return true;
  this->traverseTag = traverseCounter; // mark this ECR as "visited"

  bool ret = true;
  if(ecrfp) ret = ecrfp(*this); // apply to this ECR

  if(aofp){ // apply to this's AOs
    suco_iterator<AO *> aoi(this->aoset);
    while(ret && aoi.Iterate())
      ret = aofp(*aoi.Current());
  }

  suco_iterator<ECR *> ei(inclTo); // follow inclusion edges
  while(ret && ei.Iterate())
    ret = ei.Current()->followECR().followInclEdges(aofp, ecrfp);

  return ret;
}

bool ECR::traverseAliases(bool (*aofp)(AO& o), bool (*ecrfp)(ECR& e))
{
  traverseCounter++;
  return this->followECR().followInclEdges(aofp, ecrfp);
}

//-- using this only increments traverseCounter once, to avoid
//   repeated visits from multiple ecrs in eset
bool ECR::traverseSetAliases(suco_set<ECR *>& eset, bool (*aofp)(AO& o), bool (*ecrfp)(ECR& e))
{
  traverseCounter++;
  bool ret = true;
  suco_iterator<ECR *> ei(eset);
  while(ret && ei.Iterate())
    ret = ei.Current()->followECR().followInclEdges(aofp, ecrfp);
  return ret;
}

//- affecting=true: for "star x", collect aliases for x also
void ECR::collectAliasECRs(suco_set<ECR *>& eset, bool affecting)
{
  ECR& tecr = this->followECR();

  if(eset.Insert(&tecr)){

    suco_iterator<ECR *> ei(tecr.inclTo); // follow inclusion edges
    while(ei.Iterate())
      ei.Current()->collectAliasECRs(eset, affecting);

    //- for "op x" and "ext x", must collect also
    //  aliases of x
    //- if affecting=true, ditto for "star x"
    suco_iterator<AO *> aoi(tecr.getAOset());
    while(aoi.Iterate()){
      AO * target_ao = aoi.Current();
      while(1){
        if(target_ao->getKind() == AO::aoOp){
          target_ao = &((AOOp *)target_ao)->getTarget();
        } else if(target_ao->getKind() == AO::aoExt){
          target_ao = &((AOExt *)target_ao)->getTarget();
        } else if(affecting && (target_ao->getKind() == AO::aoStar)){
          target_ao = &((AOStar *)target_ao)->getTarget();
        } else {
          break; //- break out of loop
        }
        if(target_ao->isVal()){
          break; //- break out of loop
        }
        if(!eset.Contains(&ECR::getECR(*target_ao))){
          ECR::getECR(*target_ao).collectAliasECRs(eset, affecting);
        }
      }
    }
  } //- else: already present, don't traverse children
}

void ECR::filterLocArgRet(suco_set<ECR *>& eset)
{
  suco_iterator<ECR *> ei(eset);
  while(ei.Iterate()){
    bool has_loc = false;
    suco_iterator<AO *> ai(ei.Current()->getAOset());
    while(ai.Iterate()){
      if(ai.Current()->isLocArgRet()){
        has_loc = true;
        break;
      }
    }
    if(!has_loc){
      ei.DeleteCurrent();
    }
  }
}

void ECR::collectInclFromECRs(suco_set<ECR *>& eset)
{
  ECR& tecr = this->followECR(); //- should not be necessary

  if(eset.Insert(&tecr)){

    suco_iterator<ECR *> ei(tecr.inclFrom); // follow inclusion edges backwards
    while(ei.Iterate())
      ei.Current()->collectInclFromECRs(eset);

  } //- else: already present, don't traverse parent
}

// WARNING: this function caches its lookup result,
// so it should only be called after the points-to graph
// has quiesced, which is to say after completion of
// all points-to analysis phases.
suco_set<ECR *>& ECR::getAliasECRs()
{
  if(!this->alias_ecrset){
    this->alias_ecrset = new suco_set<ECR *>;
    this->collectAliasECRs(*this->alias_ecrset);
    ECR::filterLocArgRet(*this->alias_ecrset);
  }
  return *this->alias_ecrset;
}

//- mark this node invalid, and propagate backwards along incl edges
void ECR::markInvalid(bool propDeref)
{
  if(invalid) return; //-- terminates backward search
  invalid = true;
  suco_iterator<ECR *> ifi(inclFrom); //SY: verify that inclFrom contains all included-from edges
  while(ifi.Iterate()) ifi.Current()->followECR().markInvalid(propDeref);

  if(propDeref){
    //-- find deref children: if p is invalid, then *p is also invalid
    suco_iterator<AO *> ai(aoset);
    while(ai.Iterate()){
      AO * starAO = ai.Current()->find(AO::aoStar);
      if(starAO)
        starAO->getECR().markInvalid(propDeref);
    }
  }

  //-- also - find struct/union children, and mark those
  //   (not necessary if collapse-always)
//TODO
}

//- limit_malloc: consider only malloc objects
//- do_touched: check not only exposed, but touched flag as well
//- do_vuln: vuln mode, check for vuln_locs rather than exposed
ExposedStatus ECR::inclToExposed(bool limit_malloc, bool do_touched, bool do_vuln)
{
  if(this->traverseTag == traverseCounter) // cycle; return
    return ExposedStatus::NA;
  this->traverseTag = traverseCounter; // mark this ECR as "visited"

  ExposedStatus ret = ExposedStatus::NA;

  if(do_vuln){ //- vuln mode: search for vuln_locs (using vuln_deref flag as hint)
    if(this->is_vuln_deref){
      suco_iterator<AO *> aoi(this->aoset);
      while(aoi.Iterate()){
        AO& ao = *aoi.Current();
        if((limit_malloc)?(ao.getKind() == AO::aoMalloc)
			 :(ao.isLoc())){
          if(ao.isVulnerableLoc() && ( do_touched ? this->touched : true )){
            ret.combine(ExposedStatus::All);
            if(ret.isSome()) return ret; //- short-circuit; no need to check further
          } else {
            ret.combine(ExposedStatus::None);
            if(ret.isSome()) return ret; //- short-circuit; no need to check further
          }
        }
      }
    } else { //- no vulnerable aliases; stop search
      return ExposedStatus::None;
    }

  } else { //- exposed/touched mode: can shortcut using exposed/touched flags
    bool count_this_step;

    if(!limit_malloc){
      count_this_step = !this->aoset.IsEmpty();
    } else { //- if limit_malloc, only count this step if this has malloc nodes
      count_this_step = false;
      //-- see if there are any malloc nodes
      suco_iterator<AO *> aoi(this->aoset);
      while(aoi.Iterate()){
        if(aoi.Current()->getKind() == AO::aoMalloc){
          count_this_step = true;
          break;
        }
      }
    }

    if(count_this_step && this->exposed && ( do_touched ? this->touched : true ))
      return ExposedStatus::All;

    ret = (count_this_step)?(ExposedStatus::None):(ExposedStatus::NA); //- starting point
  }

  //- follow inclusion edge
  suco_iterator<ECR *> iei(this->inclTo);
  while(iei.Iterate()){
    ret.combine(iei.Current()->inclToExposed(limit_malloc, do_touched, do_vuln));
    if(ret.isSome()) return ret; //- short-circuit; no need to check further
  }
  return ret;
}

void ECR::propagateInfluential()
{
  if(!influential){
    influential = true;

    //- mark AOs here?
    suco_iterator<AO *> ai(aoset);
    while(ai.Iterate())
      ai.Current()->setTSC(AO::TSC_INFLUENTIAL);

    propagateExposed();
    suco_iterator<ECR *> fi(inclFrom);
    while(fi.Iterate())
      fi.Current()->followECR().propagateInfluential();
  }
}

void ECR::propagateExposed()
{
  if(!exposed){
    exposed = true;

    //- mark AOs here?
    suco_iterator<AO *> ai(aoset);
    while(ai.Iterate()){
      ai.Current()->setTSC(AO::TSC_EXPOSED);
      if(flag_exposed_deref_is_unsafe){
        //- if p is exposed, mark *p as unsafe
        //  TODO: is *p enough?
        //        What about other expressions
        //        involving *p, like (*p).x?
        AO * ao = ai.Current()->find(AO::aoStar);
        if(ao){
          ao->setTSC(AO::TSC_POSS_INVALID);
          //-- propagate L3:influential, which propagates L4:exposed
          ECR::getECR(*ao).propagateInfluential();
        }
      }
    }

    suco_iterator<ECR *> ti(inclTo);
    while(ti.Iterate())
      ti.Current()->followECR().propagateExposed();
  }
}

void ECR::touchExposedAndPropagate()
{
  if(exposed && !this->touched){
    this->touched = true;

    //- out of laziness, AOs won't be individually marked

    //- propagate forward
    suco_iterator<ECR *> ti(inclTo);
    while(ti.Iterate())
      ti.Current()->followECR().touchExposedAndPropagate();
  }
}

void ECR::markVulnerableDerefAndPropagate()
{
  if(!this->is_vuln_deref){
    this->is_vuln_deref = true;

    //- propagate backwards
    suco_iterator<ECR *> ti(this->inclFrom);
    while(ti.Iterate())
      ti.Current()->followECR().markVulnerableDerefAndPropagate();
  }
}

//- if ecrno == 0 (unassigned), then assign an ecrno
//- further, if outf != 0 and ecrno is unassigned, then
//           write inclTo list for this ecr, which
//           triggers this process for inclTo targets
int ECR::getEcrNo(FILE * outf)
{
  static int ecrno_counter = 0;

  if(!ecrno){
    ecrno = ++ecrno_counter;
    if(outf && !inclTo.IsEmpty()){
      suco_iterator<ECR *> iti(inclTo);
      //- pass 1: trigger output of unassigned ECRs
      while(iti.Iterate())
        iti.Current()->followECR().getEcrNo(outf);
      //- pass 2: write output
      iti.Rewind();
      fprintf(outf, "> %d", ecrno);
      while(iti.Iterate())
        fprintf(outf, " %d", iti.Current()->followECR().getEcrNo());
      fprintf(outf, "\n");
    }
  }

  return ecrno;
}

void ECR::debug_traceInclude(ECR& e)
{
  fprintf(AO::aoWriteStream, "--ECR[%08x]:\n", (unsigned int) &e);
}

void ECR::debug_dump(FILE * outf)
{
  fprintf(outf, "ECR[%08x] %s poss-type:", (unsigned int)this, invalid?"invalid":"valid");
  poss_type.debug_dump(outf);
  fprintf(outf, " contains:\n");
  suco_iterator<AO *> aoi(this->aoset);
  while(aoi.Iterate())
    aoi.Current()->debug_dump(outf);
  fprintf(outf, "INCLUDES TO ECRs:");
  suco_iterator<ECR *> ii(inclTo);
  while(ii.Iterate()) fprintf(outf, " [%08x]", (unsigned int)&ii.Current()->followECR());
  fprintf(outf, "\n");
  fprintf(outf, "POINTS TO ECR[%08x].\n", ptsTo?((unsigned int)&getPointsTo()):0);
  if(ptsTo && ptsTo->aoset.IsEmpty()){
    fprintf(outf, "([%08x] points to [%08x], incl to:", (unsigned int)ptsTo, ptsTo->ptsTo?((unsigned int)&ptsTo->getPointsTo()):0);
    suco_iterator<ECR *> pi(ptsTo->inclTo);
    while(pi.Iterate()) fprintf(outf, " [%08x]", (unsigned int)&pi.Current()->followECR());
    fprintf(outf, ")\n");
  }
}

void ECR::debug_dump_list(suco_llist<ECR *>& elist, FILE * outf)
{
  fprintf(outf, "ecrlist(%d): ", elist.Length());
  suco_iterator<ECR *> ei(elist);
  while(ei.Iterate()){
    fprintf(outf, "[%8x]", (unsigned int)ei.Current());
  }
  fprintf(outf, "\n");
}

//------------------------------------------------------

