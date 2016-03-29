#include <ctype.h>
#include <stdio.h>
#include <stdlib.h> // for strtoul
#include <string.h>
#include "ao.h"
#include "ecr.h"
#include "id.h"
#include "cfg.h"
#include "tca.h" // for TCA_BUF_SIZE
#include "flags.h" // for flag_verbose, flag_use_bblocks, flag_range_filter_*_backedge
#include "diag.h" // for timer

//----------------------------------
// AID
//----------------------------------

AID AID::zero(0,0);
suco_llist<const char *> AID::filestemlist;

//- assign and write map of number to function name
void AID::writeFileStemMap(FILE * outf)
{
  int fnid = 0;
  suco_iterator<const char *> fli(AID::filestemlist);
  while(fli.Iterate()){
    fnid++;
    fprintf(outf, "! @ %d %s\n", fnid, fli.Current());
  }
}

const char * AID::lookupFileStem()
{
  const char * ret = AID::filestemlist.ElementAt(this->filestem_id);
  return ret?ret:"**ERROR**";
}

//----------------------------------
// EXP DESCR
//----------------------------------

int Dependency::compare(Dependency * ldep, Dependency * rdep)
{
  return &ldep->key - &rdep->key;
}

int DependencyMap::aoset_compare(suco_set<AO *> * laos, suco_set<AO *> * raos)
{
  suco_iterator<AO *> laoi(*laos);
  suco_iterator<AO *> raoi(*raos);
  while(laoi.Iterate() & raoi.Iterate()){
    int ret = laoi.Current() - raoi.Current();
    if(ret) return ret;
  }
  return (laoi.Current() - raoi.Current());
}

//- consumes depset
void DependencyMap::addDirectedDependency(suco_set<AO *>& keyset, suco_set<AO *>& depset)
{
  suco_iterator<AO *> kaoi(keyset);
  while(kaoi.Iterate()){
    AO& key_ao = *kaoi.Current();
    bool found = false;
    //- look for key_ao's entry
    suco_iterator<Dependency *> depi(this->deps);
    while(depi.Iterate()){
      if(&depi.Current()->getKey() == &key_ao){
        found = true;
        depi.Current()->getAOs().UnionConsume(depset);
        break;
      }
    }
    if(!found){
      Dependency * newdep = new Dependency(key_ao);
      newdep->getAOs().Attach(depset);
      this->deps.Insert(newdep);
    }
  }
}

//- consumes depset
void DependencyMap::addDependencySet(suco_set<AO *>& depset)
{
  suco_set<AO *> * newset = new suco_set<AO *>;
  newset->Attach(depset);
  if(!this->aosets.Insert(newset))
    delete newset;
}

void DependencyMap::debug_dump(FILE * outf)
{
  fprintf(outf, "Directed Dependencies:\n");
  suco_iterator<Dependency *> depi(this->deps);
  while(depi.Iterate()){
    fprintf(outf, " - ");
    depi.Current()->getKey().write_string_rep(outf, true);
    fprintf(outf, " : ");
    AO::write_list_string_rep(outf, depi.Current()->getAOs(), true);
    fprintf(outf, "\n");
  }
  fprintf(outf, "Undirected Dependency Sets:\n");
  suco_iterator<suco_set<AO *> *> aosi(this->aosets);
  while(aosi.Iterate()){
    fprintf(outf, " - ");
    AO::write_list_string_rep(outf, *aosi.Current(), true);
    fprintf(outf, "\n");
  }
}

//----------------------------------
// EXP DESCR
//----------------------------------

ExpDescr::~ExpDescr()
{
   if(affecrs) delete affecrs;
   if(aliasecrs) delete aliasecrs;
}

char * ExpDescr::stringToEstr(char * str, char ** nptr)
{
  if(*str == '{'){
    char * ep;
    for(ep = str; *ep && *ep != '}'; ++ep);
    if(*ep == '}') ep++;
    else fprintf(stderr, "ExpDescr::stringToEstr: Missing end-delimiter: %s", str);
    {
      int slen = ep - str;
      char * estr = new char[slen+1];
      if(estr){
        strncpy(estr, str, slen);
        estr[slen] = '\0';
        if(nptr) *nptr = ep;
        return estr;
      } else {
        fprintf(stderr, "ExpDescr::stringToEstr: MALLOC(new) ERROR\n");
        return "@";
      }
    }
  } else if(*str == '@'){
    if(nptr) *nptr = str+1;
    return "@";
  } else {
    fprintf(stderr, "ExpDescr::stringToEstr: Invalid expr-string: %s", str);
    return "@";
  }
}

int ExpDescr::compare(ExpDescr * lex, ExpDescr * rex)
{
  int ret = strcmp(lex->estr, rex->estr);

  if(!ret){ //- estr are equal, now compare afflocs
    suco_iterator<AO *> li(lex->estr_aos);
    suco_iterator<AO *> ri(rex->estr_aos);

    while(!ret && (li.Iterate() & ri.Iterate()))
      ret = ((int)li.Current() - (int)ri.Current());

    if(!ret)
      ret = ((int)li.Current() - (int)ri.Current());
  }

  return ret;
}

//--------------------------------------
// This is the filter function to decide
// whether this is a singleton location,
// to decide e.g. strong vs weak assignment.
// - returns ao if this is a location that
//   doesn't include a union or array component.
//   (e.g. a[0], a[0].i.j) The latter is because
//   of the way we map a[0] to I <a> directly,
//   but I<a> represents all elements of the array.
//--------------------------------------
AO * ExpDescr::getSingletonLoc()
{
  AO * ret = 0;
  suco_iterator<AO *> aoi(this->getAOs());
  while(aoi.Iterate()){
    AO& ao = *aoi.Current();
    if(ao.isLoc()){
      if(ret) return 0; //-- more than one locs
      ret = &ao;
    } else if(!ao.isVal()){
      return 0; //-- non-val and non-loc
    }
  }
  if(!ret) return 0;

  //- at this point, ret is a singleton
  //- now check for array or union component in access path
  AO * temp_ao = ret;
  while(1){
    TCtype * sty = temp_ao->getStaticType();
    if(sty && sty->getKind() == TCtype::tcArray){
      return 0;
    } else if(temp_ao->getKind() == AO::aoUDot || (sty && sty->getKind() == TCtype::tcUnion)){
      return 0;
    } else if(temp_ao->getKind() == AO::aoSDot){
      temp_ao = &((AOSDot *)temp_ao)->getParent();
    } else { //- end of the line: return
      return ret;
    }
  }
}

void ExpDescr::instantiateAffEcrs()
{
  //- instantiate affecrs: the ECRs of deref-aos deref'ed one level
  if(!this->affecrs){
    this->affecrs = new suco_set<ECR *>;
    suco_iterator<AO *> aoi(this->getAOs());
    while(aoi.Iterate()){
      AO * deref_ao = aoi.Current()->derefOneLevel();
      if(deref_ao && deref_ao->isRef() && !deref_ao->isVal()){
        this->affecrs->Union(deref_ao->getECR().getAliasECRs());
      }
    }
  }
}

bool ExpDescr::affLocsIntersects(suco_set<AO *>& aoset)
{
  //- a. check estr_aos: Ids which directly occur in the expression
  if(aoset.Intersects(this->getEstrAOs())){
    return true;
  }

  //- b. check aliases of deref-aos
  if(!this->affecrs)
    this->instantiateAffEcrs();

  if(this->affecrs->IsEmpty()){ //- optimize if nothing to check
    //- translate aoset -> ecrset 
    suco_set<ECR *> ecrset;

    suco_iterator<AO *> aoi(aoset);
    while(aoi.Iterate()){
      AO& ao = *aoi.Current();
      if(!ao.isVal())
        ecrset.Insert(&ao.getECR()); //- omit aliases!?
    }

    return this->affecrs->Intersects(ecrset);

  } else return false;
}

bool ExpDescr::affLocsIntersects(suco_set<ECR *>& ecrset)
{
  //- a. check estr_aos
  suco_iterator<AO *> aoi(this->getEstrAOs());
  while(aoi.Iterate()){
    AO& ao = *aoi.Current();
    if(!ao.isVal() && ecrset.Contains(&ao.getECR())){
      return true;
    }
  }  

  //- b. check affecrs
  if(!this->affecrs)
    this->instantiateAffEcrs();

  return this->affecrs->Intersects(ecrset);
}

//- include also this->estr_aos not covered by this->aos
void ExpDescr::collectAffectingAliasLocECRs(suco_set<ECR *>& eset)
{
  suco_iterator<AO *> aoi(this->getAOs());
  while(aoi.Iterate()){
    aoi.Current()->getECR().collectAliasECRs(eset, true); //- true=follow de-starred aos' aliases
  }
  suco_iterator<AO *> eaoi(this->getEstrAOs());
  while(eaoi.Iterate()){
    eaoi.Current()->getECR().collectAliasECRs(eset, true); //- true=follow de-starred aos' aliases
  }
  ECR::filterLocArgRet(eset);
}

//- Collect aos on which this ExpDescr depend:
//  namely, the union of this->aos and this->estr_aos,
//  less aos in this->estr_aos for which this->aos
//  contains its "addrof".
//  If uncaptured=true, then only collect those that
//  are not in this->aos.
//  ~> this arises in array indexing (A[i]), in which
//	this->aos is just "op A" and not "op i".
void ExpDescr::collectDependencies(suco_set<AO *>& dep, bool uncaptured)
{
  //- compute addrof_filter: { x | "addrof x" in this->aos }
  suco_set<AO *> addrof_filter;
  suco_iterator<AO *> taoi(this->getAOs());
  while(taoi.Iterate()){
    if(taoi.Current()->getKind() == AO::aoAddrOf){
      addrof_filter.Insert(&((AOAddrOf *)taoi.Current())->getTarget());
    }
  }

  if(uncaptured){ //- collect only uncaptured dependencies
    suco_set<ECR *>& alias_ecrs = this->getAliasECRs();
    suco_iterator<AO *> eaoi(this->getEstrAOs());
    while(eaoi.Iterate()){
      if(!alias_ecrs.Contains(&eaoi.Current()->getECR()) &&
	 !addrof_filter.Contains(eaoi.Current())){
        dep.Insert(eaoi.Current());
      }
    }
  } else { //- collect all dependencies
    dep.Union(this->getAOs());
    suco_iterator<AO *> eaoi(this->getEstrAOs());
    while(eaoi.Iterate()){
      if(!addrof_filter.Contains(eaoi.Current())){
        dep.Insert(eaoi.Current());
      }
    }
  }
}

suco_set<ECR *>& ExpDescr::getAliasECRs()
{
  //- check cache; if empty, compute
  if(!this->aliasecrs){

    this->aliasecrs = new suco_set<ECR *>;

    suco_iterator<AO *> aoi(this->getAOs());
    while(aoi.Iterate()){
      AO& ao = *aoi.Current();
      if(!ao.isVal()){
        if(ao.isLoc()) this->aliasecrs->Insert(&ao.getECR()); //- slight optimization: improves precision
								 //  TODO: however, not safe w.r.t. unions?
        else this->aliasecrs->Union(ao.getECR().getAliasECRs());
      }
    }
  }

  return *this->aliasecrs;
}

//----------------------------------
// LOC SET
//----------------------------------

LocSet * const CFGnode::ALL_AOS = (LocSet *)1;

//TODO: using AO::isLocArgRet to filter --
//      for arg/ret, should only filter "[FR] X I <>" and not "[FR] D <>"?

int ECRlocSet::numAOs()
{
  int ret = 0;
  suco_iterator<ECR *> ei(this->ecrset);
  while(ei.Iterate()){
    suco_iterator<AO *> aoi(ei.Current()->getAOset());
    while(aoi.Iterate()){
      if(aoi.Current()->isLocArgRet()){
        ret++;
      }
    }
  }
  return ret;
}

bool ECRlocSet::Insert(AO& ao)
{
  return ao.isLocArgRet() && this->ecrset.Insert(&ao.getECR());
}
 
bool ECRlocSet::Contains(AO& ao)
{
  return this->ecrset.Contains(&ao.getECR());
}

void ECRlocSet::AbsorbConsume(suco_set<ECR *>& ecrset)
{
  //- filter out non-Loc'ed ecrs
  suco_iterator<ECR *> ei(ecrset);
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
  this->ecrset.UnionConsume(ecrset);
}

void ECRlocSet::addToAOset(suco_set<AO *>& aoset)
{
  suco_iterator<ECR *> ei(this->ecrset);
  while(ei.Iterate()){
    aoset.Union(ei.Current()->getAOset());
  }
  //- filter out non-location AOs
  suco_iterator<AO *> ai(aoset);
  while(ai.Iterate()){
    if(!ai.Current()->isLoc())
      ai.DeleteCurrent();
  }
}
 
void ECRlocSet::debug_dump(FILE * outf)
{
  suco_iterator<ECR *> ecri(this->ecrset);
  while(ecri.Iterate())
    AO::write_list_string_rep(outf, ecri.Current()->getAOset(), true);
  fprintf(outf, "\n");
}

bool ECRlocSet::Intersects(suco_set<AO *>& aoset)
{
  suco_iterator<AO *> aoi(aoset);
  while(aoi.Iterate()){
    if(this->ecrset.Contains(&aoi.Current()->getECR())){
      return true;
    }
  }
  return false;
}

bool AOlocSet::Intersects(suco_set<ECR *>& ecrset)
{
  suco_iterator<AO *> aoi(this->aoset);
  while(aoi.Iterate()){
    if(ecrset.Contains(&aoi.Current()->getECR())){
      return true;
    }
  }
  return false;
}

void ECRlocSet::writeIntersectingAOs(BaseLocSet& lset, suco_set<AO *>& aoset)
{
  lset.addToAOset(aoset);
  suco_set<AO *> tmp;
  this->addToAOset(tmp);
  aoset.Intersect(tmp);
}

void AOlocSet::writeIntersectingAOs(BaseLocSet& lset, suco_set<AO *>& aoset)
{
  lset.addToAOset(aoset);
  aoset.Intersect(this->aoset);
}

void AOlocSet::AbsorbConsume(suco_set<ECR *>& ecrset)
{
  suco_iterator<ECR *> ecri(ecrset);
  while(ecri.Iterate()){
    this->aoset.Union(ecri.Current()->getAOset());
  }
  //- filter out non-loc AOs
  suco_iterator<AO *> aoi(this->aoset);
  while(aoi.Iterate()){
    if(!aoi.Current()->isLoc()){
      aoi.DeleteCurrent();
    }
  }
}

void AOlocSet::addToEcrSet(suco_set<ECR *>& ecrset)
{
  suco_iterator<AO *> aoi(this->aoset);
  while(aoi.Iterate()){
    ecrset.Insert(&aoi.Current()->getECR());
  }
}

//----------------------------------
// READ FUNCTIONS
//----------------------------------

PgmExpr * PgmExpr::read(InputState& is, char * buf, PgmStmt * parent)
{
  PgmExpr * df = 0;

  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);
  int fsid = (parent)?(parent->getParentFunction().getFileStemId()):0;

  if(buf[0] == mPfx){
    switch(buf[2]){
      case mCall: {
        if(!parent){
          fprintf(stderr, "Reading call node with no parent PgmStmt!\n");
        } else {
          char * cp = &buf[4];
          int aidno = strtoul(cp, &cp, 10);
          suco_set<AO *>& faos = (suco_set<AO *>&) AO::stringToAOlist(true, cp, &cp, is.aidmap, is.pidmap, is.values);
          int nargs = strtoul(cp, &cp, 10);
          PExprCall& dfc = *new PExprCall(AID(fsid,aidno), faos, nargs, parent);
          df = &dfc;
          for(int i = 0; i < nargs; ++i){
            int argaid = strtoul(cp, &cp, 10);
            suco_set<AO *>& argaos = (suco_set<AO *>&) AO::stringToAOlist(true, cp, &cp, is.aidmap, is.pidmap, is.values);
            suco_llist<AO *>& argestr_aos = AO::stringToAOlist(false, cp, &cp, is.aidmap, is.pidmap, is.values);
            while(*cp && isspace(*cp)) cp++;
            char * argestr = ExpDescr::stringToEstr(cp, &cp);
            dfc.args[i] = new PExprArg(AID(fsid, argaid), argaos, argestr_aos, argestr);
          }
        }
      } break;
      case mAssign: {
        char * cp = &buf[4];
        int aidno = strtoul(cp, &cp, 10);
        suco_set<AO *>& e1aos = (suco_set<AO *>&) AO::stringToAOlist(true, cp, &cp, is.aidmap, is.pidmap, is.values);
        suco_set<AO *>& e2aos = (suco_set<AO *>&) AO::stringToAOlist(true, cp, &cp, is.aidmap, is.pidmap, is.values);
        suco_llist<AO *>& e1str_aos = AO::stringToAOlist(false, cp, &cp, is.aidmap, is.pidmap, is.values);
        suco_llist<AO *>& e2str_aos = AO::stringToAOlist(false, cp, &cp, is.aidmap, is.pidmap, is.values);
        while(*cp && isspace(*cp)) cp++;
        char * e1str = ExpDescr::stringToEstr(cp, &cp);
        char * e2str = ExpDescr::stringToEstr(cp, &cp);
        df = new PExprAssign(AID(fsid,aidno), e1aos, e1str_aos, e1str, e2aos, e2str_aos, e2str, parent);
      } break;
      case mPredicate: {
        enum PExprPredicate::prKind prk = PExprPredicate::prStmt;
        switch(buf[4]){
          case mPrAnd:	  prk = PExprPredicate::prAnd; break;
          case mPrOr:	  prk = PExprPredicate::prOr; break;
          case mPrQC:	  prk = PExprPredicate::prQC; break;
          case mPrStmt:	  prk = PExprPredicate::prStmt; break;
          case mPrSwitch: prk = PExprPredicate::prSwitch; break;
          default :
            fprintf(stderr, "Invalid Predicate Kind (%c) in input line: %s\n", buf[4], buf);
            return 0;
        }
        char * cp = &buf[6];
        int aidno = strtoul(cp, &cp, 10);
        suco_llist<AO *>& estr_aos = AO::stringToAOlist(false, cp, &cp, is.aidmap, is.pidmap, is.values);
        while(*cp && isspace(*cp)) cp++;
        char * estr = ExpDescr::stringToEstr(cp, &cp);
        df = new PExprPredicate(AID(fsid,aidno), estr_aos, estr, prk, parent);
      } break;
      case mFormal: {
        char * cp = &buf[4];
        int argno = strtoul(cp, &cp, 10);
        int pidno = strtoul(cp, &cp, 10);
        ID * pid = is.pidmap.lookup(pidno);
        if(pid) df = new PExprDecl(pid->get_AOId(false), fsid, PExprDecl::dFormal, parent, false, argno);
        else fprintf(stderr, "Invalid Pid (%d) read for CFG formal node\n", pidno);
      } break;
      case mVerify: {
        enum PExprVerify::vtKind vtk = PExprVerify::vtNone;
        enum PExprVerify::vpKind vpk = PExprVerify::vpNone;
        switch(buf[4]){
          case mVTag:	  vtk = PExprVerify::vtTag;  vpk = PExprVerify::vpNone; break;
          case mVTagPtr:  vtk = PExprVerify::vtTag;  vpk = PExprVerify::vpPtr;  break;
          case mVTagPtrW: vtk = PExprVerify::vtTag;  vpk = PExprVerify::vpPtrW; break;
          case mVRhs:	  vtk = PExprVerify::vtRhs;  vpk = PExprVerify::vpNone; break;
          case mVRhsPtr:  vtk = PExprVerify::vtRhs;  vpk = PExprVerify::vpPtr;  break;
          case mVPtr:	  vtk = PExprVerify::vtNone; vpk = PExprVerify::vpPtr;  break;
          case mVPtrW:	  vtk = PExprVerify::vtNone; vpk = PExprVerify::vpPtrW; break;
          default :
            fprintf(stderr, "Invalid Verify Kind (%c) in input line: %s\n", buf[4], buf);
            return 0;
        }
        char * cp = &buf[6];
        int aidno = strtoul(cp, &cp, 10);
        suco_set<AO *>& aos = (suco_set<AO *>&) AO::stringToAOlist(true, cp, &cp, is.aidmap, is.pidmap, is.values);
        suco_llist<AO *>& estr_aos = AO::stringToAOlist(false, cp, &cp, is.aidmap, is.pidmap, is.values);
        while(*cp && isspace(*cp)) cp++;
        char * estr = ExpDescr::stringToEstr(cp, &cp);
        df = new PExprVerify(vtk, vpk, AID(fsid,aidno), aos, estr_aos, estr, parent);
      } break;
      case mLocalDecl: {
        char * cp = &buf[4];
        bool iszeroed = false;
        if(*cp == 'z'){
          iszeroed = true;
          cp += 2;
        }
        int pidno = strtoul(cp, &cp, 10);
        ID * pid = is.pidmap.lookup(pidno);
        if(pid) df = new PExprDecl(pid->get_AOId(iszeroed), fsid, PExprDecl::dLocal, parent, iszeroed);
        else fprintf(stderr, "Invalid Pid (%d) read for CFG local-decl node\n", pidno);
      } break;
      case mStaticDecl: {
        char * cp = &buf[4];
        int pidno = strtoul(cp, &cp, 10);
        ID * pid = is.pidmap.lookup(pidno);
        if(pid) df = new PExprDecl(pid->get_AOId(true), fsid, PExprDecl::dStatic, parent, true);
        else fprintf(stderr, "Invalid Pid (%d) read for CFG static-decl node\n", pidno);
      } break;
      case mMallocDecl: {
        char * cp = &buf[4];
        bool iszeroed = false;
        bool isalloca = false;
        if(*cp == 'z'){
          iszeroed = true;
          cp += 2;
        } else if(*cp == 'a'){
          isalloca = true;
          cp += 2;
        }
        int aidno = strtoul(cp, &cp, 10);
        ID * aid = is.aidmap.lookup(aidno);
        suco_set<AO *>& eaos = (suco_set<AO *>&) AO::stringToAOlist(true, cp, &cp, is.aidmap, is.pidmap, is.values);
        suco_llist<AO *>& estr_aos = AO::stringToAOlist(false, cp, &cp, is.aidmap, is.pidmap, is.values);
        while(*cp && isspace(*cp)) cp++;
        char * estr = ExpDescr::stringToEstr(cp, &cp);
        if(aid) df = new PExprDecl(aid->get_AOMalloc(isalloca,iszeroed), fsid, PExprDecl::dMalloc, parent, iszeroed, 0,
					new ExpDescr(estr_aos, estr, eaos));
        else fprintf(stderr, "Invalid Aid (%d) read for CFG malloc-decl node\n", aidno);
      } break;
      case mStart: {
        switch(buf[4]){
          case mParallel: {
            df = &PExprParallel::read(is, buf, parent);
          } break;
          case mBranch: {
            df = &PExprBranch::read(is, buf, parent);
          } break;
          default: {
            // not valid PgmExpr; return 0
          } break;
        }
      } break;
      case mNext:
      case mEnd: {
        // end of PgmExpr list; return 0
      } break;
      default: {
        fprintf(stderr, "PgmExpr::read: invalid node ignored: %s", buf);
      } break;
    }
  } else fprintf(stderr, "PgmExpr::read: bad prefix on line %s", buf);

  return df;
}

void PgmExpr::readlist(InputState& is, char * buf, suco_llist<PgmExpr *>& nodelist, PgmStmt * parent)
{
  while(!feof(is.inf)){
    PgmExpr * df = PgmExpr::read(is,buf,parent);
    if(df) nodelist.Append(df);
    else break; // non-node item; break out
    buf[0] = 0;
  }
}

PExprParallel& PExprParallel::read(InputState& is, char * buf, PgmStmt * parent)
{
  PExprParallel * df = new PExprParallel(parent);

  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);

  //-- process node start header: read numlists (redundant)
  if(buf[0] == mPfx && buf[2] == mStart && buf[4] == mParallel){
    char * cp = &buf[6];
    int numlists = strtoul(cp, &cp, 10);

    //-- process nodelists
    int countlists = 0;
    fgets(buf, TCA_BUF_SIZE, is.inf);
    while(!feof(is.inf) && buf[0] == mPfx && buf[2] == mNext && buf[4] == mParallel){
      suco_llist<PgmExpr *>& nodelist = *new suco_llist<PgmExpr *>;
      buf[0] = 0;
      PgmExpr::readlist(is, buf, nodelist, parent);
      df->nodelists.Append(&nodelist);
      countlists++;
    }

    // (assert end marker)
    if(buf[0] != mPfx || buf[2] != mEnd || buf[4] != mParallel)
      fprintf(stderr, "PExprParallel::read: expecting end marker on line %s", buf);
    // (assert numlists)
    if(countlists != numlists)
      fprintf(stderr, "PExprParallel::read: numlist mismatch: header = %d, count = %d\n", numlists, countlists);

  } else fprintf(stderr, "PExprParallel::read: bad start marker on line %s", buf);

  return *df;
}

PExprBranch& PExprBranch::read(InputState& is, char * buf, PgmStmt * parent)
{
  PExprBranch * df = new PExprBranch(parent);

  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);

  //-- process node start header: assert numlists
  if(buf[0] != mPfx || buf[2] != mStart || buf[4] != mBranch
     || strtoul(buf+6, 0, 10) != 3)
    fprintf(stderr, "PExprBranch::read: bad start marker on line %s", buf);

  //-- process predicate nodelist
  fgets(buf, TCA_BUF_SIZE, is.inf);
  if(buf[0] == mPfx && buf[2] == mNext && buf[4] == mBranch){
    buf[0] = 0;
    suco_llist<PgmExpr *> prednodelist;
    PgmExpr::readlist(is, buf, prednodelist, parent);
    if(prednodelist.Length() == 1){
      if(prednodelist.Head()->getKind() == PgmExpr::fPredicate)
        df->prednode = (PExprPredicate *) prednodelist.Head();
      else fprintf(stderr, "PExprBranch::read: prednode is not a predicate node.\n");
    } else if(prednodelist.Length() > 1)
      fprintf(stderr, "PExprBranch::read: prednode list has %d elements (should be <=1)\n", prednodelist.Length());
  }
  else fprintf(stderr, "PExprBranch::read: expecting predicate list in %s", buf );

  //-- process true nodelist
  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);
  if(buf[0] == mPfx && buf[2] == mNext && buf[4] == mBranch){
    buf[0] = 0;
    PgmExpr::readlist(is, buf, df->tnodelist, parent);
  }
  else fprintf(stderr, "PExprBranch::read: expecting true list in %s", buf );

  //-- process false nodelist
  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);
  if(buf[0] == mPfx && buf[2] == mNext && buf[4] == mBranch){
    buf[0] = 0;
    PgmExpr::readlist(is, buf, df->fnodelist, parent);
  }
  else fprintf(stderr, "PExprBranch::read: expecting false list in %s", buf );

  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);
  // (assert end marker)
  if(buf[0] != mPfx || buf[2] != mEnd || buf[4] != mBranch)
    fprintf(stderr, "PExprBranch::read: expecting end marker on line %s", buf);

  return *df;
}

PgmStmt& PgmStmt::read(InputState& is, CFGfunction& par, char * buf)
{
  PgmStmt * cfgn = new PgmStmt(par);

  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);

  //-- process node start header: read aid and numlabels
  if(buf[0] == mPfx && buf[2] == mStart && buf[4] == mNode){
    char * cp = &buf[6];

    cfgn->aid.aid = strtoul(cp, &cp, 10);
    cfgn->aid.filestem_id = par.getFileStemId();
    cfgn->nlabels = strtoul(cp, &cp, 10);

    //-- process labels
    if(cfgn->nlabels){
      cfgn->labels = new char * [cfgn->nlabels];
      for(int i = 0; i < cfgn->nlabels; ++i){
        fgets(buf, TCA_BUF_SIZE, is.inf);
        char * cp = &buf[4];
        if(buf[0] == mPfx && buf[2] == mLabel
           && cfgn->aid.aid == (int) strtoul(cp, &cp, 10)){ // asserts, and advances cp!
          while(isspace(*cp)) ++cp;
          int s = strlen(cp) - 1;
          while(isspace(cp[s])) --s;
          cp[++s] = 0;
          cfgn->labels[i] = new char[s+1];
          strcpy(cfgn->labels[i], cp);
        } else fprintf(stderr, "PgmStmt::read: bad label marker on line %s", buf);
      }
    }

    //-- process node next (assert)
    {
      fgets(buf, TCA_BUF_SIZE, is.inf);
      if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mNode)
        fprintf(stderr, "PgmStmt::read: next assertion failed on line %s", buf);
    }

    //-- process nodes, until node end
    fgets(buf, TCA_BUF_SIZE, is.inf);
    while(!feof(is.inf) && (buf[0] != mPfx || buf[2] != mEnd || buf[4] != mNode)){
      cfgn->exprlist.Append(PgmExpr::read(is, buf, cfgn));
      fgets(buf, TCA_BUF_SIZE, is.inf);
    }

  } else fprintf(stderr, "PgmStmt::read: bad start marker on line %s", buf);

  return *cfgn;
}

void PgmStmt::readlist(InputState& is, CFGfunction& par, char * buf, PgmStmt *& current, suco_llist<PgmStmt *>& nodelist)
{
  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);
/**//*SY:see if following condition is preferable?*/
  while(!feof(is.inf)){ // && (buf[0] != mPfx || (buf[2] != mNext && buf[2] != mEnd))){
    if(!PgmStmt::readentry(is,par,buf,current,nodelist))
      return;
    fgets(buf, TCA_BUF_SIZE, is.inf);
  }
}

bool PgmStmt::readentry(InputState& is, CFGfunction& par, char * buf, PgmStmt *& current, suco_llist<PgmStmt *>& nodelist)
{
  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);

  int fsid = par.getFileStemId();

  if(buf[0] == mPfx){
    switch(buf[2]){
      case mStart: {
	switch(buf[4]){
          case mWhile: {

	    //-- predicate node
	    buf[0] = 0;
	    PgmStmt& pred_node = PgmStmt::read(is, par, buf);
	    pred_node.annot.kind = Annot::aWhile;

	    //-- while next marker / verify
	    fgets(buf, TCA_BUF_SIZE, is.inf);
	    if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mWhile)
	      fprintf(stderr, "PgmStmt::readentry: expecting while next on line %s", buf);

	    //-- attach pred_node's predecessor, set current, add to nodelist
	    if(current) current->addSuccessor(pred_node);
	    pred_node.initSuccsCount(2);
	    current = &pred_node;
	    nodelist.Append(&pred_node);

	    //-- statements (until while end marker)
	    buf[0] = 0;
	    suco_llist<PgmStmt *> snodelist;
	    PgmStmt * scurrent = &pred_node;
	    PgmStmt::readlist(is, par, buf, scurrent, snodelist);

	    //-- while end marker / verify
	    if(buf[0] != mPfx || buf[2] != mEnd || buf[4] != mWhile)
	      fprintf(stderr, "PgmStmt::readentry: expecting while end on line %s", buf);

	    if(snodelist.IsEmpty()){ // empty while-body: self-loop

	      pred_node.initPredsCount(2);
	      pred_node.addSuccessor(pred_node);

	    } else {

	      //-- collect break and continue nodes
	      suco_llist<PgmStmt *> continuenodes;
	      suco_llist<PgmStmt *> breaknodes;

	      suco_iterator<PgmStmt *> sni(snodelist);
	      while(sni.Iterate()){
	        PgmStmt * n = sni.Current();
		if(n->annot.kind == Annot::aContinue && !n->succs)
		  continuenodes.Append(n);
		if(n->annot.kind == Annot::aBreak && !n->succs)
		  breaknodes.Append(n);
	      }

	      //-- attach statement exit / continue nodes to pred_node
	      if(continuenodes.Length()){
	        //-- add scurrent (if non-null) to continuenodes
	        if(scurrent) continuenodes.Append(scurrent);

	        pred_node.initPredsCount(1 + continuenodes.Length());
	        suco_iterator<PgmStmt *> cni(continuenodes);
	        while(cni.Iterate()) cni.Current()->addSuccessor(pred_node);

	      } else {

	        pred_node.initPredsCount(2);
	        if(scurrent) scurrent->addSuccessor(pred_node);

	      }

	      //-- attach breaks, if any, to dummy outnode
	      if(breaknodes.Length()){

	        //-- create dummy outnode
	        PgmStmt * outnode = new PgmStmt(par);
		outnode->annot.kind = Annot::aOutNode;
	        snodelist.Append(outnode);

	        pred_node.addSuccessor(*outnode);
	        current = outnode;

	        outnode->initPredsCount(1 + breaknodes.Length());
	        suco_iterator<PgmStmt *> bni(breaknodes);
	        while(bni.Iterate()) bni.Current()->addSuccessor(*outnode);
	      }

	      //-- attach into nodelist; note: can only do this after we've "processed" snodelist
	      nodelist.Attach(snodelist);
	    }

          } break;

          case mDoWhile: {

	    //-- predicate node
	    buf[0] = 0;
	    PgmStmt& pred_node = PgmStmt::read(is, par, buf);
	    pred_node.annot.kind = Annot::aDoWhile;

	    //-- while next marker / verify
	    fgets(buf, TCA_BUF_SIZE, is.inf);
	    if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mDoWhile)
	      fprintf(stderr, "PgmStmt::readentry: expecting do-while next on line %s", buf);

	    //-- statements (until do-while end marker)
	    buf[0] = 0;
	    suco_llist<PgmStmt *> snodelist;
	    PgmStmt::readlist(is, par, buf, current, snodelist);

	    //-- while end marker / verify
	    if(buf[0] != mPfx || buf[2] != mEnd || buf[4] != mDoWhile)
	      fprintf(stderr, "PgmStmt::readentry: expecting do-while end on line %s", buf);

	    //-- attach pred_node's predecessor, set current
	    if(current) current->addSuccessor(pred_node);
	    pred_node.initSuccsCount(2);
	    current = &pred_node;

	    if(snodelist.IsEmpty()){ // empty do-while body: self-loop

	      pred_node.initPredsCount(2);
	      pred_node.addSuccessor(pred_node);
	      nodelist.Append(&pred_node);

	    } else {

	      //-- attach pred_node to snodelist.head
	      snodelist.Head()->initPredsCount(2);
	      pred_node.addSuccessor(*snodelist.Head());

	      //-- collect break and continue nodes
	      suco_llist<PgmStmt *> continuenodes;
	      suco_llist<PgmStmt *> breaknodes;

	      suco_iterator<PgmStmt *> sni(snodelist);
	      while(sni.Iterate()){
	        PgmStmt * n = sni.Current();
		if(n->annot.kind == Annot::aContinue && !n->succs)
		  continuenodes.Append(n);
		if(n->annot.kind == Annot::aBreak && !n->succs)
		  breaknodes.Append(n);
	      }

	      //-- attach into nodelist; note: can only do this after we've "processed" snodelist
	      nodelist.Attach(snodelist);

	      //-- for the sake of debugging readability, will append nodes in "expected" order
	      nodelist.Append(&pred_node);

	      //-- attach continue nodes, if any, to pred_node
	      if(continuenodes.Length()){

	        pred_node.initPredsCount(1 + continuenodes.Length());
	        suco_iterator<PgmStmt *> cni(continuenodes);
	        while(cni.Iterate()) cni.Current()->addSuccessor(pred_node);

	      }

	      //-- attach breaks, if any, to dummy outnode
	      if(breaknodes.Length()){

	        //-- create dummy outnode
	        PgmStmt * outnode = new PgmStmt(par);
		outnode->annot.kind = Annot::aOutNode;
	        nodelist.Append(outnode);

	        outnode->initPredsCount(1 + breaknodes.Length());

	        pred_node.addSuccessor(*outnode);
	        current = outnode;

	        suco_iterator<PgmStmt *> bni(breaknodes);
	        while(bni.Iterate()) bni.Current()->addSuccessor(*outnode);
	      }
	    }

          } break;

          case mFor: {

	    //-- node1
	    buf[0] = 0;
	    PgmStmt& node1 = PgmStmt::read(is, par, buf);
	    node1.annot.kind = Annot::aFor;

	    //-- node2
	    buf[0] = 0;
	    PgmStmt& node2 = PgmStmt::read(is, par, buf);

	    //-- node3
	    buf[0] = 0;
	    PgmStmt& node3 = PgmStmt::read(is, par, buf);

	    //-- for next marker / verify
	    fgets(buf, TCA_BUF_SIZE, is.inf);
	    if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mFor)
	      fprintf(stderr, "PgmStmt::readentry: expecting for next on line %s", buf);

	    //-- attach node1's predecessor, node1 to node 2, node 3 to node 2,
	    //   set current, add to nodelist
	    if(current) current->addSuccessor(node1);
	    node1.addSuccessor(node2);
	    node2.initPredsCount(2);
	    node2.initSuccsCount(2);
	    node3.addSuccessor(node2);
	    nodelist.Append(&node1);
	    nodelist.Append(&node2);
	    nodelist.Append(&node3);
	    current = &node2;

	    //-- statements, until for end marker
	    buf[0] = 0;
	    suco_llist<PgmStmt *> snodelist;
	    PgmStmt * scurrent = &node2;
	    PgmStmt::readlist(is, par, buf, scurrent, snodelist);

	    //-- for end marker / verify
	    if(buf[0] != mPfx || buf[2] != mEnd || buf[4] != mFor)
	      fprintf(stderr, "PgmStmt::readentry: expecting for end on line %s", buf);

	    if(snodelist.IsEmpty()){ // empty for-body: link node2 to node3

	      node2.addSuccessor(node3);

	    } else {

	      //-- collect break and continue nodes
	      suco_llist<PgmStmt *> continuenodes;
	      suco_llist<PgmStmt *> breaknodes;

	      suco_iterator<PgmStmt *> sni(snodelist);
	      while(sni.Iterate()){
	        PgmStmt * n = sni.Current();
		if(n->annot.kind == Annot::aContinue && !n->succs)
		  continuenodes.Append(n);
		if(n->annot.kind == Annot::aBreak && !n->succs)
		  breaknodes.Append(n);
	      }

	      //-- attach statement exit / continue nodes to node3
	      if(continuenodes.Length()){
	        //-- add scurrent (if non-null) to continuenodes
	        if(scurrent) continuenodes.Append(scurrent);

	        node3.initPredsCount(continuenodes.Length());
	        suco_iterator<PgmStmt *> cni(continuenodes);
	        while(cni.Iterate()) cni.Current()->addSuccessor(node3);

	      } else {

	        if(scurrent){
	          node3.initPredsCount(2);
 		  scurrent->addSuccessor(node3);
		}

	      }

	      //-- attach breaks, if any, to dummy outnode
	      if(breaknodes.Length()){

	        //-- create dummy outnode
	        PgmStmt * outnode = new PgmStmt(par);
		outnode->annot.kind = Annot::aOutNode;
	        snodelist.Append(outnode);

	        node2.addSuccessor(*outnode);
	        current = outnode;

	        outnode->initPredsCount(1 + breaknodes.Length());
	        suco_iterator<PgmStmt *> bni(breaknodes);
	        while(bni.Iterate()) bni.Current()->addSuccessor(*outnode);
	      }

	      //-- attach into nodelist; note: can only do this after we've "processed" snodelist
	      nodelist.Attach(snodelist);
	    }

          } break;

          case mIf: {

	    //-- predicate node
	    buf[0] = 0;
	    PgmStmt& pred_node = PgmStmt::read(is, par, buf);
	    pred_node.annot.kind = Annot::aIf;

	    //-- attach pred_node's predecessor
	    if(current) current->addSuccessor(pred_node);
	    pred_node.initSuccsCount(2);
	    nodelist.Append(&pred_node);

	    //-- if next marker / verify
	    fgets(buf, TCA_BUF_SIZE, is.inf);
	    if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mIf)
	      fprintf(stderr, "PgmStmt::readentry: failed verify if next on line %s", buf);

	    //-- true statements, until if next marker
	    buf[0] = 0;
	    current = &pred_node;
	    PgmStmt::readlist(is, par, buf, current, nodelist);

	    //-- create dummy exit node; connect from current
	    PgmStmt * outnode = new PgmStmt(par);
	    outnode->annot.kind = Annot::aOutNode;
	    outnode->initPredsCount(2);
	    if(current) current->addSuccessor(*outnode);

	    //-- if next marker
	    if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mIf)
	      fprintf(stderr, "PgmStmt::readentry: expecting if next on line %s", buf);

	    //-- false statements, until if end marker
	    buf[0] = 0;
	    current = &pred_node; // reset current
	    PgmStmt::readlist(is, par, buf, current, nodelist);

	    //-- connect current to outnode
	    if(current) current->addSuccessor(*outnode);

	    //-- if end marker
	    if(buf[0] != mPfx || buf[2] != mEnd || buf[4] != mIf)
	      fprintf(stderr, "PgmStmt::readentry: expecting if end on line %s", buf);

	    //-- set current, add to nodelist
	    current = outnode;
	    nodelist.Append(outnode);

          } break;

          case mSwitch: {

	    //-- predicate node
	    buf[0] = 0;
	    PgmStmt& pred_node = PgmStmt::read(is, par, buf);
	    pred_node.annot.kind = Annot::aSwitch;

	    //-- attach pred_node's predecessor, add to nodelist
	    if(current) current->addSuccessor(pred_node);
	    nodelist.Append(&pred_node);

	    //-- predicate next marker / verify
	    fgets(buf, TCA_BUF_SIZE, is.inf);
	    if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mSwitch)
	      fprintf(stderr, "PgmStmt::readentry: expecting switch next on line %s", buf);

	    //-- statements, until switch end marker
	    buf[0] = 0;
	    PgmStmt * scurrent = 0;
	    suco_llist<PgmStmt *> snodelist;
	    PgmStmt::readlist(is, par, buf, scurrent, snodelist);

	    //-- switch end marker
	    if(buf[0] != mPfx || buf[2] != mEnd || buf[4] != mSwitch)
	      fprintf(stderr, "PgmStmt::readentry: expecting switch end on line %s", buf);

	    //-- piece together

	    if(snodelist.IsEmpty()){ //- empty switch!?!
	      current = &pred_node;
	    } else {

	      //-- collect case/break nodes
	      suco_llist<PgmStmt *> casenodes;
	      suco_llist<PgmStmt *> breaknodes;
	      bool hasdefault = false;

	      suco_iterator<PgmStmt *> sni(snodelist);
	      while(sni.Iterate()){
	        PgmStmt * n = sni.Current();
		if(n->annot.kind == Annot::aCaseLabel && !n->hasSwitchPred())
		  casenodes.Append(n);
		if(n->annot.kind == Annot::aDefaultLabel && !n->hasSwitchPred()){
		  casenodes.Append(n);
		  hasdefault = true;
		}
		if(n->annot.kind == Annot::aBreak && !n->succs)
		  breaknodes.Append(n);
	      }

	      //-- create dummy exit node
	      PgmStmt * outnode = new PgmStmt(par);
	      outnode->annot.kind = Annot::aOutNode;

	      //-- if no default label, add outnode to casenodes
	      if(!hasdefault) casenodes.Append(outnode);

	      //-- if scurrent, add to breaknodes
	      if(scurrent) breaknodes.Append(scurrent);

	      //-- attach pred_node to casenodes
	      if(!casenodes.IsEmpty()){
	        pred_node.initSuccsCount(casenodes.Length());
	        suco_iterator<PgmStmt *> cni(casenodes);
	        while(cni.Iterate()){
		  if(cni.Current()->preds) cni.Current()->initPredsCount(2);
		  pred_node.addSuccessor(*cni.Current());
	        }
	      }

	      //-- attach breaknodes to outnode
	      if(!breaknodes.IsEmpty()){
	        outnode->initPredsCount(breaknodes.Length()
					+ (hasdefault?0:1)); //- subtle issue: casenodes part will have
							     //  added outnode as pred_node's successor
	        suco_iterator<PgmStmt *> bni(breaknodes);
	        while(bni.Iterate()) bni.Current()->addSuccessor(*outnode);
	      }

	      nodelist.Attach(snodelist);
	      nodelist.Append(outnode);
	      current = outnode;
	    }

          } break;

          case mNode: {
	    PgmStmt& cfgn = PgmStmt::read(is, par, buf);
	    if(current) current->addSuccessor(cfgn);
	    nodelist.Append(&cfgn);
	    current = &cfgn;
          } break;
          default: {
	    fprintf(stderr, "PgmStmt::readentry: bad start marker on line %s", buf);
	    return false;
          } break;
	}
      } break;
      case mJump: {
	char * cp = &buf[6];
	unsigned int aid = strtoul(cp, &cp, 10);
	switch(buf[4]){
	  case mGoto: {
	    PgmStmt * n = new PgmStmt(par, AID(fsid,aid));

	    while(isspace(*cp)) ++cp;
	    int s = strlen(cp) - 1;
	    while(isspace(cp[s])) --s;
	    cp[++s] = 0;
	    char * nc = new char[s+1];
	    strcpy(nc, cp);
	    n->annot.kind = Annot::aGoto;
	    n->annot.u.string = nc;

	    if(current) current->addSuccessor(*n);
	    nodelist.Append(n);
	    current = 0; //- this won't connect with next node
	  } break;
	  case mBreak: {
	    PgmStmt * n = new PgmStmt(par, AID(fsid,aid));
	    n->annot.kind = Annot::aBreak;
	    if(current) current->addSuccessor(*n);
	    nodelist.Append(n);
	    current = 0; //- this won't connect with next node
	  } break;
	  case mContinue: {
	    PgmStmt * n = new PgmStmt(par, AID(fsid,aid));
	    n->annot.kind = Annot::aContinue;
	    if(current) current->addSuccessor(*n);
	    nodelist.Append(n);
	    current = 0; //- this won't connect with next node
	  } break;
	  case mReturn: {
	    PgmStmt * n = new PgmStmt(par, AID(fsid,aid));

	    while(isspace(*cp)) ++cp;

            suco_set<AO *>& raos = (suco_set<AO *>&) AO::stringToAOlist(true, cp, &cp, is.aidmap, is.pidmap, is.values);
            suco_llist<AO *>& restr_aos = AO::stringToAOlist(false, cp, &cp, is.aidmap, is.pidmap, is.values);
            while(*cp && isspace(*cp)) cp++;
            char * restr = ExpDescr::stringToEstr(cp, &cp);

	    n->annot.kind = Annot::aReturn;
            n->annot.u.edesc = new ExpDescr(restr_aos, restr, raos);

	    if(current) current->addSuccessor(*n);
	    nodelist.Append(n);
	    current = 0; //- this won't connect with next node
	  } break;
	  case mVReturn: {
	    PgmStmt * n = new PgmStmt(par, AID(fsid,aid));
	    n->annot.kind = Annot::aReturn;
	    if(current) current->addSuccessor(*n);
	    nodelist.Append(n);
	    current = 0; //- this won't connect with next node
	  } break;
	  case mCase: {
	    PgmStmt * n = new PgmStmt(par, AID(fsid,aid));
	    n->annot.kind = Annot::aCaseLabel;
	    n->annot.u.integer = strtol(cp, &cp, 10);
	    if(current) current->addSuccessor(*n);
	    nodelist.Append(n);
	    current = n;
	  } break;
	  case mDefault: {
	    PgmStmt * n = new PgmStmt(par, AID(fsid,aid));
	    n->annot.kind = Annot::aDefaultLabel;
	    if(current) current->addSuccessor(*n);
	    nodelist.Append(n);
	    current = n;
	  } break;
	  default: {
	    fprintf(stderr, "PgmStmt::readentry: bad jump marker on line %s", buf);
	    return false;
	  } break;
	}
      } break;
      default: {
	return false;
      }
    }
  } else {
    fprintf(stderr, "PgmStmt::readentry: bad prefix on line %s", buf);
    return false;
  }
  return true;
}

CFGfunction& CFGfunction::read(InputState& is, char * buf, int filestem_id)
{
  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);

  int aidno = 0;
  AOId * ao = 0;

  //-- process function start header: read pid/aid
  if(buf[0] == mPfx && buf[2] == mStart && buf[4] == mFunction){
    char * cp = &buf[6];
    int pidno = strtoul(cp, &cp, 10);
    ID * pid = is.pidmap.lookup(pidno);
    if(pid) ao = &pid->get_AOId();
    else fprintf(stderr, "Invalid Pid (%d) read for CFG function\n", pidno);
    aidno = strtoul(cp, &cp, 10);
  } else fprintf(stderr, "CFGfunction::read: bad start marker on line %s", buf);

  if(!ao) exit(fprintf(stderr, "Bad function id AO, aborting\n"));

  CFGfunction * cfn = new CFGfunction(*ao, AID(filestem_id,aidno));
  PgmStmt * current = 0;

  //-- process formals
  {
    buf[0] = 0;
    PgmStmt& cfgn = PgmStmt::read(is, *cfn, buf);
    cfn->cfglist.Insert(&cfgn);
    current = &cfgn;
  }

  //-- process function next (assert)
  {
    fgets(buf, TCA_BUF_SIZE, is.inf);
    if(buf[0] != mPfx || buf[2] != mNext || buf[4] != mFunction)
      fprintf(stderr, "CFGfunction::read: next assertion failed on line %s", buf);
  }

  //-- process body, until function end
  fgets(buf, TCA_BUF_SIZE, is.inf);
  while(!feof(is.inf) && (buf[0] != mPfx || buf[2] != mEnd || buf[4] != mFunction)){
    PgmStmt::readentry(is, *cfn, buf, current, cfn->cfglist);
    fgets(buf, TCA_BUF_SIZE, is.inf);
  }

  //-- find goto and return nodes
  suco_llist<PgmStmt *> gotonodes;
  suco_iterator<PgmStmt *> cni(cfn->cfglist);
  while(cni.Iterate()){
    PgmStmt * n = cni.Current();
    if(n->isReturn()) cfn->exitnodes.Append(n);
    if(n->isGoto()) gotonodes.Append(n);
  }

  //-- connect goto nodes
  if(!gotonodes.IsEmpty()){
    suco_llist<PgmStmt *> labelcache;
    suco_iterator<PgmStmt *> gni(gotonodes);
    while(gni.Iterate()){
      PgmStmt * gn = gni.Current();
      const char * glabel = gn->getGotoLabel();
      if(labelcache.IsEmpty()){ //- collect labeled nodes
        suco_iterator<PgmStmt *> ani(cfn->cfglist);
        while(ani.Iterate())
          if(ani.Current()->hasLabel())
            labelcache.Append(ani.Current());
      }
      suco_iterator<PgmStmt *> lni(labelcache);
      while(lni.Iterate())
        if(lni.Current()->hasLabel(glabel))
          gn->addSuccessor(*lni.Current()); //- in goto case, we don't initPredsCount
    }
  }

  //- if current is non-null and not already marked as a return node
  //  create dummy return node, add to exitnodes
  if(current && !current->isReturn()){
    PgmStmt * retn = new PgmStmt(*cfn);
    retn->annot.kind = PgmStmt::Annot::aReturn;

    current->addSuccessor(*retn);

    cfn->exitnodes.Insert(retn);
    cfn->cfglist.Append(retn);
  }

  //- assign entrystmt
  cfn->entrystmt = cfn->cfglist.Head();

  return *cfn;
}

void CFG::read(InputState& is, char * buf, const char * filestem)
{
  if(!buf[0]) fgets(buf, TCA_BUF_SIZE, is.inf);

  //-- add entry to filestemlist
  AID::filestemlist.Append(filestem);
  int filestem_id = AID::filestemlist.Length();

  while(!feof(is.inf)){
    if(buf[2] == mStart && buf[4] == mFunction) {
      CFGfunction& cfn = CFGfunction::read(is, buf, filestem_id);
      this->fnlist.Append(&cfn);

      //-- identify "main"
      if(!strcmp(cfn.idao.getPid().getname(), ".main")){
        //-- set mainfn pointer
        this->mainfn = &cfn;
      }
    } else {
      PgmExpr * df = PgmExpr::read(is, buf, 0);
      if(df) this->globnodes.Append(df);
      else fprintf(stderr, "CFG::read: error reading global decl: %s", buf);
    }
    fgets(buf, TCA_BUF_SIZE, is.inf);
  }
}

//----------------------------------
// END READ FUNCTIONS
//----------------------------------

//----------------------------------
// DEBUG DUMP FUNCTIONS
//----------------------------------

static const char * getIndent(int indent)
{
  static const char space_buffer[] = "                                                  ";
  static const char * spaces = &space_buffer[sizeof(space_buffer)-1];

  if(indent > (int) sizeof(space_buffer) - 1) indent = sizeof(space_buffer) - 1;

  return spaces-indent;
}

//- if indent == 0, then fit on one line
void ExpDescr::debug_dump(FILE * outf, int indent)
{
  const char * ispaces = getIndent(indent);
  const char * delim = indent?"\n":"  ";
  fprintf(outf, "%sAOs: ", ispaces);
  AO::write_list_string_rep(outf, this->aos, true);
  fprintf(outf, delim);
  fprintf(outf, "%saffAOs: ", ispaces);
  AO::write_list_string_rep(outf, this->estr_aos, true);
  fprintf(outf, delim);
  fprintf(outf, "%sestr: %s", ispaces, this->estr?this->estr:"@");
  fprintf(outf, delim);
}

void PExprCall::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);

  fprintf(outf, "%s--{ Expr %s", ispaces, getKindString());
  if(this->isDirectCall()) fprintf(outf, " (direct)");
  if(this->isRecursiveCall()) fprintf(outf, " (recursive)");
  if(widen_locs){
    fprintf(outf, " widen");
    if(widen_locs != CFGnode::ALL_AOS){
      fprintf(outf, "(%d/%d)", widen_locs->numAOs(), widen_locs->Size());
    }
  }
  fprintf(outf, " (aid=<%d,%d>, nargs=%d)\n", aid.filestem_id, aid.aid, nargs);

  fprintf(outf, "%s    FnAOs: ", ispaces);
  AO::write_list_string_rep(outf, faos, true);
  fprintf(outf, "\n");
  
  fprintf(outf, "%s    TargetFns:", ispaces);
  suco_iterator<CFGfunction *> tfi(targetfns);
  while(tfi.Iterate()){
    fprintf(outf, " ");
    tfi.Current()->getId().write_string_rep(outf, true);
  }
  fprintf(outf, "\n");

  fprintf(outf, "%s    Undef TargetFns:", ispaces);
  suco_iterator<AOId *> utfi(undef_tgtfns);
  while(utfi.Iterate()){
    fprintf(outf, " ");
    utfi.Current()->write_string_rep(outf, true);
  }
  fprintf(outf, "\n");

  for(int i = 0; i < nargs; ++i){
    fprintf(outf, "%s    Arg %d", ispaces, i+1);
    if(args[i]){
      fprintf(outf, " (aid=<%d,%d>):\n", args[i]->getAid().filestem_id, args[i]->getAid().aid);
      args[i]->getDesc().debug_dump(outf, indent+8);
    }
  }

  if(!brief){
    fprintf(outf, "%s    RDAfact: ", ispaces);
    rda_fact.debug_dump(outf, false);

    fprintf(outf, "%s    RDAlocal: ", ispaces);
    rda_local.debug_dump(outf, false);

    fprintf(outf, "%s    MBUfact: ", ispaces);
    mbu_fact.debug_dump(outf, false);

    fprintf(outf, "%s    REDfact: ", ispaces);
    red_fact.debug_dump(outf, false);

    fprintf(outf, "%s    RANfact: ", ispaces);
    ran_fact.debug_dump(outf, false);
  }

  fprintf(outf, "%s--}\n", ispaces);
}

void PExprAssign::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);

  fprintf(outf, "%s--{ Expr %s (aid=<%d,%d>)\n",
		ispaces, getKindString(), aid.filestem_id, aid.aid);
  fprintf(outf, "%s    e1:\n", ispaces);
  e1desc.debug_dump(outf, indent+8);
  fprintf(outf, "%s    e2:\n", ispaces);
  e2desc.debug_dump(outf, indent+8);
  fprintf(outf, "%s--}\n", ispaces);
}

void PExprPredicate::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);

  fprintf(outf, "%s--{ Expr %s / %s (aid=<%d,%d>)\n",
		ispaces, getKindString(), getPrKindString(), aid.filestem_id, aid.aid);
  edesc.debug_dump(outf, indent+8);
  fprintf(outf, "%s--}\n", ispaces);
}

void PExprDecl::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);

  fprintf(outf, "%s--{ Expr %s / %s (", ispaces, getKindString(), getDkindString());
  if(dkind == dFormal)
    fprintf(outf, "argno=%d,", argno);
  fprintf(outf, " ao = ");
  ao.write_string_rep(outf, true);
  fprintf(outf, ") ");
  if(iszeroed)
    fprintf(outf, "iszeroed ");
  if(dkind == dMalloc && mallocsize){
    fprintf(outf, "size =\n", argno);
    mallocsize->debug_dump(outf, indent+8);
  fprintf(outf, "%s  ", ispaces);
  }
  fprintf(outf, "}--\n");
}

void PExprVerify::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);

  fprintf(outf, "%s--{ Expr %s%s%s (aid=<%d,%d>)\n",
		ispaces, getKindString(), getVtKindString(), getVpKindString(),
		aid.filestem_id, aid.aid);
  desc.debug_dump(outf, indent+4);
  fprintf(outf, "%s--}\n", ispaces);
}

void PExprParallel::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);
  fprintf(outf, "%s--{ Parallel:\n", ispaces);

  suco_iterator<suco_llist<PgmExpr *> *> flsi(nodelists);
  while(flsi.Iterate()){
    fprintf(outf, "%s--: NEXT :\n", ispaces);
    suco_iterator<PgmExpr *> fli(*flsi.Current());
    while(fli.Iterate()) fli.Current()->debug_dump(outf, indent+2);
  }

  fprintf(outf, "%s--} end Parallel\n", ispaces);
}

void PExprBranch::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);
  fprintf(outf, "%s--{ Branch:\n", ispaces);

  fprintf(outf, "%s--: PREDICATE :\n", ispaces);
  if(prednode) prednode->debug_dump(outf, indent+2);

  fprintf(outf, "%s--: TRUE :\n", ispaces);
  suco_iterator<PgmExpr *> tfi(tnodelist);
  while(tfi.Iterate()) tfi.Current()->debug_dump(outf, indent+2);

  fprintf(outf, "%s--: FALSE :\n", ispaces);
  suco_iterator<PgmExpr *> ffi(fnodelist);
  while(ffi.Iterate()) ffi.Current()->debug_dump(outf, indent+2);

  fprintf(outf, "%s--} end Branch\n", ispaces);
}

void PgmStmt::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);
  fprintf(outf, "%s--{ Stmt %d/%d (aid=<%d,%d>)%s",
		ispaces, parent.getCallGraphId(), traverse_id,
		aid.filestem_id, aid.aid, hascall?" hascall ":"");
  if(getNsuccs()){
    fprintf(outf, "(succ =");
    for(int i = 0; getSuccStmt(i); ++i){
      fprintf(outf, " %d", getSuccStmt(i)->traverse_id);
      int bndx = (i < 2)?i:0;
      if(backedge_locs[bndx]){
        fprintf(outf, "bk");
        if(backedge_locs[bndx] != CFGnode::ALL_AOS){
          fprintf(outf, "(%d/%d)", backedge_locs[bndx]->numAOs(), backedge_locs[bndx]->Size());
        }
      }
    }
    fprintf(outf, " )");
  }
  if(getNpreds()){
    fprintf(outf, "(pred =");
    for(int i = 0; getPred(i); ++i)
      fprintf(outf, " %d", getPredStmt(i)->traverse_id);
    fprintf(outf, " )");
  }
  if(nlabels){
    fprintf(outf, "(labels =");
    for(int i = 0; i < nlabels; ++i)
      fprintf(outf, " %s", labels[i]);
    fprintf(outf, " )");
  }
  switch(annot.kind){
    case Annot::aGoto: {
      fprintf(outf, "(GOTO %s)", (annot.u.string)?(annot.u.string):"null!");
    } break;
    case Annot::aBreak: {
      fprintf(outf, "(BREAK)");
    } break;
    case Annot::aContinue: {
      fprintf(outf, "(CONTINUE)");
    } break;
    case Annot::aReturn: {
      if(annot.u.edesc){
        fprintf(outf, "(RETURN ");
        annot.u.edesc->debug_dump(outf, 0);
        fprintf(outf, ")");
      } else {
        fprintf(outf, "(RETURN)");
      }
    } break;
    case Annot::aWhile: {
      fprintf(outf, "(WHILE)");
    } break;
    case Annot::aDoWhile: {
      fprintf(outf, "(DOWHILE)");
    } break;
    case Annot::aFor: {
      fprintf(outf, "(FOR)");
    } break;
    case Annot::aIf: {
      fprintf(outf, "(IF)");
    } break;
    case Annot::aSwitch: {
      fprintf(outf, "(SWITCH)");
    } break;
    case Annot::aCaseLabel: {
      fprintf(outf, "(CASE %d)", annot.u.integer);
    } break;
    case Annot::aDefaultLabel: {
      fprintf(outf, "(DEFAULT)");
    } break;
    case Annot::aOutNode: {
      fprintf(outf, "(OUTNODE)");
    } break;

    default: {
    } break;
  }

  if(brief){

    fprintf(outf, " }--\n");

  } else {

    fprintf(outf, ":\n");

    suco_iterator<PgmExpr *> fli(this->exprlist);
    while(fli.Iterate()){
      fli.Current()->debug_dump(outf, indent+2);
    }

    fprintf(outf, "%s -- RDAfact: ", ispaces);
    rda_fact.debug_dump(outf, false);

    fprintf(outf, "%s -- MBUfact: ", ispaces);
    mbu_fact.debug_dump(outf, false);

    fprintf(outf, "%s -- REDfact: ", ispaces);
    red_fact.debug_dump(outf, false);

    fprintf(outf, "%s -- RANfact1: ", ispaces);
    ran_fact_pair.getFact1().debug_dump(outf, false);
    if(ran_fact_pair.getFact2()){
      fprintf(outf, "%s -- RANfact2: ", ispaces);
      ran_fact_pair.getFact2()->debug_dump(outf, false);
    }

    fprintf(outf, "%s--} end Stmt %d/%d (aid=<%d,%d>)\n",
		  ispaces, parent.getCallGraphId(), traverse_id, aid.filestem_id, aid.aid);
  }
}

void CFGbblock::debug_dump(FILE * outf, int indent, bool brief)
{
  const char * ispaces = getIndent(indent);
  fprintf(outf, "%s--{ BBlock %d/%d%s", ispaces, parent.getCallGraphId(), traverse_id,
					hascall?" hascall":"");
  if(getNsuccs()){
    fprintf(outf, " (succ =");
    for(int i = 0; getSucc(i); ++i){
      fprintf(outf, " %d", ((CFGbblock*)getSucc(i))->traverse_id);
      int bndx = (i < 2)?i:0;
      if(backedge_locs[bndx]){
        fprintf(outf, "bk");
        if(backedge_locs[bndx] != CFGnode::ALL_AOS)
          fprintf(outf, "(%d/%d)", backedge_locs[bndx]->numAOs(), backedge_locs[bndx]->Size());
      }
    }
    fprintf(outf, " )");
  }
  if(getNpreds()){
    fprintf(outf, " (pred =");
    for(int i = 0; getPred(i); ++i)
      fprintf(outf, " %d", ((CFGbblock*)getPred(i))->traverse_id);
    fprintf(outf, " )");
  }
  fprintf(outf, " #stmts = %d", stmtlist.Length());
  if(brief || stmtlist.IsEmpty()){
    fprintf(outf, " }--\n");
  } else {
    fprintf(outf, ":\n");

    suco_iterator<PgmStmt *> psi(stmtlist);
    while(psi.Iterate()){
      psi.Current()->debug_dump(outf, indent+2);
    }

    fprintf(outf, "%s -- RDAfact: ", ispaces);
    rda_fact.debug_dump(outf, false);

    fprintf(outf, "%s -- MBUfact: ", ispaces);
    mbu_fact.debug_dump(outf, false);

    fprintf(outf, "%s -- REDfact: ", ispaces);
    red_fact.debug_dump(outf, false);

    fprintf(outf, "%s -- RANfact1: ", ispaces);
    ran_fact_pair.getFact1().debug_dump(outf, false);
    if(ran_fact_pair.getFact2()){
      fprintf(outf, "%s -- RANfact2: ", ispaces);
      ran_fact_pair.getFact2()->debug_dump(outf, false);
    }

    fprintf(outf, "%s--} end BBlock %d/%d\n", ispaces, parent.getCallGraphId(), traverse_id);
  }
}

void CFGfunction::debug_dump(FILE * outf, bool brief)
{
  fprintf(outf, " --{ Function (ao = ");
  idao.write_string_rep(outf, true);
  fprintf(outf, ", aid=<%d,%d>, entry=%d, exit=%d, callgraph_id=%d, recursion_id=%d):\n",
			aid.filestem_id, aid.aid, entrystmt->getAid().aid, exitnodes.Length(),
			getCallGraphId(), getRecursionId());
  if(!brief){
    fprintf(outf, "     LocalVars: ");
    AO::write_list_string_rep(outf, (suco_set<AO *>&)localvars, true);
    fprintf(outf, "\n");

    fprintf(outf, "     GMOD: ");
    this->gmod.debug_dump(outf);

    fprintf(outf, "     GREF: ");
    this->gref.debug_dump(outf);

    if(flag_compute_iref){
      fprintf(outf, "     IREF: ");
      this->iref.debug_dump(outf);
    }

    fprintf(outf, "     GFREE-heap: ");
    suco_iterator<ECR *> gfhi(this->gfreeheap_ecrs);
    while(gfhi.Iterate())
      AO::write_list_string_rep(outf, gfhi.Current()->getAOset(), true);
    fprintf(outf, "\n");

    fprintf(outf, "     GFREE-stack: ");
    AO::write_list_string_rep(outf, gfreestack, true);
    fprintf(outf, "\n");

    fprintf(outf, "   { Callees:\n");
    suco_iterator<CFGfunction *> cgi(callgraph);
    while(cgi.Iterate()){
      fprintf(outf, "      %s\n", cgi.Current()->getId().getPid().getname());
    }
    fprintf(outf, "   } end callees\n");

    fprintf(outf, "   { Callsites:\n");
    suco_iterator<PExprCall *> csi(callsites);
    while(csi.Iterate()){
      csi.Current()->debug_dump(outf, 4, true);
    }
    fprintf(outf, "   } end callsites\n");

    if(flag_use_bblocks){
      suco_iterator<CFGbblock *> bbi(bblocks);
      while(bbi.Iterate())
        bbi.Current()->debug_dump(outf, 2);
    } else {
      suco_iterator<PgmStmt *> nli(cfglist);
      while(nli.Iterate())
        nli.Current()->debug_dump(outf, 2);
    }
  }

  fprintf(outf, " --} end Function (aid=<%d,%d>)\n", aid.filestem_id, aid.aid);
}

void CFG::debug_dump(FILE * outf)
{
  fprintf(outf, "--{ CFG (%d functions):\n", fnlist.Length());
  if(entry){
    fprintf(outf, "    Entry Node:\n");
    entry->debug_dump(outf, 4);
  }
  suco_iterator<CFGfunction *> fli(fnlist);
  while(fli.Iterate())
    fli.Current()->debug_dump(outf);
  fprintf(outf, "--} end CFG (%d functions)\n", fnlist.Length());
}

//----------------------------------
// END DEBUG DUMP FUNCTIONS
//----------------------------------

//----------------------------------
// WRITE DFA RESULTS
//----------------------------------

//----------------------------------
// END WRITE MBU RESULTS
//----------------------------------

void CFG::remapRecursionId()
{
  suco_llist<CFGfunction *> highest_id_recurse_list;

  //- 1. find for each recursion_id the node with the highest callgraph_id
  suco_iterator<CFGfunction *> fni(this->fnlist);
  while(fni.Iterate()){
    CFGfunction& cfn = *fni.Current();
    if(cfn.recursion_id){
      bool found = false;
      suco_iterator<CFGfunction *> hirli(highest_id_recurse_list);
      while(hirli.Iterate()){
        if(hirli.Current()->getRecursionId() == cfn.getRecursionId()){
          found = true;
          if(hirli.Current()->getCallGraphId() < cfn.getCallGraphId()){
            hirli.DeleteCurrent();
            highest_id_recurse_list.Insert(&cfn);
          }
          break;
        }
      }
      if(!found) highest_id_recurse_list.Insert(&cfn);
    }
  }
  if(!highest_id_recurse_list.IsEmpty()){
    //- 2. Renumber each recursion_id with the highest callgraph_id
    //- 2a. first skip the "representative nodes"
    fni.Rewind();
    while(fni.Iterate()){
      CFGfunction& cfn = *fni.Current();
      if(cfn.recursion_id){
        suco_iterator<CFGfunction *> hirli(highest_id_recurse_list);
        while(hirli.Iterate()){
          if(&cfn != hirli.Current() &&
		cfn.recursion_id == hirli.Current()->recursion_id){
            cfn.recursion_id = hirli.Current()->getCallGraphId();
            break;
          }
        }
      }
    }
    //- 2b. final pass: renumber the "representative nodes"
    {
      suco_iterator<CFGfunction *> hirli(highest_id_recurse_list);
      while(hirli.Iterate()){
        hirli.Current()->recursion_id = hirli.Current()->getCallGraphId();
      }
    }
  }
}

void CFG::prepareSuperGraph()
{
  connectSuperGraph();

  if(flag_verbose) TCstats::timer("traversing callgraph");
  this->mainfn->traverseCallGraph(0);
  if(flag_verbose) TCstats::timer(0);

  if(flag_verbose) TCstats::timer("remapping recursion id");
  remapRecursionId();
  if(flag_verbose) TCstats::timer(0);
}

void CFG::prepareCFG(bool prepare_widen)
{
  if(this->prepared){
    return; //- already prepared
  }
  this->prepared = true;

  if(!this->mainfn){
    fprintf(stderr, "FATAL ERROR: CFG has no main function\n");
    return;
  }

  collectLocalsAndIMODREF();
  prepareSuperGraph();
  collectGMODREF();

  constructBBlocks();
  assignIds(); //- note: also marks backedges

  //- compute (intraprocedural) loop backedge filters
  if(prepare_widen && flag_range_filter_loop_backedge)
    computeBackedgeFilters();

  //- compute widen_locs for (recursive) call nodes
  if(prepare_widen){
    TCstats::timer("finding recursive widen-ecrs");
    suco_iterator<CFGfunction *> fni(this->fnlist);
    while(fni.Iterate()){
      if(fni.Current()->getRecursionId()){
        suco_iterator<PgmStmt *> cni(fni.Current()->cfglist);
        while(cni.Iterate()){
          cni.Current()->traverseRootNodes(CFGnode::computeWidenAOs);
        }
      }
    }
    TCstats::timer(0);
  }

  //-- create new empty CFG node, representing the entry
  //   point to entire CFG
  // - instantiate them with globnodes!
  {
    PgmStmt * entrystmt = new PgmStmt(*mainfn);
    entrystmt->exprlist.Copy(globnodes);	//- (ab)use friendship to initialize exprlist

    if(flag_use_bblocks){
      suco_set<PgmStmt *> dummy;
      this->entry = new CFGbblock(*entrystmt, dummy);
      if(!dummy.IsEmpty()) //- assert
        fprintf(stderr, "ERROR(CFG::prepareCFG): new entry bblock - pendingnodes not empty\n");
    } else {
      this->entry = entrystmt;
    }
  }

  if(mainfn->getEntryNode()){

    this->entry->addSuccessor(*mainfn->getEntryNode());

    if(prepare_widen){
      //- identify remaining backedges in supergraph (due to false loops from context-insensitivity)
      TCstats::timer("finding remaining backedges");
      mainfn->getEntryNode()->findRemainingBackedges();
      TCstats::timer(0);
    }

  } else fprintf(stderr, "Main function has no entry node!\n");
}

//----------------------------------
// TRAVERSE ROOT PGMEXPR NODES
//----------------------------------

bool PgmExpr::traverseRootNodes(bool (*fp)(PgmExpr& dn))
{
  bool alive = true;
  switch(kind){
    case fParallel: { //- recurse
        suco_iterator<suco_llist<PgmExpr *> *> fli(((PExprParallel *)this)->nodelists);
        while(alive && fli.Iterate()){
          suco_iterator<PgmExpr *> dfi(*fli.Current());
          while(alive && dfi.Iterate()){
            alive &= dfi.Current()->traverseRootNodes(fp);
          }
        }
      } break;
    case fBranch: { //- recurse
        suco_iterator<PgmExpr *> tfi(((PExprBranch *)this)->tnodelist);
        while(alive && tfi.Iterate())
          alive &= tfi.Current()->traverseRootNodes(fp);
        suco_iterator<PgmExpr *> ffi(((PExprBranch *)this)->fnodelist);
        while(alive && ffi.Iterate())
          alive &= ffi.Current()->traverseRootNodes(fp);
      } break;
    case fCall:
    case fAssign:
    case fPredicate:
    case fDecl:
    case fVerify:
    default : {
        alive &= fp(*this);
      } break;
  }
  return alive;
}

bool PgmStmt::traverseRootNodes(bool (*fp)(PgmExpr& dn))
{
  bool alive = true;
  suco_iterator<PgmExpr *> nli(this->exprlist);
  while(alive && nli.Iterate())
    alive &= nli.Current()->traverseRootNodes(fp);
  return alive;
}

bool CFGbblock::traverseRootNodes(bool (*fp)(PgmExpr& dn))
{
  bool alive = true;
  suco_iterator<PgmStmt *> nli(this->stmtlist);
  while(alive && nli.Iterate())
    alive &= nli.Current()->traverseRootNodes(fp);
  return alive;
}

bool CFG::traverseRootNodes(bool (*fp)(PgmExpr& dn))
{
  bool alive = true;
  //- FIRST: traverse entry node
  if(this->getEntryNode()){
    alive &= this->entry->traverseRootNodes(fp);
  }
  //- now, traverse each function's statement list
  suco_iterator<CFGfunction *> fni(this->getFunctionList());
  while(alive && fni.Iterate()){
    suco_iterator<PgmStmt *> psi(fni.Current()->getStmtList());
    while(alive && psi.Iterate()){
      alive &= psi.Current()->traverseRootNodes(fp);
    }
  }
  return alive;
}

//----------------------------------
// CFG CONNECT CALL/FN NODES (SUPERGRAPH)
//----------------------------------

PExprCall * CFG::act_caller_node = 0;
suco_llist<CFGfunction *> * CFG::act_fnlist = 0;

bool CFG::addAOtoCallTarget(AO& ao)
{
  if(ao.getKind() == AO::aoFunction){
    if(CFG::act_fnlist && CFG::act_caller_node){ // assertion
      AO& idao = ((AOFunction&)ao).getTarget();
      if(idao.getKind() == AO::aoId){ // more assert
        suco_iterator<CFGfunction *> fli(*CFG::act_fnlist);
        while(fli.Iterate()){
          if(&fli.Current()->idao == (AOId *)&idao){
            //-- CFG: connect caller node to callsite
            CFG::act_caller_node->targetfns.Insert(fli.Current());
            fli.Current()->callsites.Insert(CFG::act_caller_node);

            //-- callgraph: add callee to caller's enclosing function
            //   Note: caller->getParentNode already validated to be non-null during input
            CFG::act_caller_node->getParentNode()->getParentFunction().callgraph.Insert(fli.Current());

            return true; //- true ensures full traversal of ECRs
          }
        }
        //- call to undefined function (library)
        CFG::act_caller_node->undef_tgtfns.Insert((AOId *) &idao);
      }
    }
  }
  return true; //- true ensures full traversal of ECRs
}

bool CFG::assignCallTargets(PgmExpr& dn)
{
  if(dn.kind == PgmExpr::fCall){
    CFG::act_caller_node = (PExprCall *)&dn;
    suco_iterator<AO *> faoi(CFG::act_caller_node->faos);
    while(faoi.Iterate()){
      faoi.Current()->getECR().getPointsTo().traverseAliases(addAOtoCallTarget);
    }
  }
  return true; //- true ensures full traversal
}

void CFG::connectSuperGraph()
{
  if(flag_verbose) TCstats::timer("connecting supergraph (call/return assignments)");

  CFG::act_fnlist = &this->fnlist;
  suco_iterator<CFGfunction *> fni(this->fnlist);
  while(fni.Iterate()){
    CFGfunction * fn = fni.Current();
    suco_iterator<PgmStmt *> cni(fn->cfglist);
    while(cni.Iterate()){
      cni.Current()->traverseRootNodes(CFG::assignCallTargets);
    }
  }
  CFG::act_fnlist = 0;
  if(flag_verbose) TCstats::timer(0);
}

//----------------------------------
// REPORT IF CALLNODE CALLS FREE()
//----------------------------------

bool PExprCall::callsFree()
{
  //-- check direct call
  AO * fao = this->faos.Head();
  if(fao && fao->getKind() == AO::aoId &&
	!strcmp(((AOId *)fao)->getPid().getname(), ".free")){
    return true;
  }

  //-- check indirect call
  suco_iterator<AOId *> uti(this->undef_tgtfns);
  while(uti.Iterate()){
    if(!strcmp(uti.Current()->getPid().getname(), ".free"))
      return true;
  }
  return false;
}

ID * PExprCall::isDirectCall()
{
  if(this->getFaos().Length() == 1){
    if((this->getTargetFns().Length() == 1)
	     && (&this->getTargetFns().Head()->getId() == this->getFaos().Head())){
      return &this->getTargetFns().Head()->getId().getPid();
    }
    if((this->getUndefTargetFns().Length() == 1)
	     && (this->getUndefTargetFns().Head() == this->getFaos().Head())){
      return &this->getUndefTargetFns().Head()->getPid();
    }
  }
  return 0;
}

bool PExprCall::isRecursiveCall()
{
  int parent_recurs_id = (this->getParentNode())
			 ? (this->getParentNode()->getParentFunction().getRecursionId())
			 : 0;
  if(parent_recurs_id){
    suco_iterator<CFGfunction *> tfi(this->getTargetFns());
    while(tfi.Iterate()){
      if(tfi.Current()->getRecursionId() == parent_recurs_id){
        return true;
      }
    }
  }
  return false;
}

//--------------------------------------
//- GREF/GMOD accessor functions

//- returns true if ao points to something in this->gfreeheap
//  (i.e. ao points to ecr that intersects with this->gfreeheap_ecrs)
bool CFGfunction::GFreeHeapAffects(AO& ao)
{
  return ao.getECR().getPointsTo().getAliasECRs().Intersects(this->getGFreeHeap_ecrs());
}

//----------------------------------
// TRAVERSE CALL GRAPH
//----------------------------------

//- traversal occurs in 2 modes:
//  1. recursing=false:
//     a. reach an inactive node
//        -> process childen recursing=false
//     b. reach a visited node
//        -> cross-edge, stop
//     c. reach an active node
//        i. node is marked recursive
//           -> return that recursion_id
//              *propagate it backwards until reaching
//               a node already marked recursive
//        ii. node is not marked recursive
//            -> switch to recursing=true, process that node
//  2. recursing=true:
//     a. reach an inactive node
//        -> stop
//     b. reach a visited node
//        -> stop
//     c. reach an active node
//        i. node is marked recursive
//            -> stop
//        ii. node is not marked recursive
//            -> mark it recursive, continue processing
//               children with recursing=true
//
int CFGfunction::traverseCallGraph(int recursing)
{
  static int callgraph_counter = 0;
  static int recursing_counter = 0;
  
  if(this->callgraph_id == 0){ //-- 0 = inactive
    if(recursing){ //- (2.a.) if recursing, stop traversal
      return 0;
    } else { //- (1.a.)
      this->callgraph_id = -1; //: -1 = active
      suco_iterator<CFGfunction *> cgi(this->callgraph);
      int rid = 0;
      while(cgi.Iterate()){
        int lrid = cgi.Current()->traverseCallGraph(0);
        if(lrid) rid = lrid;
      }
      callgraph_counter++; //- depth-first numbering
      this->callgraph_id = callgraph_counter;
      if(rid && !this->recursion_id){ //- (1.c.i.*)
        this->recursion_id = rid;
        return rid; //- propagate backwards
      } else {
        return 0;
      }
    }
  } else if(this->callgraph_id == -1){ //-- -1 = active
    if(this->recursion_id){ //- (1.c.i., 2.c.i.) if already recursive, stop traversal
      return (recursing)?(0):(this->recursion_id);
    } else { //- (1.c.ii., 2.c.ii.) traverse actives, set recursion_id
      if(!recursing){ //- (1.c.ii.)
        recursing_counter++;
        recursing = recursing_counter;
      }
      this->recursion_id = recursing;
      suco_iterator<CFGfunction *> cgi(this->callgraph);
      while(cgi.Iterate()){
        cgi.Current()->traverseCallGraph(this->recursion_id);
      }
      return 0;
    }
  } else { //--(1.b., 2.b.) positive=visited
    return 0;
  }
}

//----------------------------------
// CONSTRUCT BASIC BLOCKS
//----------------------------------

bool PgmExpr::notFunctionCall(PgmExpr& dn)
{
  return (dn.getKind() != PgmExpr::fCall);
}

bool PgmStmt::hasFunctionCalls()
{
  this->hascall = !this->traverseRootNodes(PgmExpr::notFunctionCall);
  return this->hascall;
}

CFGbblock::CFGbblock(PgmStmt& headnode, suco_set<PgmStmt *>& pendingnodes)
: CFGnode(headnode.getParentFunction()),
  stmtlist()
{
  this->stmtlist.Append(&headnode);
  headnode.bblock = this;
  this->hascall = headnode.hasFunctionCalls();

  PgmStmt * cnode = &headnode;

  if(!this->hascall){
    while(cnode->countSuccs() == 1		  //- only one successor
	  && cnode->getSuccStmt(0)->countPreds() == 1  //- successor has only one predecessor
	  && !cnode->getSuccStmt(0)->hasFunctionCalls()) //- successor has no function calls
    {
      //- add successor to this basic block
      cnode = cnode->getSuccStmt(0);
      this->stmtlist.Append(cnode);
      cnode->bblock = this;
    }
  }
  //- add unassigned successors to pendingnodes
  for(int i = 0; cnode->getSuccStmt(i); ++i)
    if(cnode->getSuccStmt(i) && !cnode->getSuccStmt(i)->bblock)
      pendingnodes.Insert(cnode->getSuccStmt(i));
}

void CFGfunction::constructBBlocks()
{
  suco_set<PgmStmt *> pendingnodes;

  if(!this->entrystmt){
    fprintf(stderr, "Entry node not assigned!\n");
    return;
  }

  //- consume entrystmt
  pendingnodes.Insert(this->entrystmt);

  //- construct and collect basic block nodes
  while(!pendingnodes.IsEmpty()){
    PgmStmt& cnode = *pendingnodes.RemoveHead();
    CFGbblock& newblk = *new CFGbblock(cnode, pendingnodes);
    this->bblocks.Append(&newblk);
    if(&cnode == this->entrystmt) //-- assign entryblock
      this->entryblock = &newblk;
  }

  int stat_numblocks = 0;
  int stat_numnodes = 0;
  int stat_maxnodes = 0;

  //-- connect basic blocks
  suco_iterator<CFGbblock *> bbi(this->bblocks);
  while(bbi.Iterate()){
    CFGbblock& bb = *bbi.Current();

    if(flag_verbose){
      stat_numblocks++;
      int nnodes = bb.stmtlist.Length();
      stat_numnodes += nnodes;
      if(stat_maxnodes < nnodes) stat_maxnodes = nnodes;
    }

    //-- preds
    {
      PgmStmt& hnode = *bb.stmtlist.Head();

      // - allocate preds array
      bb.npreds = hnode.countPreds();
      bb.preds = new CFGnode * [bb.npreds+1];

      // - lookup and assign
//-NOTE: lookup failure may be due to unreachable nodes (artifact
//       of my translation of program structure?), or from
//       _prog_main's "global entry" node.
//TODO: make that global entry node a basic block?
      int i = 0;
      for(int j = 0; hnode.getPred(j); ++j){
        if((bb.preds[i] = hnode.getPredStmt(j)->bblock) != 0)
          i++;
      }
      bb.preds[i] = 0;
    }

    //-- succs
    // - allocate succs array
    {
      PgmStmt& tnode = *bb.stmtlist.Last();

      // - allocate succs array
      bb.nsuccs = tnode.countSuccs();
      bb.succs = new CFGnode * [bb.nsuccs+1];

      // - lookup and assign
      int i = 0;
      for(int j = 0; tnode.getSuccStmt(j); ++j){
        if((bb.succs[i] = tnode.getSuccStmt(j)->bblock) != 0)
          i++;
      }
      bb.succs[i] = 0;
    }
  }
  if(flag_verbose > 2)
    fprintf(stderr, "\t%d\t%d\t%d\t%.2f\t%s\n",
		stat_numblocks, stat_numnodes, stat_maxnodes,
		(float)stat_numnodes/(float)stat_numblocks,
		this->getId().getPid().getname());
}

void CFG::constructBBlocks()
{
  if(flag_verbose){
    TCstats::timer("constructing basic blocks");
    if(flag_verbose > 2)
      fprintf(stderr, "(BLOCKS/NODES/MAX/AVG)\n");
  }

  suco_iterator<CFGfunction *> fni(this->fnlist);
  while(fni.Iterate())
    fni.Current()->constructBBlocks();

  if(flag_verbose) TCstats::timer(0);
}

//----------------------------------
// ASSIGN CFGNODE IDS
//----------------------------------

////////////////////////////////////
// Post-Order Depth-First numbering, with special loop handling
//-EXTRA: I've hi-jacked this traversal function to
//	  also mark backedges for widening/narrowing

void CFG::assignId(CFGnode& cnode)
{
  if(cnode.traverse_id > 0){

    return;

  } else if(cnode.traverse_id == -1){ //- backedge

    for(int i = 0; cnode.getSucc(i); ++i)
      if(!cnode.getSucc(i)->traverse_id)
        assignId(*cnode.getSucc(i));

  } else { //- new node

    cnode.traverse_id = -1; //- mark "visiting"

      /////////////////////////////////////////////
    { //- mark backedges to do widening/narrowing
      if(cnode.getNsuccs() == 2){ //- two-way branches
        if(cnode.getSucc(0) && cnode.getSucc(0)->traverse_id == -1)
          cnode.backedge_locs[0] = CFGnode::ALL_AOS;
        if(cnode.getSucc(1) && cnode.getSucc(1)->traverse_id == -1)
          cnode.backedge_locs[1] = CFGnode::ALL_AOS;
      } else { //- all other cases
        for(int i = 0; cnode.getSucc(i); ++i)
          if(cnode.getSucc(i)->traverse_id == -1){ //- if *ANY* successor is back-edge,	then mark backedge_locs[0]=true
            cnode.backedge_locs[0] = CFGnode::ALL_AOS;
            break;
          }
      }
    } /////////////////////////////////////////////

    for(int i = 0; cnode.getSucc(i); ++i)
      assignId(*cnode.getSucc(i));

    cnode.traverse_id = ++assignIds_counter;
  }
}

//- also marks backedges for widen/narrow (see comment above assignId()).
//- and also computes backedge filter sets
void CFG::assignIds()
{
  if(flag_verbose) TCstats::timer("assigning CFG ids (post-order depth-first)");

  suco_iterator<CFGfunction *> fni(this->getFunctionList());
  while(fni.Iterate()){
    //-- number both stmts and bblocks
    // - for now, use same counter (i.e., non-clashing ids)
    assignIds_counter = 0;
    if(fni.Current()->entrystmt)
      assignId(*fni.Current()->entrystmt);
    if(fni.Current()->entryblock)
      assignId(*fni.Current()->entryblock);
  }

  if(flag_verbose) TCstats::timer(0);
}

//----------------------------------
// COLLECT BACKEDGE FILTERS
//----------------------------------

//- SY NOTE: this function does not collect GFreeHeap info.
//  Since the result of this function is used only for a
//  backedge-widening filter, we don't need to worry about
//  MayFree effects, because that should only affect dataflow
//  facts finitely (right?).
bool CFG::collectMODecrs(PgmExpr& pe)
{
  switch(pe.getKind()){
    case PgmExpr::fCall:{
      suco_iterator<CFGfunction *> tfi(((PExprCall&)pe).getTargetFns());
      while(tfi.Iterate()){
        tfi.Current()->getGMOD().addToEcrSet(CFGfunction::clie_imod_ecrs);
      }
    } break;
    case PgmExpr::fAssign:{
      CFGfunction::collectIMODREF(((PExprAssign&)pe).getLHS().getAOs(), true);	// writes directly into CFGfunction::clie_imod_ecrs
    } break;
  }
  return true; //- true ensures full traversal
}

//- return true if OK, false if problem (i.e., if has goto or label)
bool CFG::collectModSetUntil(PgmStmt * exitnode, suco_iterator<PgmStmt *>& si, LocSet& modlocs)
{
  CFGfunction::clie_imod_ecrs.Clear();
  while(si.Iterate() && si.Current() != exitnode){
    PgmStmt& stmt = *si.Current();
    if(stmt.nlabels || (stmt.annot.kind == PgmStmt::Annot::aGoto)){
      return false;  //- has goto or label
    }
    //- collect mod set into clie_imod_ecrs
    stmt.traverseRootNodes(CFG::collectMODecrs);
  }
  modlocs.AbsorbConsume(CFGfunction::clie_imod_ecrs);
  CFGfunction::clie_imod_ecrs.Clear();
  return true;
}

void CFG::computeBackedgeFilters()
{
  TCstats::timer("computing backedge filters");
  suco_iterator<CFGfunction *> fni(this->getFunctionList());
  while(fni.Iterate()){

    //- iterate through sequential stmt list searching for structured loops

    suco_iterator<PgmStmt *> si(fni.Current()->getStmtList());
    while(si.Iterate()){
      PgmStmt& stmt = *si.Current();

      CFGnode * cnode = stmt.getCFGactiveNode();
      if(cnode){
        //- find loop-head signatures: 1. While
	//  [(p)while]--T->[loop head].../-->[loop exit]
	//            \-F---------------/
	if(stmt.annot.kind == PgmStmt::Annot::aWhile){
	  if(cnode->getNsuccs() == 2 && cnode->getSucc(1)){
	    LocSet modlocs;
	    suco_iterator<PgmStmt *> tsi(si);
	    tsi.Rewind(); //- must include current
	    if(collectModSetUntil(cnode->getSucc(1)->getStmtList().Head(), tsi, modlocs)){
	      //- find backedge(s)
	      for(int i = 0; cnode->getPred(i); ++i)
		cnode->getPred(i)->updateBackEdgeIfSet(*cnode, modlocs);
	    }
	  } else fprintf(stderr, "ERROR(CFG::computeBackedgeFilters): malformed while loop has %d successors\n", cnode->getNsuccs());
	}

	//- find loop-head signatures: 2. For
	//  [for]-->[pred]<--[incr] /-->[loop head].../-->[loop exit]
	//               \-T-------/                 /
	//                \-F-----------------------/
	if(stmt.annot.kind == PgmStmt::Annot::aFor){
	  if(si.Iterate()){
	    PgmStmt& predstmt = *si.Current();
	    //- TODO?: assert (pred in stmt.succ) and (succ in pred.pred)
	  
	    CFGnode * pnode = predstmt.getCFGactiveNode();
	    if(pnode){
	      if(pnode->getNsuccs() == 2 && pnode->getSucc(1)){
		LocSet modlocs;
		suco_iterator<PgmStmt *> tsi(si);
		tsi.Rewind(); //- must include current (pred)
		if(collectModSetUntil(pnode->getSucc(1)->getStmtList().Head(), tsi, modlocs)){
		  //- find backedge(s) (should be only one, from [incr] to [pred])
		  for(int i = 0; pnode->getPred(i); ++i)
		    pnode->getPred(i)->updateBackEdgeIfSet(*pnode, modlocs);
		}
	      } else fprintf(stderr, "ERROR(CFG::computeBackedgeFilters): malformed FOR predicate has %d successors\n", pnode->getNsuccs());
	    } //- !cnode: stmt has no bblock, must be unreachable
	  } else fprintf(stderr, "ERROR(CFG::computeBackedgeFilters): malformed FOR loop - misplaced predicate statement\n");
	}

	//- find loop-head signatures: 3. DoWhile
	//  [loop head]<--(bk)T--[(p)dowhile]--F-->[loop exit]
	for(int i = 0; cnode->getPred(i); ++i){
	  CFGnode& prednode = *cnode->getPred(i);
	  PgmStmt * predstmt = prednode.getStmtList().Last();
	  if(predstmt && predstmt->annot.kind == PgmStmt::Annot::aDoWhile && prednode.getSucc(0) == cnode){
	    PgmStmt * tailstmt = predstmt;
	    if(prednode.getNsuccs() == 2 && prednode.getSucc(1)){
	      tailstmt = prednode.getSucc(1)->getStmtList().Head();
	    } else fprintf(stderr, "ERROR(CFG::computeBackedgeFilters): malformed DOWHILE loop has %d successors\n", prednode.getNsuccs());
	    LocSet modlocs;
	    suco_iterator<PgmStmt *> tsi(si);
	    tsi.Rewind(); //- must include current (pred)
	    if(collectModSetUntil(tailstmt, tsi, modlocs)){
	      //- find backedge(s) (should be only one, from [dowhile] to [loop head]
	      prednode.updateBackEdgeIfSet(*cnode, modlocs);
	    }
	  }
	}
      } //- !cnode: stmt has no bblock, must be unreachable
    }
  }
  TCstats::timer(0);
}

//- - -

//------------------------------------------------------
// "Find Remaining Backedges"
//------------------------------------------------------
//- Called in CFG initialization cycle, to detect cycles
//  after per-procedure DFS search and recursion-check.
//  These would be faux-cycles due to context-insensitivity.
//- The traversal proceeds as follows:
//  - find_remaining_backedges_touched is the marker, initially all 0
//  - when traversing, set marker to -1; when done, set marker to 1
//  - three different kinds of nodes to handle:
//	- call node: traverse callee entry nodes, with entryfn=callee
//	- return node: traverse caller nodes' successors, with entryfn=return node's parent
//	- otherwise: traverse successors, with entryfn=0
//  - when loop encountered, i.e. if marker is already -1
//	- if entryfn is set, then must enable the backedges of
//	  all of entryfn's return nodes 
//	- if entryfn is not set, do nothing (because loop
//	  is an intra-procedural loop, already previously detected)
void CFGnode::findRemainingBackedges(CFGfunction * entryfn)
{
  //- 0=new, -1=active, 1=visited
  if(this->find_remaining_backedges_touched == 1){ //- visited: cross-edge, do nothing
    return;
  } else if(this->find_remaining_backedges_touched == -1){ //- active: back-edge
    if(entryfn){
      // enable backedges for all of entryfn's retnodes
/**/  bool debug_written = false;
      suco_iterator<PgmStmt *> eni(entryfn->getExitNodes());
      while(eni.Iterate()){
        CFGnode * exnode = eni.Current()->getCFGactiveNode();
        if(exnode){
/**/      if(flag_verbose && !debug_written && !exnode->backedge_locs[0]){
/**/        fprintf(stderr, "NOTE: Found Remaining Backedge For Function %s\n", entryfn->getId().getPid().getname());
/**/        debug_written = true;
/**/      }
          if(flag_range_filter_call_backedge){
            if(!exnode->backedge_locs[0]){
              exnode->backedge_locs[0] = new LocSet;
            }
            if(exnode->backedge_locs[0] != ALL_AOS){
              exnode->backedge_locs[0]->Union(entryfn->getGREF());	//- gref, because modification may not be within entryfn
            }
          } else {
            exnode->backedge_locs[0] = ALL_AOS;
          }
        } else fprintf(stderr, "ERROR(findRemainingBackedges): null cnode encountered!\n");
      }
    } //- else do nothing
    return;
  } else { //- new: check calls successors; traverse return successors; traverse regular successors

    this->find_remaining_backedges_touched = -1; //- mark active

    if(this->hascall){	// this node has calls: traverse call successors
			// NOTE: imprecision: this node may have non-call path -- assuming they don't exist.  Safe??
      //- assert: never any calls in exit nodes?
      if(this->isExitNode())
        fprintf(stderr, "ERROR(findRemainingBackedges): call node is an exit node!\n");

      //- process calls in this node
      suco_iterator<PgmStmt *> psi(this->getStmtList());
      while(psi.Iterate()){
        psi.Current()->traverseRootNodes(CFGnode::findRemainingCallBackedges);
      }

    } else if(this->isExitNode()){ //- return node: traverse return successors
      //- assert: return node has no regular successors
      if(this->getNsuccs())
        fprintf(stderr, "ERROR(findRemainingBackedges): return node has %d regular successors!\n", this->getNsuccs());

      //- find callsite successors
      //- NOTE: because our callsite nodes aren't split into two
      //  (which would've made life a lot easier), we'll check
      //  starting from the _successor_ of the callsite node.
      suco_iterator<PExprCall *> csi(this->getParentFunction().getCallSites());
      while(csi.Iterate()){
        CFGnode * cnode = (csi.Current()->getParentNode())
			  ? (csi.Current()->getParentNode()->getCFGactiveNode())
			  : 0;
        if(cnode){
          for(int i = 0; cnode->getSucc(i); ++i){
            cnode->getSucc(i)->findRemainingBackedges(&this->getParentFunction());
          }
        } else fprintf(stderr, "ERROR(findRemainingBackedges): callsite has no CFGnode!\n");
      }
    } else { //- non-call non-return node: traverse regular successors

      for(int i = 0; this->getSucc(i); ++i){
        this->getSucc(i)->findRemainingBackedges();
      }

    }

    this->find_remaining_backedges_touched = 1; //- mark done
  }
}

bool CFGnode::findRemainingCallBackedges(PgmExpr& pe)
{
  if(pe.getKind() == PgmExpr::fCall){
    PExprCall& pec = (PExprCall&)pe;
    if(pec.getTargetFns().IsEmpty()){ //- no target function: must resume from callnode's successor

      CFGnode * cnode = (pec.getParentNode())
			? (pec.getParentNode()->getCFGactiveNode())
			: 0;
      if(cnode){
        for(int i = 0; cnode->getSucc(i); ++i){
          cnode->getSucc(i)->findRemainingBackedges();
        }
      } else fprintf(stderr, "ERROR(findRemainingCallBackedges): callsite has no CFGnode!\n");

    } else {

      suco_iterator<CFGfunction *> tfi(pec.getTargetFns());
      while(tfi.Iterate()){
        CFGnode * fentry = tfi.Current()->getEntryNode();
        if(fentry){
          fentry->findRemainingBackedges(tfi.Current());
        } else fprintf(stderr, "ERROR(findRemainingCallBackedges): callee has no entry node!\n");
      }

    }
  }
  return true; //- true ensures full traversal
}

//----------------------------------
// COLLECT LOCAL DECLS
//----------------------------------

suco_set<AOId *> * CFGfunction::clie_curlvars = 0;
suco_set<AO *> * CFGfunction::clie_ifree_stack = 0;
suco_set<ECR *> CFGfunction::clie_ifree_heap_ecrs;
suco_set<ECR *> CFGfunction::clie_iref_ecrs;
suco_set<ECR *> CFGfunction::clie_imod_ecrs;
suco_set<AO *> CFGfunction::clie_callsite_retaos;

void CFGfunction::collectIMODREF(suco_set<AO *>& aoset, bool imod)
{
  suco_iterator<AO *> aoi(aoset);
  while(aoi.Iterate()){
    if(!aoi.Current()->isVal()){ //- filter out values
      if(flag_gmodref_skip_locals && CFGfunction::clie_curlvars){
        ////////////////////////////////////////////////////////////
        //- if -gmodref-skip-locals flag is on, then exclude
        //  direct accesses to local variables from GMOD/REF set.
        //~ Uses clie_curlvars to do the filtering.
        //  (clie_curlvars == 0 means this is being called from
        //   collectMODecrs, for backedge filtering, in which case
        //   we want to keep the locals.)
        AO * tgt_ao = aoi.Current();
        //- de-(op/ext)
        while(1){
          if(tgt_ao->getKind() == AO::aoOp){
            tgt_ao = &((AOOp *)tgt_ao)->getTarget();
          } else if(tgt_ao->getKind() == AO::aoExt){
            tgt_ao = &((AOExt *)tgt_ao)->getTarget();
          } else {
            break; //- break out of loop
          }
        }
        if(tgt_ao->isLoc() && CFGfunction::clie_curlvars->Contains((AOId *)&tgt_ao->getEnclosingStruct())){
          //- discard
        } else {
          if(imod) CFGfunction::clie_imod_ecrs.Union(tgt_ao->getECR().getAliasECRs());
          else     CFGfunction::clie_iref_ecrs.Union(tgt_ao->getECR().getAliasECRs());
        }
      } else {
        if(imod) CFGfunction::clie_imod_ecrs.Union(aoi.Current()->getECR().getAliasECRs());
        else     CFGfunction::clie_iref_ecrs.Union(aoi.Current()->getECR().getAliasECRs());
      }
    }
  }
}

bool CFGfunction::collectLocalsAndIMODREFexpr(PgmExpr& df)
{
  switch(df.getKind()){
    case PgmExpr::fDecl: {
        if(CFGfunction::clie_curlvars){ //- assertion
          PExprDecl& dd = (PExprDecl &)df;
          if(dd.isLocal() || dd.isFormal() || dd.isAlloca()){
            CFGfunction::clie_curlvars->Insert((AOId *)&dd.getAO());
            CFGfunction::clie_ifree_stack->Insert((AOId *)&dd.getAO());
          } //-- else isMalloc or isStatic
        } else fprintf(stderr, "CFGfunction::collectLocalsAndIREFexpr (decl) error -- clie_curlvars not set\n");
      } break;
    case PgmExpr::fCall: {
        PExprCall& dc = (PExprCall &)df;
        //- add I <fao>s (to iref)
        CFGfunction::collectIMODREF(dc.getFaos(), false);

        //- collect R D I <fao>s, which will be added to both imod and iref
        suco_iterator<AO *> faoi(dc.getFaos());
        while(faoi.Iterate())
          CFGfunction::clie_callsite_retaos.Insert(&faoi.Current()->get_AOStar().get_AOReturn());

        //- add actual args (to iref)
        for(int i = 1; dc.getArg(i); ++i){
          CFGfunction::collectIMODREF(dc.getArg(i)->getDesc().getAOs(), false);
        }
        if(dc.callsFree()){
          PExprArg * freearg = dc.getArg(1);
          if(freearg){ //- collect freed heap ecrs
            suco_iterator<AO *> aoi(freearg->getDesc().getAOs());
            while(aoi.Iterate()){
              if(!aoi.Current()->isVal()){ //- skip values
                CFGfunction::clie_ifree_heap_ecrs.Union(aoi.Current()->getECR().getPointsTo().getAliasECRs());
              }
            }
          }
        }
      } break;
    case PgmExpr::fAssign: {
        PExprAssign& da = (PExprAssign &)df;
        CFGfunction::collectIMODREF(da.getLHS().getAOs(), true);  // > separate imod from iref for now
        CFGfunction::collectIMODREF(da.getLHS().getAOs(), false); // > TODO: may want to consider imod part of iref?
        CFGfunction::collectIMODREF(da.getRHS().getAOs(), false); //-- note: redundant with vtRhs collection below
      } break;
    case PgmExpr::fPredicate: {
        //- ignore
      } break;
    case PgmExpr::fVerify: { //-- note: vtRhs verifies are redundant with fAssign rhs collection above
        PExprVerify& dv = (PExprVerify &)df;
        CFGfunction::collectIMODREF(dv.getAOs(), false);
      } break;
    case PgmExpr::fParallel:
    case PgmExpr::fBranch:
    default: {
	fprintf(stderr, "WARNING: collectLocalsAndIMODREFexpr with invalid node kind\n");
      } break;
  }
  return true; //- true ensures full traversal
}

//- HAS BEEN AUGMENTED to compute also:
//  - MayFreeHeap and MayFreeStack sets
void CFGfunction::collectLocalsAndIMODREF()
{
  CFGfunction::clie_curlvars = &this->localvars;
  CFGfunction::clie_ifree_stack = &this->gfreestack;
  CFGfunction::clie_ifree_heap_ecrs.Clear();
  CFGfunction::clie_iref_ecrs.Clear();
  CFGfunction::clie_imod_ecrs.Clear();
  CFGfunction::clie_callsite_retaos.Clear();
  suco_iterator<PgmStmt *> cni(this->cfglist);
  while(cni.Iterate()){
    cni.Current()->traverseRootNodes(CFGfunction::collectLocalsAndIMODREFexpr);
  }

  //- collect AOs from ECR sets; filter out non-location AOs
  if(flag_compute_iref){
    this->iref.AbsorbConsume(CFGfunction::clie_iref_ecrs);
  }
  this->gref.AbsorbConsume(CFGfunction::clie_iref_ecrs);
  this->gmod.AbsorbConsume(CFGfunction::clie_imod_ecrs);

  //- absorb callsite retaos (R D <ao>) into both iref and imod
  suco_iterator<AO *> raoi(CFGfunction::clie_callsite_retaos);
  while(raoi.Iterate()){
    if(flag_compute_iref) this->iref.Insert(*raoi.Current());		//-?
    this->gref.Insert(*raoi.Current());
    this->gmod.Insert(*raoi.Current());
  }

  //- special: add return node "R X I <thisfn>" to gmod/gref
  AOFunction& fnao = (AOFunction&) this->getId().get_AOFunction();
  this->gmod.Insert(fnao.get_AOReturn());
  this->gref.Insert(fnao.get_AOReturn());

  //- special: add argument nodes "F X I <thisfn>" to gref
  //  NOTE: also added to gmod -- this is at least needed for identifying
  //        recursive-call widening filter.
  //        TODO: think about whether this makes sense as a general policy.
  for(int i = 1; i <= fnao.getLargNo(); ++i){
    AO * argao = fnao.get_AOArg(i);
    if(argao){
      this->gmod.Insert(*argao);
      this->gref.Insert(*argao);
    } else {
      fprintf(stderr, "ERROR(CFGfunction::collectLocalsAndIMODREF): arg(%d) ao not found for ", i);
      fnao.dump_descr(stderr);
      fprintf(stderr, "\n");
    }
  }

  //- free-heap ecrs
  this->gfreeheap_ecrs.UnionConsume(CFGfunction::clie_ifree_heap_ecrs);
  //- filter out ecrs with no malloc aos
  suco_iterator<ECR *> ei(this->gfreeheap_ecrs);
  while(ei.Iterate()){
    bool has_malloc = false;
    suco_iterator<AO *> ai(ei.Current()->getAOset());
    while(ai.Iterate()){
      if(ai.Current()->getKind() == AO::aoMalloc){
        has_malloc = true;
        break;
      }
    }
    if(!has_malloc){
      ei.DeleteCurrent();
    }
  }

  if(flag_verbose){
    fprintf(stderr, "(IMOD %d aos in %d entries)(IREF %d aos in %d entries)\n"
		    "(IFREE:heap %d ecrs, stack %d aos)\n"
		  , this->gmod.numAOs(), this->gmod.Size()
		  , this->gref.numAOs(), this->gref.Size()
		  , this->gfreeheap_ecrs.Length()
		  , this->gfreestack.Length());
  }
  CFGfunction::clie_callsite_retaos.Clear();
  CFGfunction::clie_iref_ecrs.Clear();
  CFGfunction::clie_imod_ecrs.Clear();
  CFGfunction::clie_ifree_heap_ecrs.Clear();
  CFGfunction::clie_curlvars = 0;
  CFGfunction::clie_ifree_stack = 0;
}

void CFG::collectLocalsAndIMODREF()
{
  TCstats::timer("collecting function local variables and IMOD/IREF");

  int numlocals = 0;
  int numimods = 0;
  int numirefs = 0;
  int numifreeheap = 0;
  int numifreestack = 0;
  suco_iterator<CFGfunction *> fni(this->fnlist);
  while(fni.Iterate()){
    if(flag_verbose) TCstats::timer(fni.Current()->getId().getPid().getname());
    fni.Current()->collectLocalsAndIMODREF();
    numlocals += fni.Current()->getLocalVars().Length();
    numimods += fni.Current()->getGMOD().numAOs();
    numirefs += fni.Current()->getGREF().numAOs();
    numifreeheap += fni.Current()->getGFreeHeap_ecrs().Length();
    numifreestack += fni.Current()->getGFreeStack().Length();
    if(flag_verbose) TCstats::timer(0);
  }

  TCstats::timer(0);
  fprintf(stderr, "(locals %d, imod %d, iref %d, free-heap %d, free-stack %d)\n",
		numlocals, numimods, numirefs, numifreeheap, numifreestack);
}

int CFGfunction::compareByCallgraphId(CFGfunction * fn1, CFGfunction * fn2)
{
  int rid1 = fn1->getRecursionId();
  int rid2 = fn2->getRecursionId();
  int cid1 = fn1->getCallGraphId();
  int cid2 = fn2->getCallGraphId();

  if(rid1 != rid2){
    cid1 = rid1?rid1:cid1;
    cid2 = rid2?rid2:cid2;
  }

  return cid1-cid2;
}

void CFG::collectGMODREF()
{
  if(flag_verbose) TCstats::timer("collecting GMOD/GREF");

  suco_set<CFGfunction *> ofnset(CFGfunction::compareByCallgraphId);

  {
    suco_iterator<CFGfunction *> fni(fnlist);
    while(fni.Iterate())
      ofnset.Insert(fni.Current());
  }

  //-- ofnset is now fnlist ordered by callgraph_id

  if(flag_verbose >= 3){
    fprintf(stderr, "---CALLGRAPH---\n");

    suco_iterator<CFGfunction *> ofni(ofnset);
    while(ofni.Iterate()){
      CFGfunction& fn = *ofni.Current();

      fprintf(stderr, "%s-%d(r%d):", fn.getId().getPid().getname(),
					fn.getCallGraphId(), fn.getRecursionId());

      suco_iterator<CFGfunction *> cgi(fn.callgraph);
      while(cgi.Iterate()){
        fprintf(stderr, " %s-%d(r%d)", cgi.Current()->getId().getPid().getname(),
					cgi.Current()->getCallGraphId(), cgi.Current()->getRecursionId());
      }
      fprintf(stderr, "\n");
    }
    fprintf(stderr, "---END CALLGRAPH---\n");
  }

  while(!ofnset.IsEmpty()){
    CFGfunction& fn = *ofnset.RemoveHead();

    //- union imods/irefs from children
    suco_iterator<CFGfunction *> cgi(fn.callgraph);
    while(cgi.Iterate()){
      fn.gmod.Union(cgi.Current()->gmod);
      fn.gref.Union(cgi.Current()->gref);
      fn.gfreeheap_ecrs.Union(cgi.Current()->gfreeheap_ecrs);
      fn.gfreestack.Union(cgi.Current()->gfreestack);
    }

    //- if recursive...
    if(fn.getRecursionId()){
      //- union imods/irefs for all nodes with same recursion_id
      suco_iterator<CFGfunction *> rfi(ofnset);
      while(rfi.Iterate() && rfi.Current()->getRecursionId() == fn.getRecursionId()){
        fn.gmod.Union(rfi.Current()->gmod);
        fn.gref.Union(rfi.Current()->gref);
        fn.gfreeheap_ecrs.Union(rfi.Current()->gfreeheap_ecrs);
        fn.gfreestack.Union(rfi.Current()->gfreestack);
        //- do their children as well!
        suco_iterator<CFGfunction *> rci(rfi.Current()->callgraph);
        while(rci.Iterate()){
          fn.gmod.Union(rci.Current()->gmod);
          fn.gref.Union(rci.Current()->gref);
          fn.gfreeheap_ecrs.Union(rci.Current()->gfreeheap_ecrs);
          fn.gfreestack.Union(rci.Current()->gfreestack);
        }
      }
      //- replicate this union-set onto these members, and remove them
      while(ofnset.Head() && ofnset.Head()->getRecursionId() == fn.getRecursionId()){
        ofnset.Head()->gmod.Union(fn.gmod);
        ofnset.Head()->gref.Union(fn.gref);
        ofnset.Head()->gfreeheap_ecrs.Union(fn.gfreeheap_ecrs);
        ofnset.Head()->gfreestack.Union(fn.gfreestack);
        ofnset.RemoveHead();
      }
    }
  }

  if(flag_verbose) TCstats::timer(0);
}

//----------------------------------
// OTHER MEMBER FUNCTIONS
//----------------------------------

CFGnode * CFGfunction::getEntryNode() const
{
  if(flag_use_bblocks)
    return entryblock;
  else
    return entrystmt;
}

//- used by RDMBc
void CFGfunction::collectCallers(CFGfunction& fn, suco_set<CFGfunction *>& fns)
{
  if(fns.Insert(&fn)){
    suco_iterator<PExprCall *> csi(fn.getCallSites());
    while(csi.Iterate()){
      PgmStmt * par = csi.Current()->getParentNode();
      if(par) CFGfunction::collectCallers(par->getParentFunction(), fns);
    }
  } //- else already added
}

void CFGfunction::writeCallgraphDotEntry(FILE * outf)
{
#define BUFSZ 16
  static char buf[BUFSZ] = {0};

  fprintf(outf, "  n%d_%d[label=\"(%s)\\n%s\"];\n",
		this->aid.filestem_id, this->aid.aid,
		this->aid.lookupFileStem(),
		strncpy(buf, strrchr(this->idao.getPid().getname(), '.')+1, BUFSZ-1));

  suco_iterator<CFGfunction *> cgi(this->callgraph);
  while(cgi.Iterate()){
    CFGfunction& tgtfn = *cgi.Current();
    fprintf(outf, "  n%d_%d -> n%d_%d%s;\n",
		this->aid.filestem_id, this->aid.aid,
		tgtfn.aid.filestem_id, tgtfn.aid.aid,
		(this->getRecursionId() && (this->getRecursionId() == tgtfn.getRecursionId()))
			?"[label=\"(r)\"]":"");
  }
#undef BUFSZ
}

void CFGfunction::write_descr(FILE * outf, const char * prefix, int indent)
{
  const char * ispaces = getIndent(indent);
  fprintf(outf, "%s%s%d:%s(%s)", ispaces, prefix,
		this->getCallGraphId(),
		this->idao.getPid().getname(),
		this->aid.lookupFileStem());

  if(this->getRecursionId()){
    fprintf(outf, " [SCC %d]\n", this->getRecursionId());
  } else {
    fprintf(outf, "\n");
  }
}

void CFGfunction::bottomUpWriteCalltrace(FILE * outf, int indent)
{
#define INDENT_LIMIT 10
  this->write_descr(outf, "*", indent);
  suco_set<CFGfunction *> callers;
  suco_iterator<PExprCall *> csi(this->callsites);
  while(csi.Iterate()){
    CFGnode * parnode = csi.Current()->getParentNode();
    if(parnode &&
	callers.Insert(&parnode->getParentFunction()))
      parnode->getParentFunction().write_descr(outf, "<-", indent+2);
  }

  if(indent <= INDENT_LIMIT){
    suco_iterator<CFGfunction *> cfi(callers);
    while(cfi.Iterate()){
      if(cfi.Current()->getCallGraphId() > this->getCallGraphId())
        cfi.Current()->bottomUpWriteCalltrace(outf, indent+2);
    }
  }
#undef INDENT_LIMIT
}

void CFGfunction::topDownWriteCalltree(FILE * outf, int indent)
{
  this->write_descr(outf, "", indent);
  if(!this->getRecursionId()){
    suco_iterator<CFGfunction *> cgi(this->callgraph);
    while(cgi.Iterate()){
      cgi.Current()->topDownWriteCalltree(outf, indent+2);
    }
  }
}

void CFG::writeCallgraph(FILE * outf, bool dot, char * trace_fn)
{
  this->prepareSuperGraph();
  if(dot){
    fprintf(outf, "digraph callgraph {\n");
    suco_iterator<CFGfunction *> fni(this->getFunctionList());
    while(fni.Iterate()){
      fni.Current()->writeCallgraphDotEntry(outf);
    }
    fprintf(outf, "}\n");
  } else if(trace_fn){
    suco_iterator<CFGfunction *> fni(this->getFunctionList());
    while(fni.Iterate()){
      if(strstr(fni.Current()->getId().getPid().getname(), trace_fn)){
        fprintf(outf, "CALL TRACE FOR %s:\n", fni.Current()->getId().getPid().getname());
        fni.Current()->bottomUpWriteCalltrace(outf, 0);
      }
    }
  } else {
    //-- main tree
    fprintf(outf, "MAIN:\n");
    if(this->mainfn){
      this->mainfn->topDownWriteCalltree(outf, 0);
    }
    //-- recursive subtrees
    suco_set<int> recursids;
    suco_iterator<CFGfunction *> fni(this->getFunctionList());
    while(fni.Iterate())
      recursids.Insert(fni.Current()->getRecursionId());

    suco_iterator<int> ri(recursids);
    while(ri.Iterate()){
      fprintf(outf, "[SCC %d]:\n", ri.Current());
      fni.Rewind();
      while(fni.Iterate()){
        if(fni.Current()->getRecursionId() == ri.Current()){
          fni.Current()->write_descr(outf, "", 0);
          suco_iterator<CFGfunction *> fcgi(fni.Current()->callgraph);
          while(fcgi.Iterate()){
            fcgi.Current()->topDownWriteCalltree(outf, 2);
          }
        }
      }
    }
  }
}

const char * PgmExpr::getKindString() const
{
  switch(kind){
    case fCall:     return "Call";
    case fAssign:   return "Assign";
    case fPredicate:return "Predicate";
    case fDecl:     return "Decl";
    case fVerify:   return "Verify";
    case fParallel: return "Parallel";
    case fBranch:   return "Branch";
  }
  return "<Kind:ERROR>";
}

bool PgmExpr::isLib()
{
  return this->parent && this->parent->getParentFunction().getId().isLib();
}

suco_set<AO *> PExprPredicate::empty_aos; //- static placeholder: empty list

const char * PExprPredicate::getPrKindString() const
{
  switch(pkind){
    case prAnd:	   return "And";
    case prOr:	   return "Or";
    case prQC:	   return "QC";
    case prStmt:   return "Stmt";
    case prSwitch: return "Switch";
  }
  return "<PrKind:ERROR>";
}

const char * PExprDecl::getDkindString() const
{
  switch(dkind){
    case dFormal: return "Formal";
    case dLocal:  return "Local";
    case dStatic: return "Static";
    case dMalloc: return "Malloc";
  }
  return "<DKind:ERROR>";
}

const char * PExprVerify::getVtKindString() const
{
  switch(vtkind){
    case vtTag:  return " vtTag";
    case vtRhs:  return " vtRhs";
    case vtNone: return "";
  }
  return "<VtKind:ERROR>";
}

const char * PExprVerify::getVpKindString() const
{
  switch(vpkind){
    case vpPtr:  return " vtPtr";
    case vpPtrW: return " vtPtrW";
    case vpNone: return "";
  }
  return "<VpKind:ERROR>";
}

//- - - - - - - - - - - - - - - - - -

LocSet * CFGnode::getBackEdge() const
{
  if(backedge_locs[0] && backedge_locs[1] &&
	backedge_locs[0] != backedge_locs[1]){ //- assertion
    fprintf(stderr, "WARNING(CFGnode::getBackEdge): requested one backedge when two are present\n");
    return ALL_AOS;
  }
  return backedge_locs[0]?backedge_locs[0]:backedge_locs[1];
}

LocSet * CFGnode::getBackEdge(bool tf) const
{
  return backedge_locs[tf?0:1];
}

void CFGnode::updateBackEdgeIfSet(CFGnode& tgtnode, LocSet& lset)
{
  int index = (this->getSucc(1) == &tgtnode)?1:0;	//- if tgtnode is first (false) successor,
							//  then backedge indicator is at [1], else it's at [0]
  if(backedge_locs[index] == ALL_AOS){
    backedge_locs[index] = new LocSet;
    backedge_locs[index]->Union(lset);
  } else if(backedge_locs[index]){
    backedge_locs[index]->Union(lset);
  } //- else not set, so don't update
}

//- If pe is a recursive call, must set both widen_locs
//  and retnode->backedge_locs (to ALL_AOS or GMOD,
//  depending on flag_range_filter_call_backedge)
//  - note that each recursive call results in two
//    loops that must be broken:
//    - caller-to-callee_entry (captured by widen_locs)
//    - callee_return-to-caller (captured by retnode->backedge_locs)
bool CFGnode::computeWidenAOs(PgmExpr& pe)
{
  if(pe.getKind() == PgmExpr::fCall){
    PExprCall& pec = (PExprCall&)pe;
    int pec_recurs_id = (pec.getParentNode())
			? (pec.getParentNode()->getParentFunction().getRecursionId())
			: 0;
    if(pec_recurs_id){

      suco_iterator<CFGfunction *> tfi(pec.getTargetFns());
      while(tfi.Iterate()){
        if(tfi.Current()->getRecursionId() == pec_recurs_id){

          //-- call is recursive: DO STUFF HERE

          if(flag_range_filter_call_backedge){
            if(pec.widen_locs != CFGnode::ALL_AOS){	//- paranoid check
              if(!pec.widen_locs){
                pec.widen_locs = new LocSet;
              }
              pec.widen_locs->Union(tfi.Current()->getGMOD());
            } else {
/**/          fprintf(stderr, "ERROR(computeWidenAOs): ALL_AOS widen_aos encountered!\n");
            }

            suco_iterator<PgmStmt *> eni(tfi.Current()->getExitNodes());
            while(eni.Iterate()){
              CFGnode * cnode = eni.Current()->getCFGactiveNode();
              if(cnode && cnode->backedge_locs[0] != CFGnode::ALL_AOS){
                if(!cnode->backedge_locs[0]){
                  cnode->backedge_locs[0] = new LocSet;
                }
                cnode->backedge_locs[0]->Union(tfi.Current()->getGMOD());
              } else {
/**/            fprintf(stderr, "ERROR(computeWidenAOs): null cnode or ALL_AOS backedge_locs encountered!\n");
              }
            }

          } else { //- don't filter: just mark and break out (no need to check individual targets further)
            pec.widen_locs = CFGnode::ALL_AOS;
            suco_iterator<PgmStmt *> eni(tfi.Current()->getExitNodes());
            while(eni.Iterate()){
              CFGnode * cnode = eni.Current()->getCFGactiveNode();
              if(cnode) cnode->backedge_locs[0] = CFGnode::ALL_AOS;
            }
          }
        }
      }
    }
  }
  return true; //- true ensures full traversal
}

void CFGnode::addSuccessor(CFGnode& n)
{
  //-- add &n to successor list
  if(!succs){
    succs = new CFGnode * [2];
    nsuccs = 1;
    succs[0] = &n;
    succs[nsuccs] = 0;
  }
  else {
    int i;
    for(i = 0; i < nsuccs; ++i){
      if(!succs[i]){
        succs[i] = &n;
        break;
      }
    }
    if(i == nsuccs){
      fprintf(stderr, "CFGnode::addSuccessor: infrequent case encountered! (nsuccs=%d)\n", nsuccs);
      CFGnode ** temp = succs;
      succs = new CFGnode * [nsuccs+2];
      for(i = 0; i < nsuccs; ++i)
        succs[i] = temp[i];
      succs[i] = &n;
      nsuccs++;
      succs[nsuccs] = 0;
      delete [] temp;
    }
  }
  //-- add this to n.predecessor list
  if(!n.preds){
    n.preds = new CFGnode * [2];
    n.npreds = 1;
    n.preds[0] = this;
    n.preds[n.npreds] = 0;
  }
  else {
    int i;
    for(i = 0; i < n.npreds; ++i){
      if(!n.preds[i]){
        n.preds[i] = this;
        break;
      }
    }
    if(i == n.npreds){
//      fprintf(stderr, "CFGnode::addPredecessor: infrequent case encountered! (npreds=%d)\n", n.npreds);
      CFGnode ** temp = n.preds;
      n.preds = new CFGnode * [n.npreds+2];
      for(i = 0; i < n.npreds; ++i)
        n.preds[i] = temp[i];
      n.preds[i] = this;
      n.npreds++;
      n.preds[n.npreds] = 0;
      delete [] temp;
    }
  }
}

void CFGnode::initSuccsCount(int n)
{
  if(n){
    if(!succs){
      succs = new CFGnode * [n+1];
      nsuccs = n;
      for(int i = 0; i <= nsuccs; ++i) succs[i] = 0;
    } else if(nsuccs < n){
      CFGnode ** temp = succs;
      succs = new CFGnode * [n+1];
      int i;
      for(i = 0; i < nsuccs; ++i) succs[i] = temp[i];
      delete [] temp;
      nsuccs = n;
      for(; i <= nsuccs; ++i) succs[i] = 0;
    }
  }
}

void CFGnode::initPredsCount(int n)
{
  if(n){
    if(!preds){
      preds = new CFGnode * [n+1];
      npreds = n;
      for(int i = 0; i <= npreds; ++i) preds[i] = 0;
    } else if(npreds < n){
      CFGnode ** temp = preds;
      preds = new CFGnode * [n+1];
      int i;
      for(i = 0; i < npreds; ++i) preds[i] = temp[i];
      delete [] temp;
      npreds = n;
      for(; i <= npreds; ++i) preds[i] = 0;
    }
  }
}

int CFGnode::countPreds() const
{
  int i;
  for(i = 0; i < npreds && preds[i]; ++i)
    ;
  return i;
}

int CFGnode::countSuccs() const
{
  int i;
  for(i = 0; i < nsuccs && succs[i]; ++i)
    ;
  return i;
}

bool CFGnode::isEntryNode() const
{
  return (this == parent.getEntryNode());
}

bool CFGbblock::isExitNode()
{
  PgmStmt * tail = stmtlist.Last();
  return (tail && tail->isReturn());
}

bool PgmStmt::isExitNode()
{
  return this->isReturn();
}

bool PgmStmt::hasLabel(const char * s) const
{
  if(s){ //-- if non-null, find matching label
    for(int i = 0; i < nlabels; ++i)
      if(!strcmp(labels[i], s))
        return true;
    return false;
  } else { //-- if s is null, return if we have *any* label
    return (nlabels != 0);
  }
}

CFGnode * PgmStmt::getCFGactiveNode()
{
  if(flag_use_bblocks) return this->getBBlock();
  else return this;
}

bool PgmStmt::hasSwitchPred() const
{
  for(int i = 0; getPred(i); ++i)
    if(getPredStmt(i)->annot.kind == Annot::aSwitch)
      return true;
  return false;
}

//----------------------------------
// END OTHER MEMBER FUNCTIONS
//----------------------------------

