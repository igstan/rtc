#include <ctype.h>
#include <stdio.h>
#include <stdlib.h> // for strtoul, exit

#include "ao.h"
#include "flags.h"
#include "ecr.h"

//------------------------------------------------------
//- AOlist

AOlist::~AOlist()
{
  node * n = head;
  while(n){
    node * del = n;
    n = n->next;
    delete &del->ao;
    delete del;
  }
}

void AOlist::debug_dump(FILE * os)
{
  fprintf(os, "---AOlist:\n");
  for(node * n = head; n; n = n->next){
    fprintf(os, "\t");
    n->ao.debug_dump(os);
    fprintf(os, "\n");
  }
  fprintf(os, "---End AOlist:\n");
}

void AOlist::foreachAO(void (*fp)(AO& o))
{
  for(node * n = head; n; n = n->next)
    fp(n->ao);
}

void AOlist::traverseAOs(void (*fp)(AO& o))
{
  for(node * n = head; n; n = n->next)
    n->ao.traverseAOs(fp);
}

//------------------------------------------------------
//-ExposedStatus

const ExposedStatus ExposedStatus::All(esAll);
const ExposedStatus ExposedStatus::Some(esSome);
const ExposedStatus ExposedStatus::None(esNone);
const ExposedStatus ExposedStatus::NA(esNA);

char ExposedStatus::exposedCode()
{
  switch(status){
    case esAll:   return 'A';
    case esSome:  return 'S';
    case esNone:  return 'N';
    case esNA:    return 'N';
  }
  return 'X'; //- error
}

void ExposedStatus::combine(ExposedStatus es)
{
  switch(this->status){
    case esAll:
      switch(es.status){
        case esSome:
        case esNone: this->status = esSome;
                     break;
        case esAll:
        case esNA:   break;
      } break;
    case esNone:
      switch(es.status){
        case esAll:
        case esSome: this->status = esSome;
                     break;
        case esNone:
        case esNA:   break;
      } break;
    case esNA:
      this->status = es.status;
      break;
    case esSome:
      break;
  }
}

//------------------------------------------------------
//- AO

void AO::traverseAOs(void (*fp)(AO& o))
{
  fp(*this);
  aolist.traverseAOs(fp);
}

/* NOTE: This function potentially creates and frees many
         edges; could not cache and recycle nodes because
         C++ doesn't allow copies into the reference members
         of TCassignEdge. Oh well...
 */
TCassignEdge * AO::assignTo(AO& tgt, TCtype& ty)
{
  tgt.isAssignedAO = true;
  TCassignEdge * newe = new TCassignEdge(tgt, *this, ty);
  if(this->assignsTo.Insert(newe)){
    tgt.assignsFrom.Insert(newe);
    return newe;
  } else {
    delete newe;
    return 0;
  }
}

//- Does this AO represent a direct array access?
//  Include only "loc" AOs:
//   - a[i], a[i][j], s.a[i], s[i].a[j]
//  But not: s->a[i], (*p)[i].
bool AO::isDirectArrayAccess()
{
  //- root case: <loc: ty = array>
  //- struct/union case: (S|U <direct-array>) : ty = array>
  if(this->getStaticType()
	&& this->getStaticType()->getKind() == TCtype::tcArray){
    if(this->isLoc()){
      return true;
    }
    if(this->getKind() == aoSDot){
      return ((AOSDot *)this)->getParent().isDirectArrayAccess();
    }
    if(this->getKind() == aoUDot){
      return ((AOUDot *)this)->getParent().isDirectArrayAccess();
    }
  }
  //- general case: D O A <direct-array>
  if(this->getKind() == AO::aoStar){
    AO& ao2 = ((AOStar *)this)->getTarget();
    if(ao2.getKind() == AO::aoOp){
      AO& ao3 = ((AOOp &)ao2).getTarget();
      if(ao3.getKind() == AO::aoAddrOf){
        AO& ao4 = ((AOAddrOf &)ao3).getTarget();
        return ao4.isDirectArrayAccess();
      }
    }
  }
  return false;
}

//--------
// Check to see if this AO points to anything exposed
// Arguments are actually return values!
ExposedStatus AO::pointsToExposed(bool limit_malloc, bool do_touched, bool do_vuln)
{
  if(ecr){
    ECR& e = ecr->followECR();
    if(e.ptsTo){
      ECR::traverseCounter++;
      return e.getPointsTo().inclToExposed(limit_malloc, do_touched, do_vuln);
    } //- else, no points-to, leave alone
  } //- else, no ecr, leave alone
  return ExposedStatus::NA; //- SY: or should it be esNone?
}

//- for struct and union types, as well as arrays of struct/union types,
//  instantiate all AOSDot/AOUDot aos!
//~ Note: must duplicate tylist, because get_or_create_AOSDot may consume it.
//        This may also lead to leaks!
void AO::instantiateStructUnionMembers()
{
  TCtype * sty = this->getStaticType();
  if(sty){
    while(sty->getKind() == TCtype::tcArray){
      sty = &((TCnumType *)sty)->getBaseType();
    }
    if(sty->getKind() == TCtype::tcStruct){
      suco_llist<TCtype *> * tylist = &((TCstructUnionType *)sty)->getTypeList();
      while(!tylist->IsEmpty()){
        //-- now, lookup AOSDot
        AO& sao = this->get_or_create_AOSDot(*tylist, false); //- cannot deleteTyList: if found, can delete list, but not constituent types!
        sao.instantiateStructUnionMembers();
        //-- create copy of tylist, minus last element
        suco_iterator<TCtype *> tli(*tylist);
        tylist = new suco_llist<TCtype *>;
        tli.Iterate();
        TCtype * ty = 0;
        while(1){
          ty = tli.Current();
          if(tli.Iterate())
            tylist->Append(ty);
          else break;
        }
      }
      //- cleanup
      if(tylist != &((TCstructUnionType *)sty)->getTypeList())
        delete tylist; // should be empty
    }
    if(sty->getKind() == TCtype::tcUnion){
      suco_iterator<TCtype *> tli(((TCstructUnionType *)sty)->getTypeList());
      while(tli.Iterate()){
        AO& uao = this->get_or_create_AOUDot(*tli.Current(), false); //- cannot deleteTy
        uao.instantiateStructUnionMembers();
      }
    }
  }
}

void AO::setStaticType(TCtype& ty) //- must consume ty
{
  if(!staticType){
    staticType = &ty;
    if(flag_instantiate_structunion)
      this->instantiateStructUnionMembers(); //- should only be called for Loc or ArgRet objects
  } else {
    if(!staticType->equiv(ty)){
//-TODO: try to select "stronger" type?
//	 Actually, need help from front end to differentiate
//	 between explicit decls and implicits or externs.
    //-For now, NOP: just arbitrarily stick with first-encountered instance
/*******************************************/
/**/if( (this->getKind() != aoFunction) &&
/**/	(this->getKind() != aoArg) &&
/**/	(this->getKind() != aoReturn) ){
/**/fprintf(stderr, "\nWARNING(AO::setStaticType): mismatched static types for ");
/**/this->dump_descr(stderr);
/**/fprintf(stderr, "\n  was: ");
/**/this->staticType->debug_dump(stderr);
/**/fprintf(stderr, "  now: ");
/**/ty.debug_dump(stderr);
/**/fprintf(stderr, "\n");
/**/}
/*******************************************/
      TCtype::deleteTy(ty); //- must consume ty
    } else {
      TCtype::deleteTy(ty); //- must consume ty
    }
  }
}

//- sets "last argno" and "is varg?"
//  NOTE: largno is the ellipsis position for vararg,
//        and one past the last for non-varg functions.
//  ~ instantiate all arg aos up to largno!
void AOFunction::setAttributes(int larg_no, bool is_varg)
{
  this->largno = larg_no;
  this->isvarg = is_varg;
  for(int i = 1; i <= this->largno; ++i)
    this->get_or_create_AOArg(i);
}

void AOMalloc::meetMallocStaticType(TCtype * ty, unsigned int size)
{
  if(this->mtype && this->mtype->getKind() == TCtype::tcVoid) //- tcVoid means "bottom": do nothing
    return;
  if(ty && size){ //- known type/size
    //- normalize ty/size pair
    while(ty->getKind() == TCtype::tcArray){
      size *= ((TCnumType *)ty)->getSize();
      ty = &((TCnumType *)ty)->getBaseType();
    }
    if(!this->mtype){ //- null means "uninitialized"; tcVoid means "bottom"
      this->mtype = ty;
      this->msize = size;
/**/if(flag_verbose){
/**/fprintf(stderr, "NOTE(meetMallocStaticType): setting mtype/msize for ao = ");
/**/this->write_string_rep(stderr, true);
/**/fprintf(stderr, "   mtype = ");
/**/this->mtype->write_string_rep(stderr);
/**/fprintf(stderr, "   msize = %d\n", this->msize);
/**/}
      return;
    }
    if(this->mtype->equiv(*ty)){
      if(size < this->msize){
/**/fprintf(stderr, "NOTE(meetMallocStaticType): changing msize for ao = ");
/**/this->write_string_rep(stderr, true);
/**/fprintf(stderr, "   mtype = ");
/**/this->mtype->write_string_rep(stderr);
/**/fprintf(stderr, "   msize = %d -> %d\n", this->msize, size);
        this->msize = size;
      } //- else OK: same or greater size
      return;
    } else {
/**/fprintf(stderr, "WARNING(meetMallocStaticType): type mismatch; ao = ");
/**/this->write_string_rep(stderr, true);
/**/fprintf(stderr, "   oldty = ");
/**/mtype->write_string_rep(stderr);
/**/fprintf(stderr, "   newty = ");
/**/ty->write_string_rep(stderr);
/**/fprintf(stderr, "   newsize = %d\n", size);
      this->mtype = &TCtype::tcVoidType;
      this->msize = 0;
    }
  } else { //- unknown type/size
    if(this->mtype){
/**/fprintf(stderr, "NOTE(meetMallocStaticType): meet with void; ao = ");
/**/this->write_string_rep(stderr, true);
/**/fprintf(stderr, "   oldty = ");
/**/this->mtype->write_string_rep(stderr);
/**/fprintf(stderr, "   oldsize = %d\n", this->msize);
    }
    this->mtype = &TCtype::tcVoidType;
    this->msize = 0;
  }
}

void AO::setRequiredType(TCtype& ty)
{
  //-- For now, for struct/union objects, set to top
  //   Later: perhaps set types for aggregate components?
  if(ty.getKind() == TCtype::tcStruct || ty.getKind() == TCtype::tcUnion)
    reqdType = TClatType::tclTop;
  else if(ty.getKind() == TCtype::tcVoid) //- treat void as Top (only occurs for fn return nodes)
    reqdType = TClatType::tclTop;
  else
    reqdType = TClatType::Join(reqdType, TClatType::getLatKindFor(ty));
}

void AO::setVerifyPtrType(TCtype& ty) //- must consume ty
{
  if(!verifyPtrType){
    verifyPtrType = &ty;
  } else {
    if(!verifyPtrType->equiv(ty)){
      TCtype::deleteTy(ty); // cleanup
      TCtype::deleteTy(*verifyPtrType); //- cleanup also old vpty!
      verifyPtrType = &TCtype::tcVoidType; //-- set to "largest" type!?!
    } else {
      TCtype::deleteTy(ty); // cleanup
    }
  }
}

void AO::debug_dump(FILE * os)
{
  dump_descr(os);
  if(getTScode()) fprintf(os, "-tsl:%c-", getTScode());
  if(getTSCcode()) fprintf(os, "-tsc:%c-", getTSCcode());
  if(ecr){
    ECR& e = ecr->followECR();
    fprintf(os, "[ECR:%08x/poss-type:", (unsigned int)&e);
    e.possType().debug_dump(os);
    fprintf(os, "]");
  } else {
    fprintf(os, "[ECR:0]");
  }
  fprintf(os, "[reqd-type:");
  TClatType::Debug_dump(reqdType, os);
  fprintf(os, "]");
  if(staticType){
    fprintf(os, "[static-type:");
    staticType->debug_dump(os);
    fprintf(os, "]");
  }
}

//- sets tsl to UNSAFE, then
//  - propagate along assign edges
//  - propagate to all SDot and UDot children
//  - propagate to all Star children
//  - propagate to all Ext children
void AO::setTS_UNSAFE()
{
  if(tsl == TS_UNSAFE || isVal()) //- skip if done or if is value-AO
    return;
  tsl = TS_UNSAFE;

  //- propagate along assignment edges
  //  - note: if-condition above will prevent infinite cycling
  suco_iterator<TCassignEdge *> ei(this->getOutgoingAssignEdges());
  while(ei.Iterate())
    ei.Current()->getTo().setTS_UNSAFE();

  //- propagate to SDot, UDot, Star, and Ext children
  //  - note: no cycles to worry about
  for(AOlist::node * n = aolist.head; n; n = n->next){
    switch(n->ao.getKind()){
      case aoSDot:
      case aoUDot:
      case aoStar:
      case aoExt:
	n->ao.setTS_UNSAFE();
	break;
      default:
	;
    }
  }
}

void AO::setTSC(ts_categ c)
{
  if(c < tsc) tsc = c;
}

FILE * AO::aoWriteStream = 0;

void AO::writeTSlevel(AO& ao)
{
  char c = ao.getTScode();
  if(c && aoWriteStream) {
    fprintf(aoWriteStream, "~ %c %d ", c, ao.getECR().getEcrNo());
    ao.write_string_rep(aoWriteStream, flag_readable_output);
    fprintf(aoWriteStream, "\n");
  }
}

void AO::writeTScateg(AO& ao)
{
  char c = ao.getTSCcode();
  if(c && aoWriteStream) {
    fprintf(aoWriteStream, "~ %c %d ", c, ao.getECR().getEcrNo());

    //--if exposed, write exposed type?
//TODO

    ao.write_string_rep(aoWriteStream, flag_readable_output);
    fprintf(aoWriteStream, "\n");
  }
}

void AO::writeUntouchedExposed(AO& ao)
{
  if(aoWriteStream &&
	(ao.getTSC() == TSC_EXPOSED)){
    ECR& ecr = ao.getECR();
    if(!ecr.touched){
      fprintf(aoWriteStream, "_ %c ", ecr.touched?'T':'U');
      ao.write_string_rep(aoWriteStream, flag_readable_output);
      fprintf(aoWriteStream, "\n");
    }
  }
}

void AO::writeVulnerable(AO& ao)
{
  if(ao.isLoc() && ao.isVulnerableLoc()){
    fprintf(aoWriteStream, "V l ");
    ao.write_string_rep(aoWriteStream, flag_readable_output);
    fprintf(aoWriteStream, "\n");
  }
  if(ao.isRef() && ao.getECR().is_vuln_deref){
    fprintf(aoWriteStream, "V d ");
    ao.write_string_rep(aoWriteStream, flag_readable_output);
    fprintf(aoWriteStream, "\n");
  }
}

void AO::writeECR(AO& ao)
{
  if(aoWriteStream) {
    fprintf(aoWriteStream, "ECR for ");
    ao.debug_dump(aoWriteStream);
    fprintf(aoWriteStream, " : ");
    ao.getECR().debug_dump(aoWriteStream);
    fprintf(aoWriteStream, "\n");
  }
}

void AO::write_list_string_rep(FILE * os, suco_llist<AO *>& list, bool readable)
{
  suco_iterator<AO *> si(list);
  while(si.Iterate()){
    fprintf(os, ", ");
    si.Current()->write_string_rep(os, readable);
  }
  fprintf(os, ". ");
}

void AO::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "Z "); //- error: invalid object
}

//-NOTE: this function should be called only after
// pt-analysis is over and ECRs have been "finalized".
// Prior to that, use ECR::getECR().
ECR& AO::getECR()
{
  if(!this->ecr){
    fprintf(stderr, "FATAL ERROR(getECR): ao has no ECR!\n\t");
    this->dump_descr(stderr);
    fprintf(stderr, "\n");
    exit(1);
  }
  return *this->ecr;
}

void AO::assignEcrNosAndWriteToFile(AO& ao)
{
  if(ao.ecr)
    ao.getECR().getEcrNo(aoWriteStream);
}

char AO::getTScode()
{
  switch(tsl){
    case TS_SAFE:    return 'S';
    case TS_TRACKED: return 'T';
    case TS_UNSAFE:  return 'U';
    default:         return 0;
  }
}

AO::ts_categ AO::getSetTSC(suco_set<AO *>& set)
{
  ts_categ ret = TSC_SAFE;
  suco_iterator<AO *> si(set);
  while(si.Iterate()){
    ts_categ tsc = si.Current()->getTSC();
    if(tsc < ret) ret = tsc;
  }
  return ret;
}

char AO::getTSCcode()
{
  switch(tsc){
    case TSC_POSS_INVALID:	return 'P';
    case TSC_BADLY_TYPED:	return 'B';
    case TSC_INFLUENTIAL:	return 'I';
    case TSC_EXPOSED:		return 'E';
    case TSC_SAFE:		return 'S';
    default:		return 0;
  }
}

AO * AO::stringToAO(char * str, char ** nptr, IDmap& aidmap, IDmap& pidmap, AOlist& valueAOs)
{
  char * c = str;
  while(isspace(*c)) c++;
  switch(*c){
    case 'A': {
	AO * o = stringToAO(c+1, nptr, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed addr-of object\n"), (AO*)0);
	else return &o->get_AOAddrOf();
      }
    case 'D': {
	AO * o = stringToAO(c+1, nptr, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed deref object\n"), (AO*)0);
	else return &o->get_AOStar();
      }
    case 'E': {
	AO * o = stringToAO(c+1, &c, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed extension object\n"), (AO*)0);
	TCtype * tty = TCtype::stringToTy(c,&c);
	TCtype * fty = TCtype::stringToTy(c,nptr);
	if(tty && fty) return &o->get_AOExt(*tty,*fty);
	else {
/**/fprintf(stderr, "LEAK(stringToAO): tty=");
/**/if(tty) tty->write_string_rep(stderr); else fprintf(stderr, "NULL");
/**/fprintf(stderr, " fty=");
/**/if(fty) fty->write_string_rep(stderr); else fprintf(stderr, "NULL");
/**/fprintf(stderr, "\n");
	  return 0;
	}
      }
    case 'F': {
	unsigned int argno = strtoul(c+1, &c, 10);
	AO * o = stringToAO(c, nptr, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed function-arg object\n"), (AO*)0);
	return &o->get_or_create_AOArg(argno);
      }
    case 'I': {
	unsigned int pidno = strtoul(c+1, nptr, 10);
	ID * pid = pidmap.lookup(pidno);
	if(pid) return &pid->get_AOId();
	else return (fprintf(stderr, "Invalid Pid (%d) for identifier object\n", pidno), (AO*)0);
      }
    case 'L': {
	unsigned int aidno = strtoul(c+1, nptr, 10);
	ID * aid = aidmap.lookup(aidno);
	if(aid) return &aid->get_AOStringLit();
	else return (fprintf(stderr, "Invalid Aid (%d) for string literal\n", aidno), (AO*)0);
      }
    case 'M': {
	unsigned int aidno = strtoul(c+1, nptr, 10);
	ID * aid = aidmap.lookup(aidno);
	if(aid) return &aid->get_AOMalloc();
	else return (fprintf(stderr, "Invalid Aid (%d) for malloc object\n", aidno), (AO*)0);
      }
    case 'O': {
	AO * o = stringToAO(c+1, &c, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed op-object\n"), (AO*)0);
	TCtype * ty = TCtype::stringToTy(c,nptr);
	if(ty) return &o->get_AOOp(*ty);
	else return 0;
      }
    case 'R': {
	AO * o = stringToAO(c+1, nptr, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed function-return object\n"), (AO*)0);
	else return &o->get_AOReturn();
      }
    case 'S': {
	AO * o = stringToAO(c+1, &c, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed struct-dot object\n"), (AO*)0);
	suco_llist<TCtype *>& tylist = TCtype::stringToTyList(c,nptr);
	return &o->get_or_create_AOSDot(tylist); //- needs to consume tylist
      }
    case 'U': {
	AO * o = stringToAO(c+1, &c, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed union-dot object\n"), (AO*)0);
	TCtype * ty = TCtype::stringToTy(c,nptr);
	if(ty) return &o->get_or_create_AOUDot(*ty); //- needs to consume ty
	else return 0;
      }
    case 'V': {
	TCtype * ty = TCtype::stringToTy(c+1,nptr);
	if(ty) return &valueAOs.get_AOValue(*ty);
	else return (fprintf(stderr, "Malformed Value object\n"), (AO*)0);
      }
    case 'X': {
	AO * o = stringToAO(c+1, nptr, aidmap, pidmap, valueAOs);
	if(!o) return (fprintf(stderr, "Malformed function object\n"), (AO*)0);
	else return &o->get_AOFunction();
      }
//    case '$':
    default : return (fprintf(stderr, "Invalid object identifier (%c)\n", *c), (AO*)0);
  }
}

suco_llist<AO *>& AO::stringToAOlist(bool do_set, char * str, char ** nptr,
				IDmap& aidmap, IDmap& pidmap, AOlist& valueAOs)
{
  suco_llist<AO *>& ret =
		(do_set)?(*new suco_set<AO *>)
			:(*new suco_llist<AO *>);
  char * c = str;
  while(isspace(*c)) c++;
  while(*c == ','){
    AO * ao = stringToAO(c+1, &c, aidmap, pidmap, valueAOs);
    if(ao){
      if(do_set) ((suco_set<AO *>&)ret).Insert(ao);
      else ret.Append(ao);
    } else break;
    while(isspace(*c)) c++;
  }
  if(*c == '.') *nptr = c+1;
  else {
    fprintf(stderr, "Error encountered while reading AOlist.\n");
    *nptr = c;
  }
  return ret;
}

void AO::markVulnerableLocAndPropagate(const char * vuln_fn_name)
{
  if(this->isLoc() && !this->is_vuln_loc){
    this->is_vuln_loc = true;
    this->getECR().markVulnerableDerefAndPropagate();

    //- informational output
//    if(vuln_fn_name){
//      fprintf(stderr, "VULNSTAT(%s): ", vuln_fn_name);
//      this->write_string_rep(stderr, true);
//      fprintf(stderr, "\n");
//    }
  }
}

//- - - - - - - - - - - - - - - - - - - - - -

AOlist::node ** AO::findAO(AOlist::node ** np, aoKind k)
{
  for(; *np; np = &(*np)->next)
    if((*np)->ao.getKind() == k)
      break;
  return np;
}

AO& AOlist::get_AOValue(TCtype &t) //- deletes t if found
{
  node ** np = &head;
  for(; *np; np = &(*np)->next)
    if(((AOValue&)(*np)->ao).getTy().equals(t)){
      TCtype::deleteTy(t);
      return (*np)->ao;
    }
  *np = new node(*new AOValue(t));
  return (*np)->ao;
}

//- - - - - - - - - - - - - - - - - - - - - -

AO * AO::find(aoKind k)
{
  AOlist::node ** np = findAO(&aolist.head, k);
  if(*np) return &(*np)->ao;
  else return 0;
}

AO& AO::get_AOAddrOf()
{
  AOlist::node ** np = findAO(&aolist.head, aoAddrOf);
  if(!*np) *np = new AOlist::node(*new AOAddrOf(*this));
  return (*np)->ao;
}

AO& AO::get_AOFunction()
{
  AOlist::node ** np = findAO(&aolist.head, aoFunction);
  if(!*np) *np = new AOlist::node(*new AOFunction(*this));
  return (*np)->ao;
}

AO& AO::get_AOStar()
{
  AOlist::node ** np = findAO(&aolist.head, aoStar);
  if(!*np) *np = new AOlist::node(*new AOStar(*this));
  return (*np)->ao;
}

AO& AO::get_AOExt(TCtype& t, TCtype& f) //- deletes t,f if found
{
  AOlist::node ** np = findAO(&aolist.head, aoExt);
  while(*np){
    if(((AOExt&)(*np)->ao).getTty().equals(t)
	&& ((AOExt&)(*np)->ao).getFty().equals(f)){
      TCtype::deleteTy(t);
      TCtype::deleteTy(f);
      return (*np)->ao;
    }
    np = findAO(&(*np)->next, aoExt);
  }
  *np = new AOlist::node(*new AOExt(*this, t, f));
  return (*np)->ao;
}

AO& AO::get_AOOp(TCtype &t) //- deletes t if found
{
  AOlist::node ** np = findAO(&aolist.head, aoOp);
  while(*np){
    if(((AOOp&)(*np)->ao).getTy().equals(t)){
      TCtype::deleteTy(t);
      return (*np)->ao;
    }
    np = findAO(&(*np)->next, aoOp);
  }
  *np = new AOlist::node(*new AOOp(*this, t));
  return (*np)->ao;
}

AO& AO::get_AOReturn()
{
  AOlist::node ** np = findAO(&aolist.head, aoReturn);
  if(!*np) *np = new AOlist::node(*new AOReturn(*this));
  return (*np)->ao;
}

AO * AO::get_AOSDot(suco_llist<TCtype *>& l)
{
  AOlist::node * np = *findAO(&aolist.head, aoSDot);
  while(np){
    if(TCtype::listEquiv(((AOSDot&)np->ao).getTyList(), l)){
      return &np->ao;
    }
    np = *findAO(&np->next, aoSDot);
  }
  return 0;
}

AO * AO::get_AOUDot(TCtype &t)
{
  AOlist::node * np = *findAO(&aolist.head, aoUDot);
  while(np){
    if(((AOUDot&)np->ao).getTy().equiv(t)){
      return &np->ao;
    }
    np = *findAO(&np->next, aoUDot);
  }
  return 0;
}

AO * AO::get_AOArg(int n)
{
  AOlist::node ** np = findAO(&aolist.head, aoArg);
  while(*np){
    if(((AOArg&)(*np)->ao).argNo() == n)
      return &(*np)->ao;
    np = findAO(&(*np)->next, aoArg);
  }
  return 0;
}


AO& AO::get_or_create_AOSDot(suco_llist<TCtype *>& l, bool delty) //- deletes l if found
{
  AOlist::node ** np = findAO(&aolist.head, aoSDot);
  while(*np){
    if(TCtype::listEquiv(((AOSDot&)(*np)->ao).getTyList(), l)){
      if(delty) TCtype::deleteTyList(l);
      return (*np)->ao;
    }
    np = findAO(&(*np)->next, aoSDot);
  }
  AO& new_ao = *new AOSDot(*this, l);
  *np = new AOlist::node(new_ao);
  return (*np)->ao;
}

AO& AO::get_or_create_AOUDot(TCtype &t, bool delty) //- deletes t if found
{
  AOlist::node ** np = findAO(&aolist.head, aoUDot);
  while(*np){
    if(((AOUDot&)(*np)->ao).getTy().equiv(t)){
      if(delty) TCtype::deleteTy(t);
      return (*np)->ao;
    }
    np = findAO(&(*np)->next, aoUDot);
  }
  AO& new_ao = *new AOUDot(*this, t);
  *np = new AOlist::node(new_ao);
  return (*np)->ao;
}

AO& AO::get_or_create_AOArg(int n)
{
  AOlist::node ** np = findAO(&aolist.head, aoArg);
  while(*np){
    if(((AOArg&)(*np)->ao).argNo() == n)
      return (*np)->ao;
    np = findAO(&(*np)->next, aoArg);
  }
  *np = new AOlist::node(*new AOArg(*this, n));
  return (*np)->ao;
}

TCtype * AOReturn::getStaticType()
{
  if(AO::getStaticType()){
    return AO::getStaticType();
  } else {
    TCtype * fnty = this->getParent().getStaticType();
    if(fnty && fnty->getKind() == TCtype::tcFunction){
      return &((TCfunctionType *)fnty)->getReturnType();
    }
  }
  return 0;
}

//- - - - - - - - - - - - - - - - - - - - - -

void AOAddrOf::dump_descr(FILE * os)
{
  fprintf(os, "AOAddrOf(");
  ao.dump_descr(os);
  fprintf(os,")");
}

void AOFunction::dump_descr(FILE * os)
{
  fprintf(os, "AOFunction(");
  ao.dump_descr(os);
  fprintf(os,")");
}

void AOStar::dump_descr(FILE * os)
{
  fprintf(os, "AOStar(");
  ao.dump_descr(os);
  fprintf(os,")");
}

void AOExt::dump_descr(FILE * os)
{
  fprintf(os, "AOExt(");
  ao.dump_descr(os);
  fprintf(os,")[");
  tty.debug_dump(os);
  fprintf(os,"<-");
  fty.debug_dump(os);
  fprintf(os,"]");
}

void AOArg::dump_descr(FILE * os)
{
  fprintf(os, "AOArg[%d](", no);
  ao.dump_descr(os);
  fprintf(os,")");
}

void AOId::dump_descr(FILE * os)
{
  fprintf(os, "AOId(%d=[%s])", pid.getkey(), pid.getname());
}

void AOStringLit::dump_descr(FILE * os)
{
  fprintf(os, "AOStringLit(%d=[%s])", aid.getkey(), aid.getname());
}

void AOMalloc::dump_descr(FILE * os)
{
  fprintf(os, "AOMalloc(%d=[%s])", aid.getkey(), aid.getname());
}

void AOOp::dump_descr(FILE * os)
{
  fprintf(os, "AOOp(");
  ao.dump_descr(os);
  fprintf(os,")[");
  ty.debug_dump(os);
  fprintf(os,"]");
}

void AOReturn::dump_descr(FILE * os)
{
  fprintf(os, "AOReturn(");
  ao.dump_descr(os);
  fprintf(os,")");
}

void AOSDot::dump_descr(FILE * os)
{
  fprintf(os, "AOSDot(");
  ao.dump_descr(os);
  fprintf(os,")[");
  TCtype::debug_dump_list(tylist, os);
  fprintf(os,"]");
}

void AOUDot::dump_descr(FILE * os)
{
  fprintf(os, "AOUDot(");
  ao.dump_descr(os);
  fprintf(os,")[");
  ty.debug_dump(os);
  fprintf(os,"]");
}

void AOValue::dump_descr(FILE * os)
{
  fprintf(os, "AOValue[");
  ty.debug_dump(os);
  fprintf(os,"]");
}

//- - - - - - - - - - - - - - - - - - - - - -

void AOAddrOf::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "A ");
  ao.write_string_rep(os, readable);
}

void AOFunction::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "X ");
  ao.write_string_rep(os, readable);
}

void AOStar::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "D ");
  ao.write_string_rep(os, readable);
}

void AOExt::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "E ");
  ao.write_string_rep(os, readable);
  tty.write_string_rep(os);
  fty.write_string_rep(os);
}

void AOArg::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "F %d ", no);
  ao.write_string_rep(os, readable);
}

void AOId::write_string_rep(FILE * os, bool readable)
{
  if(readable){
    fprintf(os, "I[%s]:", pid.getname());
    if(getStaticType()){
      getStaticType()->write_string_rep(os);
    } else fprintf(os, "? ");
  } else {
    fprintf(os, "I %d ", pid.getkey());
  }
}

void AOStringLit::write_string_rep(FILE * os, bool readable)
{
  if(readable){
    fprintf(os, "L <%s> ", aid.getname());
    if(getStaticType()){
      fprintf(os, "[ty: ");
      getStaticType()->write_string_rep(os);
      fprintf(os, "] ");
    }
  } else {
    fprintf(os, "L %d ", aid.getkey());
  }
}

void AOMalloc::write_string_rep(FILE * os, bool readable)
{
  if(readable){
    fprintf(os, "M <%s> ", aid.getname());
    if(getStaticType()){
      fprintf(os, "[ty: ");
      getStaticType()->write_string_rep(os);
      fprintf(os, "] ");
    }
  } else {
    fprintf(os, "M %d ", aid.getkey());
  }
}

void AOOp::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "O ");
  ao.write_string_rep(os, readable);
  ty.write_string_rep(os);
}

void AOReturn::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "R ");
  ao.write_string_rep(os, readable);
}

void AOSDot::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "S ");
  ao.write_string_rep(os, readable);
  TCtype::write_list_string_rep(tylist, os);
  fprintf(os,"; ");
}

void AOUDot::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "U ");
  ao.write_string_rep(os, readable);
  ty.write_string_rep(os);
}

void AOValue::write_string_rep(FILE * os, bool readable)
{
  fprintf(os, "V ");
  ty.write_string_rep(os);
}

//------------------------------------------------------
