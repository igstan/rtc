#include "cfg.h"
#include "diag.h"
#include "ecr.h"
#include "flags.h"	//for flag_range_collect_stats
#include <stdlib.h>	//for strtoul
#include <string.h>	//for strncmp

//----------------------------------
//- RANfact:

RANfact::~RANfact()
{
  clearMap();
}

DFAfact& RANfact::newClone(bool preserve)
{
  RANfact& nrf = *new RANfact;
  nrf.meet(*this, preserve);
  return nrf;
}

void RANfact::setTop()
{
  clearMap();
  is_top = true;
}

void RANfact::setBottom()
{
  clearMap();
  is_top = false;
}

bool RANfact::meet(DFAfact& df, bool preserve, bool warnNotLE)
{
  return extended_meet((RANfact&) df, preserve, warnNotLE, RAN::WN_MEET);
}

bool RANfact::extended_meet(RANfact& rf, bool preserve, bool warnNotLE,
			    enum RAN::wn_mode wn, LocSet * backedge_filter)
{
  if(rf.is_top){ //- meet with top: no op

    if(warnNotLE && !this->is_top){
      fprintf(stderr, "WARNING(RANfact::meet): rf=top, higher than : ");
      this->debug_dump(stderr);
    }

    return false; //- unchanged either way!

  } else if(this->is_top){ //- top meet non-top: copy over

    this->is_top = false;

    if(preserve){
      interval_node ** tnp = &this->map;
      interval_node * rn = rf.map;
      while(rn){
        *tnp = new interval_node(rn->ao, rn->interval, *tnp);
        tnp = &(*tnp)->next;
        rn = rn->next;
      }
    } else {
      this->map = rf.map;
      rf.map = 0;
    }
    return true;

  } else { //- non-top meet non-top
    
    bool this_changed = false;

    if(wn == RAN::WN_NARROW){ //- Narrowing mode: union interval nodes
      //- note: this code was pretty much copy-and-pasted from the join() code
      interval_node ** tnp = &this->map;
      interval_node ** rnp = &rf.map;

      while(*rnp){

        if((!*tnp) || (&(*tnp)->ao > &(*rnp)->ao)){ //- copy rnp into tnp
          if((*rnp)->interval.isBottom()){ //- but not if rnp is bottom
            rnp = &(*rnp)->next;
          } else {
            if(preserve){
              *tnp = new interval_node((*rnp)->ao, (*rnp)->interval, *tnp);
              rnp = &(*rnp)->next;
            } else {
              interval_node * tmp = *rnp;
              *rnp = tmp->next;
              tmp->next = *tnp;
              *tnp = tmp;
            }
            this_changed = true;
            tnp = &(*tnp)->next;
          }
        } else if(&(*tnp)->ao == &(*rnp)->ao){ //- do tnp narrow rnp

          this_changed |= (*tnp)->interval.narrow((*rnp)->interval);
          tnp = &(*tnp)->next;
          rnp = &(*rnp)->next;

        } else { //- if(&(*tnp)->ao < &(*rnp)->ao) //-skip ahead?
          //- narrowing: don't delete
          tnp = &(*tnp)->next;
        }
      }
    } else { //- wn != RAN::NARROW //- Meet or Widen mode: intersect interval nodes

      interval_node ** tnp = &this->map;
      interval_node * rn = rf.map;

      while(*tnp){

        if((!rn) || (&(*tnp)->ao < &rn->ao)){ //- not in rf, delete

          interval_node * tmp = *tnp;
          *tnp = (*tnp)->next;
          delete tmp;

          this_changed = true;

        } else if(&(*tnp)->ao == &rn->ao){ //- interval-meet rn into tnp

           ////////////////////////////////////////
          // main part                          //
          if(wn == RAN::WN_WIDEN){ //----widening----
            if(!backedge_filter || backedge_filter->Contains(rn->ao))
              this_changed |= (*tnp)->interval.widen(rn->interval);
            else
              this_changed |= (*tnp)->interval.meet(rn->interval);
          } else { //- if(wn = RAN::WN_MEET) //----normal meet----
            this_changed |= (*tnp)->interval.meet(rn->interval);
          }
         //                                    //
        ////////////////////////////////////////

          //- cleanup if bottom
          if((*tnp)->interval.isBottom()){
            interval_node * tmp = *tnp;
            *tnp = (*tnp)->next;
            delete tmp;
          } else {
            tnp = &(*tnp)->next;
          }
          rn = rn->next;

        } else { //- if(&(*tnp)->ao > &rn->ao) //- skip ahead
          //- todo: if warnNotLE, report
          rn = rn->next;
        }
      }
    }

    return this_changed;
  }
}

void RANfact::join(DFAfact& df, bool preserve)
{
  RANfact& rf = (RANfact&) df;

  if(this->is_top){ //- already top: do nothing
    //- nop
  } else if(rf.is_top){ //- join with top: set to top

    this->setTop();

  } else { //- non-top join non-top: "union" intervals

    interval_node ** tnp = &this->map;
    interval_node ** rnp = &rf.map;

    while(*rnp){

      if((!*tnp) || (&(*tnp)->ao > &(*rnp)->ao)){ //- absorb rnp into tnp

        if(preserve){
          *tnp = new interval_node((*rnp)->ao, (*rnp)->interval, *tnp);
          rnp = &(*rnp)->next;
        } else {
          interval_node * tmp = *rnp;
          *rnp = tmp->next;
          tmp->next = *tnp;
          *tnp = tmp;
        }
        tnp = &(*tnp)->next;

      } else if(&(*tnp)->ao == &(*rnp)->ao){ //- interval-join rnp into tnp

        (*tnp)->interval.join((*rnp)->interval);

        tnp = &(*tnp)->next;
        rnp = &(*rnp)->next;

      } else { //- if(&(*tnp)->ao < &(*rnp)->ao) //- skip ahead

        tnp = &(*tnp)->next;

      }
    }
  }
}

//-----------------------------------------------------------
// Evaluate the expression ed to get an Interval.
// The expression is encoded in the pair <es,aoi>,
// where es is a string representation of the
// expression (with 'V' and 'S' placeholders for
// location objects), and aoi iterates a list of
// abstract objects mapping (in left-to-right order)
// to the 'V's and 'S' in es.
//=SY-NOTE: actually no need to differentiate S from V?
//
// There are two levels of evaluation:
// - evalSubexpr: returns the Interval for the expressions.
// - evalAddrRange: returns the Interval for what the
//   (lvalue) expression points to.
//-----------------------------------------------------------
Interval RANfact::evalExpr(ExpDescr& ed)
{
  //- estr is either "@" or is surrounded by "{}"
  char * es = ed.getEstr();
  if(es && *es == '{'){
    char * str = es+1;
    suco_iterator<AO *> aoi(ed.getEstrAOs());
    try {
      return evalSubexpr(str, aoi);
    } catch(char * s) {
      fprintf(stderr, "ERROR(RANfact::evalExpr): parse error at position %d in string %s\n", s - es, es);
    }
  }
  return Interval::Bottom;
}

//- lookup ed's deref range, for bounds checking
Interval RANfact::getDerefRangeFor(ExpDescr& ed)
{
  //- estr is either "@" or is surrounded by "{}"
  char * es = ed.getEstr();
  if(es && *es == '{'){
    char * str = es+1;
    suco_iterator<AO *> aoi(ed.getEstrAOs());
    try {
      return evalAddrRange(str, aoi);
    } catch(char * s) {
      fprintf(stderr, "ERROR(RANfact::getDerefRangeFor): parse error at position %d in string %s\n", s - es, es);
    }
  }
  return Interval::Bottom;
}

Interval RANfact::evalAddrRange(char *& s, suco_iterator<AO *>& aoi)
{
  switch(*s++){
    //- non-Lvalues: should never occur
    case 'A': // AddrOf(exp)
    case 'I': // IntConst(li)
    case 'o': // binop(e1,e2), one char
    case 'O': // binop(e1,e2), two chars
    case 'p': // Plus/Minus[+-_#=](e1,e2,elty)
    case 'Q': // QuestionColon(e1,e2,e3)
    case 'R': // RealConst(r)
    case 'T': // Cast(ctype,exp)
    case 'u': // unop(exp), one char
    case 'U': // unop(exp), two chars
    case 'Z': // SizeOf(ty)
      fprintf(stderr, "ERROR(evalAddrRange): rvalue encountered at %s\n", s-1);
      skipArgs(s, aoi);
      break;

    case 'B': { // Sub(e1,e2,elty)
      //- eval(e1) + eval(e2)
      consume(s, '(');
      Interval iv1 = evalSubexpr(s, aoi);
      consume(s, ',');
      Interval iv2 = evalSubexpr(s, aoi);
      consume(s, ',');
      TCtype * cty = TCtype::stringToTy(s, &s);
      if(!cty) throw s;
      consume(++s, ')');
      return iv1.plus(iv2, cty);	//NOTE: if cty is non-trivial, it will
					//      be leaked.  But we cannot delete
					//      it, because it _may_ be consumed
					//	by plus/minus!
    } break;
    case 'D': { // Deref(exp)
      //- eval(exp)
      consume(s, '(');
      Interval iv = evalSubexpr(s, aoi);
      consume(s, ')');
      return iv;
    } break;
    case 'M': { // Member(exp,tylist)
      //- addr(exp) + offset(name)
      consume(s, '(');
      Interval iv = evalAddrRange(s, aoi);
      consume(s, ',');
      suco_llist<TCtype *>& tylist = TCtype::stringToTyList(s, &s);
      consume(++s, ')');
      iv.adjustStructOffset(tylist);
      TCtype::deleteTyList(tylist);
      return iv;
    } break;
    case 'S':   // struct/union member atom
    case 'V': { // Id atom
      if(aoi.Iterate()){
        return Interval(*aoi.Current());
      }
    } break;
    case 'W': { // Arrow(exp,tylist)
      //- eval(exp) + offset(tylist)
      consume(s, '(');
      Interval iv = evalSubexpr(s, aoi);
      consume(s, ',');
      suco_llist<TCtype *>& tylist = TCtype::stringToTyList(s, &s);
      consume(++s, ')');
      iv.adjustStructOffset(tylist);
      TCtype::deleteTyList(tylist);
      return iv;
    } break;
    default: {
      fprintf(stderr, "ERROR(evalAddrRange): unrecognized expression type at %s\n", s-1);
    } break;
  }
  return (this->is_top)?(Interval::Top):(Interval::Bottom);
} // RANfact::evalAddrRange(char *& s, suco_iterator<AO *>& aoi)

Interval RANfact::evalSubexpr(char *& s, suco_iterator<AO *>& aoi, bool skipahead)
{
  switch(*s++){
    case 'A': { // AddrOf(exp)
      if(skipahead) skipArgs(s, aoi);
      else {
        consume(s, '(');
        Interval iv = evalAddrRange(s, aoi);
        consume(s, ')');
        return iv;
      }
    } break;
    case 'B':   // Sub(e1,e2)
    case 'D':   // Deref(exp)
    case 'W': { // Arrow(exp,tylist)
      Interval iv = evalAddrRange(--s, aoi);
      if(iv.inBounds(false) && iv.tgtAO()){
        return this->getInterval(*iv.tgtAO());
      }
    } break;
    case 'I': { // IntConst(li)
      if(skipahead) skipArgs(s, aoi);
      else {
        consume(s, '(');
        int ival = strtoul(s, &s, 10);
        consume(s, ')');
        return Interval(ival);
      }
    } break;
    case 'M': { // Member(exp,tylist)
      //- this case is for exp != loc; when exp == loc, will produce 'S'=struct member object
      skipArgs(s, aoi);
    } break;
    case 'o': { // binop(e1,e2), one char
      if(skipahead){
        s++;
        skipArgs(s, aoi);
      } else {
        char op = *s++;       

        consume(s, '(');
        Interval iv1 = evalSubexpr(s, aoi);
        consume(s, ',');
        Interval iv2 = evalSubexpr(s, aoi);
        consume(s, ')');

        switch(op){
          case '*': // Times
            return iv1.times(iv2);
          case '/': // Divide
            return iv1.divide(iv2);
          case '%': // Mod
            return iv1.modulo(iv2);
          case '>': // Gt
          case '<': // Lt
            break;
          case '|': // BitOr
	    //TODO?
            break;
          case '&': // BitAnd
	    //TODO?
            break;
          case '^': // BitXor
	    //TODO?
            break;
          default: {
            fprintf(stderr, "ERROR(evalSubexpr): unrecognized binop %c before %s\n", op, s);
          } break;
        }
      }
    } break;
    case 'O': { // binop(e1,e2), two chars
      if(skipahead){
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,">=",2)){ // Gte
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"<=",2)){ // Lte
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"==",2)){ // Eq
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"!=",2)){ // Neq
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"&&",2)){ // And
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"||",2)){ // Or
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"<<",2)){ // Lshift
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,">>",2)){ // Rshift
        s += 2;
        skipArgs(s, aoi);
      } else if((!strncmp(s,"*=",2))|| // TimesAssign
		(!strncmp(s,"/=",2))|| // DivAssign
		(!strncmp(s,"%=",2))){ // ModAssign
        //-During evaluation, op-assign are treated as having been effected
        char op = *s;
        s += 2;

        consume(s, '(');
        Interval iv1 = evalSubexpr(s, aoi);
        consume(s, ',');
        Interval iv2 = evalSubexpr(s, aoi);
        consume(s, ')');

        switch(op){
          case '*': // Times
            return iv1.times(iv2);
          case '/': // Divide
            return iv1.divide(iv2);
          case '%': // Mod
            return Interval(0, iv2.minus(Interval(1), &TCtype::tcVoidType).Max());
          default: {
            fprintf(stderr, "ERROR(evalSubexpr): unrecognized assign-binop %c before %s\n", op, s);
          } break;
        }
      } else if(!strncmp(s,"^=",2)){ // XorAssign
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"|=",2)){ // OrAssign
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"&=",2)){ // AndAssign
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"=<",2)){ // LshiftAssign
        s += 2;
        skipArgs(s, aoi);
      } else if(!strncmp(s,"=>",2)){ // RshiftAssign
        s += 2;
        skipArgs(s, aoi);
      } else fprintf(stderr, "ERROR(evalSubexpr): unrecognized Binop at %s\n", s);
    } break;
    case 'p': { // Plus/Minus[+-_#=](e1,e2,elty)
      if(skipahead){
        s++;
        skipArgs(s, aoi);
      } else {
        char op = *s++;       

        consume(s, '(');
        Interval iv1 = evalSubexpr(s, aoi);
        consume(s, ',');
        Interval iv2 = evalSubexpr(s, aoi);
        consume(s, ',');
        TCtype * cty = TCtype::stringToTy(s, &s);
        if(!cty) throw s;
        consume(++s, ')');

        switch(op){
          case '+': // Plus
          case '#': // PlusEquals
            return iv1.plus(iv2, cty);
          case '-': // Minus
          case '=': // MinusEquals
            return iv1.minus(iv2, cty);
          case '_': {// Minus (ptr-ptr)
            return iv1.ptrs_minus(iv2, *cty);
          } break;
          default: {
            fprintf(stderr, "ERROR(evalSubexpr): unrecognized plus/minus binop '%c'\n", op);
          } break;
        }
      }
    } break;
    case 'Q': { // QuestionColon(e1,e2,e3)
      skipArgs(s, aoi);
    } break;
    case 'R': { // RealConst(r)
      skipArgs(s, aoi);
    } break;
    case 'S': { // struct/union member atom
      //- same as 'V' case
      if(aoi.Iterate() && !skipahead){
        Interval riv = this->getInterval(*aoi.Current());
        if(!riv.isBottom()) return riv;
        else {
          //- Optimize, assuming char size is 1 byte
          TCtype * aoty = aoi.Current()->getStaticType();
          if(aoty && (aoty->getKind() == TCtype::tcChar)){
            //-- for now, assume either signedness; TODO: handle signedness
            return Interval(-128,255);
          }
        }
      }
    } break;
    case 'T': { // Cast(ctype,exp)
      if(skipahead) skipArgs(s, aoi);
      else {
        consume(s, '(');

        TCtype * cty = TCtype::stringToTy(s, &s);
        if(!cty) throw s;
        consume(++s, ',');
        Interval iv = evalSubexpr(s, aoi);
        consume(s, ')');

//-TODO: handle type-cast? for now, disregard cast.
        TCtype::deleteTy(*cty);

        return iv;
      }
    } break;
    case 'u': { // unop(exp), one char
      if(skipahead){
        s++;
        skipArgs(s, aoi);
      } else {
        switch(*s++){
          case '+': { // Uplus
            consume(s, '(');
            Interval iv = evalSubexpr(s, aoi);
            consume(s, ')');
            return iv;
          } break;
          case '!': { // Not
            skipArgs(s, aoi);
          } break;
          case '-': { // Negate
            consume(s, '(');
            Interval iv = evalSubexpr(s, aoi);
            consume(s, ')');
            return Interval(0).minus(iv, &TCtype::tcVoidType);
          } break;
          case '~': { // BitNot
            skipArgs(s, aoi);
          } break;
          default: {
            fprintf(stderr, "ERROR(evalSubexpr): unrecognized unop at %s\n", s-1);
          } break;
        }
      }
    } break;
    case 'U': { // unop(exp), two chars
      //-During evaluation, Pre/Post Inc/Dec are treated as having been effected
      if(skipahead){
        s += 2;
        skipArgs(s, aoi);
      } else if((!strncmp(s,"+<",2))|| // PreInc
		(!strncmp(s,"+>",2))){ // PostInc
        s += 2;
        consume(s, '(');
        Interval iv = evalSubexpr(s, aoi);
        consume(s, ',');
        TCtype * cty = TCtype::stringToTy(s, &s);
        if(!cty) throw s;
        consume(++s, ')');
        return iv.plus(Interval(1), cty);
      } else if((!strncmp(s,"-<",2))|| // PreDec
		(!strncmp(s,"->",2))){ // PostDec
        s += 2;
        consume(s, '(');
        Interval iv = evalSubexpr(s, aoi);
        consume(s, ',');
        TCtype * cty = TCtype::stringToTy(s, &s);
        if(!cty) throw s;
        consume(++s, ')');
        return iv.minus(Interval(1), cty);
      } else fprintf(stderr, "ERROR(evalSubexpr): unrecognized Unop at %s\n", s);
    } break;
    case 'V': { // Id atom, or fncall atom (new)
      //- same as 'S' case
      if(aoi.Iterate() && !skipahead){
        Interval riv = this->getInterval(*aoi.Current());
        if(!riv.isBottom()) return riv;
        else {
          //- Optimize, assuming char size is 1 byte
          TCtype * aoty = aoi.Current()->getStaticType();
          if(aoty && (aoty->getKind() == TCtype::tcChar)){
            //-- for now, assume either signedness; TODO: handle signedness
            return Interval(-128,255);
          }
        }
      }
    } break;
    case 'Z': { // SizeOf(ty)
      skipArgs(s, aoi);
    } break;
    default: {
      fprintf(stderr, "ERROR(evalSubexpr): unrecognized expression type at %s\n", s-1);
    } break;
  }
  return (this->is_top)?(Interval::Top):(Interval::Bottom);
} // RANfact::evalSubexpr(char *& s, suco_iterator<AO *>& aoi, bool skipahead)

//-------------------------------------------------------------
// Evaluates the expression <s,aoi> (see comment at evalExpr())
// and compare it to the interval iv; then split the dataflow
// fact pair dfp according to this comparison (true:dfp.fact1,
// false:dfp.fact2).  If the comparison is inconclusive, merge
// dfp into one fact.
// NOTE: the three modes of comparison are eqne(==), ltge(<),
// and gtle(>); to get the negation, just flip dfp.
// RETURNS: true if there is a statically-decidable predicate
//	    (where at least one predicate variable maps to top)
//-------------------------------------------------------------
bool RANfact::evalPredExpr(char * s, suco_iterator<AO *>& aoi, enum cfmode mode, Interval iv, DFAfactPair& dfp)
{
  bool has_top = false;
  switch(*s++){
    //case 'A': // AddrOf(exp)
//TODO
    //case 'B': // Sub(e1,e2)
    //case 'D': // Deref(exp)
    //case 'I': // IntConst(li)
    //case 'M': // Member(exp,tylist)
    case 'p': { // Plus/Minus[+-_#=](e1,e2,elty)
      char c = *s++;
      switch(c){
        case '+':   // Plus
        case '-':   // Minus
        case '_': { // Minus (ptr-ptr)

          consume(s, '(');
          char * s1 = s;
          suco_iterator<AO *> aoi1(aoi);
          Interval iv1 = this->evalSubexpr(s, aoi);
          consume(s, ',');
          char * s2 = s;
          suco_iterator<AO *> aoi2(aoi);
          Interval iv2 = this->evalSubexpr(s, aoi);
          consume(s, ',');
          TCtype * cty = TCtype::stringToTy(s, &s);
          if(!cty) throw s;
          consume(++s, ')');

          //-- cases: int + int < int   int - int < int
          //          ptr + int < ptr   ptr - int < ptr   ptr - ptr < int
          //          ---------------   ---------------   ---------------
          //          ip1 + ii2 < ip    ip1 - ii2 < ip    p1 - p2  < i
          //- case A: ip1 < ip - ii2    ip1 < ip  + ii2   p1 < p2 + i
          //- case B: ii2 < ip - ip1    ii2 > ip1 - ip    p2 > p1 - i
          // 
          //- REMEMBER: ptr-arith is now non-commutative, so int+ptr
          //            must be normalized to ptr+int.

          Interval ivA;
          Interval ivB;
          enum cfmode modeB = mode;
          switch(c){
            case '+': { // Plus
              ivA = iv.minus(iv2, cty);
              ivB = iv.minus(iv1, cty);
            } break;
            case '-': { // Minus
              ivA = iv.plus(iv2, cty);
              if(cty->getKind() == TCtype::tcVoid){ // plain arith
                ivB = iv1.minus(iv, cty);
              } else { // ptr arith
                ivB = iv1.ptrs_minus(iv, *cty);
              }
              modeB = flipmode(mode);
            } break;
            case '_': { // Minus (ptr-ptr)
              ivA = iv2.plus(iv, cty);
              ivB = iv1.minus(iv, cty);
              modeB = flipmode(mode);
            } break;
          }
          if(!ivA.isBottom())
            has_top |= evalPredExpr(s1, aoi1, mode, ivA, dfp);
          if(!ivB.isBottom())
            has_top |= evalPredExpr(s2, aoi2, modeB, ivB, dfp);

        } break;
        case '#':   // PlusEquals
        case '=': { // MinusEquals
          //-During predicate evaluation, op-assign are treated as nops
          // (actually, should never occur in predicate node?)
/**/      fprintf(stderr, "WARNING(RANfact::evalPredExpr): encountered op-assign in estr %s\n", s);
          consume(s, '(');
          has_top |= evalPredExpr(s, aoi, mode, iv, dfp);
        } break;
      }
    } break;

    case 'o': { // binop(e1,e2), one char
      char c = *s++;
      switch(c){
        case '>':   // Gt
        case '<': { // Lt
          if((mode == m_eqne) && (iv == Interval(0))){

            consume(s, '(');
            char * s1 = s;
            suco_iterator<AO *> aoi1(aoi);
            Interval iv1 = this->evalSubexpr(s, aoi);
            consume(s, ',');
            char * s2 = s;
            suco_iterator<AO *> aoi2(aoi);
            Interval iv2 = this->evalSubexpr(s, aoi);
            consume(s, ')');

            dfp.meetIfPair(RANfactHandler::handler); //- redundant?

            if(!iv2.isBottom()){
              has_top |= evalPredExpr(s1, aoi1, (c=='<')?m_ltge:m_gtle, iv2, dfp);
            }
            if(!iv1.isBottom()){
              has_top |= evalPredExpr(s2, aoi2, (c=='<')?m_gtle:m_ltge, iv1, dfp);
            }

            //- REMEMBER: since the outer condition is ==0, we need to do here the complement!
            dfp.flipFacts(RANfactHandler::handler);

          } //- else skipahead: cannot handle!
        } break;
        //case '*': // Times
        //case '/': // Divide
        //case '%': // Mod
        //case '|': // BitOr
        //case '&': // BitAnd
        //case '^': // BitXor
        default: break;
      }
    } break;
    case 'O': { // binop(e1,e2), two chars
      if((!strncmp(s,">=",2))|| // Gte
	 (!strncmp(s,"<=",2))|| // Lte
	 (!strncmp(s,"==",2))|| // Eq
	 (!strncmp(s,"!=",2))){ // Neq

        char c = *s;
        s += 2;

        if((mode == m_eqne) && (iv == Interval(0))){

          consume(s, '(');
          char * s1 = s;
          suco_iterator<AO *> aoi1(aoi);
          Interval iv1 = this->evalSubexpr(s, aoi);
          consume(s, ',');
          char * s2 = s;
          suco_iterator<AO *> aoi2(aoi);
          Interval iv2 = this->evalSubexpr(s, aoi);
          consume(s, ')');

          dfp.meetIfPair(RANfactHandler::handler); //- redundant?

          //- REMEMBER: since the outer condition is ==0, we need to do here the complement!
          if(!iv2.isBottom()){
            switch(c){
              case '>': has_top |= evalPredExpr(s1, aoi1, m_ltge, iv2, dfp);
                        break;
              case '<': has_top |= evalPredExpr(s1, aoi1, m_gtle, iv2, dfp);
                        break;
              case '=': dfp.flipFacts(RANfactHandler::handler); //- nop; but left here for symmetry with below
                        has_top |= evalPredExpr(s1, aoi1, m_eqne, iv2, dfp);
                        dfp.flipFacts(RANfactHandler::handler);
                        break;
              case '!': has_top |= evalPredExpr(s1, aoi1, m_eqne, iv2, dfp);
                        break;
            }
          }
          if(!iv1.isBottom()){
            switch(c){
              case '>': has_top |= evalPredExpr(s2, aoi2, m_gtle, iv1, dfp);
                        break;
              case '<': has_top |= evalPredExpr(s2, aoi2, m_ltge, iv1, dfp);
                        break;
              case '=': dfp.flipFacts(RANfactHandler::handler);
                        has_top |= evalPredExpr(s2, aoi2, m_eqne, iv1, dfp);
                        dfp.flipFacts(RANfactHandler::handler);
                        break;
              case '!': has_top |= evalPredExpr(s2, aoi2, m_eqne, iv1, dfp);
                        break;
            }
          }
        } //- else skipahead: cannot handle!
      } else if(!strncmp(s,"&&",2)){ // And
        s += 2;
        if((mode == m_eqne) && (iv == Interval(0))){
          consume(s, '(');
          char * s1 = s;
          suco_iterator<AO *> aoi1(aoi);
          this->evalSubexpr(s, aoi, /*skipahead=*/true);
          consume(s, ',');

          dfp.meetIfPair(RANfactHandler::handler); //- redundant?

          //- to handle (p1 && p2) == 0   equiv  (p1==0) || (p2==0)
          //  (p1 is s1/aoi1, p2 is s/aoi)
          //- First, do (p1==0) to get T1/F1
          has_top |= evalPredExpr(s1, aoi1, m_eqne, Interval(0), dfp);

          DFAfact * t1 = 0;
          if(dfp.getFact2()){
            dfp.flipFacts(RANfactHandler::handler);
            t1 = dfp.getFact2();
            dfp.resetFact2();
          } else {
            t1 = &dfp.getFact1().newClone();
          }

          //- next, feed F1 into (p2 == 0) to get T2/F2
          has_top |= evalPredExpr(s, aoi, m_eqne, Interval(0), dfp);

          //- finally, T/F == (T1 meet T2 / F2)
          dfp.getFact1().meet(*t1, false);

          RANfactHandler::handler.deleteFact(*t1); //- discard t1
        }
      } else if(!strncmp(s,"||",2)){ // Or
        s += 2;
        if((mode == m_eqne) && (iv == Interval(0))){
          consume(s, '(');
          char * s1 = s;
          suco_iterator<AO *> aoi1(aoi);
          this->evalSubexpr(s, aoi, /*skipahead=*/true);
          consume(s, ',');

          dfp.meetIfPair(RANfactHandler::handler); //- redundant?

          //- to handle (p1 || p2) == 0   equiv  (p1==0) && (p2==0)
          //  (p1 is s1/aoi1, p2 is s/aoi)
          //- First, do (p1==0) to get T1/F1
          has_top |= evalPredExpr(s1, aoi1, m_eqne, Interval(0), dfp);

          DFAfact * f1 = 0;
          if(dfp.getFact2()){
            f1 = dfp.getFact2();
            dfp.resetFact2();
          } else {
            f1 = &dfp.getFact1().newClone();
          }

          //- next, feed T1 into (p2 == 0) to get T2/F2
          has_top |= evalPredExpr(s, aoi, m_eqne, Interval(0), dfp);

          //- finally, T/F == (T2 / F1 meet F2)
          dfp.splitIfSingle();
          dfp.getFact2()->meet(*f1, false);

          RANfactHandler::handler.deleteFact(*f1); //- discard f1
        }
      // } else if(!strncmp(s,"<<",2)){ // Lshift
      // } else if(!strncmp(s,">>",2)){ // Rshift
      } else if((!strncmp(s,"*=",2))|| // TimesAssign
		(!strncmp(s,"/=",2))|| // DivAssign
		(!strncmp(s,"%=",2))|| // ModAssign
		(!strncmp(s,"^=",2))|| // XorAssign
		(!strncmp(s,"|=",2))|| // OrAssign
		(!strncmp(s,"&=",2))|| // AndAssign
		(!strncmp(s,"=<",2))|| // LshiftAssign
		(!strncmp(s,"=>",2))){ // RshiftAssign
        //-During predicate evaluation, op-assign are treated as nops
        // (actually, should never occur in predicate node?)
/**/    fprintf(stderr, "WARNING(RANfact::evalPredExpr): encountered op-assign in estr %s\n", s);
        s += 2;
        consume(s, '(');
        has_top |= evalPredExpr(s, aoi, mode, iv, dfp);
      }
    } break;
    //case 'Q': // QuestionColon(e1,e2,e3)
    //case 'R': // RealConst(r)
    case 'T': { // Cast(ctype,exp)
      char * es = strchr(s, ',');
      if(es){
        consume(es, ',');
        has_top |= evalPredExpr(es, aoi, mode, iv, dfp);
      } else {
        fprintf(stderr, "ERROR(RANfact::evalPredExpr): expecting comma after type in cast string %s\n", s);
        throw s;
      }
    } break;
    case 'u': { // unop(exp), one char
      switch(*s++){
        case '!': { // Not
          if((mode == m_eqne) && (iv == Interval(0))){
            consume(s, '(');
            dfp.meetIfPair(RANfactHandler::handler); //- redundant?
            has_top |= evalPredExpr(s, aoi, m_eqne, iv, dfp);
            dfp.flipFacts(RANfactHandler::handler);
          } //- else skipahead: cannot handle!
        } break;
        case '+':{ // Uplus
          consume(s, '(');
          has_top |= evalPredExpr(s, aoi, mode, iv, dfp);
        } break;
        case '-':{ // Negate
          consume(s, '(');
          has_top |= evalPredExpr(s, aoi, flipmode(mode), Interval(0).minus(iv, &TCtype::tcVoidType), dfp);
        } break;
        //case '~': // BitNot
        default: break;
      }
    } break;
    case 'U': { // unop(exp), two chars
      //-During predicate evaluation Pre/Post Inc/Dec are treated as nops
      // (actually, should never occur in predicate node?)
/**/  fprintf(stderr, "WARNING(RANfact::evalPredExpr): encountered pre/post inc/dec in estr %s\n", s);
      //if(!strncmp(s,"+<",2)) // PreInc
      //if(!strncmp(s,"+>",2)) // PostInc
      //if(!strncmp(s,"-<",2)) // PreDec
      //if(!strncmp(s,"->",2)) // PostDec
      s += 2;
      consume(s, '(');
      has_top |= evalPredExpr(s, aoi, mode, iv, dfp);
    } break;
    case 'S':   // struct/union member atom
    case 'V': { // Id atom
      if(aoi.Iterate()){
        AO& ao = *aoi.Current();

        if(!iv.isBottom()){
          dfp.splitIfSingle();
          RANfact& trf = (RANfact&) dfp.getFact1();
          RANfact& frf = (RANfact&) *dfp.getFact2();

//-TODO: this is very hackish, including the Interval(Interval,int,int)
//	 constructor: find better solution!

          //- compute min/max for true/false branches
          Interval iv_true, iv_false;

          switch(mode){
            case m_ltge:
              iv_true = Interval(iv, Interval::MINUS_INF
				   , iv.minus(Interval(1), 0).Max());
              iv_false = Interval(iv, iv.Min()
				    , Interval::PLUS_INF);
              break;
            case m_gtle:
              iv_true = Interval(iv, iv.plus(Interval(1), 0).Min()
				   , Interval::PLUS_INF);
              iv_false = Interval(iv, Interval::MINUS_INF
				    , iv.Max());
              break;
            case m_eqne:
              iv_true = iv;
              iv_false.setBottom();
              break;
          }

          //- join interval for true branch
          //NOTE: if ao is a return node, DON'T UPDATE its interval
          //	  in dfp, because it may be used again, which would
          //      correspond to a separate call (i.e., return nodes
          //      are not mutable w.r.t. the analysis).
          //      So, we'll just check for emptyness (top).
          if(!iv_true.isBottom()){
            interval_node ** tin = trf.lookupIntervalPosn(ao);
            if(*tin && (&(*tin)->ao == &ao)){
              Interval tmp = (*tin)->interval;
              Interval& aoiv = (ao.getEnclosingStruct().getKind() == AO::aoReturn)
				? (tmp) : ((*tin)->interval);
              Interval old = aoiv;
              aoiv.cond_intersect(iv_true);
              if(aoiv.isTop()){
                has_top = true;
                if(flag_debug_range_collect_phase){
                  fprintf(stderr, "ISTOP: ");
                  ao.write_string_rep(stderr, true);
                  fprintf(stderr, " -> ");
                  old.debug_dump(stderr);
                  fprintf(stderr, " INTERSECT ");
                  iv_true.debug_dump(stderr);
                  fprintf(stderr, "\n");
                }
              }
            } else if(ao.dfa_relevant
			&& (ao.getEnclosingStruct().getKind() != AO::aoReturn)	//- skip if return node
			&& (!trf.is_top || !iv_true.isTop())){			//- skip if top/top
              *tin = new interval_node(ao, iv_true, *tin);
            }
          }

          //- join interval for false branch
          if(!iv_false.isBottom()){
            interval_node ** fin = frf.lookupIntervalPosn(ao);
            if(*fin && (&(*fin)->ao == &ao)){
              Interval tmp = (*fin)->interval;
              Interval& aoiv = (ao.getEnclosingStruct().getKind() == AO::aoReturn)
				? (tmp) : ((*fin)->interval);
              Interval old = aoiv;
              aoiv.cond_intersect(iv_false);
              if(aoiv.isTop()){
                has_top = true;
                if(flag_debug_range_collect_phase){
                  fprintf(stderr, "ISTOP: ");
                  ao.write_string_rep(stderr, true);
                  fprintf(stderr, " -> ");
                  old.debug_dump(stderr);
                  fprintf(stderr, " INTERSECT ");
                  iv_false.debug_dump(stderr);
                  fprintf(stderr, "\n");
                }
              }
            } else if(ao.dfa_relevant
			&& (ao.getEnclosingStruct().getKind() == AO::aoReturn)	//- skip if return node
			&& (!frf.is_top || !iv_false.isTop())){			//- skip if top/top
              *fin = new interval_node(ao, iv_false, *fin);
            }
          }
        }
      }
    } break;
    //case 'W': // Arrow(exp,tylist)
    //case 'Z': // SizeOf(ty)
    default:
      break;
  }
  return has_top;
} // RANfact::evalPredExpr(char * s, suco_iterator<AO *>& aoi, enum cfmode mode, Interval iv, DFAfactPair& dfp)

void RANfact::consume(char *& s, char c)
{
  if(*s == c) s++;
  else {
    fprintf(stderr, "ERROR(RANfact::consume): expecting char %c at %s\n", c, s);
    throw (char *)s;
  }
}

//- NOTE: relies on the fact that 'S' and 'V' always refer to AO!
void RANfact::skipArgs(char *& s, suco_iterator<AO *>& aoi)
{
  if(*s == '('){
    int slevel = 1;
    while(*++s){
      if(*s == 'S' || *s == 'V') aoi.Iterate();
      if(*s == '(') slevel++;
      if(*s == ')' && !(--slevel)){
        s++;
        return;
      }
    }
    fprintf(stderr, "ERROR(RANfact::skipArgs): premature end-of-string\n");
    throw (char *)s;
  }
}

void RANfact::debug_dump(FILE * outf, bool brief)
{
  if(this->is_top) fprintf(outf, "top\n");
  else {
    fprintf(outf, "\n");
    interval_node * mp = this->map;
    while(mp){
      fprintf(outf, "\t");
      mp->interval.debug_dump(outf);
      fprintf(outf, ":");
      mp->ao.dump_descr(outf);
      fprintf(outf, "\n");
      mp = mp->next;
    }
  }
}

RANfact::interval_node ** RANfact::lookupIntervalPosn(AO& ao)
{
  interval_node ** mp = &this->map;
  while((*mp) && (&(*mp)->ao < &ao))
    mp = &(*mp)->next;
  return mp;
}

//- if ao is struct, update componentwise
void RANfact::updateInterval(AO& ao, Interval iv, TCtype * aoty)
{
  if(this->is_top && iv.isTop()) return;
  this->is_top = false;

  if(!aoty) aoty = ao.getStaticType();
  if(aoty && aoty->getKind() == TCtype::tcStruct){

    suco_iterator<TCtype *> tli(((TCstructUnionType *)aoty)->getTypeList());
    suco_llist<TCtype *> tlist;
    while(tli.Iterate()){
      tlist.Append(tli.Current());
      AO * sdao = ao.get_AOSDot(tlist);
      if(sdao){
        if(sdao->dfa_relevant){ //- filter
          this->updateInterval(*sdao, iv, tli.Current());
        }
      } else {
        if(flag_instantiate_structunion){
/**/      fprintf(stderr, "ERROR(RANfact::updateInterval): aoSdot not found: ao = ");
/**/      ao.write_string_rep(stderr, true);
/**/      fprintf(stderr, "  tylist = ");
/**/      TCtype::write_list_string_rep(tlist, stderr);
/**/      fprintf(stderr, "\n");
        }
      }
    }

  } else if(aoty && aoty->getKind() == TCtype::tcArray){
    if(ao.dfa_relevant){ //- filter
      this->updateInterval(ao, iv, &((TCnumType *)aoty)->getBaseType());
    }
  } else {
    if(ao.dfa_relevant){ //- filter
      if(iv.isBottom()){
        deleteInterval(ao);
      } else {
        interval_node ** mp = lookupIntervalPosn(ao);
        if(*mp && (&(*mp)->ao == &ao)){
          (*mp)->interval = iv;
        } else {
          *mp = new interval_node(ao, iv, *mp);
        }
      }
    }
  }
}

void RANfact::deleteInterval(AO& ao)
{
  interval_node ** mp = lookupIntervalPosn(ao);
  if(*mp && (&(*mp)->ao == &ao)){
    interval_node * tmp = *mp;
    *mp = (*mp)->next;
    delete tmp;
  }
}

Interval RANfact::getInterval(AO& ao)
{
  if(this->is_top) return Interval::Top;
  interval_node * mp = *lookupIntervalPosn(ao);
  if(mp && (&mp->ao == &ao))
    return mp->interval;
  else return Interval::Bottom;
}

//- for each fact whose ao.ecr is in ecrset, meet with iv
//TODO: handle top?
void RANfact::meetIntervals(suco_set<ECR *>& ecrset, Interval iv)
{
  interval_node ** mp = &this->map;
  while(*mp){
    if(ecrset.Contains(&(*mp)->ao.getECR())){
      (*mp)->interval.meet(iv); //- meet with iv
      //- cleanup if bottom
      if((*mp)->interval.isBottom()){
        interval_node * tmp = *mp;
        *mp = (*mp)->next;
        delete tmp;
      } else {
        mp = &(*mp)->next;
      }
    } else {
      mp = &(*mp)->next;
    }
  }
}

void RANfact::removeIntervalsPointingTo(suco_set<ECR *>& tgt_ecrset)
{
  interval_node ** mp = &this->map;
  while(*mp){
    if((!(*mp)->interval.isNullTgted())	//- not null-targeted
	&& (((*mp)->interval.tgtAO())	//- do we have "precise" points-to information from the interval?
	   ? (tgt_ecrset.Contains(&(*mp)->interval.tgtAO()->getECR()))	//- if so: use it
	   : ((*mp)->ao.getECR().getPointsTo().getAliasECRs().Intersects(tgt_ecrset)))){ //- if not, revert to flow-insens pt-set
      //- delete interval
      interval_node * tmp = *mp;
      *mp = (*mp)->next;
      delete tmp;
    } else {
      mp = &(*mp)->next;
    }
  }
}

//- "Disable" any interval whose target component is ao-based.
//  Return true if any disabled
bool RANfact::disableIntervalsWithTarget(AO& ao)
{
  bool changed = false;
  interval_node ** mp = &this->map;
  while(*mp){
    changed |= (*mp)->interval.disableIfTargetIsPartOf(ao);
    if((*mp)->interval.isBottom()){ //- cleanup if bottom
      interval_node * tmp = *mp;
      *mp = (*mp)->next;
      delete tmp;
    } else {
      mp = &(*mp)->next;
    }
  }
  return changed;
}

int RANfact::countIntervals() const
{
  int ret = 0;
  interval_node * n;
  for(n = this->map; n; n = n->next) ret++;
  return ret;
}

void RANfact::clearMap()
{
  while(this->map){
    interval_node * tmp = this->map;
    this->map = this->map->next;
    delete tmp;
  }
}

//- This is an intertwining of two similar functions with opposite behaviors:
//  - mode 1: if in_fact != 0, then
//	Move facts in GREF/GMayFree from this to in_fact.
//	After, this and in_fact will be disjoint sets whose union
//	is the original this.
//	The dual of this function is reconstitute().
//  - mode 2: if in_fact == 0, then
//	keep only facts in GREF/GMayFree; delete the rest.
void RANfact::filterGrefMayFreeInto(CFGfunction& fn, RANfact * in_fact)
{
  interval_node ** mp = &this->map;
  interval_node ** fmp = in_fact?(&in_fact->map):0;
  while(*mp){
    if(fn.getGREF().Contains((*mp)->ao) || fn.GFreeHeapAffects((*mp)->ao)){ //- in GREF or GMayFree
      if(fmp){	//- mode 1: move to in_fact
        interval_node * tmp = *mp;
        *mp = (*mp)->next;
        while(*fmp && (&(*fmp)->ao < &tmp->ao)){
          fmp = &(*fmp)->next;
        }
        //- assume no clash (todo: assert?)
        tmp->next = *fmp;
        *fmp = tmp;
      } else {	//- mode 2: keep
        mp = &(*mp)->next;
      }
    } else { //- not in GREF or GMayFree
      if(fmp){	//- mode 1: keep
        mp = &(*mp)->next;
      } else {	//- mode 2: delete
        interval_node * tmp = *mp;
        *mp = (*mp)->next;
        delete tmp;
      }
    }
  }
}

//- Do a union of nodes from this and rf_local, which should be disjoint.
//  NEW: they may not be disjoint with function pointers:
//	foo1:  call fp -> {bar1,bar2}, gref_cumulative={x,y}
//	bar1:  gref={x}, but from foo1 callsite, may return {x,y}
//	foo2:  call bar1, gref={x}
//		~~> may reconstitute two {y}s
//  In such a situation, rf_local version should be preferred!
void RANfact::reconstitute(RANfact& rf_local)
{
  interval_node ** tnp = &this->map;
  interval_node ** rnp = &rf_local.map;

  while(*rnp){

    if((!*tnp) || (&(*tnp)->ao >= &(*rnp)->ao)){ //- copy rnp into tnp
      if((*tnp) && (&(*tnp)->ao == &(*rnp)->ao)){ //- a clash: delete tnp, then copy rnp into tnp
        interval_node * tmp = *tnp;
        *tnp = (*tnp)->next;
        delete tmp;
      }

      //- copy rnp into tnp
      if((*rnp)->interval.isBottom()){ //- but not if rnp is bottom
        rnp = &(*rnp)->next;
      } else {
        interval_node * tmp = *rnp;
        *rnp = tmp->next;
        tmp->next = *tnp;
        *tnp = tmp;
        tnp = &(*tnp)->next;
      }

    } else { //- if(&(*tnp)->ao < &(*rnp)->ao) //-skip ahead
      tnp = &(*tnp)->next;
    }
  }
}

//----------------------------------
//- RANfactHandler

RANfactHandler RANfactHandler::handler;

DFAfact& RANfactHandler::newTopFact()
{
  return *new RANfact;
}

void RANfactHandler::deleteFact(DFAfact& df)
{
  delete (RANfact*) &df;
}

//- this function filters df into df (cached and used in call node)
//	and df_local (fallthrough, bypasses call).
//  the two pieces are recombined by reconstituteFilteredFacts() below.
//- default behavior is: here, split set into df, df_local, and reconstitute with "meet"
//- but for RAN, since absense of a fact implies bottom, and the idea behind this filter
//  is to optimize the number of facts to pass around, we instead do:
//  - here, split into df and df_local, which must be disjoint sets;
//    df contains only facts whose AO intersect with the function's GREF*
//    (*new complication: "intersect with GREF" now means "intersect with GREF or
//      points to something in GFreeHeap")
//    -- in effect, the filtered-out facts are converted from "top" to "bottom"
//    (this is OK since, not being in any GREF, they'll have no effect on the callees).
//  - below, reconstituteFilteredFacts will do a (disjoint) union of df and
//    df_local, and will complain if they are not disjoint.
// (NOTE: old implementation tried to piggyback on NARROW function to do the
//  reconstitution -- it just made things more complicated.)
void RANfactHandler::filterCallsiteFacts(PExprCall& dc, DFAfact& df, DFAfact& df_local)
{
  RANfact& ranf = (RANfact&) df;
  RANfact& ranf_local = (RANfact&) df_local;
  
  //- 1. move all facts to ranf_local
  ranf_local.meet(ranf, false);

  //- 2. filter elements satisfying GREF/GFreeHeap constraints back into ranf
  suco_iterator<CFGfunction *> tfi(dc.getTargetFns());
  while(tfi.Iterate()){
    ranf_local.filterGrefMayFreeInto(*tfi.Current(), &ranf);
  }

  //- ??. remove R D I <fn>
  //      (previously, could not remove, as it may be an actual argument to this call;
  //	   but now that args are pre-evaluated, it should no longer be needed)
  suco_iterator<AO *> faoi(dc.getFaos()); //- note: difference between getFaos and getTargetFns!
  while(faoi.Iterate()){
    ranf_local.updateInterval(faoi.Current()->get_AOStar().get_AOReturn(), Interval::Bottom);
  }
}

void RANfactHandler::interProcHandleCallArgs(DFAfact& df, PExprCall& dc)
{
  RANfact& rf = (RANfact&) df;
  //- for each tgtfn, assign into F X I <tgtfn>
  suco_iterator<CFGfunction *> tfi(dc.getTargetFns());
  while(tfi.Iterate()){
    AOFunction& tgtfn_ao = (AOFunction&) tfi.Current()->getId().get_AOFunction();
    int lhs_largno = tgtfn_ao.getLargNo();
    for(int i = 1; dc.getArg(i); ++i){
      bool upto_largno = (i <= lhs_largno); //- up to (and including) largno: strong assign; after, weak assign (takes care of varargs)
      AO * arg_ao = tgtfn_ao.get_AOArg(upto_largno?i:lhs_largno);
      if(arg_ao){
        ExpDescr& actual_edesc = dc.getArg(i)->getDesc();
        //- special handling of structs: precisely handle only if actual is 'S' or 'V'
        if((!strcmp(actual_edesc.getEstr(),"{S}")) || (!strcmp(actual_edesc.getEstr(),"{V}"))){
          AO& rhs_ao = *actual_edesc.getEstrAOs().Head();
          TCtype * sty = rhs_ao.getStaticType();
          if(sty) rf.handleStructAssign(*arg_ao, *sty, &rhs_ao, upto_largno);
          else {
            if(arg_ao->dfa_relevant){ // minor optimization (non-struct, can bypass if irrelevant)
              Interval actual_iv = rf.evalExpr(actual_edesc);
              if(!upto_largno && !actual_iv.isBottom()) //- weak-assign!
                actual_iv.meet(rf.getInterval(*arg_ao));
              rf.updateInterval(*arg_ao, actual_iv);
            }
          }
        } else {
          Interval actual_iv = rf.evalExpr(actual_edesc);
          if(!upto_largno && !actual_iv.isBottom()) //- weak-assign!
            actual_iv.meet(rf.getInterval(*arg_ao));
          rf.updateInterval(*arg_ao, actual_iv);
        }
      } else {
        fprintf(stderr, "ERROR(RAN::interProcHandleCallArgs): arg(%d) not found for ", i);
        tgtfn_ao.dump_descr(stderr);
        fprintf(stderr, "\n");
      }
    }
  }
}

void RANfact::collectAOset(suco_set<AO *>& aoset)
{
  interval_node * mp = this->map;
  while(mp){
    aoset.Insert(&mp->ao);
    mp = mp->next;
  }
}

//- add or set a top-mapped node for each variable in aoset
//- called by RANfactHandler::interProcPrepareReturnCollector()
void RANfact::createTopMappings(suco_set<AO *>& aoset)
{
  if(aoset.IsEmpty()) return;
  if(this->is_top) this->is_top = false;

  suco_iterator<AO *> aoi(aoset);
  while(aoi.Iterate()){
    this->updateInterval(*aoi.Current(), Interval::Top);
  }
}

//- map the following to top:
//  - a variable that occurs in _ALL_ of dc's return-nodes' facts
//  - callee retval nodes (R X I <tgtfn>)
//- these top facts will subsequently be window-meeted with retval
//  node results.
void RANfactHandler::interProcPrepareReturnCollector(DFAfact& df, PExprCall& dc)
{
  RANfact& rf = (RANfact&) df;

  suco_set<AO *> intersect_aos;
  suco_set<AO *> retval_aos;
  bool first_time = true;
  bool all_top = true;

  suco_iterator<CFGfunction *> tfi(dc.getTargetFns());
  while(tfi.Iterate()){
    retval_aos.Insert(&tfi.Current()->getId().get_AOFunction().get_AOReturn());
    suco_iterator<PgmStmt *> eni(tfi.Current()->getExitNodes());
    while(eni.Iterate()){
      CFGnode * retnode = eni.Current()->getCFGactiveNode();
      //-- meet with return-node fact
      if(retnode){ //- retnode may be null bblock, if unreachable
        RANfact& rf_retnode = (RANfact&) this->lookupNodeFact(*retnode);
        all_top &= rf_retnode.isTop();
        if(!rf_retnode.isTop()){ //- if node is top, aoset should be "all"
          if(first_time){
            first_time = false;
            rf_retnode.collectAOset(intersect_aos);
          } else {
            suco_set<AO *> local_aos;
            rf_retnode.collectAOset(local_aos);
            intersect_aos.Intersect(local_aos);
          }
        }
      }
    }
  }
  if(all_top) { //- special treatment: if rhs is all top
    rf.setTop();
  } else {
    rf.createTopMappings(intersect_aos);
    rf.createTopMappings(retval_aos);
  }
}

//- meet df into this, but filter in only facts that are tgtfn_relevant,
//  namely tgtfn's GREF set (including R X I <tgtfn>) and MayFree set.
//  For everying outside of this set, keep the old version in this.
void RANfact::meetFiltered(DFAfact& df, CFGfunction& tgtfn)
{
  RANfact& rf = (RANfact&) df;

  if(rf.is_top){ //- meet with top: no op
    return;
  } else if(this->is_top){ //- top meet sth = sth (filtered by GREF/GMayFree)

    this->is_top = false;
    interval_node ** tnp = &this->map;
    interval_node * rn = rf.map;
    while(rn){
      if(tgtfn.getGREF().Contains(rn->ao) || tgtfn.GFreeHeapAffects(rn->ao)){
        *tnp = new interval_node(rn->ao, rn->interval, *tnp);
        tnp = &(*tnp)->next;
      }
      rn = rn->next;
    }

  } else { //- non-top meet non-top
    
    interval_node ** tnp = &this->map;
    interval_node * rn = rf.map;

    while(*tnp){

      if((!rn) || (&(*tnp)->ao < &rn->ao)){ //- in this, not in rf
        if(tgtfn.getGREF().Contains((*tnp)->ao) || tgtfn.GFreeHeapAffects((*tnp)->ao)){ //- if in GREF or GMayFree, delete
          interval_node * tmp = *tnp;
          *tnp = (*tnp)->next;
          delete tmp;
        } else {
          tnp = &(*tnp)->next;
        }

      } else if(&(*tnp)->ao == &rn->ao){ //- interval-meet rn into tnp

        if(tgtfn.getGREF().Contains((*tnp)->ao) || (tgtfn.GFreeHeapAffects((*tnp)->ao))){
	  //- if in GREF or GMayFree, meet	(check may be redundant)
          (*tnp)->interval.meet(rn->interval);
        }

        //- cleanup if bottom
        if((*tnp)->interval.isBottom()){
          interval_node * tmp = *tnp;
          *tnp = (*tnp)->next;
          delete tmp;
        } else {
          tnp = &(*tnp)->next;
        }
        rn = rn->next;

      } else { //- if(&(*tnp)->ao > &rn->ao) //- in rf, not in this; skip ahead
        rn = rn->next;
      }
    }
  }
}

//- assign to "R D <fao>" from "R X I <tgtfn>"
//  also, remove "R X I <tgtfn>" mapping
void RANfactHandler::interProcHandleRetvalAssign(DFAfact& df, PExprCall& dc)
{
  RANfact& rf = (RANfact&) df;
  bool strong_assign = (dc.getTargetFns().Length() == 1);

  //- if weak assign: first, set "R D <fao>" to top
  if(!strong_assign){
    suco_iterator<AO *> faoi(dc.getFaos()); //- should usually be singleton
    while(faoi.Iterate()){
      rf.updateInterval(faoi.Current()->get_AOStar().get_AOReturn(), Interval::Top);
    }
  }

  //- now, iterate over targetfns
  suco_iterator<CFGfunction *> tfi(dc.getTargetFns());
  while(tfi.Iterate()){

    AO& retao = tfi.Current()->getId().get_AOFunction().get_AOReturn();
    TCtype * retty = retao.getStaticType();

    if(retty){
      suco_iterator<AO *> faoi(dc.getFaos()); //- this should usually be a singleton
      while(faoi.Iterate()){
        rf.handleStructAssign(faoi.Current()->get_AOStar().get_AOReturn(), *retty, &retao, strong_assign);
      }
    } else {
      Interval tgtfnr_iv = rf.getInterval(retao);
      suco_iterator<AO *> faoi(dc.getFaos()); //- this should usually be a singleton
      while(faoi.Iterate()){
        rf.updateInterval(faoi.Current()->get_AOStar().get_AOReturn(), tgtfnr_iv);
      }
    }
    //- CLEANUP: can remove "R X I <tgtfn>"
    rf.updateInterval(retao, Interval::Bottom);
  }
}

void RANfactHandler::reconstituteFilteredFacts(DFAfact& df, DFAfact& df_local)
{
  RANfact& rf = (RANfact&) df;
  RANfact& rf_local = (RANfact&) df_local;
  rf.reconstitute(rf_local);
}

DFAfact& RANfactHandler::lookupNodeFact(CFGnode& cn, CFGnode * tfSucc)
{
  DFAfactPair& dfp = cn.getRANfactPair();
  if(cn.getNsuccs() == 2){
    if(cn.getSucc(0) == tfSucc) return dfp.getFact1();
    if(cn.getSucc(1) == tfSucc && dfp.getFact2()) return *dfp.getFact2();
  }
  return dfp.meetIfPair(*this);
}

DFAfactPair& RANfactHandler::lookupNodeFactPair(CFGnode& cn)
{
  return cn.getRANfactPair();
}

DFAfact& RANfactHandler::lookupNodeFact(PExprCall& dc)
{
  return dc.getRANfact();
}

DFAfact& RANfactHandler::lookupNodeFact(PExprParallel& dp)
{
  return dp.getRANfact();
}

void RANfact::handleStructAssign(AO& lhs, TCtype& lhsty, AO * rhs, bool strong)
{
  if(lhsty.getKind() == TCtype::tcStruct){
    //- struct: must treat as component-wise assignment!

    suco_iterator<TCtype *> tli(((TCstructUnionType &)lhsty).getTypeList());
    suco_llist<TCtype *> tlist;
    while(tli.Iterate()){
      tlist.Append(tli.Current());
      AO * sdao = lhs.get_AOSDot(tlist);
      if(sdao){
        this->handleStructAssign(*sdao, *tli.Current(), rhs?rhs->get_AOSDot(tlist):0, strong);
      } else {
        if(flag_instantiate_structunion){
/**/      fprintf(stderr, "ERROR(RANfact::handleStructAssign): aoSdot not found: ao = ");
/**/      lhs.write_string_rep(stderr, true);
/**/      fprintf(stderr, "  tylist = ");
/**/      TCtype::write_list_string_rep(tlist, stderr);
/**/      fprintf(stderr, "\n");
        }
      }
    }

  } else if(lhsty.getKind() == TCtype::tcArray){
    //- array: weak assign!
    this->handleStructAssign(lhs, ((TCnumType &)lhsty).getBaseType(), rhs, false);
  } else {
    //- non-struct non-array: do the assignment here
    if(lhs.dfa_relevant){ // minor optimization (non-struct, can bypass if irrelevant)
      Interval rhs_iv = rhs?(this->getInterval(*rhs)):(Interval::Bottom);
      if(!strong && !rhs_iv.isBottom())
        rhs_iv.meet(this->getInterval(lhs));
      this->updateInterval(lhs, rhs_iv);
    }
  }
}

bool RANfactHandler::handleStrongAssign(DFAfact& df, AO& lhs, ExpDescr& rhs,
			    PgmExpr * dnode, PgmStmt * cnode, PExprArg * arg)
{
  RANfact& rf = (RANfact&) df;

  TCtype * lhsty = lhs.getStaticType();
  if(lhsty && (lhsty->getKind() == TCtype::tcStruct)){
    //- struct: must treat as component-wise assignment!

    //- precisely handle only if RHS is S or V
    AO * rhs_ao = 0;
    if((!strcmp(rhs.getEstr(),"{S}")) || (!strcmp(rhs.getEstr(),"{V}")))
      rhs_ao = rhs.getEstrAOs().Head();

    rf.handleStructAssign(lhs, *lhsty, rhs_ao, true);

  } else { //- non-struct case
    rf.updateInterval(lhs, rf.evalExpr(rhs));
  }
  return false; //- not useless
}

bool RANfactHandler::handleWeakAssign(DFAfact& df, ExpDescr& lhs, ExpDescr& rhs, PExprAssign& dnode)
{
  RANfact& rf = (RANfact&) df;

  //- collect lhs ecrset
  suco_set<ECR *>& lecrset = lhs.getAliasECRs();
  Interval riv = rf.evalExpr(rhs);
  rf.meetIntervals(lecrset, riv);

  return false; //- not useless
}

bool RANfactHandler::handleFormal(DFAfact& df, PExprDecl& dd, CFGfunction& parfn, bool interproc)
{
  if(interproc){
    RANfact& rf = (RANfact&) df;

    if(!strcmp(parfn.getId().getPid().getname(),".main")){ //- main function formals: map to bottom
      rf.updateInterval(dd.getAO(), Interval::Bottom);
    } else {
      //- NOTE: vararg taken care of at callsite (see interProcHandleCallArgs())
      AO * argao = parfn.getId().get_AOFunction().get_AOArg(dd.getArgNo());
      if(argao){
        TCtype * sty = dd.getAO().getStaticType();
        if(sty){
          rf.handleStructAssign(dd.getAO(), *sty, argao, true);
        } else {
	  //-- (currently: ellipses have no static type)
          if(dd.getAO().dfa_relevant){ // minor optimization (non-struct, can bypass if irrelevant)
            rf.updateInterval(dd.getAO(), rf.getInterval(*argao));
          }
        }
        //-CLEANUP: delete argao from rf, as it's no longer needed!
        rf.updateInterval(*argao, Interval::Bottom);
      } else {
        fprintf(stderr, "ERROR(RAN::handleFormal): arg(%d) not found for ", dd.getArgNo());
        parfn.getId().get_AOFunction().dump_descr(stderr);
        fprintf(stderr, "\n");
      }
    }
    return false; //- false=not useless

  } else { //- intraproc: ignore formal (better yet: remove from df?)
    return true; //- true=useless
  }
}

bool RANfactHandler::handleReturnStmt(DFAfact& df, PgmStmt& retnode, ExpDescr * retedesc, bool interproc)
{
  if(interproc){
    RANfact& rf = (RANfact&) df;
    if(retedesc){
      //- redirect to handleStrongAssign
      this->handleStrongAssign(df, retnode.getParentFunction().getId().get_AOFunction().get_AOReturn(), *retedesc, 0, 0, 0);
    }

    //- cleanup: remove return nodes except R X I <parfn>
    rf.removeRetIntervalsExcept(&retnode.getParentFunction().getId().get_AOFunction());

    //- handle deallocation:
    //  1. delete intervals for localvars
    //  2. delete intervals for aos that may point to anything in localvars
    suco_set<ECR *> localvars_ecrset;
    suco_iterator<AOId *> aoi(retnode.getParentFunction().getLocalVars());
    while(aoi.Iterate()){
      rf.updateInterval(*aoi.Current(), Interval::Bottom);		//- delete from rf
      localvars_ecrset.Insert(&aoi.Current()->getECR());	//- add to localvars ecrset
    }
    //- Now, remove intervals pointing to anything in localvars ecrset
    rf.removeIntervalsPointingTo(localvars_ecrset);

    return false; //- false=not useless

  } else { //- intraproc: ignore return?
    return true; //- true=useless
  }
}

TCtype * RANfact::evalSizeOfExpr(char *& s)
{
  TCtype * ret = 0;
  if(*s == 'Z'){ //- sizeof
    consume(++s, '(');
    ret = TCtype::stringToTy(s, &s);
    consume(++s, ')');
  }
  return ret;
}

//- r_size,r_type are return values
void RANfact::evalMallocSize(char * s, suco_iterator<AO *>& aoi, unsigned int& r_size, TCtype *& r_type)
{
  //- look for "sizeof(T)"
  r_type = evalSizeOfExpr(s);
  if(r_type){ //- sizeof
    r_size = 1;
    return;
  }
  //- look for "e * sizeof(T)" or "sizeof(T) * e"
  if(*s == 'o' && *(s+1) == '*'){
    char * ts = s + 2;
    suco_iterator<AO *> taoi = aoi;
    consume(ts, '(');
    r_type = evalSizeOfExpr(ts);
    if(r_type){ //- sizeof * exp
      consume(ts, ',');
      Interval iv2 = evalSubexpr(ts, taoi);
      consume(ts, ')');
      if(iv2.isNullTgted() && iv2.Min() > 0)
        r_size = iv2.Min();
      return;
    } else {
      Interval iv1 = evalSubexpr(ts, taoi);
      consume(ts, ',');
      r_type = evalSizeOfExpr(ts);
      if(r_type){ //- exp * sizeof
        if(iv1.isNullTgted() && iv1.Min() > 0)
          r_size = iv1.Min();
        return;
      }
      //- else fallthrough
    }
  }
  //- non-sizeof: just try to evaluate
  Interval iv = evalSubexpr(s, aoi);
  if(iv.isNullTgted() && iv.Min() > 0){
    r_size = iv.Min();
    r_type = &TCtype::tcCharType;
  }
  return;
}

bool RANfactHandler::handleDecl(DFAfact& df, PExprDecl& dd)
{
  RANfact& rf = (RANfact&) df;

  //- 0: handle malloc: try to evaluate its size
  ExpDescr * msed = dd.getMallocSize();
  if(dd.isMalloc() && msed){
    //- estr is either "@" or is surrounded by "{}"
    char * es = msed->getEstr();
    if(es && *es == '{'){
      char * str = es+1;
      suco_iterator<AO *> aoi(msed->getEstrAOs());
      try {
        unsigned int size = 0;
        TCtype * type = 0;
        rf.evalMallocSize(str, aoi, size, type);
        ((AOMalloc &)dd.getAO()).meetMallocStaticType(type, size);
      } catch(char * s) {
        fprintf(stderr, "ERROR(RANfactHandler::handleDecl/Malloc): parse error at position %d in string %s\n", s - es, es);
      }
    }
  }

  //- 1: reset dd.ao in rf
  rf.updateInterval(dd.getAO(), dd.isZeroed()?(Interval(0)):(Interval::Bottom));

  //- 2: if something in rf points to dd.ao, must remove that
  //	reference: if must_have_tgt_ao set to bottom, else set
  //	tgt_ao to unknown.
  //	Otherwise we could mis-handle subtraction or comparison
  //	of pointers to two different instantiations of dd.ao.
  //	NOTE: as it turns out, we could've done without this fix,
  //	because (1) as above, each recursive loop should have an
  //	incoming edge without dd.ao as a target; (2) in such a
  //	case our meet should produce either bottom or a zero-ed
  //	fact, which would fail on comparison and subtraction anyways.
  //	But still, for future extension of interval representation,
  //	this might be necessary.
  if(rf.disableIntervalsWithTarget(dd.getAO())){
/**/fprintf(stderr, "NOTE(handleDecl): disabled intervals pointing to ");
/**/dd.getAO().dump_descr(stderr);
/**/fprintf(stderr, "\n");
  }
  return false; //- not useless
}

bool RANfactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
//- ignore
  return true;
}

bool RANfactHandler::handlePredicate(DFAfactPair& dfp, PExprPredicate& dp)
{
  char * es = dp.getDesc().getEstr();
  if(flag_range_handle_preds
	&& !dp.isSwitch()	//- don't evaluate switch predicate (for now?)
	&& es && *es == '{'){
    suco_iterator<AO *> aoi(dp.getDesc().getEstrAOs());

    //- create a copy of the fact for lookup
    RANfact& lookup_rf = (RANfact&) dfp.getFact1().newClone();
    if(dfp.getFact2()) lookup_rf.meet(*dfp.getFact2());

    try {
      if(lookup_rf.evalPredExpr(es+1, aoi, RANfact::m_eqne, Interval(0), dfp)){
        reportKnownPredicate(dp); //- report predicate where at least one predicate variable is top
      }
    } catch(char * exs) {
      fprintf(stderr, "ERROR(RANfactHandler::handlePredicate): parse error at position %d in string %s\n", exs - es, es);
    }
    dfp.flipFacts(*this);
    this->deleteFact(lookup_rf); //- discard lookup_rf
    return false; //- not useless
  } else {
    return true; //- useless node
  }
}

void RANfactHandler::handleFreeCall(DFAfact& df, PExprCall& dc)
{
  RANfact& rf = (RANfact&) df;

  //- Get Freed ECR set
  //  TODO: may want to cache freed_ecrset in PExprCall?
  suco_set<ECR *> freed_ecrset;
  PExprArg * freearg = dc.getArg(1);
  if(freearg){ //- collect freed heap ecrs
    //- 1. Try "precise" points-to information from intervals
    Interval iv = rf.evalExpr(freearg->getDesc());
    if(iv.tgtAO()){ // check also for in-bounds?  or for [0,0]??
      freed_ecrset.Insert(&iv.tgtAO()->getECR());
    } else if(!iv.isNullTgted()){ //- (should never be null-targeted!?!)
      //- 2. Revert to flow-insens points-to information
      suco_iterator<AO *> aoi(freearg->getDesc().getAOs());
      while(aoi.Iterate()){
        if(!aoi.Current()->isVal()){ //- skip values
          freed_ecrset.Union(aoi.Current()->getECR().getPointsTo().getAliasECRs());
        }
      }
    }
  }

  //- Now, remove intervals pointing to anything in Freed ECR set
  ((RANfact&)df).removeIntervalsPointingTo(freed_ecrset);
}

//- fnao=null means delete all AOReturn intervals
void RANfact::removeRetIntervalsExcept(AO * fnao)
{
  interval_node ** mp = &this->map;
  while(*mp){
    AO& tsao = (*mp)->ao.getEnclosingStruct();
    if((tsao.getKind() == AO::aoReturn)
	&& (&((AOReturn &)tsao).getParent() != fnao)){
      interval_node * tmp = *mp;
      *mp = (*mp)->next;
      delete tmp;
    } else {
      mp = &(*mp)->next;
    }
  }
}

void RANfactHandler::interProcFilterEntryFact(DFAfact& df, CFGnode& cn) 
{
  RANfact& rf = (RANfact&) df;

  //- filter in only things in gref/mayfree
  rf.filterGrefMayFreeInto(cn.getParentFunction(), 0);

  //- filter out all return facts
  rf.removeRetIntervalsExcept(0);
}

void RANfactHandler::intraProcInitializeEntryFact(DFAfact& df, CFGnode& cn)
{
  //- initially: bottom (all vars map implicitly to bottom)
  df.setBottom();
}

void RANfactHandler::intraProcHandleCall(DFAfact& df, PExprCall& dc)
{
  RANfact& rf = (RANfact&) df;

  suco_iterator<CFGfunction *> fni(dc.getTargetFns());
  while(fni.Iterate()){
    RANfact trash;
    //- remove intervals for aos in GREF/GMayFree
    rf.filterGrefMayFreeInto(*fni.Current(), &trash);

    //- remove intervals for aos pointing to anything in GFreeHeap
    rf.removeIntervalsPointingTo(fni.Current()->getGFreeHeap_ecrs());
  }
}

//----------------------------------
//- RAN/RANwiden/RANnarrow: range analysis

bool RAN::isUselessNode(CFGnode& cn)
{
  return cn.RANisUseless();
}

bool RAN::isUselessNode(PgmExpr& dn)
{
  return dn.RANisUseless();
}

bool RAN::markUselessNode(CFGnode& cn)
{
  cn.RANsetUseless();
  return true;
}

bool RAN::markUselessNode(PgmExpr& dn)
{
  dn.RANsetUseless();
  return true;
}

void RAN::print_debug_stats(CFG& cfg, FILE * outf)
{
//TODO
}

//- if wn_always, then widen/narrow
//- else, if backedge, then widen/narrow
//        else just meet
bool RAN::absorbAndCompare(DFAfact& df1, DFAfact& df2, LocSet * backedge_filter)
{
  return ((RANfact&)df1).extended_meet((RANfact&)df2, false, true
	 , (wn_always || backedge_filter) ? wnmode : WN_MEET
	 , (backedge_filter == CFGnode::ALL_AOS)?0:backedge_filter);
}

//----------------------------------
// RANcFactHandler

bool RANcFactHandler::handleVerify(DFAfact& df, PExprVerify& dv)
{
  RANfact& rf = (RANfact&) df;

  //- diagnostic data
  {
    int rfsize = rf.countIntervals();
    TCstats::ranc_verify_visit_factsizes += rfsize;
    if(TCstats::ranc_verify_visit_max_factsize < rfsize)
       TCstats::ranc_verify_visit_max_factsize = rfsize;
    TCstats::ranc_verify_visits++;
  }

  //- do VP-all
  if((dv.getVpKind() == PExprVerify::vpPtrW) ||
	(this->readwrite && (dv.getVpKind() == PExprVerify::vpPtr))){

    Interval ran_iv = rf.getDerefRangeFor(dv.getDesc());

    //- collect stats about interval finiteness
    if(flag_range_collect_stats && !dv.isLib()){
      this->all_vps.Insert(&dv);
      if(ran_iv.isTargetedFinite())
        this->finite_vps.Insert(&dv);
      if(ran_iv.isTargetedHalfFinite())
        this->half_finite_vps.Insert(&dv);
    }

    if(ran_iv.inBounds(dv.getVpKind() == PExprVerify::vpPtrW)){
      if(!dv.isLib()){
        if(this->inbounds_vps.Insert(&dv)){
          if(strchr(dv.getDesc().getEstr(), 'M')
	    || strchr(dv.getDesc().getEstr(), 'W')){ //- count sdot and arrow expressions
            num_inb_sdotarrows++;
          }
        }
      }
    } else { //- mark as touched
      suco_iterator<AO *> aoi(dv.getAOs());
      while(aoi.Iterate())
        aoi.Current()->getECR().touchExposedAndPropagate();
    }
  }

  // RANFactHandler::handler.handleVerify(df, dv); //- NOP anyways
  return true;
}

void RANcFactHandler::reportKnownPredicate(PExprPredicate& dp)
{
  if(flag_range_collect_stats && !dp.isLib()){
    if(dp.isStmtOrQC()){ //- record only stmt or qc predicates
      this->known_preds.Insert(&dp);
    }
/**/fprintf(stderr, "NOTE(reportKnownPredicate): ");
/**/dp.debug_dump(stderr);
  }
}

int RANcFactHandler::countVPs(suco_set<PExprVerify *>& vps, bool limit_array)
{
  if(limit_array){
    int count = 0;
    suco_iterator<PExprVerify *> vpi(vps);
    while(vpi.Iterate()){
      AO * ao = vpi.Current()->getDesc().getAOs().GetSingleton();
      if(ao && ao->isDirectArrayAccess())
        count++;
    }
    return count;
  } else {
    return vps.Length();
  }
}

//----------------------------------
// RANc: collect redundant results

//(no functions)

//----------------------------------

