#include "ao.h"
#include "flags.h" //- for flag_range_*
#include "interval.h"
#include <stdio.h>

void Interval::setTgt(AO& ao)
{
  if(!flag_range_handle_malloc && ao.getKind() == AO::aoMalloc){
    this->setBottom();
  } else {
    this->tgt_ao = &ao;
    if(flag_strlit_readonly && ao.getKind() == AO::aoStringLit){
      this->tgt_readonly = true;
    }
    if(ao.getKind() == AO::aoMalloc){ //-malloc: use mtype/msize
      AOMalloc& mao = (AOMalloc&)ao;
      if(mao.getMallocType()){
        this->tgt_elty = mao.getMallocType();
        this->tgt_numelts = mao.getMallocSize();
      } else {
        this->setBottom();
      }
    } else { //- non-malloc
      TCtype * aoty = ao.getStaticType();
      if(aoty){
        this->setTgt(*aoty);
      } else {
        fprintf(stderr, "WARNING(Interval::setTgt): ao has no static type: ");
        ao.dump_descr(stderr);
        fprintf(stderr, "\n");
        this->setBottom();
      }
    }
  }
}

void Interval::setTgt(TCtype& ty)
{
  if(ty.getKind() == TCtype::tcVoid){
    this->setBottom();
  } else {
    int numelts = 1;
    TCtype * typ = &ty;
    while(typ->getKind() == TCtype::tcArray){
          TCnumType& arty = (TCnumType&)*typ;
          typ = &arty.getBaseType();
          numelts *= arty.getSize();
    }
    this->tgt_elty = typ;
    this->tgt_numelts = numelts;
  }
}

void Interval::debug_dump(FILE * outf) const
{
  if(isTop()){
    fprintf(outf, "top");
  } else {
    if(tgt_readonly)
      fprintf(outf, ":READONLY:");
    if(has_zero)
      fprintf(outf, "{0}U");
    if(tgt_elty){
      fprintf(outf, "&(");
      if(tgt_ao){
        tgt_ao->dump_descr(outf);
        fprintf(outf, ":");
      }
      tgt_elty->debug_dump(outf);
      fprintf(outf, "[%d])+", tgt_numelts);
    }
    fprintf(outf, "[");
    if(min == MINUS_INF) fprintf(outf, "-INF");
    else fprintf(outf, "%d", min);
    fprintf(outf, ",");
    if(max == PLUS_INF) fprintf(outf, "+INF");
    else fprintf(outf, "%d", max);
    fprintf(outf, "]");
  }
}

//- Note: the idea behind the has_zero flag is that
//	  this inBounds check will ignore it, since
//	  we'll allow a null dereference to go unchecked
bool Interval::inBounds(bool iswrite) const
{
  return ( (this->tgt_elty)
	   && (this->min >= 0)
	   && (this->max < this->tgt_numelts)
	   && !(iswrite && this->tgt_readonly)
	 );
}

//- adjust this interval by struct/union offset
//  * only do this if inBounds and has no zero
void Interval::adjustStructOffset(suco_llist<TCtype *>& tylist)
{
  if(this->isTop()) return;
  if(this->inBounds(false)
	&& !this->has_zero
	&& (this->tgt_elty)){
    //- note: (parts of) old tgt_elty will be leaked
    switch(this->tgt_elty->getKind()){
      case TCtype::tcStruct: {
        TCtype * tailty = TCtype::listEquivPrefix(tylist, ((TCstructUnionType *) this->tgt_elty)->getTypeList());
        if(tailty){
          if(this->tgt_ao){
            TCtype * ao_sty = this->tgt_ao->getStaticType();
            if(ao_sty && (ao_sty->equiv(*this->tgt_elty)
			  || (ao_sty->getKind() == TCtype::tcArray
				&& ((TCnumType *)ao_sty)->getBaseType().equiv(*this->tgt_elty)))){
              this->tgt_ao = this->tgt_ao->get_AOSDot(tylist);
/**/          if(flag_instantiate_structunion && !this->tgt_ao)
/**/            fprintf(stderr, "ERROR(adjustStructOffset): aoSdot not found\n");
              this->setTgt(*tailty);	//- sets tgt_elty,tgt_numelts
              this->min = this->max = 0;
              return;
            } else {
/**/          fprintf(stderr, "ERROR(adjustStructOffset): struct ao-ty does not match, ao = ");
/**/          this->tgt_ao->write_string_rep(stderr, true);
/**/          fprintf(stderr, "  elty = ");
/**/          this->tgt_elty->write_string_rep(stderr);
/**/          fprintf(stderr, "\n");
              if(flag_range_must_have_ao){
                this->setBottom();
                return;
              }
              this->tgt_ao = 0;
            }
          }
          this->setTgt(*tailty);	//- sets tgt_elty,tgt_numelts
          this->min = this->max = 0;
          return;
        } else {
//fprintf(stderr, "NOTE(adjustStructOffset): could not struct-adjust ");
//this->debug_dump(stderr);
//fprintf(stderr, " to ");
//TCtype::debug_dump_list(tylist, stderr);
//fprintf(stderr, "\n");
          this->setBottom();
        }
      } break;
      case TCtype::tcUnion: {
        if(tylist.Length() == 1){
          suco_iterator<TCtype *> tli(((TCstructUnionType *) this->tgt_elty)->getTypeList());
          while(tli.Iterate()){
            if(tli.Current()->equiv(*tylist.Head())){
              if(this->tgt_ao){
                TCtype * ao_sty = this->tgt_ao->getStaticType();
                if(ao_sty && (ao_sty->equiv(*this->tgt_elty)
			      || (ao_sty->getKind() == TCtype::tcArray
				  && ((TCnumType *)ao_sty)->getBaseType().equiv(*this->tgt_elty)))){
                  this->tgt_ao = this->tgt_ao->get_AOUDot(*tli.Current());
/**/              if(flag_instantiate_structunion && !this->tgt_ao)
/**/                fprintf(stderr, "ERROR(adjustStructOffset): aoUdot not found\n");
                } else {
/**/              fprintf(stderr, "ERROR(adjustStructOffset): union ao-ty does not match, ao = ");
/**/              this->tgt_ao->dump_descr(stderr);
/**/              fprintf(stderr, "  elty = ");
/**/              this->tgt_elty->debug_dump(stderr);
/**/              fprintf(stderr, "\n");
                  if(flag_range_must_have_ao){
                    this->setBottom();
                    return;
                  }
                  this->tgt_ao = 0;
                }
              }
              this->setTgt(*tli.Current());	//- sets tgt_elty,tgt_numelts
              this->min = this->max = 0;
              return;
            }
          }
/**/fprintf(stderr, "NOTE(adjustStructOffset): could not union-adjust ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, " to ");
/**/TCtype::debug_dump_list(tylist, stderr);
/**/fprintf(stderr, "\n");
          this->setBottom();
        } else { //- tylist is longer than 1 (or zero?)
//TODO?: look for embedded structs
/**/fprintf(stderr, "ERROR(adjustStructOffset): union target, but tylist is not 1: ");
/**/TCtype::debug_dump_list(tylist, stderr);
/**/fprintf(stderr, "  this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "\n");
          this->setBottom();
        }
      } break;
      default: {
        this->setBottom();
      } break;
    }
  } else this->setBottom();
}

//- return true if disabled
bool Interval::disableIfTargetIsPartOf(AO& ao)
{
  if(this->tgt_ao
	&& (&this->tgt_ao->getEnclosingStruct() == &ao)){
    if(flag_range_must_have_ao)
      this->setBottom();
    else
      this->tgt_ao = 0;
    return true;
  }
  return false;
}

const Interval Interval::Top(PLUS_INF,MINUS_INF);
const Interval Interval::Bottom(MINUS_INF,PLUS_INF);

bool Interval::isTop() const
{
  //- note: I'm ignoring tgt_elty/numelts
  return( (this->min > this->max)
	||(this->min == PLUS_INF)
	||(this->max == MINUS_INF)
	);
}

bool Interval::isBottom() const
{
  //- note: even if this->tgt_elty != 0, it's still bottom
  return (this->min == MINUS_INF) && (this->max == PLUS_INF);
}

bool Interval::isTargetedFinite() const
{
  return (this->tgt_ao) && (this->min != MINUS_INF) && (this->max != PLUS_INF);
}

bool Interval::isTargetedHalfFinite() const
{
  return (this->tgt_ao) && ((this->min != MINUS_INF) || (this->max != PLUS_INF));
}

void Interval::setTop()
{
  this->tgt_ao = 0;
  this->tgt_elty = 0;
  this->tgt_numelts = 0;
  this->min = PLUS_INF;
  this->max = MINUS_INF;
  this->has_zero = false;
  this->tgt_readonly = false;
}

void Interval::setBottom()
{
  this->tgt_ao = 0;
  this->tgt_elty = 0;
  this->tgt_numelts = 0;
  this->min = MINUS_INF;
  this->max = PLUS_INF;
  this->has_zero = false;
  this->tgt_readonly = false;
}

bool Interval::widen(Interval iv)
{
  if(this->isBottom()){ //- bottom widen iv = bottom
    return false;
  } else if(iv.isBottom()){ //- this widen bottom = bottom
    this->setBottom();
    return true;
  } else if((this->tgt_elty == iv.tgt_elty) ||
	    (this->tgt_elty && iv.tgt_elty && this->tgt_elty->equiv(*iv.tgt_elty))){

    bool changed = false;

    //- adjust tgt_ao
    if(this->tgt_ao != iv.tgt_ao){
      if(flag_range_must_have_ao){
        this->setBottom();
        return true;
      }
      this->tgt_ao = 0;
      changed = true;
    }

    //- adjust numelts (shouldn't occur?)
    if(this->tgt_numelts > iv.tgt_numelts){
//fprintf(stderr, "WARNING(Interval::widen): adjusted tgt_numelts %d -> %d\n",
//				this->tgt_numelts, iv.tgt_numelts);
      if(flag_range_must_have_ao && !flag_range_ty_can_change){
        this->setBottom();
        return true;
      }
      this->tgt_numelts = iv.tgt_numelts;
      changed = true;
    }

    if(this->min > iv.min){
      //- pragmatic improvement: if both are >0, then widen to 0
      //- :only doing this case because 0 is valid lowerbound for
      //   array access.
      if(iv.min >= 0){
        this->min = 0;
      } else {
        this->min = MINUS_INF;
      }
      changed = true;
    }
    if(this->max < iv.max){
       this->max = PLUS_INF;
       changed = true;
    }
    if(flag_range_zero_special &&
	iv.has_zero && !this->has_zero){
      this->has_zero = true;
      changed = true;
    }
    if(iv.tgt_readonly && !this->tgt_readonly){
      this->tgt_readonly = true;
      changed = true;
    }

    return changed;

  } else if(flag_range_zero_special &&
	    this->tgt_elty && iv.iszero()){ //- widen with zero
    if(!this->has_zero){
      this->has_zero = true;
      return true;
    }
  } else if(flag_range_zero_special &&
	    iv.tgt_elty && this->iszero()){ //- zero widen sth = sth U {0}
    *this = iv;
    this->has_zero = true;
    return true;
  } else {
/**/if(flag_verbose){
/**/fprintf(stderr, "WARNING(Interval::widen): mismatched eltys or aos, this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "  iv = ");
/**/iv.debug_dump(stderr);
/**/fprintf(stderr, "\n");
/**/}
    this->setBottom();
    return true;
  }
  return false;
}

bool Interval::narrow(Interval iv)
{
  bool changed = false;
  if(this->isBottom()){ //- bottom narrow iv = iv
    *this = iv;
    changed = !iv.isBottom();
  } else if(this->isTop()){ //- top narrow iv = iv (??)
//fprintf(stderr, "WARNING(Interval::narrow): top narrow this = ");
//this->debug_dump(stderr);
//fprintf(stderr, "\n");
    *this = iv;
    changed = !iv.isTop();
  } else if(iv.isBottom()){ //- this narrow bottom = bottom
/**/fprintf(stderr, "WARNING(Interval::narrow): narrow with bottom, this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "\n");
    this->setBottom();
    changed = true;
  } else if(iv.isTop()){ //- this narrow top = this?
//fprintf(stderr, "WARNING(Interval::narrow): narrow with top, this = ");
//this->debug_dump(stderr);
//fprintf(stderr, "\n");
    //-NOP
  } else if((this->tgt_elty == iv.tgt_elty) ||
	    (this->tgt_elty && iv.tgt_elty && this->tgt_elty->equiv(*iv.tgt_elty))){

    //-NOTE: we can play with targeted intervals here, because in theory
    // narrowing should only ever be called with intervals referring to
    // the same location?  (It's like a "cleanup" phase.)

    //- adjust tgt_ao
    if(this->tgt_ao != iv.tgt_ao){

/**/  fprintf(stderr, "WARNING(Interval::narrow): mismatched aos, this = ");
/**/  this->debug_dump(stderr);
/**/  fprintf(stderr, "  iv = ");
/**/  iv.debug_dump(stderr);
/**/  fprintf(stderr, "\n");

      if(!this->tgt_ao && iv.tgt_ao){
        this->tgt_ao = iv.tgt_ao;	//- (eltys already match; no need to adjust)
        changed = true;
      }
      if(this->tgt_ao && iv.tgt_ao){
        this->tgt_ao = 0;	//???
      }
      if(flag_range_must_have_ao && !this->tgt_ao){
        this->setBottom();
        return true;
      }
    }

    if(this->tgt_numelts > iv.tgt_numelts){
/**/  if(flag_verbose)
/**/    fprintf(stderr, "WARNING(Interval::narrow): adjusted tgt_numelts %d -> %d\n",
/**/				this->tgt_numelts, iv.tgt_numelts);
      if(flag_range_must_have_ao && !flag_range_ty_can_change){
        this->setBottom();
        return true;
      }
      this->tgt_numelts = iv.tgt_numelts;
    }

    if(this->min == MINUS_INF && iv.min != MINUS_INF
			      && iv.min != PLUS_INF){	//- ??
       this->min = iv.min;
       changed = true;
    }
    if(this->max == PLUS_INF && iv.max != PLUS_INF
			     && iv.max != MINUS_INF){	//- ??
       this->max = iv.max;
       changed = true;
    }
    if(flag_range_zero_special &&
	iv.has_zero && !this->has_zero){
      this->has_zero = true;
      changed = true;
    }
    if(iv.tgt_readonly && !this->tgt_readonly){
      this->tgt_readonly = true;
      changed = true;
    }
//TODO: warn if less than?
  } else if(flag_range_zero_special &&
		this->has_zero && iv.iszero()){
    //-NOP
  } else if(flag_range_zero_special &&
	     iv.has_zero && this->iszero()){
    *this = iv;
    changed = true;
  } else {
/**/if(flag_verbose){
/**/fprintf(stderr, "WARNING(Interval::narrow): mismatched eltys or aos, this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "  iv = ");
/**/iv.debug_dump(stderr);
/**/fprintf(stderr, "\n");
/**/}
    //-NOP?
  }
  return changed;
}

bool Interval::meet(Interval iv) //- "expand"
{
  bool changed = false;
  if(this->isBottom()){ //- bottom meet iv = bottom
    changed = false;
  } else if(iv.isBottom()){ //- this meet bottom = bottom
    this->setBottom();
    changed = true;
  } else if(iv.isTop()){ //- this meet top = this
  } else if(this->isTop()){ //- top meet iv = iv
    *this = iv;
    changed = true;
  } else if((this->tgt_elty == iv.tgt_elty) ||
	    (this->tgt_elty && iv.tgt_elty && this->tgt_elty->equiv(*iv.tgt_elty))){

    //- adjust tgt_ao
    if(this->tgt_ao != iv.tgt_ao){
      if(flag_range_must_have_ao){
        this->setBottom();
        return true;
      }
      this->tgt_ao = 0;
      changed = true;
    }

    //- adjust numelts
    if(this->tgt_numelts > iv.tgt_numelts){
      if(flag_range_must_have_ao && !flag_range_ty_can_change){
        this->setBottom();
        return true;
      }
      this->tgt_numelts = iv.tgt_numelts;
      changed = true;
    }

    changed |= this->minmax_union(iv.min, iv.max);

    if(flag_range_zero_special &&
	iv.has_zero && !this->has_zero){
      this->has_zero = true;
      changed = true;
    }
    if(iv.tgt_readonly && !this->tgt_readonly){
      this->tgt_readonly = true;
      changed = true;
    }
  } else if(flag_range_zero_special &&
	    this->tgt_elty && iv.iszero()){ //- meet with zero
    if(!this->has_zero){
      changed = true;
      this->has_zero = true;
    }
  } else if(flag_range_zero_special &&
	    iv.tgt_elty && this->iszero()){ //- zero meet sth = sth U {0}
    *this = iv;
    this->has_zero = true;
    changed = true;
  } else { //- set to bottom
/**/if(flag_verbose){
/**/fprintf(stderr, "NOTE(Interval::meet): mismatched eltys or aos, this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "  iv = ");
/**/iv.debug_dump(stderr);
/**/fprintf(stderr, "\n");
/**/}
    this->setBottom();
    changed = true;
  }
  return changed;
}

//  COMPARE with cond_intersect() below.
bool Interval::join(Interval iv) //- "shrink"
{
  bool changed = false;
  if(this->isBottom()){ //- bottom join iv = iv
    *this = iv;
    changed = !this->isBottom();
  } else if(iv.isBottom()){ //- this join bottom = this
    changed = false;
  } else if((this->tgt_elty == iv.tgt_elty) ||
	    (this->tgt_elty && iv.tgt_elty && this->tgt_elty->equiv(*iv.tgt_elty))){

    //- adjust tgt_ao
    if(this->tgt_ao != iv.tgt_ao){

/**/  if(this->tgt_ao && iv.tgt_ao){
/**/    fprintf(stderr, "WARNING(Interval::join): mismatched aos, this = ");
/**/    this->debug_dump(stderr);
/**/    fprintf(stderr, "  iv = ");
/**/    iv.debug_dump(stderr);
/**/    fprintf(stderr, "\n");
/**/  }

      if(!this->tgt_ao){
        this->tgt_ao = iv.tgt_ao;
        changed = true;
      }
    }

    //- adjust numelts
    if((!flag_range_must_have_ao || flag_range_ty_can_change) &&
	this->tgt_numelts < iv.tgt_numelts){
      this->tgt_numelts = iv.tgt_numelts;
      changed = true;
    }

    if(this->min < iv.min){
       this->min = iv.min;
       changed = true;
    }
    if(this->max > iv.max){
       this->max = iv.max;
       changed = true;
    }

  } else if(flag_range_zero_special &&
	    this->inclzero() && iv.inclzero()){

//fprintf(stderr, "WARNING(Interval::join): targeted elty(s) but with zero, this = ");
//this->debug_dump(stderr);
//fprintf(stderr, "  iv = ");
//iv.debug_dump(stderr);
//fprintf(stderr, "\n");

    if(!this->iszero()){
      this->tgt_ao = 0;
      this->tgt_elty = 0;
      this->tgt_numelts = 0;
      this->min = 0;
      this->max = 0;
      this->has_zero = false;
      this->tgt_readonly = false;
      changed = true;
    }

  } else {
    //- targeted eltys: set to Top
    this->setTop();
    changed = true;
  }
  return changed;
}

//----------------------------------------------
//- Modify this interval to be (a conservative
//  superset of) its intersection with iv.
//  This function is called when evaluating a
//  predicate which constrains this interval
//  to be bounded by iv.
//  Conservative superset means we can always
//  leave this unchanged.
//- The difference between this function and join()
//  is that this needs a superset (unchanged by default),
//  while join needs a subset (top by default).
//- RULES:
//   1. If both are numeric (non-targeted), intersect.
//   2. If both have same (non-bottom) target loc,
//      _AND_ their eltypes are compatible: adjust
//      numelts (if necessary) and intersect.
//   3. If both have (non-bottom) target loc
//      that don't alias (that aren't fields of
//      the same structure), AND THAT ARE IN-BOUNDS,
//      then can set to Top.
//   4. Otherwise, leave unchanged.
bool Interval::cond_intersect(Interval iv)
{
  bool changed = false;
  if(this->isTop()){ //- top intersect iv = top
    changed = false;
  } else if(iv.isTop()){ //- top intersect iv = top
    this->setTop();
    changed = true;
  } if(this->isBottom()){ //- bottom intersect iv = iv
    *this = iv;
    changed = !this->isBottom();
  } else if(iv.isBottom()){ //- this intersect bottom = this
    changed = false;
  } else if(flag_range_skip_targeted_preds &&
	    (this->tgt_elty || iv.tgt_elty)){
    changed = false;
  } else if(( (this->tgt_ao && this->tgt_ao == iv.tgt_ao)
	       && (this->tgt_elty && iv.tgt_elty && this->tgt_elty->equiv(*iv.tgt_elty)))
	    || (!this->tgt_elty && !iv.tgt_elty)){
    //- same target loc and target ty, or non-targeted (integral)

    //- adjust numelts
    if(this->tgt_numelts < iv.tgt_numelts){

/**/  fprintf(stderr, "WARNING(cond_intersect): adjusted numelts from %d to %d! this = ", this->tgt_numelts, iv.tgt_numelts);
/**/  this->debug_dump(stderr);
/**/  fprintf(stderr, "\n");

      this->tgt_numelts = iv.tgt_numelts;
      changed = true;
    }
    if(this->min < iv.min){
       this->min = iv.min;
       changed = true;
    }
    if(this->max > iv.max){
       this->max = iv.max;
       changed = true;
    }
  } else if(this->tgt_ao && iv.tgt_ao				//- if both have targeted locs, and
	    && (&this->tgt_ao->getRootAO() != &iv.tgt_ao->getRootAO()) //- and they don't alias
	    && this->inBounds(false) && iv.inBounds(false)){	//- and they're in-bounds,
    this->setTop();						//--> can set to top!
    changed = true;
  } else {
    //- targeted interval(s): leave unchanged
    changed = false;
  }
  return changed;
}

//- DESCRIPTION: fix the types of iv1,iv2 for elty-arithmetic:
//    elty = 0       means "use underlying type" (no conflict)
//    elty = tcVoid  means plain (non-pointer) arithmetic
//    elty = T       means pointer arithmetic on pointer-to-T
// :The check is for (ptr +/- int), or (int +/- int); that is,
//  (int +/- ptr) has already been normalized to (ptr +/- int).
//  (ptr - ptr) is not handled by this function (see ptrs_minus()).
// :Try to adjust either iv1 or iv2 to be consistent with elty-arithmetic;
//  return true if successful; false if cannot fix.
//- The idea is for this function to be extensible to apply different
//  typing policies (implemented by TCtype::countInstancesOf()):
//  - Currently, doing most-portable policy, which assumes that
//    . |char| = 1
//    . |char| <= all types except void, bitfield
//    . |char| <= |short|<=|int|<=|long|<=|longlong|
//    . |float| <= |double|<=|longdouble|
//    . |t[x]| = |t| * x
//    . all pointers have same size
//    . struct and union containment
//  - Future extensions can use (non-portable) relative sizes of the
//    different types, etc.
//- CONVERSION CHOICES:    (Notation: |ty| = sizeof ty)
//  Let iv1 = <tgty,size> + [tmin,tmax]
//      iv2 = [rmin,rmax]
//  SOLUTION 1: normalize iv1 and iv2 to unit-arithmetic (we assume char
//  is of size one, so that char-arithmetic is equivalent to "plain"
//  unit-arithmetic):
//      iv1' = <char,size*|tgty|> + [tmin*|tgty|, tmax*|tgty|]
//      iv2' = [rmin*|elty|, rmax*|elty|]
//  SOLUTION 2: let's convert only one of iv1 or iv2.  So, pick one of
//  the following:        (Notation: |ty1/ty2| is shorthand for |ty1|/|ty2|)
//    a. iv1'' = <elty,size*|tgty/elty|> + [tmin*|tgty/elty|, tmax*|tgty/elty|]
//    b. iv2'' = [rmin*|elty/tgty|, rmax*|elty/tgty|]
//  APPROXIMATIONS: the above are "precise" solutions, which can be computed
//  if we know the size of the types.  When approximating, the constraints
//  must follow the augmented-interval lattice, so APPROX "less-than" TRUE means:
//     size_APPROX <= size_TRUE
//      min_APPROX <= min_TRUE
//      max_APPROX >= max_TRUE
//  IMPLEMENTED SOLUTION: given above (portable) size assumptions, estimate the
//  ratio |tgty/elty|:     (assume elty != tgty, else no conversion is needed)
//   A. if |elty| <= |tgty|, which means r = |tgty/elty| >= 1,
//      then let:
//                                {tmin, if tmin >= 0} {tmax, if tmax <= 0}
//        iv1a = <elty,size*r> + [{                  },{                  }]
//                                {-INF, if tmin < 0 } {+INF, if tmax > 0 }
//                {rmin, if rmin <= 0} {rmax, if rmax >= 0}
//        iv2a = [{                  },{                  }]
//                {  0,  if rmin > 0 } {  0,  if rmax < 0 }
//      Since iv1a is "less-than" iv1'', and iv2a is "less than" iv2'',
//      we can choose to convert either iv1 or iv2, heuristically avoiding
//      divergence as much as possible.
//      Current solution: aim for precise max; prefer iv2a
//   B. if |tgty| <= |elty|, which means |elty/tgty| >= 1,
//      then let:
//                    {rmin, if rmin >= 0} {rmax, if rmax <= 0}
//            iv2b = [{                  },{                  }]
//                    {-INF, if rmin < 0 } {+INF, if rmax > 0 }
//      We thus convert iv2 to iv2b, since iv2b is "less-than" iv2''.
//   C. if we don't know the relative sizes of elty and tgty,
//      then: we can make no precise approximations, so return false.
bool Interval::adjustTypes(Interval& iv1, Interval& iv2, TCtype * elty, bool addition)
{
#define A_T_OUTPUT_NOTE_HEAD(descr)	do {				\
	fprintf(stderr, "NOTE(adjustTypes:" descr ":%c): iv1 = ",	\
		addition?'+':'-');					\
	iv1.debug_dump(stderr);						\
	fprintf(stderr, "  iv2 = ");					\
	iv2.debug_dump(stderr);						\
	fprintf(stderr, "  elty = ");					\
	elty->debug_dump(stderr);					\
	fprintf(stderr, "\n");						\
	} while(0)

#define A_T_OUTPUT_NOTE_TAIL(descr,ivdescr,iv)	do {			\
	fprintf(stderr, "NOTE(adjustTypes:" descr ".%c): ... ",		\
		addition?'+':'-');					\
	fprintf(stderr, ivdescr " = ");					\
	iv.debug_dump(stderr);						\
	fprintf(stderr, "\n");						\
	} while(0)

/////////////////////////////////////////////////////////////////////////

  if(iv2.tgt_elty){
    //- OK if unit-addition, and lhs has no target
    if(addition && !iv1.tgt_elty
	&& elty && ((elty->getKind() == TCtype::tcVoid)
		    || (elty->getKind() == TCtype::tcChar))){
/**/A_T_OUTPUT_NOTE_HEAD("flipping_lhs_rhs");
      //- flip
      Interval ivt = iv1;
      iv1 = iv2;
      iv2 = ivt;
      //- fallthrough...
    } else {
/**/  static bool first_time = true;
/**/  if(first_time){
/**/    first_time = false;
/**/    A_T_OUTPUT_NOTE_HEAD("rhs_has_tgt");
/**/  }
      return false;
    }
  }
  if(iv2.iszero()){ //- plus/minus zero: OK
    return true;
  }
  if(iv1.tgt_elty && iv1.has_zero){
/**/A_T_OUTPUT_NOTE_HEAD("tgt_w/zero");
    return false;
  } else if(!elty){ //- "use underlying type" (no conflict)
    return true;
  } else if(iv1.tgt_elty && iv1.tgt_elty->equiv(*elty)){ //- types match: OK ==> should be the common case <==
      return true;
  } else if(((elty->getKind() == TCtype::tcVoid)					//- elty is plain (non-ptr) arith
	     || (elty->getKind() == TCtype::tcChar))					//       or char ptr arith
	    && ((!iv1.tgt_elty) || (iv1.tgt_elty->getKind() == TCtype::tcChar))){	//- and tgt is NULL or char-typed
    return true;
  } else { //- mismatched type

    if(!iv1.tgt_elty){ //- iv1 has no target, and elty is non-void non-char

/**/  A_T_OUTPUT_NOTE_HEAD("tgtynull");
      if(flag_range_exact_sizes){
        // convert iv2
        int elty_size = elty->SizeOf();
        iv2 = iv2.times(Interval(elty_size,elty_size));
      } else {
        // convert iv2b
        if(iv2.min < 0) iv2.min = MINUS_INF;
        if(iv2.max > 0) iv2.max = PLUS_INF;
      }
/**/  A_T_OUTPUT_NOTE_TAIL("tgtynull","iv2b",iv2);
      return true;

    } else if(elty->getKind() == TCtype::tcArray){ //- elty is array

      //- REFINEMENT A: if rhs is an array, work at its element level.
      //  This happens when we're dealing with multi-dimensional arrays.
      //  So, change: elty=T[i], iv1=<T1,s1>+[m1,n1], iv2=[m2,n2]
      //  To:         elty=T,    iv1=<T1,s1>+[m1,n2], iv2=[m2,n3] * [i,i]

//    A_T_OUTPUT_NOTE_HEAD("rhs_array");
      TCnumType& elty_arr = *((TCnumType *) elty);
      iv2 = iv2.times(Interval(elty_arr.getSize()));
//    A_T_OUTPUT_NOTE_TAIL("rhs_array","iv2_adj",iv2);
      return adjustTypes(iv1,iv2,&elty_arr.getBaseType(), addition);

    } else { //- iv1.tgt_elty does not match elty (both non-null)

      if(flag_range_exact_sizes){

        int tgty_size = iv1.tgt_elty->SizeOf();
        int elty_size = elty->SizeOf();

        if(tgty_size == 0 || elty_size == 0){

/**/      if(iv1.tgt_elty->getKind() != TCtype::tcVoid){
/**/        fprintf(stderr, "ERROR(adjustTypes): zero size encountered: sizeof tgty : ");
/**/        iv1.tgt_elty->debug_dump(stderr);
/**/        fprintf(stderr, " = %d,  sizeof elty : ", tgty_size);
/**/        elty->debug_dump(stderr);
/**/        fprintf(stderr, " = %d\n", elty_size);
/**/      }

          return false;

        } else if(flag_range_ty_can_change && (tgty_size > elty_size)){

          //- do iv1''
          int floor = tgty_size / elty_size;
          int ceil = (tgty_size % elty_size) ? (floor+1) : floor;

/**/      if(ceil != floor){
/**/        A_T_OUTPUT_NOTE_HEAD("imprecise_iv1");
/**/      }

          iv1.tgt_elty = elty;		//- NOTE: old tgt_elty may be leaked
          iv1.tgt_numelts *= floor;
          if(iv1.min != MINUS_INF && iv1.min != PLUS_INF)
            iv1.min *= floor;
          if(iv1.max != MINUS_INF && iv1.max != PLUS_INF)
            iv1.max *= ceil;

/**/      if(ceil != floor){
/**/        A_T_OUTPUT_NOTE_TAIL("imprecise_iv1","iv1_adj",iv1);
/**/      }

          return true;

        } else {

          //- do iv2''
          int floor = elty_size / tgty_size;
          int ceil = (elty_size % tgty_size) ? (floor+1) : floor;

/**/      if(ceil != floor){
/**/        A_T_OUTPUT_NOTE_HEAD("imprecise_iv2");
/**/      }

          if(iv2.min != MINUS_INF && iv2.min != PLUS_INF)
            iv2.min *= floor;
          if(iv2.max != MINUS_INF && iv2.max != PLUS_INF)
            iv2.max *= ceil;

/**/      if(ceil != floor){
/**/        A_T_OUTPUT_NOTE_TAIL("imprecise_iv2","iv2_adj",iv2);
/**/      }

          return true;

        }

      } else { //- !flag_range_exact_sizes

        int num_elty_in_tgt = iv1.tgt_elty->countInstancesOf(*elty);
        int num_tgt_in_elty = elty->countInstancesOf(*iv1.tgt_elty);

        if(num_elty_in_tgt > 0 && num_tgt_in_elty > 0){
/**/fprintf(stderr, "ERROR(adjustTypes): elty in tgt = %d, tgt in elty = %d\n", num_elty_in_tgt, num_tgt_in_elty);
/**/fprintf(stderr, "tgt  = ");
/**/iv1.tgt_elty->debug_dump(stderr);
/**/fprintf(stderr, "\nelty = ");
/**/elty->debug_dump(stderr);
/**/fprintf(stderr, "\n");
          return false;
        }

        if(num_elty_in_tgt > 0){
          Interval iv1a = Interval::Bottom;
          Interval iv2a = Interval::Bottom;

          //- I. try iv1a: only if |elty| <= |iv1.elty|
          if(flag_range_ty_can_change){

            iv1a = iv1;
            iv1a.slide_if_constant(); //- FIRST: slide iv1a if offset is constant
        
            //- convert iv1a, but with elty instead of char
            iv1a.tgt_elty = elty;	//NOTE: old tgt_elty may be leaked ~~ but currently, there's no helping it.
            iv1a.tgt_numelts *= num_elty_in_tgt;
            if(iv1a.min < 0) iv1a.min = MINUS_INF;
            if(iv1a.max > 0) iv1a.max = PLUS_INF;
          }

          //- II. try iv2a
          {
            iv2a = iv2;
            if(iv2a.min > 0) iv2a.min = 0;
            if(iv2a.max < 0) iv2a.max = 0;
          }

          //- III. see which (if any) is better:
          //  * choose iv1a if iv1a is [0,0], or if iv2a is bottom.
	  //  * otherwise, always prefer iv2a.
          if(((iv1a.min == 0) && (iv1a.max == 0)) || iv2a.isBottom()){
            //- choose iv1a
            if(iv1a.isBottom()){
              return false;
            }
/**/        if(flag_verbose){
/**/          A_T_OUTPUT_NOTE_HEAD("iv1a");
/**/          A_T_OUTPUT_NOTE_TAIL("iv1a","iv1a",iv1a);
/**/        }
            iv1 = iv1a;
          } else {
            //- choose iv2a
            if(iv2a.isBottom()){
              return false;
            }
/**/        A_T_OUTPUT_NOTE_HEAD("iv2a");
/**/        A_T_OUTPUT_NOTE_TAIL("iv2a","iv2a",iv2a);
            iv2 = iv2a;
          }
          return true;
        }

        if(num_tgt_in_elty){ //- iv2b
/**/      A_T_OUTPUT_NOTE_HEAD("tgtychar");
          if(iv2.min < 0) iv2.min = MINUS_INF;
          if(iv2.max > 0) iv2.max = PLUS_INF;
/**/      A_T_OUTPUT_NOTE_TAIL("tgtychar","iv2b",iv2);
          return true;
        }

/**/    if(iv1.tgt_elty->getKind() != TCtype::tcVoid){
/**/      A_T_OUTPUT_NOTE_HEAD("mismatch");
/**/    }
        return false;
      }
    }
  }
#undef A_T_OUTPUT_NOTE_TAIL
#undef A_T_OUTPUT_NOTE_HEAD
}

//- If this has the form A[s]+[i,i], where i < s,
//  then convert to A[s-i]+[0,0]
void Interval::slide_if_constant()
{
  if(flag_range_do_sliding
     && (this->tgt_elty)
     && (this->min == this->max)
     && (this->min != 0)
     && (this->min < this->tgt_numelts)){
//fprintf(stderr, "NOTE(slide_if_constant): ");
//this->debug_dump(stderr);
    this->tgt_numelts -= this->min;
    this->min = this->max = 0;
//fprintf(stderr, "  -->  ");
//this->debug_dump(stderr);
//fprintf(stderr, "\n");
  }
}

//- returns true if changed
bool Interval::minmax_union(int min, int max)
{
  bool changed = false;
  if(this->min > min){
     this->min = min;
     changed = true;
  }
  if(this->max < max){
     this->max = max;
     changed = true;
  }
  return changed;
}

//- elty = 0      means "use underlying type"
//  elty = tcVoid means plain (non-pointer) arithmetic
//  elty = T      means pointer arithmetic on pointer-to-T
//- NOTE: assume (int+pointer) has been normalized to (ptr+int)
Interval Interval::plus(Interval iv, TCtype * elty) const
{
  Interval ret = Interval::Bottom;
  
  if(this->isTop() || iv.isTop()){
    ret = Interval::Top;
  } else if(this->isBottom() || iv.iszero()){ //- short circuit some degenerate cases
    ret = *this;
  } else if(iv.isBottom()){ //- short circuit some degenerate cases
    ret = iv;
  } else {
    ret = *this;
    if(adjustTypes(ret, iv, elty, true)){
      if((ret.min != MINUS_INF) && (iv.min != MINUS_INF))
        ret.min += iv.min;
      else ret.min = MINUS_INF;

      if((ret.max != PLUS_INF) && (iv.max != PLUS_INF))
        ret.max += iv.max;
      else ret.max = PLUS_INF;
    } else {
      ret.setBottom();
    }
  }
  return ret;
}

//- elty = 0      means "use underlying type"
//  elty = tcVoid means plain (non-pointer) arithmetic
//  elty = T      means pointer arithmetic on pointer-to-T (ptr-int)
// NOTE: (ptr-ptr) is covered by ptrs_minus().
Interval Interval::minus(Interval iv, TCtype * elty) const
{
  Interval ret = Interval::Bottom;
  if(this->isTop() || iv.isTop()){
    ret = Interval::Top;
  } else if(this->isBottom() || iv.isBottom()){
    //-NOP: to suppress informational warnings
  } else if(iv.iszero()){
    ret = *this;
  } else {
    ret = *this;
    if(adjustTypes(ret, iv, elty, false)){
      if((ret.min != MINUS_INF) && (iv.max != PLUS_INF))
        ret.min -= iv.max;
      else ret.min = MINUS_INF;
      if((ret.max != PLUS_INF) && (iv.min != MINUS_INF))
        ret.max -= iv.min;
      else ret.max = PLUS_INF;
    } else {
      ret.setBottom();
    }
  }
  return ret;
}

//- ptr minus ptr
Interval Interval::ptrs_minus(Interval iv, TCtype& elty) const
{
  if(this->isTop() || iv.isTop()){
    return Interval::Top;
  } else if(!this->tgt_elty && !iv.tgt_elty && (elty.getKind() == TCtype::tcChar)){
    return this->minus(iv, &TCtype::tcVoidType);
  } else if(this->tgt_ao && (this->tgt_ao == iv.tgt_ao)
	    && !this->has_zero && !iv.has_zero){
    Interval ret = Interval::Bottom;
    if(this->tgt_elty && this->tgt_elty->equiv(*iv.tgt_elty)
		      && this->tgt_elty->equiv(elty)){
      if((this->min != MINUS_INF) && (iv.max != PLUS_INF))
        ret.min = this->min - iv.max;
      if((this->max != PLUS_INF) && (iv.min != MINUS_INF))
        ret.max = this->max - iv.min;
    } else {
/**/  fprintf(stderr, "NOTE(Interval::ptrs_minus): same tgt_ao but mismatched types, this = ");
/**/  this->debug_dump(stderr);
/**/  fprintf(stderr, "  iv = ");
/**/  iv.debug_dump(stderr);
/**/  fprintf(stderr, "\n");
    }
    return ret;
  } else {
/**/if(!this->isBottom() && !iv.isBottom()){
/**/  fprintf(stderr, "NOTE(Interval::ptrs_minus): this = ");
/**/  this->debug_dump(stderr);
/**/  fprintf(stderr, "  iv = ");
/**/  iv.debug_dump(stderr);
/**/  fprintf(stderr, "\n");
/**/}
    return Interval::Bottom;
  }
}

Interval Interval::times(Interval iv) const
{
  Interval ret = Interval::Bottom;

  if(this->isTop() || iv.isTop()){
    ret = Interval::Top;
  } else if((this->tgt_elty) || (iv.tgt_elty)){

/**/fprintf(stderr, "WARNING(Interval::times): non-empty eltys, this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "  iv = ");
/**/iv.debug_dump(stderr);
/**/fprintf(stderr, "\n");

  } else if((!this->tgt_elty) && (!iv.tgt_elty) && (!this->isBottom()) && (!iv.isBottom())){

//TODO: detect overflows?

    ret.setTop();

    if( (this->min == MINUS_INF) || (iv.min == MINUS_INF) ||
	(this->max == MINUS_INF) || (iv.max == MINUS_INF) )
      ret.minmax_union(MINUS_INF,MINUS_INF);

    if( (this->min == PLUS_INF) || (iv.min == PLUS_INF) ||
	(this->max == PLUS_INF) || (iv.max == PLUS_INF) )
      ret.minmax_union(PLUS_INF,PLUS_INF);

    //- max * max
    if( (this->max != MINUS_INF) && (iv.max != MINUS_INF) &&
	(this->max != PLUS_INF)  && (iv.max != PLUS_INF) ){
      int tmp = this->max * iv.max;
      ret.minmax_union(tmp,tmp);
    }

    //- min * max
    if( (this->min != MINUS_INF) && (iv.max != MINUS_INF) &&
	(this->min != PLUS_INF)  && (iv.max != PLUS_INF) ){
      int tmp = this->min * iv.max;
      ret.minmax_union(tmp,tmp);
    }

    //- max * min
    if( (this->max != MINUS_INF) && (iv.min != MINUS_INF) &&
	(this->max != PLUS_INF)  && (iv.min != PLUS_INF) ){
      int tmp = this->max * iv.min;
      ret.minmax_union(tmp,tmp);
    }

    //- min * min
    if( (this->min != MINUS_INF) && (iv.min != MINUS_INF) &&
	(this->min != PLUS_INF)  && (iv.min != PLUS_INF) ){
      int tmp = this->min * iv.min;
      ret.minmax_union(tmp,tmp);
    }
  }
  return ret;
}

Interval Interval::divide(Interval iv) const
{
  if(this->isTop() || iv.isTop()){
    return Interval::Top;
  }

  if((this->tgt_elty) || (iv.tgt_elty)){
/**/fprintf(stderr, "WARNING(Interval::divide): non-empty eltys, this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "  iv = ");
/**/iv.debug_dump(stderr);
/**/fprintf(stderr, "\n");
    return Interval::Bottom;
  }

  Interval ret = Interval::Top;

  int maxpos1 = this->maxpos();
  int maxneg1 = this->maxneg();
  int minpos1 = this->minpos();
  int minneg1 = this->minneg();
  int maxpos2 = iv.maxpos();
  int maxneg2 = iv.maxneg();
  int minpos2 = iv.minpos();
  int minneg2 = iv.minneg();

  //- line 1: [ maxpos1/minneg2 , maxpos1/minpos2 ]
  ret.minmax_union((maxpos1 && minneg2)
			? ((maxpos1 == PLUS_INF || minneg2 == MINUS_INF)
			   ? MINUS_INF
			   : (maxpos1/minneg2))
			: PLUS_INF
		   ,(maxpos1 && minpos2)
			? ((maxpos1 == PLUS_INF || minpos2 == PLUS_INF)
			   ? PLUS_INF
			   : (maxpos1/minpos2))
			: MINUS_INF
		   );
  //- line 2: [ maxneg1/minpos2 , maxneg1/minneg2 ]
  ret.minmax_union((maxneg1 && minpos2)
			? ((maxneg1 == MINUS_INF || minpos2 == PLUS_INF)
			   ? MINUS_INF
			   : (maxneg1/minpos2))
			: PLUS_INF
		   ,(maxneg1 && minneg2)
			? ((maxneg1 == MINUS_INF || minneg2 == MINUS_INF)
			   ? PLUS_INF
			   : (maxneg1/minneg2))
			: MINUS_INF
		   );
  
  //- line 3: [ minpos1/maxpos2 , minpos1/maxneg2 ]
  ret.minmax_union((minpos1 && maxpos2)
			? ((minpos1 == PLUS_INF || maxpos2 == PLUS_INF)
			   ? MINUS_INF
			   : (minpos1/maxpos2))
			: PLUS_INF
		     ,(minpos1 && maxneg2)
			? ((minpos1 == PLUS_INF || maxneg2 == MINUS_INF)
			   ? PLUS_INF
			   : (minpos1/maxneg2))
			: MINUS_INF
		     );
  //- line 4: [ minneg1/maxneg2 , minneg1/maxpos2 ]
  ret.minmax_union((minneg1 && maxneg2)
			? ((minneg1 == MINUS_INF || maxneg2 == MINUS_INF)
			   ? MINUS_INF
			   : (minneg1/maxneg2))
			: PLUS_INF
		     ,(minneg1 && maxpos2)
			? ((minneg1 == MINUS_INF || maxpos2 == PLUS_INF)
			   ? PLUS_INF
			   : (minneg1/maxpos2))
			: MINUS_INF
		     );

  if(this->inclzero()){
    ret.minmax_union(0,0);
//  } else if(iv.inclzero()){	//- ignore divide-by-zero
//    ret.setBottom();
  }

  return ret;
}

Interval Interval::modulo(Interval iv) const
{
  Interval ret = Interval::Bottom;

  if(this->isTop() || iv.isTop()){
    ret = Interval::Top;
  } else if((this->tgt_elty) || (iv.tgt_elty)){

/**/fprintf(stderr, "WARNING(Interval::modulo): non-empty eltys, this = ");
/**/this->debug_dump(stderr);
/**/fprintf(stderr, "  iv = ");
/**/iv.debug_dump(stderr);
/**/fprintf(stderr, "\n");

  } else if(!this->isBottom() && !iv.isBottom()){

    int max_abs = 0;
    if((iv.max > 0) && (max_abs < iv.max-1))
      max_abs = iv.max-1;
    if((iv.min < 0) && (max_abs < -(iv.min+1)))
      max_abs = -(iv.min+1);
    
    bool pos = false, neg = false;
    if(this->max > 0 && iv.max > 0) pos = true;
    if(this->min < 0 && iv.min < 0) pos = true;
    if(this->max > 0 && iv.min < 0) neg = true;
    if(this->min < 0 && iv.max > 0) neg = true;

    ret = Interval(neg?(-max_abs):0, pos?max_abs:0);
  }
  return ret;
}

//TODO: detect overflows?
int Interval::maxpos() const
{
  return (max > 0)?max:0;
}

int Interval::maxneg() const
{
  return (min < 0)?min:0;
}

int Interval::minpos() const
{
  if(min > 0) return min;
  else if(max > 0) return 1;
  else return 0;
}

int Interval::minneg() const
{
  if(max < 0) return max;
  else if(min < 0) return -1;
  else return 0;
}

bool Interval::inclzero() const
{
  return has_zero || ((tgt_ao == 0) && (tgt_elty == 0) && (min <= 0) && (max >= 0));
}

bool Interval::iszero() const
{
  return ((tgt_ao == 0) && (tgt_elty == 0) && (min == 0) && (max == 0));
}

