#include <ctype.h>
#include <stdio.h>
#include <stdlib.h> // for strtoul

#include "flags.h"
#include "diag.h"
#include "ty.h"

//------------------------------------------------------
//- TCtype

TCtype TCtype::tcVoidType = TCtype(tcVoid);
TCtype TCtype::tcZeroType = TCtype(tcZero);
TCtype TCtype::tcIntType = TCtype(tcInt);
TCtype TCtype::tcCharType = TCtype(tcChar);
TCtype TCtype::tcShortType = TCtype(tcShort);
TCtype TCtype::tcLongType = TCtype(tcLong);
TCtype TCtype::tcLongLongType = TCtype(tcLongLong);
TCtype TCtype::tcFloatType = TCtype(tcFloat);
TCtype TCtype::tcDoubleType = TCtype(tcDouble);
TCtype TCtype::tcLongDoubleType = TCtype(tcLongDouble);

TCpointerType TCpointerType::tcVoidPtrType = TCpointerType(0);

const char * TCtype::kindString()
{
  switch(kind){
    case tcVoid: return "tcVoid";
    case tcZero: return "tcZero";
    case tcInt: return "tcInt";
    case tcChar: return "tcChar";
    case tcShort: return "tcShort";
    case tcLong: return "tcLong";
    case tcLongLong: return "tcLongLong";
    case tcFloat: return "tcFloat";
    case tcDouble: return "tcDouble";
    case tcLongDouble: return "tcLongDouble";
    case tcPointer: return "tcPointer";
    case tcStruct: return "tcStruct";
    case tcUnion: return "tcUnion";
    case tcArray: return "tcArray";
    case tcBitField: return "tcBitField";
    case tcFunction: return "tcFunction";
    default: return "ERROR";
  }
}

char TCtype::kindCode()
{
  switch(kind){
    case tcVoid: return 'v';
    case tcZero: return 'z';
    case tcInt: return 'i';
    case tcChar: return 'c';
    case tcShort: return 'h';
    case tcLong: return 'l';
    case tcLongLong: return 'g';
    case tcFloat: return 'f';
    case tcDouble: return 'd';
    case tcLongDouble: return 'e';
    case tcPointer: return 'p';
    case tcStruct: return 's';
    case tcUnion: return 'u';
    case tcArray: return 'a';
    case tcBitField: return 'b';
    case tcFunction: return 'x';
    default: return '!'; //-error
  }
}

void TCtype::debug_dump(FILE * os)
{
  fprintf(os, kindString());
}

void TCtype::write_string_rep(FILE * os)
{
  fprintf(os, "%c ", kindCode());
}

bool TCtype::equals(TCtype &t) const
{
  if(kind != t.kind) return false;
  switch(kind){
    case tcVoid:
    case tcZero:
    case tcInt:
    case tcChar:
    case tcShort:
    case tcLong:
    case tcLongLong:
    case tcFloat:
    case tcDouble:
    case tcLongDouble:
	 return true;

    case tcPointer:
	 return ((TCpointerType *)this)->ty.equals(((TCpointerType&)t).ty);

    case tcStruct:
    case tcUnion:
	 return TCtype::listEquals(((TCstructUnionType *)this)->tylist,((TCstructUnionType&)t).tylist);

    case tcArray:
    case tcBitField:{
	   TCnumType * lhs = (TCnumType *)this;
	   TCnumType * rhs = (TCnumType *)&t;
	   return (lhs->size == rhs->size) && lhs->ty.equals(rhs->ty);
	 }

    case tcFunction:{
	   TCfunctionType * lhs = (TCfunctionType *)this;
	   TCfunctionType * rhs = (TCfunctionType *)&t;
	   return lhs->rty.equals(rhs->rty) &&
		TCtype::listEquals(lhs->tylist, rhs->tylist);
	 }

    default: return true; //-error
  }
}

bool TCtype::equiv(TCtype &t) const
{
  if(kind != t.kind) return false;
  switch(kind){
    case tcVoid:
    case tcZero:
    case tcInt:
    case tcChar:
    case tcShort:
    case tcLong:
    case tcLongLong:
    case tcFloat:
    case tcDouble:
    case tcLongDouble:
    case tcPointer:
	 return true;

    case tcStruct:
    case tcUnion:
	 return TCtype::listEquiv(((TCstructUnionType *)this)->tylist, ((TCstructUnionType&)t).tylist);

    case tcArray:
    case tcBitField:{
	   TCnumType * lhs = (TCnumType *)this;
	   TCnumType * rhs = (TCnumType *)&t;
	   return (lhs->size == rhs->size) && lhs->ty.equiv(rhs->ty);
	 }

    case tcFunction:{
	   TCfunctionType * lhs = (TCfunctionType *)this;
	   TCfunctionType * rhs = (TCfunctionType *)&t;
	   return lhs->isEmpty() || rhs->isEmpty() ||
		(lhs->rty.equiv(rhs->rty) &&
		 TCtype::listEquiv(lhs->tylist, rhs->tylist));
	 }

    default: return true; //-error
  }
}

TCtype * TCtype::stringToTy(char * str, char ** nptr)
{
  char * c = str;
  while(isspace(*c)) c++;
  *nptr = c+1;
  switch(*c){
    case 'a': {
        unsigned int size = strtoul(c+1, &c, 10);
	TCtype * bty = stringToTy(c, nptr);
	if(!bty){
	  fprintf(stderr, "Malformed array type\n");
          return 0;
	}
	return new TCnumType(tcArray, size, *bty);
      }
    case 'b': {
        unsigned int size = strtoul(c+1, &c, 10);
	TCtype * bty = stringToTy(c, nptr);
	if(!bty){
	  fprintf(stderr, "Malformed bitfield type\n");
          return 0;
	}
	return new TCnumType(tcBitField, size, *bty);
      }
    case 'c': return &tcCharType;
    case 'd': return &tcDoubleType;
    case 'e': return &tcLongDoubleType;
    case 'f': return &tcFloatType;
    case 'g': return &tcLongLongType;
    case 'h': return &tcShortType;
    case 'i': return &tcIntType;
    case 'l': return &tcLongType;
    case 'p': return &TCpointerType::tcVoidPtrType;
    case 's':{
        suco_llist<TCtype *>& tylist = TCtype::stringToTyList(c+1, nptr);
	return new TCstructUnionType(tcStruct, tylist);
      }
    case 'u':{
        suco_llist<TCtype *>& tylist = TCtype::stringToTyList(c+1, nptr);
	return new TCstructUnionType(tcUnion, tylist);
      }
    case 'v': return &tcVoidType; /* return new TCtype(tcVoid); */
    case 'x': {
	TCtype * ty = stringToTy(c+1, &c);
	if(!ty){
	  fprintf(stderr, "Malformed function type\n");
          ty = new TCtype(tcInt);
	}
        suco_llist<TCtype *>& tylist = TCtype::stringToTyList(c, nptr);
	return new TCfunctionType(*ty, tylist);
      }
    case 'z': return &tcZeroType; /* return new TCtype(tcZero); */
    default :
      fprintf(stderr, "Invalid type identifier [%c] (%s)\n", *c, c);
      return 0; // invalid type
  }
}

void TCtype::deleteTy(TCtype& ty)
{
  if(ty.cleanup()) delete &ty;
}

//------------------------------------------------------
//- TCtype subtype inherited methods

void TCstructUnionType::debug_dump(FILE * os)
{
  fprintf(os, "%s(", kindString());
  debug_dump_list(tylist, os);
  fprintf(os, ")");
}

void TCfunctionType::debug_dump(FILE * os)
{
  fprintf(os, "%s[", kindString());
  rty.debug_dump(os);
  fprintf(os, "](");
  debug_dump_list(tylist, os);
  fprintf(os, ")");
}

void TCpointerType::debug_dump(FILE * os)
{
  fprintf(os, "%s[", kindString());
  ty.debug_dump(os);
  fprintf(os, "]");
}

void TCnumType::debug_dump(FILE * os)
{
  fprintf(os, "%s[", kindString());
  ty.debug_dump(os);
  fprintf(os, "](%d)",size);
}

//- - - - - - - - - - - - - - - -

void TCstructUnionType::write_string_rep(FILE * os)
{
  fprintf(os, "%c ", kindCode());
  write_list_string_rep(tylist, os);
  fprintf(os, "; ");
}

void TCfunctionType::write_string_rep(FILE * os)
{
  fprintf(os, "%c ", kindCode());
  rty.write_string_rep(os);
  write_list_string_rep(tylist, os);
  fprintf(os, "; ");
}

void TCpointerType::write_string_rep(FILE * os)
{ //- string rep of pointer excludes its target type,
  //  to avoid infinite recursion
  fprintf(os, "%c ", kindCode());
}

void TCnumType::write_string_rep(FILE * os)
{
  fprintf(os, "%c %d ", kindCode(), size);
  ty.write_string_rep(os);
}

//- - - - - - - - - - - - - - - -
//- countInstancesOf: estimate the ratio |this|/|ty|
//  by counting the occurrences of "ty" or larger -typed
//  objects in this object.
//- Scalar size assumptions are:
//  1. |char| <= everything except bitfields
//  2. |char|<=|short|<=|int|<=|long|<=|longlong|
//  3. |float|<=|double|<=|longdouble|?
int TCtype::countInstancesOf(TCtype& ty) const
{
  // this in {tcVoid, tcZero, tcInt, tcChar, tcShort, tcLong, tcLongLong,
  //          tcFloat, tcDouble, tcLongDouble}

  if((this->getKind() == tcVoid) || (this->getKind() == tcZero)){
    return 0; //- report error?
  }
  switch(ty.getKind()){
    case tcChar:
      return 1;
    case tcShort:
      return ( (this->getKind() == tcShort)
		|| (this->getKind() == tcInt)
		|| (this->getKind() == tcLong)
		|| (this->getKind() == tcLongLong)
	     ) ? 1 : 0;
    case tcInt:
      return ( (this->getKind() == tcInt)
		|| (this->getKind() == tcLong)
		|| (this->getKind() == tcLongLong)
	     ) ? 1 : 0;
    case tcLong:
      return ( (this->getKind() == tcLong)
		|| (this->getKind() == tcLongLong)
	     ) ? 1 : 0;
    case tcLongLong:
      return (this->getKind() == tcLongLong) ? 1 : 0;

    case tcFloat:
      return ( (this->getKind() == tcFloat)
		|| (this->getKind() == tcDouble)
		|| (this->getKind() == tcLongDouble)
	     ) ? 1 : 0;
    case tcDouble:
      return ( (this->getKind() == tcDouble)
		|| (this->getKind() == tcLongDouble)
	     ) ? 1 : 0;
    case tcLongDouble:
      return (this->getKind() == tcLongDouble) ? 1 : 0;

    case tcPointer:
    case tcStruct:
    case tcUnion:
    case tcArray:
    case tcBitField:
      return 0;

    default:
    case tcFunction:
    case tcVoid:
    case tcZero:
      // report error?
      return 0;
  }
}

//- this in { tcStruct, tcUnion }
int TCstructUnionType::countInstancesOf(TCtype& ty) const
{
  if(this->getKind() == tcStruct){
    //- First, check to see if this struct is a subtype of ty,
    //  where subtype is defined by the C "common-initial-sequence"
    //  criterion.
    if(ty.getKind() == tcStruct &&
	TCtype::listEquivPrefix(((TCstructUnionType &)ty).tylist, this->tylist)){
      return 1;
    }

    //- return sum { t.countInstancesOf(ty) | t in tylist }
    int sum = 0;
    suco_iterator<TCtype *> ti(this->tylist);
    while(ti.Iterate())
      sum += ti.Current()->countInstancesOf(ty);
    return sum;

  } else { //- tcUnion
    //- return max { t.countInstancesOf(ty) | t in tylist }
    int max = 0;
    suco_iterator<TCtype *> ti(this->tylist);
    while(ti.Iterate()){
      int n = ti.Current()->countInstancesOf(ty);
      if(max < n) max = n;
    }
    return max;

  }
  return 0;
}

int TCfunctionType::countInstancesOf(TCtype& ty) const
{
  return 0; //- report error?
}

//- this = tcPointer
int TCpointerType::countInstancesOf(TCtype& ty) const
{
  return ((ty.getKind() == tcPointer) || (ty.getKind() == tcChar))
	 ? 1 : 0;
}

//- this in { tcArray, tcBitField }
int TCnumType::countInstancesOf(TCtype& ty) const
{
  if(this->getKind() == tcArray){
    return this->getBaseType().countInstancesOf(ty) * this->getSize();
  } else { //- tcBitField
    return 0;
  }
}

//- - - - - - - - - - - - - - - -
//- SizeOf()
//  - return size based on a "common" scheme:
//    - |char|		= 1
//    - |short|		= 2
//    - |int|		= 4
//    - |long|		= 4
//    - |long long|	= 8
//    - |float|		= 4
//    - |double|	= 8
//    - |long double|	= 12
//    - |any pointer|	= 4
//    - array: |t[c]|	= c * |t|
//    - struct/union: assume padding to get 4-byte alignment
int TCtype::SizeOf() const
{
  switch(this->getKind()){
    case tcChar:	return 1;
    case tcShort:	return 2;
    case tcInt:		return 4;
    case tcLong:	return 4;
    case tcLongLong:	return 8;
    case tcFloat:	return 4;
    case tcDouble:	return 8;
    case tcLongDouble:	return 12;
    case tcPointer:	return 4;

    case tcStruct:
    case tcUnion:
    case tcArray:
    case tcBitField:
    default:
    case tcFunction:
    case tcVoid:
    case tcZero:	return 0;
  }
}

int TCstructUnionType::SizeOf() const
{
#define ALIGNMENT_OF(numbytes) (		\
		(numbytes == 0)			\
		? 0 : ( (numbytes % 2)		\
			? 1 : ( (numbytes % 4)	\
				? 2 : 4		\
			)	))

#define ADD_BYTES_TO_SUM(numbytes) do {			\
	  int new_align = ALIGNMENT_OF(numbytes);	\
	  if(align < new_align)				\
	    align = new_align;				\
	  if(sum % new_align != 0)			\
	    sum += new_align - (sum % new_align);	\
	  sum += numbytes;				\
	} while(0)

#define ADD_BITSIZES_TO_SUM do {			\
	  if(bit_tysize) {				\
	    int numbytes = 1 + numbits/(bit_tysize<<3);	\
	    ADD_BYTES_TO_SUM(numbytes);			\
	    bit_tysize = numbits = 0;			\
	  }						\
	} while(0)

  if(this->getKind() == tcStruct){
    int sum = 0;
    int align = 0;
    int numbits = 0;
    int bit_tysize = 0;
    suco_iterator<TCtype *> ti(this->tylist);
    while(ti.Iterate()){
      if(ti.Current()->getKind() == tcBitField){
        TCnumType& nt = *((TCnumType *)ti.Current());
        if(bit_tysize == nt.getBaseType().SizeOf()){
          numbits += nt.getSize();
        } else {
          ADD_BITSIZES_TO_SUM;
          bit_tysize = nt.getBaseType().SizeOf();
          numbits += nt.getSize();
        }
      } else {
        ADD_BITSIZES_TO_SUM;
        int elsize = ti.Current()->SizeOf();
        ADD_BYTES_TO_SUM(elsize);
      }
    }
    ADD_BITSIZES_TO_SUM;
    if(sum % align != 0)	//- terminal padding
      sum += align - (sum % align);
    return sum;

  } else { //- tcUnion
    int max = 0;
    int align = 0;
    suco_iterator<TCtype *> ti(this->tylist);
    while(ti.Iterate()){
      int n = ti.Current()->SizeOf();
      int n_align = ALIGNMENT_OF(n);
      if(align < n_align)
         align = n_align;
      if(max < n)
         max = n;
    }
    if(max % align != 0)
      max += align - (max % align);
    return max;
  }
#undef ADD_BITSIZES_TO_SUM
#undef ADD_BYTES_TO_SUM
#undef ALIGNMENT_OF
}

int TCnumType::SizeOf() const
{
  if(this->getKind() == tcArray)
    return this->size * this->ty.SizeOf();
  else //- bitfield
    return 0;
}

//------------------------------------------------------
//- TCtype list operations

void TCtype::debug_dump_list(suco_llist<TCtype *>& tl, FILE * os)
{
  suco_iterator<TCtype *> ti(tl);
  bool initial = true;
  while(ti.Iterate()){
    if(initial) initial = false;
    else fprintf(os, ",");
    ti.Current()->debug_dump(os);
  }
}

void TCtype::write_list_string_rep(suco_llist<TCtype *>& tl, FILE * os)
{
  suco_iterator<TCtype *> ti(tl);
  while(ti.Iterate())
    ti.Current()->write_string_rep(os);
}

suco_llist<TCtype *>& TCtype::stringToTyList(char * str, char ** nptr)
{
  suco_llist<TCtype *>& list = *new suco_llist<TCtype *>;
  char * c = str;
  while(isspace(*c)) c++;
  while(*c && *c != ';'){
    TCtype * ty = TCtype::stringToTy(c,&c);
    if(!ty){ //- error
      fprintf(stderr, "Malformed type list (%s)\n", c);
      *nptr = c;
      return list;
    }
    list.Append(ty);
    while(isspace(*c)) c++;
  }
  *nptr = c+1;
  return list;
}

void TCtype::deleteTyList(suco_llist<TCtype *>& tl)
{
  suco_iterator<TCtype *> ti(tl);
  while(ti.Iterate())
    deleteTy(*ti.Current());
  delete &tl;
}

bool TCtype::listEquals(suco_llist<TCtype *>& l1, suco_llist<TCtype *>& l2)
{
  suco_iterator<TCtype *> i1(l1);
  suco_iterator<TCtype *> i2(l2);
  while(i1.Iterate() & i2.Iterate())  //- DON'T DO LOGICAL-AND: don't short-circuit i2.Iterate!
    if(!i1.Current()->equals(*i2.Current()))
      return false;
  return (!i1.Current() && !i2.Current());
}

bool TCtype::listEquiv(suco_llist<TCtype *>& l1, suco_llist<TCtype *>& l2)
{
  suco_iterator<TCtype *> i1(l1);
  suco_iterator<TCtype *> i2(l2);
  while(i1.Iterate() & i2.Iterate()) //- DON'T DO LOGICAL-AND: don't short-circuit i2.Iterate!
    if(!i1.Current()->equiv(*i2.Current()))
      return false;
  return (!i1.Current() && !i2.Current());
}

//- Is prefix_list a prefix of full_list?
//  If so, return the element in full_list corresponding to the last element of prefix_list; else, return 0
//  Useful for identifying subtypes by C's "common-initial-sequence" criterion
//- NOTE: will return 0 if prefix_list is empty (may arise with empty structs??)
TCtype * TCtype::listEquivPrefix(suco_llist<TCtype *>& prefix_list, suco_llist<TCtype *>& full_list)
{
  suco_iterator<TCtype *> i1(prefix_list);
  suco_iterator<TCtype *> i2(full_list);
  TCtype * ret = 0;
  while(i1.Iterate()){
    if(i2.Iterate() && i1.Current()->equiv(*i2.Current())){
      ret = i2.Current();	//- key here: return i2.Current instead of i1.Current ~~ a quirk needed by Interval::adjustStructOffset()
    } else return 0;
  }
  return ret;
}

//------------------------------------------------------

TClatType::latKind TClatType::getLatKindFor(TCtype& t)
{
  switch(t.getKind()){
    case TCtype::tcChar:	return tclChar;
    case TCtype::tcShort:	return tclShort;
    case TCtype::tcInt:		return tclInt;
    case TCtype::tcLong:	return tclLong;
    case TCtype::tcLongLong:	return tclLongLong;
    case TCtype::tcFloat:	return tclFloat;
    case TCtype::tcDouble:	return tclDouble;
    case TCtype::tcLongDouble:	return tclLongDouble;
    case TCtype::tcPointer:	return tclPointer;
    case TCtype::tcZero:	return tclZero;
    default:
	fprintf(stderr, "Invalid lattice type initialization: ");
	t.debug_dump(stderr);
	fprintf(stderr, "\n");
	return tclBottom;
  }
}

TClatType::latKind TClatType::Join(latKind k1, latKind k2)
{
  if(k1 == k2) return k1;
  else {
    if((k1 / 100) == (k2 / 100)){ //- only possible if both are scalar
      return tclZero;
    } else return (k1 > k2) ? k1 : k2;
  }
}

TClatType::latKind TClatType::Meet(latKind k1, latKind k2)
{
  if(k1 == k2) return k1;
  else {
    if((k1 / 100) == (k2 / 100)){ //- only possible if both are scalar
      return tclBottom;
    } else return (k1 < k2) ? k1 : k2;
  }
}

void TClatType::join(latKind k)
{
  kind = Join(kind, k);
}

void TClatType::meet(latKind k)
{
  kind = Meet(kind, k);
}

void TClatType::Debug_dump(latKind k, FILE * os)
{
  switch(k){
    case tclBottom:	fprintf(os, "tclBottom"); break;
    case tclChar:	fprintf(os, "tclChar"); break;
    case tclShort:	fprintf(os, "tclShort"); break;
    case tclInt:	fprintf(os, "tclInt"); break;
    case tclLong:	fprintf(os, "tclLong"); break;
    case tclLongLong:	fprintf(os, "tclLongLong"); break;
    case tclFloat:	fprintf(os, "tclFloat"); break;
    case tclDouble:	fprintf(os, "tclDouble"); break;
    case tclLongDouble:	fprintf(os, "tclLongDouble"); break;
    case tclPointer:	fprintf(os, "tclPointer"); break;
    case tclZero:	fprintf(os, "tclZero"); break;
    case tclTop:	fprintf(os, "tclTop"); break;
  }
}

void TClatType::debug_dump(FILE * os)
{
  Debug_dump(kind, os);
}

//-- constrains this <= gl
//   note that this adds "this" to gl's nodesLEthis list
//   (rather than vice versa, as might be expected)
void TClatType::constrainLE(TClatType& gl)
{
  TCstats::num_pt_constraints++;
  gl.nodesLEthis.Insert(this);
}

//------------------------------------------------------

TClatRootSet::TClatRootSet()
{
  roots[lrBottom].setKind(TClatType::tclBottom);
  roots[lrChar].setKind(TClatType::tclChar);
  roots[lrShort].setKind(TClatType::tclShort);
  roots[lrInt].setKind(TClatType::tclInt);
  roots[lrLong].setKind(TClatType::tclLong);
  roots[lrLongLong].setKind(TClatType::tclLongLong);
  roots[lrFloat].setKind(TClatType::tclFloat);
  roots[lrDouble].setKind(TClatType::tclDouble);
  roots[lrLongDouble].setKind(TClatType::tclLongDouble);
  roots[lrPointer].setKind(TClatType::tclPointer);
  roots[lrZero].setKind(TClatType::tclZero);
  roots[lrTop].setKind(TClatType::tclTop);
}

void TClatRootSet::debug_dump(FILE * os)
{
  int i;
  fprintf(os, "TClatRootSet stats:\n");
  for(i = 0; i < lrMax; ++i){
    roots[i].debug_dump(os);
    fprintf(os, ": %d LE constraints\n", roots[i].nodesLEthis.Length());
  }
}

TClatType& TClatRootSet::getLatType(TCtype& t)
{
  switch(t.getKind()){
    case TCtype::tcChar:	return roots[lrChar];
    case TCtype::tcShort:	return roots[lrShort];
    case TCtype::tcInt:		return roots[lrInt];
    case TCtype::tcLong:	return roots[lrLong];
    case TCtype::tcLongLong:	return roots[lrLongLong];
    case TCtype::tcFloat:	return roots[lrFloat];
    case TCtype::tcDouble:	return roots[lrDouble];
    case TCtype::tcLongDouble:	return roots[lrLongDouble];
    case TCtype::tcPointer:	return roots[lrPointer];
    case TCtype::tcZero:	return roots[lrZero];
    case TCtype::tcVoid:	return roots[lrBottom];
    default:
	fprintf(stderr, "Invalid lattice root lookup: ");
	t.debug_dump(stderr);
	fprintf(stderr, "\n");
	return roots[lrBottom];
  }
}

void TClatRootSet::traverseMeetWith(TClatType& node, TClatType::latKind lty)
{
  if(TClatType::LE(node.kind, lty)) return; // no need to go further

  TCstats::num_constraint_solving_visits++;

  node.meet(lty);

  suco_iterator<TClatType *> lei(node.nodesLEthis);
  while(lei.Iterate())
    traverseMeetWith(*lei.Current(), lty);
}

void TClatRootSet::solveMeet()
{
  int i;
  for(i = 0; i < lrMax; ++i){
    TClatType::latKind ity = roots[i].kind;
    suco_iterator<TClatType *> lei(roots[i].nodesLEthis);
    while(lei.Iterate())
      traverseMeetWith(*lei.Current(), ity);

    if(flag_verbose > 2){
      static int last_constr_visits = 0;
      fprintf(stderr, "Nodes Visited for kind ");
      roots[i].debug_dump(stderr);
      fprintf(stderr, " = %d\n", TCstats::num_constraint_solving_visits - last_constr_visits);
      last_constr_visits = TCstats::num_constraint_solving_visits;
    }
  }
}

//------------------------------------------------------
