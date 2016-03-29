#ifndef _TYPECHECK_API_H /* { */
#define _TYPECHECK_API_H

/*****************************************/
/******** general type info stuff ********/
/*****************************************/

/* Include size_t typedef, so we don't have to include stdlib.h. */
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned int    size_t;
#endif

/* the index into type_info[] array in pmirror.c (or whatever) must
   match this enum */
typedef enum _ctype {
  _ctype_void_invalid = 0,
  _ctype_int = 1,
  _ctype_char = 2,
  _ctype_short = 3,
  _ctype_long = 4,
  _ctype_longlong = 5,
  _ctype_float = 6,
  _ctype_double = 7,
#ifdef LONGDOUBLE /* { */
/* AAL: Keep it 8 for easier debugging? */
  _ctype_longdouble = 8,
#endif /* } LONGDOUBLE */
  _ctype_pointer = 9,
  _ctype_aggregate = 10
} _ctype_t;

#ifndef TC_PREINSTR /* { */

typedef struct {
  unsigned char * ptr;
  unsigned char bit;
} _mirror_pos_t;

#include <tcapi_postinstr.h>
#if defined(TC_USE_MACROS) || defined(TC_INTERNAL) /* { */

typedef struct {
  void * staticrep_ptr;
  _typetag_t typetag;
  size_t size; 
  int logsize;
  unsigned char tagbyte; /* combines typetag and logsize */
  _ctype_t promo;
  const char * desc;
} _typeinfo_t;

extern _typeinfo_t typeinfo[];

/* newtags functions */
void _touchMirrorPage(unsigned long mapindex);

#endif /* } defined TC_USE_MACROS or TC_INTERNAL */

#if defined(TC_USE_MACROS) /* { */
/* for now, we'll define this here */
/*
#define COUNT_MACROS
*/

/* for alignment checks */
#ifdef TC_CHECK_ALIGN /* { */
#define ALIGN_CHECK(test)  test
#define ALIGN_AND          &&
#else /* } if !TC_CHECK_ALIGN { */
#define ALIGN_CHECK(test)
#define ALIGN_AND
#endif /* } TC_CHECK_ALIGN */

/* macro statistic counting stuff */
#if defined(COUNT_MACROS) /* { */

extern int _vpctr, _vpcctr, _vpactr, /* verifyPtr */
	   _vpfctr,_vpcfctr,_vpafctr;
extern int _vtctr, _vtcctr, _vtactr, /* verifyTag */
	   _vtfctr,_vtcfctr,_vtafctr;
extern int _ctctr, _ctcctr, _ctactr, /* copyTag */
	   _ctfctr,_ctcfctr,_ctafctr;
extern int _sstctr, _sstcctr, /* setScalarTag */
	   _sstfctr,_sstcfctr;
extern int _ssutctr, _ssutcctr, /* setScalarUninitTag */
	   _ssutfctr,_ssutcfctr;
extern int _patctr, _patactr, _patpctr, /* processArgTag */
	   _patfctr,_patafctr;

/* Avoid having a bogus expression when not counting macros. */
#define CNTMAC(x) (x++)
#define CNTCOMMA  ,
#else /* } if !COUNT_MACROS { */
#define CNTMAC(x)
#define CNTCOMMA
#endif /* } !COUNT_MACROS */

#endif /* } TC_USE_MACROS */

#endif /* } !TC_PREINSTR */

/****************************************************/
/****************** API functions *******************/
/****************************************************/

/********************/
/* registerFunction */
/********************/
#if defined(TC_PREINSTR) /* { */
extern void _registerFunction(const char * fname, int line, int col, void * addr);
#else /* } if !TC_PREINSTR { */
#define _registerFunction(f,l,c,a)
#endif /* } !TC_PREINSTR */

/******************/
/* registerExtern */
/******************/
#if defined(TC_PREINSTR) /* { */
extern void _registerExtern(const char * fname, int line, int col,
			void * addr, _ctype_t type, size_t size);
#else /* } if !TC_PREINSTR { */
extern void _registerExtern_nosize(const char * fname, int line, int col,
			void * addr, _ctype_t type);
#define _registerExtern(f,l,c,a,t,s) _registerExtern_nosize(f,l,c,a,t)
#endif /* } !TC_PREINSTR */

/*************/
/* verifyPtr */
/*************/
#if defined(TC_PREINSTR) /* { */
extern void _verifyPtr_int(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
extern void _verifyPtr_char(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
extern void _verifyPtr_short(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
extern void _verifyPtr_long(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
extern void _verifyPtr_longlong(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
extern void _verifyPtr_float(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
extern void _verifyPtr_double(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
#ifdef LONGDOUBLE /* { */
extern void _verifyPtr_longdouble(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
#endif /* } LONGDOUBLE */
extern void _verifyPtr_pointer(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
extern void _verifyPtr_aggregate(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);
#else /* } if !TC_PREINSTR { */
#if defined(TC_USE_MACROS) /* { */
extern void _verifyPtr(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);

/* The tests ANDing with tag masks below assume that
   _typetag_unalloc is 0, and that CONT_BIT also has
   to be cleared for the tag to represent unalloc, so
   no unalloc tags bigger than 1 byte are possible.

   Note, most of the macros in this file rely on the fact
   that sizes and tags with value 0 are bad, and so the
   outer || operators invoke the corresponding functions. */
#if defined(OLD_MACROS) || defined(OLD_VERIFY_PTR) || defined(SAFE_VERIFY_PTR) /* { */
#define _verifyPtr_char(f,l,c,exp,addr,size) ( \
	  CNTMAC(_vpcctr) CNTCOMMA \
	  ( ((unsigned long) (addr) & 0x1) \
	    ? (GET_TAG_BYTE(addr) & ODD_TAG_MASK) \
	    : (GET_TAG_BYTE(addr) & EVEN_TAG_MASK) \
	  ) || (_verifyPtr(f,l,c,exp,addr,size) \
		CNTCOMMA CNTMAC(_vpcfctr), 1) \
	)

/* The alignment test in the macro can be removed.
   It should not fail for any reasonable machine.
   Should we skip the size part of the test while the
   work is also done by _verifyTag? */
#define VP_SIZE(f,l,c,exp,addr,size) ( \
	  CNTMAC(_vpctr) CNTCOMMA \
	  ( ALIGN_CHECK(!((unsigned long) (addr) & 0x1)) \
	    ALIGN_AND ((GET_TAG_BYTE(addr) & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
			== ODD_CONT_MASK) \
	    && ((1 << (ODD_TYPE_BITS(GET_TAG_BYTE(addr)))) >= size) \
	  ) || (_verifyPtr(f,l,c,exp,addr,size) \
		CNTCOMMA CNTMAC(_vpfctr), 1) \
	)

#define _verifyPtr_aggregate(f,l,c,e,a,s) \
		(CNTMAC(_vpactr) CNTCOMMA \
		 _verifyPtr(f,l,c,e,a,s) CNTCOMMA CNTMAC(_vpafctr), 1)
#else /* } !(defined(OLD_MACROS) || defined(OLD_VERIFY_PTR) || defined(SAFE_VERIFY_PTR)) { */
#define _verifyPtr_char(f,l,c,exp,addr,size) ( \
	  CNTMAC(_vpcctr) CNTCOMMA \
	  GET_TAG_BYTE(addr) \
	  || (_verifyPtr(f,l,c,exp,addr,size) \
	      CNTCOMMA CNTMAC(_vpcfctr), 1) \
	)

#define VP_SIZE(f,l,c,exp,addr,size) ( \
	  CNTMAC(_vpctr) CNTCOMMA \
	  ( ALIGN_CHECK(!((unsigned long) (addr) & 0x1)) \
	    ALIGN_AND GET_TAG_BYTE(addr)) \
	  || (_verifyPtr(f,l,c,exp,addr,size) \
	      CNTCOMMA CNTMAC(_vpfctr), 1) \
	)

#define _verifyPtr_aggregate(f,l,c,exp,addr,size) ( \
	  CNTMAC(_vpactr) CNTCOMMA \
	  GET_TAG_BYTE(addr) \
	  && GET_TAG_BYTE((char*)addr+size-1) \
	  || (_verifyPtr(f,l,c,exp,addr,size) CNTCOMMA CNTMAC(_vpafctr), 1) \
	)
#endif /* } !(defined(OLD_MACROS) || defined(OLD_VERIFY_PTR)) */

#define _verifyPtr_int(f,l,c,e,a,s)		VP_SIZE(f,l,c,e,a,s)
#define _verifyPtr_short(f,l,c,e,a,s)		VP_SIZE(f,l,c,e,a,s)
#define _verifyPtr_long(f,l,c,e,a,s)		VP_SIZE(f,l,c,e,a,s)
#define _verifyPtr_longlong(f,l,c,e,a,s)	VP_SIZE(f,l,c,e,a,s)
#define _verifyPtr_float(f,l,c,e,a,s)		VP_SIZE(f,l,c,e,a,s)
#define _verifyPtr_double(f,l,c,e,a,s)		VP_SIZE(f,l,c,e,a,s)
#ifdef LONGDOUBLE /* { */
#define _verifyPtr_longdouble(f,l,c,e,a,s)	VP_SIZE(f,l,c,e,a,s)
#endif /* } LONGDOUBLE */
#define _verifyPtr_pointer(f,l,c,e,a,s)		VP_SIZE(f,l,c,e,a,s)

#else /* } if !TC_USE_MACROS { */

extern void _verifyPtr(const char * fname, int line, int col,
			const char * exp, const void * addr, size_t size);

#define _verifyPtr_int(f,l,c,e,a,s)		_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_char(f,l,c,e,a,s)		_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_short(f,l,c,e,a,s)		_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_long(f,l,c,e,a,s)		_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_longlong(f,l,c,e,a,s)	_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_float(f,l,c,e,a,s)		_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_double(f,l,c,e,a,s)		_verifyPtr(f,l,c,e,a,s)
#ifdef LONGDOUBLE /* { */
#define _verifyPtr_longdouble(f,l,c,e,a,s)	_verifyPtr(f,l,c,e,a,s)
#endif /* } LONGDOUBLE */
#define _verifyPtr_pointer(f,l,c,e,a,s)		_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_aggregate(f,l,c,e,a,s)	_verifyPtr(f,l,c,e,a,s)
#endif /* } !TC_USE_MACROS */
#endif /* } !TC_PREINSTR */

/*************/
/* verifyTag */
/*************/
#if defined(TC_PREINSTR) /* { */
extern void _verifyTag_int(const char * fname, int line, int col,
			const char * exp, const void * addr);
extern void _verifyTag_char(const char * fname, int line, int col,
			const char * exp, const void * addr);
extern void _verifyTag_short(const char * fname, int line, int col,
			const char * exp, const void * addr);
extern void _verifyTag_long(const char * fname, int line, int col,
			const char * exp, const void * addr);
extern void _verifyTag_longlong(const char * fname, int line, int col,
			const char * exp, const void * addr);
extern void _verifyTag_float(const char * fname, int line, int col,
			const char * exp, const void * addr);
extern void _verifyTag_double(const char * fname, int line, int col,
			const char * exp, const void * addr);
#ifdef LONGDOUBLE /* { */
extern void _verifyTag_longdouble(const char * fname, int line, int col,
			const char * exp, const void * addr);
#endif /* } LONGDOUBLE */
extern void _verifyTag_pointer(const char * fname, int line, int col,
			const char * exp, const void * addr);
extern void _verifyTag_aggregate(const char * fname, int line, int col,
			const char * exp, const void * addr);
#else /* } if !TC_PREINSTR { */

extern void _verifyTag(const char * fname, int line, int col,
			const char * exp, const void * addr, _ctype_t reftype);
extern int _verifyTagSilent(const void * addr, _ctype_t reftype);

#if defined(TC_USE_MACROS) /* { */

#ifdef NEW_VERIFY_TAG /* { */
#define _verifyTag_char(f,l,c,e,addr)	( \
	  CNTMAC(_vtcctr) CNTCOMMA \
	  !(GET_TAG_BYTE(addr) & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && GET_CHAR_TAG(GET_TAG_BYTE(addr), (unsigned long)(addr)&0x1) == _typetag_int \
	  || (_verifyTag(f,l,c,e,addr,_ctype_char) \
	      CNTCOMMA CNTMAC(_vtcfctr), 1) \
	)

#elif defined(FAST_VERIFY_TAG) /* } !NEW_VERIFY_TAG { */
static unsigned char _vt_tmp_tag;

/* Surprisingly, this version is slower than the one above,
   which repeats GET_TAG_BYTE(addr) twice. */
#define _verifyTag_char(f,l,c,e,addr)	( \
	  CNTMAC(_vtcctr) CNTCOMMA \
	  (_vt_tmp_tag = GET_TAG_BYTE(addr), \
	   !(_vt_tmp_tag & (EVEN_CONT_MASK|ODD_CONT_MASK))) \
	  && GET_CHAR_TAG(_vt_tmp_tag, (unsigned long)(addr)&0x1) == _typetag_int \
	  || (_verifyTag(f,l,c,e,addr,_ctype_char) \
	      CNTCOMMA CNTMAC(_vtcfctr), 1) \
	)
#else /* } !defined(FAST_VERIFY_TAG) { */
/* Comparison with _typetag_int assumes char is the only int type
   that can be unaligned on a two byte boundary.  Comparison with
   EVEN_TAG_MASK|ODD_CONT_MASK assumes _typetag_unalloc is 0. */
#define _verifyTag_char(f,l,c,e,addr)	( \
	  CNTMAC(_vtcctr) CNTCOMMA \
	  ( ((unsigned long) (addr) & 0x1) \
	    ? (ODD_TAG_BITS(GET_TAG_BYTE(addr)) == _typetag_int) \
	    : ((GET_TAG_BYTE(addr) & (EVEN_TAG_MASK|ODD_CONT_MASK)) \
		== _typetag_int) \
	  ) || (_verifyTag(f,l,c,e,addr,_ctype_char) \
		CNTCOMMA CNTMAC(_vtcfctr), 1) \
	)
#endif /* } !defined(FAST_VERIFY_TAG) */

/* GET_TAG_BYTE(&type##_static_rep[0]) should be changed to a
   pre-computed first byte of the tag rather than a tag lookup. */
#define VT_NONCHAR(f,l,c,e,addr,type)	( \
	  CNTMAC(_vtctr) CNTCOMMA \
	  ( ALIGN_CHECK(!((unsigned long) (addr) & 0x1)) \
	    ALIGN_AND (GET_TAG_BYTE(addr) == GET_TAG_BYTE(&type##_static_rep[0]))) \
	  || (_verifyTag(f,l,c,e,addr,_ctype##type) \
	      CNTCOMMA CNTMAC(_vtfctr), 1) \
	)

#define _verifyTag_int(f,l,c,e,a)	VT_NONCHAR(f,l,c,e,a,_int)
#define _verifyTag_short(f,l,c,e,a)	VT_NONCHAR(f,l,c,e,a,_short)
#define _verifyTag_long(f,l,c,e,a)	VT_NONCHAR(f,l,c,e,a,_long)
#define _verifyTag_longlong(f,l,c,e,a)	VT_NONCHAR(f,l,c,e,a,_longlong)
#define _verifyTag_float(f,l,c,e,a)	VT_NONCHAR(f,l,c,e,a,_float)
#define _verifyTag_double(f,l,c,e,a)	VT_NONCHAR(f,l,c,e,a,_double)
#define _verifyTag_pointer(f,l,c,e,a)	VT_NONCHAR(f,l,c,e,a,_pointer)

#ifdef LONGDOUBLE /* { */
#define _verifyTag_longdouble(f,l,c,e,a) VT_NONCHAR(f,l,c,e,a,_longdouble)
#endif /* } LONGDOUBLE */
/* Note: currently _verifyTag does nothing on aggregate types, so bypass */
#define _verifyTag_aggregate(f,l,c,e,a)	(CNTMAC(_vtactr) CNTCOMMA 1)

#else /* } if !TC_USE_MACROS { */

#define _verifyTag_int(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_int)
#define _verifyTag_char(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_char)
#define _verifyTag_short(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_short)
#define _verifyTag_long(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_long)
#define _verifyTag_longlong(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_longlong)
#define _verifyTag_float(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_float)
#define _verifyTag_double(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_double)
#ifdef LONGDOUBLE /* { */
#define _verifyTag_longdouble(f,l,c,e,a) _verifyTag(f,l,c,e,a,_ctype_longdouble)
#endif /* } LONGDOUBLE */
#define _verifyTag_pointer(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_pointer)
#define _verifyTag_aggregate(f,l,c,e,a)	_verifyTag(f,l,c,e,a,_ctype_aggregate)

#endif /* } !TC_USE_MACROS */
#endif /* } !TC_PREINSTR */

/***********/
/* copyTag */
/***********/
#if defined(TC_PREINSTR) /* { */
extern void _copyTag_int(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
extern void _copyTag_char(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
extern void _copyTag_short(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
extern void _copyTag_long(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
extern void _copyTag_longlong(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
extern void _copyTag_float(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
extern void _copyTag_double(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
#ifdef LONGDOUBLE /* { */
extern void _copyTag_longdouble(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
#endif /* } LONGDOUBLE */
extern void _copyTag_pointer(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
extern void _copyTag_aggregate(const char * fname, int line, int col,
			const void * dst, const void * src, size_t size);
#else /* } if !TC_PREINSTR { */

extern void _copyTag(const char * fname, int line, int col,
		     const void * dst, const void * src, size_t size, _ctype_t type);

#if defined(TC_USE_MACROS) /* { */

static unsigned char * _ct_tmp_dstptr, _ct_tmp_src;
static unsigned char _ct_tmp_tag;

#if defined(OLD_MACROS) || defined(OLD_COPY_TAG) /* { */
/* Lack of tests against CONT_MASKs when ((unsigned long) ([src\dst]) & 0x1)
   is 1 is due to the assumption that char is the only int type
   that can be unaligned on a two byte boundary. */
#define _copyTag_char(f,l,c,dst,src,z) (\
	  CNTMAC(_ctcctr) CNTCOMMA \
	  _ct_tmp_src = GET_TAG_BYTE(src), \
	  _ct_tmp_dstptr = &GET_TAG_BYTE(dst), \
	  !(_ct_tmp_src & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && (_ct_tmp_tag = ((unsigned long) (src) & 0x1) \
	      ? ODD_TAG_BITS(_ct_tmp_src) : EVEN_TAG_BITS(_ct_tmp_src)) \
	  && !(*_ct_tmp_dstptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && ( ((unsigned long) (dst) & 0x1) \
	       ? ((*_ct_tmp_dstptr & ODD_TAG_MASK) \
		  && (*_ct_tmp_dstptr &= ~ODD_TAG_MASK, \
		      *_ct_tmp_dstptr |= (_ct_tmp_tag << BITS_PER_TAG))) \
	       : ((*_ct_tmp_dstptr & EVEN_TAG_MASK) \
		  && (*_ct_tmp_dstptr &= ~EVEN_TAG_MASK, \
		      *_ct_tmp_dstptr |= _ct_tmp_tag))) \
	  || (_copyTag(f,l,c,dst,src,z,_ctype_char) \
	      CNTCOMMA CNTMAC(_ctcfctr), 1) \
	)

/* The alignment tests in the macro can be removed.
   They should not fail for any reasonable machine. */
#define CT_NONCHAR(f,l,c,dst,src,size,type)	( \
	  CNTMAC(_ctctr) CNTCOMMA \
	  _ct_tmp_src = GET_TAG_BYTE(src), \
	  _ct_tmp_dstptr = &GET_TAG_BYTE(dst), \
	  ( ALIGN_CHECK(!(((unsigned long)(src)|(unsigned long)(dst)) & 0x1)) \
	    ALIGN_AND ((_ct_tmp_src & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
			== ODD_CONT_MASK) \
	    && ((1 << (ODD_TYPE_BITS(_ct_tmp_src))) == size) \
	    && ((*_ct_tmp_dstptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
			== ODD_CONT_MASK) \
	    && ((1 << (ODD_TYPE_BITS(*_ct_tmp_dstptr))) == size) \
	    && (*_ct_tmp_dstptr = _ct_tmp_src) \
	  ) || (_copyTag(f,l,c,dst,src,size,_ctype##type) \
		CNTCOMMA CNTMAC(_ctfctr), 1) \
	)
#elif defined(FAST_COPY_TAG) /* } !(defined(OLD_MACROS) || defined(OLD_COPY_TAG)) { */
/* Lack of tests against CONT_MASKs when ((unsigned long) ([src\dst]) & 0x1)
   is 1 is due to the assumption that char is the only int type
   that can be unaligned on a two byte boundary. */
#define _copyTag_char(f,l,c,dst,src,z) (\
	  CNTMAC(_ctcctr) CNTCOMMA \
	  _ct_tmp_src = GET_TAG_BYTE(src), \
	  _ct_tmp_dstptr = &GET_TAG_BYTE(dst), \
	  !(_ct_tmp_src & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && !(*_ct_tmp_dstptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && GET_CHAR_TAG(_ct_tmp_src, (unsigned long)(src)&0x1) == _typetag_int \
	  && GET_CHAR_TAG(*_ct_tmp_dstptr, (unsigned long)(dst)&0x1) == _typetag_int \
	  || (_copyTag(f,l,c,dst,src,z,_ctype_char) \
	      CNTCOMMA CNTMAC(_ctcfctr), 1) \
	)

#define CT_NONCHAR(f,l,c,dst,src,size,type)	( \
	  CNTMAC(_ctctr) CNTCOMMA \
	  _ct_tmp_src = GET_TAG_BYTE(src), \
	  _ct_tmp_dstptr = &GET_TAG_BYTE(dst), \
	  ( ALIGN_CHECK(!(((unsigned long)(src)|(unsigned long)(dst)) & 0x1)) \
	    ALIGN_AND _ct_tmp_src == typeinfo[_ctype##type].tagbyte \
	    && *_ct_tmp_dstptr == _ct_tmp_src \
	  ) || (_copyTag(f,l,c,dst,src,size,_ctype##type) \
		CNTCOMMA CNTMAC(_ctfctr), 1) \
	)
#else /* } !defined(FAST_COPY_TAG) { */
/* Lack of tests against CONT_MASKs when ((unsigned long) ([src\dst]) & 0x1)
   is 1 is due to the assumption that char is the only int type
   that can be unaligned on a two byte boundary. */
#define _copyTag_char(f,l,c,dst,src,z) (\
	  CNTMAC(_ctcctr) CNTCOMMA \
	  _ct_tmp_src = GET_TAG_BYTE(src), \
	  _ct_tmp_dstptr = &GET_TAG_BYTE(dst), \
	  !(_ct_tmp_src & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && (_ct_tmp_tag = GET_CHAR_TAG(_ct_tmp_src, (unsigned long)(src)&0x1), \
	      _ct_tmp_tag == _typetag_int) \
	  && !(*_ct_tmp_dstptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && GET_CHAR_TAG(*_ct_tmp_dstptr, (unsigned long)(dst)&0x1) \
	  && SET_CHAR_TAG(*_ct_tmp_dstptr, _ct_tmp_tag, (unsigned long)(dst)&0x1) \
	  || (_copyTag(f,l,c,dst,src,z,_ctype_char) \
	      CNTCOMMA CNTMAC(_ctcfctr), 1) \
	)

#define CT_NONCHAR(f,l,c,dst,src,size,type) ( \
	  CNTMAC(_ctctr) CNTCOMMA \
	  _ct_tmp_src = GET_TAG_BYTE(src), \
	  _ct_tmp_dstptr = &GET_TAG_BYTE(dst), \
	  ALIGN_CHECK(!(((unsigned long)(src)|(unsigned long)(dst)) & 0x1)) \
	  ALIGN_AND _ct_tmp_src == typeinfo[_ctype##type].tagbyte \
	  && (*_ct_tmp_dstptr == _ct_tmp_src \
	      /* Below is just for the size comparison.  Since src is compared \
		 against static rep, we can do this simple check here. */ \
	      || (ODD_TAG_BITS(_ct_tmp_src) == ODD_TAG_BITS(*_ct_tmp_dstptr) ? \
		  (*_ct_tmp_dstptr = _ct_tmp_src) : 0) \
	      ) \
	  || (_copyTag(f,l,c,dst,src,size,_ctype##type) \
	      CNTCOMMA CNTMAC(_ctfctr), 1) \
	)
#endif /* } !defined(FAST_COPY_TAG) */

#define _copyTag_int(f,l,c,d,s,z)	CT_NONCHAR(f,l,c,d,s,z,_int)
#define _copyTag_short(f,l,c,d,s,z)	CT_NONCHAR(f,l,c,d,s,z,_short)
#define _copyTag_long(f,l,c,d,s,z)	CT_NONCHAR(f,l,c,d,s,z,_long)
#define _copyTag_longlong(f,l,c,d,s,z)	CT_NONCHAR(f,l,c,d,s,z,_longlong)
#define _copyTag_float(f,l,c,d,s,z)	CT_NONCHAR(f,l,c,d,s,z,_float)
#define _copyTag_double(f,l,c,d,s,z)	CT_NONCHAR(f,l,c,d,s,z,_double)
#ifdef LONGDOUBLE /* { */
#define _copyTag_longdouble(f,l,c,d,s,z) CT_NONCHAR(f,l,c,d,s,z,_longdouble)
#endif /* } LONGDOUBLE */
#define _copyTag_pointer(f,l,c,d,s,z)	CT_NONCHAR(f,l,c,d,s,z,_pointer)
#define _copyTag_aggregate(f,l,c,d,s,z)	\
		(CNTMAC(_ctactr) CNTCOMMA \
		 _copyTag(f,l,c,d,s,z,_ctype_aggregate) \
		 CNTCOMMA CNTMAC(_ctafctr), 1)

#else /* } if !TC_USE_MACROS { */

#define _copyTag_int(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_int)
#define _copyTag_char(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_char)
#define _copyTag_short(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_short)
#define _copyTag_long(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_long)
#define _copyTag_longlong(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_longlong)
#define _copyTag_float(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_float)
#define _copyTag_double(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_double)
#ifdef LONGDOUBLE /* { */
#define _copyTag_longdouble(f,l,c,d,s,z) _copyTag(f,l,c,d,s,z,_ctype_longdouble)
#endif /* } LONGDOUBLE */
#define _copyTag_pointer(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_pointer)
#define _copyTag_aggregate(f,l,c,d,s,z)	_copyTag(f,l,c,d,s,z,_ctype_aggregate)
#endif /* } !TC_USE_MACROS */
#endif /* } !TC_PREINSTR */

/*****************************************************************/
/* setByteTags, setStringTag, setUninitTag, setInitTag, clearTag */
/*****************************************************************/
#if defined(TC_PREINSTR) /* { */
extern void _setByteTags(const char * fname, int line, int col,
			const void * addr, size_t size, int set_tag, int clear_tag);
extern void _setStringTag(const char * fname, int line, int col,
			const char * addr, size_t str_len);
extern void _setUninitTag(const char * fname, int line, int col,
			void * addr, size_t size);
extern void _setInitTag(const char * fname, int line, int col,
			void * addr, size_t size);
extern void _clearTag(const char * fname, int line, int col,
			void * addr, size_t size);
#else /* } if !TC_PREINSTR { */
extern void _setByteTags(const char * fname, int line, int col,
			const void * addr, size_t size, int set_tag, int clear_tag);
#define _setStringTag(f,l,c,a,s) _setByteTags(f,l,c,a,s+1, _typetag_int, _typetag_uninit)
#define _setUninitTag(f,l,c,a,s) _setByteTags(f,l,c,a,s, _typetag_uninit, _typetag_unalloc)
#define _setInitTag(f,l,c,a,s)	 _setByteTags(f,l,c,a,s, _typetag_init, _typetag_unalloc)
#define _clearTag(f,l,c,a,s)	 _setByteTags(f,l,c,a,s, _typetag_unalloc, _typetag_unalloc)
#endif /* } !TC_PREINSTR */

/************************/
/* _extern_setUninitTag */
/************************/
#if defined(TC_PREINSTR) /* { */
extern void _extern_setUninitTag(const char * fname, int line, int col,
			void * addr, size_t size);
#else /* } if !TC_PREINSTR { */
extern void _extern_setUninitTag(const char * fname, int line, int col,
			void * addr, size_t size);
#endif /* } !TC_PREINSTR */

/*************************/
/* (_extern)_registerVar */
/* SY: currently unused: discard? Don't remember its intended purpose -27feb02 */
/*************************/
#if defined(TC_PREINSTR) /* { */
extern void _registerVar(const char * fname, int line, int col,
			const char * varname, void * addr, size_t size);
extern void _extern_registerVar(const char * fname, int line, int col,
			const char * varname, void * addr, size_t size);
#else /* } if !TC_PREINSTR { */
extern void _registerVar(const char * fname, int line, int col,
			const char * varname, void * addr, size_t size);
extern void _extern_registerVar(const char * fname, int line, int col,
			const char * varname, void * addr, size_t size);
#endif /* } !TC_PREINSTR */

/**********************/
/* setScalarUninitTag */
/**********************/
#if defined(TC_PREINSTR) /* { */
extern void _setScalarUninitTag_int(const char * fname,
			int line, int col, void * addr);
extern void _setScalarUninitTag_char(const char * fname,
			int line, int col, void * addr);
extern void _setScalarUninitTag_short(const char * fname,
			int line, int col, void * addr);
extern void _setScalarUninitTag_long(const char * fname,
			int line, int col, void * addr);
extern void _setScalarUninitTag_longlong(const char * fname,
			int line, int col, void * addr);
extern void _setScalarUninitTag_float(const char * fname,
			int line, int col, void * addr);
extern void _setScalarUninitTag_double(const char * fname,
			int line, int col, void * addr);
#ifdef LONGDOUBLE /* { */
extern void _setScalarUninitTag_longdouble(const char * fname,
			int line, int col, void * addr);
#endif /* } LONGDOUBLE */
extern void _setScalarUninitTag_pointer(const char * fname,
			int line, int col, void * addr);
#else /* } if !TC_PREINSTR { */
extern void _setScalarUninitTag(const char * fname, int line, int col,
			void * addr, _ctype_t type);

#if defined(TC_USE_MACROS) /* { */

static unsigned char * _ssut_tmp_tagptr;

/* This macro also relies on unalloc being the only 0 tag. */
#if defined(OLD_MACROS) || defined(OLD_SSU_TAG) /* { */
#define _setScalarUninitTag_char(f,l,c,addr)	( \
	  CNTMAC(_ssutcctr) CNTCOMMA \
	  _ssut_tmp_tagptr = &GET_TAG_BYTE(addr), \
	  !(*_ssut_tmp_tagptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && ( ((unsigned long) (addr) & 0x1) \
	       ? (*_ssut_tmp_tagptr &= ~ODD_TAG_MASK, \
		  *_ssut_tmp_tagptr |= (_typetag_uninit << BITS_PER_TAG)) \
	       : (*_ssut_tmp_tagptr &= ~EVEN_TAG_MASK, \
		  *_ssut_tmp_tagptr |= _typetag_uninit)) \
	  || (_setScalarUninitTag(f,l,c,addr,_ctype_char) \
	      CNTCOMMA CNTMAC(_ssutcfctr), 1) \
	)
#else /* } !(defined(OLD_MACROS) || defined(OLD_SSU_TAG)) { */
#define _setScalarUninitTag_char(f,l,c,addr)	( \
	  CNTMAC(_ssutcctr) CNTCOMMA \
	  _ssut_tmp_tagptr = &GET_TAG_BYTE(addr), \
	  !(*_ssut_tmp_tagptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && SET_CHAR_TAG(*_ssut_tmp_tagptr, _typetag_uninit, (unsigned long)(addr)&0x1) \
	  || (_setScalarUninitTag(f,l,c,addr,_ctype_char) \
	      CNTCOMMA CNTMAC(_ssutcfctr), 1) \
	)
#endif /* } !(defined(OLD_MACROS) || defined(OLD_SSU_TAG)) */

/* The alignment test in the macro can be removed.
   It should not fail for any reasonable machine. */
#define SSUT_NONCHAR(f,l,c,addr,type)	( \
	  CNTMAC(_ssutctr) CNTCOMMA \
	  _ssut_tmp_tagptr = &GET_TAG_BYTE(addr), \
	  ALIGN_CHECK(!((unsigned long) (addr) & 0x1)) \
	  ALIGN_AND SAME_MIRROR_PAGE((addr),(_MIRROR_##type *)(addr)+1) \
	  && !(*_ssut_tmp_tagptr & EVEN_CONT_MASK) \
	  && !(*(unsigned char *)((_MIRROR_##type *)_ssut_tmp_tagptr + 1) & EVEN_CONT_MASK) \
	  && (*(_MIRROR_##type *)_ssut_tmp_tagptr = _##type##_uninit_tag_u.uninit_mt) \
	  || (_setScalarUninitTag(f,l,c,addr,_ctype_##type) \
	      CNTCOMMA CNTMAC(_ssutfctr), 1) \
	)
#else /* } if !TC_USE_MACROS { */
#define _setScalarUninitTag_char(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_char)
#endif /* } TC_USE_MACROS */

#if defined(TC_USE_MACROS) && defined(_MIRROR_int)
#define _setScalarUninitTag_int(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,int)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_int) */
#define _setScalarUninitTag_int(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_int)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_int) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_short)
#define _setScalarUninitTag_short(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,short)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_short) */
#define _setScalarUninitTag_short(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_short)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_short) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_long)
#define _setScalarUninitTag_long(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,long)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_long) */
#define _setScalarUninitTag_long(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_long)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_long) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_longlong)
#define _setScalarUninitTag_longlong(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,longlong)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_longlong) */
#define _setScalarUninitTag_longlong(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_longlong)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_longlong) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_float)
#define _setScalarUninitTag_float(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,float)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_float) */
#define _setScalarUninitTag_float(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_float)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_float) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_double)
#define _setScalarUninitTag_double(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,double)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_double) */
#define _setScalarUninitTag_double(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_double)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_double) */

#ifdef LONGDOUBLE /* { */
#if defined(TC_USE_MACROS) && defined(_MIRROR_longdouble)
#define _setScalarUninitTag_longdouble(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,longdouble)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_longdouble) */
#define _setScalarUninitTag_longdouble(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_longdouble)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_longdouble) */
#endif /* } LONGDOUBLE */

#if defined(TC_USE_MACROS) && defined(_MIRROR_pointer)
#define _setScalarUninitTag_pointer(f,l,c,a) \
			SSUT_NONCHAR(f,l,c,a,pointer)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_pointer) */
#define _setScalarUninitTag_pointer(f,l,c,a) \
			_setScalarUninitTag(f,l,c,a,_ctype_pointer)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_pointer) */

#endif /* } !TC_PREINSTR */

/************************/
/* setScalarPtrToIntTag */
/************************/
#if defined(TC_PREINSTR) /* { */
extern void _setScalarTagPtrToInt_int(const char * fname,
			int line, int col, const void * addr);
extern void _setScalarTagPtrToInt_char(const char * fname,
			int line, int col, const void * addr);
extern void _setScalarTagPtrToInt_short(const char * fname,
			int line, int col, const void * addr);
extern void _setScalarTagPtrToInt_long(const char * fname,
			int line, int col, const void * addr);
extern void _setScalarTagPtrToInt_longlong(const char * fname,
			int line, int col, const void * addr);
extern void _setScalarTagPtrToInt_float(const char * fname,
			int line, int col, const void * addr);
extern void _setScalarTagPtrToInt_double(const char * fname,
			int line, int col, const void * addr);
#ifdef LONGDOUBLE /* { */
extern void _setScalarTagPtrToInt_longdouble(const char * fname,
			int line, int col, const void * addr);
#endif /* } LONGDOUBLE */
extern void _setScalarTagPtrToInt_pointer(const char * fname,
			int line, int col, const void * addr);
#else /* } if !TC_PREINSTR { */
extern void _setScalarTagPtrToInt(const char * fname, int line, int col,
			const void * addr, _ctype_t type);

#define _setScalarTagPtrToInt_int(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_int)
#define _setScalarTagPtrToInt_char(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_char)
#define _setScalarTagPtrToInt_short(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_short)
#define _setScalarTagPtrToInt_long(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_long)
#define _setScalarTagPtrToInt_longlong(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_longlong)
#define _setScalarTagPtrToInt_float(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_float)
#define _setScalarTagPtrToInt_double(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_double)
#ifdef LONGDOUBLE /* { */
#define _setScalarTagPtrToInt_longdouble(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_longdouble)
#endif /* } LONGDOUBLE */
#define _setScalarTagPtrToInt_pointer(f,l,c,a) \
			_setScalarTagPtrToInt(f,l,c,a,_ctype_pointer)

#endif /* } !TC_PREINSTR */

/**************************/
/* (_extern)_setScalarTag */
/**************************/
#if defined(TC_PREINSTR) /* { */
extern void _setScalarTag_int(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarTag_char(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarTag_short(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarTag_long(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarTag_longlong(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarTag_float(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarTag_double(const char * fname, int line, int col,
				const void * addr);
#ifdef LONGDOUBLE /* { */
extern void _setScalarTag_longdouble(const char * fname, int line, int col,
				const void * addr);
#endif /* } LONGDOUBLE */
extern void _setScalarTag_pointer(const char * fname, int line, int col,
				const void * addr);

extern void _setScalarInitTag_int(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarInitTag_char(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarInitTag_short(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarInitTag_long(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarInitTag_longlong(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarInitTag_float(const char * fname, int line, int col,
				const void * addr);
extern void _setScalarInitTag_double(const char * fname, int line, int col,
				const void * addr);
#ifdef LONGDOUBLE /* { */
extern void _setScalarInitTag_longdouble(const char * fname, int line, int col,
				const void * addr);
#endif /* } LONGDOUBLE */
extern void _setScalarInitTag_pointer(const char * fname, int line, int col,
				const void * addr);

extern void _extern_setScalarTag_int(const char * fname,
			int line, int col, const void * addr);
extern void _extern_setScalarTag_char(const char * fname,
			int line, int col, const void * addr);
extern void _extern_setScalarTag_short(const char * fname,
			int line, int col, const void * addr);
extern void _extern_setScalarTag_long(const char * fname,
			int line, int col, const void * addr);
extern void _extern_setScalarTag_longlong(const char * fname,
			int line, int col, const void * addr);
extern void _extern_setScalarTag_float(const char * fname,
			int line, int col, const void * addr);
extern void _extern_setScalarTag_double(const char * fname,
			int line, int col, const void * addr);
#ifdef LONGDOUBLE /* { */
extern void _extern_setScalarTag_longdouble(const char * fname,
			int line, int col, const void * addr);
#endif /* } LONGDOUBLE */
extern void _extern_setScalarTag_pointer(const char * fname,
			int line, int col, const void * addr);

#else /* } if !TC_PREINSTR { */

#define _setScalarInitTag_int(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(int),_typetag_init,_typetag_uninit)
#define _setScalarInitTag_char(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(char),_typetag_init,_typetag_uninit)
#define _setScalarInitTag_short(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(short),_typetag_init,_typetag_uninit)
#define _setScalarInitTag_long(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(long),_typetag_init,_typetag_uninit)
#define _setScalarInitTag_longlong(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(long long),_typetag_init,_typetag_uninit)
#define _setScalarInitTag_float(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(float),_typetag_init,_typetag_uninit)
#define _setScalarInitTag_double(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(double),_typetag_init,_typetag_uninit)
#ifdef LONGDOUBLE /* { */
#define _setScalarInitTag_longdouble(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(long double),_typetag_init,_typetag_uninit)
#endif /* } LONGDOUBLE */
#define _setScalarInitTag_pointer(f,l,c,a) \
			_setByteTags(f,l,c,a,sizeof(pointer),_typetag_init,_typetag_uninit)


extern void _extern_setScalarTag(const char * fname, int line, int col,
			const void * addr, _ctype_t type);

#define _extern_setScalarTag_int(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_int)
#define _extern_setScalarTag_char(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_char)
#define _extern_setScalarTag_short(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_short)
#define _extern_setScalarTag_long(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_long)
#define _extern_setScalarTag_longlong(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_longlong)
#define _extern_setScalarTag_float(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_float)
#define _extern_setScalarTag_double(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_double)
#ifdef LONGDOUBLE /* { */
#define _extern_setScalarTag_longdouble(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_longdouble)
#endif /* } LONGDOUBLE */
#define _extern_setScalarTag_pointer(f,l,c,a) \
			_extern_setScalarTag(f,l,c,a,_ctype_pointer)

extern void _setScalarTag(const char * fname, int line, int col,
			const void * addr, _ctype_t type);

#if defined(TC_USE_MACROS) /* { */

static unsigned char * _sst_tmp_tagptr;

/* This macro also relies on unalloc being the only 0 tag. */
#if defined(OLD_MACROS) || defined(OLD_SS_TAG) /* { */
#define _setScalarTag_char(f,l,c,addr)	( \
	  CNTMAC(_sstcctr) CNTCOMMA \
	  _sst_tmp_tagptr = &GET_TAG_BYTE(addr), \
	  !(*_sst_tmp_tagptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && ( ((unsigned long) (addr) & 0x1) \
	       ? (*_sst_tmp_tagptr &= ~ODD_TAG_MASK, \
		  *_sst_tmp_tagptr |= (_typetag_int << BITS_PER_TAG)) \
	       : (*_sst_tmp_tagptr &= ~EVEN_TAG_MASK, \
		  *_sst_tmp_tagptr |= _typetag_int)) \
	  || (_setScalarTag(f,l,c,addr,_ctype_char) \
	      CNTCOMMA CNTMAC(_sstcfctr), 1) \
	)
#else /* } !(defined(OLD_MACROS) || defined(OLD_SS_TAG)) { */
#define _setScalarTag_char(f,l,c,addr)	( \
	  CNTMAC(_sstcctr) CNTCOMMA \
	  _sst_tmp_tagptr = &GET_TAG_BYTE(addr), \
	  !(*_sst_tmp_tagptr & (EVEN_CONT_MASK|ODD_CONT_MASK)) \
	  && SET_CHAR_TAG(*_sst_tmp_tagptr, _typetag_int, (unsigned long)(addr)&0x1) \
	  || (_setScalarTag(f,l,c,addr,_ctype_char) \
	      CNTCOMMA CNTMAC(_sstcfctr), 1) \
	)
#endif /* } !(defined(OLD_MACROS) || defined(OLD_SS_TAG)) */

/* The alignment test in the macro can be removed.
   It should not fail for any reasonable machine. */
#define SST_NONCHAR(f,l,c,addr,type)	( \
	  CNTMAC(_sstctr) CNTCOMMA \
	  _sst_tmp_tagptr = &GET_TAG_BYTE(addr), \
	  ALIGN_CHECK(!((unsigned long) (addr) & 0x1)) \
	  ALIGN_AND SAME_MIRROR_PAGE((addr),(_MIRROR_##type *)(addr)+1) \
	  && !(*_sst_tmp_tagptr & EVEN_CONT_MASK) \
	  && !(*(unsigned char *)((_MIRROR_##type *)_sst_tmp_tagptr + 1) & EVEN_CONT_MASK) \
	  && (*(_MIRROR_##type *)_sst_tmp_tagptr = _##type##_tag_u.init_mt) \
	  || (_setScalarTag(f,l,c,addr,_ctype_##type) \
	      CNTCOMMA CNTMAC(_sstfctr), 1) \
	)
#else /* } if !TC_USE_MACROS { */
#define _setScalarTag_char(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_char)
#endif /* } TC_USE_MACROS */

#if defined(TC_USE_MACROS) && defined(_MIRROR_int)
#define _setScalarTag_int(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,int)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_int) */
#define _setScalarTag_int(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_int)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_int) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_short)
#define _setScalarTag_short(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,short)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_short) */
#define _setScalarTag_short(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_short)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_short) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_long)
#define _setScalarTag_long(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,long)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_long) */
#define _setScalarTag_long(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_long)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_long) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_longlong)
#define _setScalarTag_longlong(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,longlong)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_longlong) */
#define _setScalarTag_longlong(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_longlong)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_longlong) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_float)
#define _setScalarTag_float(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,float)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_float) */
#define _setScalarTag_float(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_float)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_float) */

#if defined(TC_USE_MACROS) && defined(_MIRROR_double)
#define _setScalarTag_double(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,double)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_double) */
#define _setScalarTag_double(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_double)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_double) */

#ifdef LONGDOUBLE /* { */
#if defined(TC_USE_MACROS) && defined(_MIRROR_longdouble)
#define _setScalarTag_longdouble(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,longdouble)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_longdouble) */
#define _setScalarTag_longdouble(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_longdouble)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_longdouble) */
#endif /* } LONGDOUBLE */

#if defined(TC_USE_MACROS) && defined(_MIRROR_pointer)
#define _setScalarTag_pointer(f,l,c,a) \
			SST_NONCHAR(f,l,c,a,pointer)
#else /* defined(TC_USE_MACROS) && defined(_MIRROR_pointer) */
#define _setScalarTag_pointer(f,l,c,a) \
			_setScalarTag(f,l,c,a,_ctype_pointer)
#endif /* defined(TC_USE_MACROS) && defined(_MIRROR_pointer) */

#endif /* } !TC_PREINSTR */

/**************************/
/* (_extern)_replicateTag */
/**************************/
#if defined(TC_PREINSTR) /* { */
extern void _replicateTag(const char * fname, int line, int col,
			void * addr, size_t size, int nelem);
extern void _extern_replicateTag(const char * fname, int line, int col,
			void * addr, size_t size, int nelem);
#else /* } if !TC_PREINSTR { */
extern void _replicateTag(const char * fname, int line, int col,
			void * addr, size_t size, int nelem);
extern void _extern_replicateTag(const char * fname, int line, int col,
			void * addr, size_t size, int nelem);
#endif /* } !TC_PREINSTR */

/**************/
/* promoteTag */
/**************/
#if defined(TC_PREINSTR) /* { */
extern void _promoteTag(const char * fname, int line, int col,
			const void ** addrptr,
                        _ctype_t opnd_type,
                        _ctype_t expr_type,
			void * tmpspace);
#else /* } if !TC_PREINSTR { */
extern void _promoteTag(const char * fname, int line, int col,
			const void ** addrptr,
                        _ctype_t opnd_type,
                        _ctype_t expr_type,
			void * tmpspace);
#endif /* } !TC_PREINSTR */

/***************/
/* *alloc_init */
/***************/
#if defined(TC_PREINSTR) /* { */
extern void * _typecheck_malloc_init(size_t size);
extern void _typecheck_free_partial(void * ptr);
extern void * _typecheck_realloc_init(void * ptr, size_t size);
extern void * _typecheck_memalign_init(size_t alignment, size_t size);
extern void * _typecheck_valloc_init(size_t size);
#else /* } if !TC_PREINSTR { */
extern void * _typecheck_malloc_init(size_t size);
extern void _typecheck_free_partial(void * ptr);
extern void * _typecheck_realloc_init(void * ptr, size_t size);
extern void * _typecheck_memalign_init(size_t alignment, size_t size);
extern void * _typecheck_valloc_init(size_t size);
#endif /* } !TC_PREINSTR */

/**********************************************/
/*************** function stuff ***************/
/**********************************************/

typedef struct {
  const void * addr;
  size_t size;
} _addr_and_size_t;

extern _addr_and_size_t * _globalArgAddrs;
extern int _globalArgCount;
extern void * _globalCallTarget;
extern void * _dummyAddr;
extern char * _globalErrlocFile;
extern int _globalErrlocLine, _globalErrlocCol;

/* for bitfields */
extern long long _dummyInt;

/* Note: aargaddrs serves two purposes:
      1 - (void *)aargaddrs marks the start of the stack frame
      2 - *aargaddrs points to the argaddrs array, if any
*/
extern void _processReturn(const char * fname, int line, int col,
		void * scaf_start, void * scaf_end, void * agrf_start, void * agrf_end,
		_addr_and_size_t ** aargaddrs, const void * addr, size_t size);

extern void _processReturnNoClear(const char * fname, int line, int col,
		_addr_and_size_t * argaddrs, const void * addr, size_t size);

#if defined(TC_PREINSTR) /* { */

extern void _tcdebug_processCall(const char * fnname);

#else /* } if !TC_PREINSTR { */

/* DEBUG: Uncomment the following to replace the debugging
   call with a nil macro */
/* #define TC_NO_TRACECALL */

#ifdef TC_NO_TRACECALL
#define _tcdebug_processCall(fnname)
#else
extern void _tcdebug_processCall_func(const char * fnname);
#define _tcdebug_processCall(fnname) _tcdebug_processCall_func(fnname)
#endif

#endif /* } !TC_PREINSTR */

/*******************/
/* process arg tag */
/*******************/
#if defined(TC_PREINSTR) /* { */
extern void _processArgTag_int(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_char(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_short(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_long(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_longlong(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_float(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_double(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
#ifdef LONGDOUBLE /* { */
extern void _processArgTag_longdouble(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
#endif /* } LONGDOUBLE */
extern void _processArgTag_pointer(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_aggregate(const char *, int, int,
		_addr_and_size_t *, int, void *, size_t);
#else /* } if !TC_PREINSTR { */

extern void _processArgTag(const char * fname, int line, int col,
			_addr_and_size_t * argaddrs,
			/*int argCount,*/ int index,
			void * addr, _ctype_t type, size_t size_aggr);

#if defined(TC_USE_MACROS) /* { */

#define PAT_SCALAR(f,l,c,r,i,a,type) \
  CNTMAC(_patctr); \
  /* If not going past the number of arguments passed in \
     and the size of the argument is as expected, just copy. */ \
  if(i <= _globalArgCount && r[i].size == typeinfo[_ctype##type].size) \
    /*NOTE: copyTag won't cough _typetag_unalloc; memory is pre-initialized*/ \
    _copyTag##type(f,l,c,a,r[i].addr,r[i].size); \
  /** type matches promotion, set to declared type **/ \
  else if(i <= _globalArgCount && \
	  r[i].size == typeinfo[typeinfo[_ctype##type].promo].size && \
	  _verifyTagSilent(r[i].addr, typeinfo[_ctype##type].promo)) \
    _setScalarTag##type(f,l,c,a) CNTCOMMA CNTMAC(_patpctr); \
  else \
    _processArgTag(f,l,c,r,i,a,_ctype##type,0) CNTCOMMA CNTMAC(_patfctr);

#define _processArgTag_int(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_int)
#define _processArgTag_char(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_char)
#define _processArgTag_short(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_short)
#define _processArgTag_long(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_long)
#define _processArgTag_longlong(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_longlong)
#define _processArgTag_float(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_float)
#define _processArgTag_double(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_double)
#ifdef LONGDOUBLE /* { */
#define _processArgTag_longdouble(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_longdouble)
#endif /* } LONGDOUBLE */
#define _processArgTag_pointer(f,l,c,r,i,a,s) \
				PAT_SCALAR(f,l,c,r,i,a,_pointer)
#define _processArgTag_aggregate(f,l,c,r,i,a,s) \
  CNTMAC(_patactr); \
  /* If not going past the number of arguments passed in \
     and the size of the argument is as expected, just copy. */ \
  if(i <= _globalArgCount && r[i].size == s) \
    /*NOTE: copyTag won't cough _typetag_unalloc; memory is pre-initialized*/ \
    _copyTag_aggregate(f,l,c,a,r[i].addr,r[i].size); \
  else _processArgTag(f,l,c,r,i,a,_ctype_aggregate,s) CNTCOMMA CNTMAC(_patafctr);

#else /* } if !TC_USE_MACROS { */

#define _processArgTag_int(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_int,s)
#define _processArgTag_char(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_char,s)
#define _processArgTag_short(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_short,s)
#define _processArgTag_long(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_long,s)
#define _processArgTag_longlong(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_longlong,s)
#define _processArgTag_float(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_float,s)
#define _processArgTag_double(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_double,s)
#ifdef LONGDOUBLE /* { */
#define _processArgTag_longdouble(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_longdouble,s)
#endif /* } LONGDOUBLE */
#define _processArgTag_pointer(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_pointer,s)
#define _processArgTag_aggregate(f,l,c,r,i,a,s) \
				_processArgTag(f,l,c,r,i,a,_ctype_aggregate,s)

#endif /* } !TC_USE_MACROS */
#endif /* } !TC_PREINSTR */

/*************************************************************/
/*************** static mirror representatives ***************/
/*************************************************************/

#if defined(TC_PREINSTR) /* { */

extern void * _ini_static_repptr;

extern void * _int_static_repptr;
extern void * _char_static_repptr;
extern void * _short_static_repptr;
extern void * _long_static_repptr;
extern void * _longlong_static_repptr;
extern void * _float_static_repptr;
extern void * _double_static_repptr;
#ifdef LONGDOUBLE /* { */
extern void * _longdouble_static_repptr;
#endif /* } LONGDOUBLE */
extern void * _pointer_static_repptr;

#else /* } if !TC_PREINSTR { */

extern char _ini_static_rep[];

extern int _int_static_rep[];
extern char _char_static_rep[];
extern short _short_static_rep[];
extern long _long_static_rep[];
extern long long _longlong_static_rep[];
extern float _float_static_rep[];
extern double _double_static_rep[];
#ifdef LONGDOUBLE /* { */
extern long double _longdouble_static_rep[];
#endif /* } LONGDOUBLE */
extern void * _pointer_static_rep[];

#define _ini_static_repptr		(&_ini_static_rep[0])

#define _int_static_repptr		(&_int_static_rep[0])
#define _char_static_repptr		(&_char_static_rep[0])
#define _short_static_repptr		(&_short_static_rep[0])
#define _long_static_repptr		(&_long_static_rep[0])
#define _longlong_static_repptr		(&_longlong_static_rep[0])
#define _float_static_repptr		(&_float_static_rep[0])
#define _double_static_repptr		(&_double_static_rep[0])
#ifdef LONGDOUBLE /* { */
#define _longdouble_static_repptr	(&_longdouble_static_rep[0])
#endif /* } LONGDOUBLE */
#define _pointer_static_repptr		(&_pointer_static_rep[0])

#endif /* } !TC_PREINSTR */

/******************************************************************/
/*************** End of API functions and variables ***************/
/******************************************************************/

/**********************/
/* reportStaticCounts */
/**********************/
#if defined(TC_PREINSTR) /* { */
extern void _reportStaticCounts(const char * fname, const char * descr, int count);
#else /* } if !TC_PREINSTR { */
extern void _reportStaticCounts(const char * fname, const char * descr, int count);
#endif /* } !TC_PREINSTR */

/**************************************************/
/*************** compile time flags ***************/
/**************************************************/

extern int strictPointer;

#endif /* } #ifndef TYPECHECK_API */

#if 0 /* { */
/******************/
/* macro template */
/******************/
#if defined(TC_PREINSTR) /* { */
_int
_char
_short
_long
_longlong
_float
_double
#ifdef LONGDOUBLE /* { */
_longdouble
#endif /* } LONGDOUBLE */
_pointer
_aggregate
#else /* } if !TC_PREINSTR { */
#if defined(TC_USE_MACROS) /* { */

#define _int
#define _char
#define _short
#define _long
#define _longlong
#define _float
#define _double
#ifdef LONGDOUBLE /* { */
#define _longdouble
#endif /* } LONGDOUBLE */
#define _pointer
#define _aggregate

#else /* } if !TC_USE_MACROS { */

#define _int
#define _char
#define _short
#define _long
#define _longlong
#define _float
#define _double
#ifdef LONGDOUBLE /* { */
#define _longdouble
#endif /* } LONGDOUBLE */
#define _pointer
#define _aggregate

#endif /* } !TC_USE_MACROS */
#endif /* } !TC_PREINSTR */

#endif /* } 0 */
