#ifndef _TYPECHECK_VERIFYPTR_API_H_ /* { */
#define _TYPECHECK_VERIFYPTR_API_H_

/* HISTO mode: redirect instrumentation calls to _tc_histo_* */
/* #define TC_HISTO */

/* NOTE: to time different instrumentation overheads, define the following */
#if defined(TC_NO_INST) /* { */

#define TC_NO_VP	/* no verifyPtr */
#define TC_NO_STRINGTAG	/* no setStringTag */
#define TC_NO_STACKTAG	/* no setTag (setByteTags, setStringTag, setUninitTag, setInitTag) */ 
			/* no clearTag, no processReturn */
#define TC_NO_HEAPTAG	/* no *alloc, free */
#define TC_NO_LIB	/* no instrumented library functions */

#endif /* } defined(TC_NO_INST) */

/* Include size_t typedef, so we don't have to include stdlib.h. */
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned int    size_t;
#endif

/**********************************************/
/* for VULN mode: tag and clear AR retval */
/**********************************************/
#if defined(TC_PREINSTR) /* { */

int _tcvuln_tagARretval;
int _tcvuln_clearARretval;

#else /* } !defined(TC_PREINSTR) { */

/* GNU solution on Linux: */
#define _tcvuln_tagARretval	_setInitTag(__FILE__,__LINE__,0,\
					((void **)__builtin_frame_address(0)) + 1,\
					sizeof(void *))

#define _tcvuln_clearARretval	_clearTag(__FILE__,__LINE__,0,\
					((void **)__builtin_frame_address(0)) + 1,\
					sizeof(void *))

#endif /* } !defined(TC_PREINSTR) */

/**********************************************/
/* MIRROR stuff */
/**********************************************/
#if defined(TC_USE_MACROS) || defined(TCPTR_INTERNAL) /* { */

#define _TC_BYTE 8
#define TAGS_PER_BYTE 8
#define TAGS_PER_BYTE_BITS 3	/* TAGS_PER_BYTE = 2 ^ TAGS_PER_BYTE_BITS */

#define DIV_TAGS_PER_BYTE(n) (((unsigned long)(n))>>TAGS_PER_BYTE_BITS)		/* optimized division by 8*/
#define MOD_TAGS_PER_BYTE(n) (((unsigned long)(n))&0x07)			/* optimized modulus 8 */

#if defined(TC_STATIC_MIRROR) /* { */
  /**************************************/
  /* STATIC MIRROR */
  /**************************************/
#define SAME_MIRROR_PAGE(addr1, addr2) 1

#define GET_TAG_BYTE(addr)	_tc_mirror[DIV_TAGS_PER_BYTE(addr)]

extern unsigned char _tc_mirror[];

#else /* } !defined(TC_STATIC_MIRROR) { */
  /**************************************/
  /* NOT STATIC MIRROR: MIRRORMAP */
  /**************************************/

/*******************************************
  Modify/experiment with following #define
 *******************************************/
#define MIRRORPAGE_NUMBITS 20

/*******************************************/
#define MIRRORPAGE_NUMBYTES (1 << MIRRORPAGE_NUMBITS)
#define MIRRORPAGE_SIZE DIV_TAGS_PER_BYTE(MIRRORPAGE_NUMBYTES)
#define MIRRORPAGE_ZERO_MASK ((-1) << MIRRORPAGE_NUMBITS)
#define MIRRORPAGE_MASK (~(MIRRORPAGE_ZERO_MASK))
#define MIRRORMAP_NUMBITS ((sizeof(void *) << 3) - MIRRORPAGE_NUMBITS)
#define MIRRORMAP_NUMELEMENTS (1 << MIRRORMAP_NUMBITS)

/* Handy macros */
/* Note possible point of confusion: MIRRORPAGE_INDEX is the mirrorpage
   portion of the address *in program space*, not in mirror space.
   To get the latter, divide by two */
#define MIRRORMAP_INDEX(addr) ((unsigned long)(addr) >> MIRRORPAGE_NUMBITS)
#define MIRRORPAGE_INDEX(addr) ((unsigned long)(addr) & MIRRORPAGE_MASK)

#define SAME_MIRROR_PAGE(addr1, addr2) \
	(MIRRORMAP_INDEX(addr1) ==  MIRRORMAP_INDEX(addr2))

/* The following macro must be an lvalue */
#define GET_TAG_BYTE(addr)	*(\
	 (mirrormap[MIRRORMAP_INDEX(addr)]) \
	    || (_touchMirrorPage(MIRRORMAP_INDEX(addr)),1), \
	    ((unsigned char *) mirrormap[MIRRORMAP_INDEX(addr)] \
		+ DIV_TAGS_PER_BYTE(MIRRORPAGE_INDEX(addr))) \
	 )

extern void * mirrormap[];

#endif /* } !defined(TC_STATIC_MIRROR) */

#endif /* } defined TC_USE_MACROS or TCPTR_INTERNAL */

/**********************************************/


#if defined(TC_PREINSTR) /* { */

/*******************************************************/
/* pseudo datatypes: placeholders to allow compilation */
typedef struct { int i; } _addr_and_size_t;
typedef enum _ctype {
  _ctype_void_invalid = 0,
  _ctype_int = 1,
  _ctype_char = 2,
  _ctype_short = 3,
  _ctype_long = 4,
  _ctype_longlong = 5,
  _ctype_float = 6,
  _ctype_double = 7,
  _ctype_longdouble = 8,
  _ctype_pointer = 9,
  _ctype_aggregate = 10
} _ctype_t;

/**************************************************/

extern void _registerExtern(const char * fname, int line, int col,
                        void * addr, _ctype_t type, size_t size);

extern void _registerFunction(const char * fname, int line, int col, void * addr);

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
extern void _verifyPtr_pointer(const char * fname, int line, int col,
                        const char * exp, const void * addr, size_t size);
extern void _verifyPtr_aggregate(const char * fname, int line, int col,
                        const char * exp, const void * addr, size_t size);

/* skipped _verifyTag_* */
/* skipped _copyTag_* */

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

/* skipped _extern_setUninitTag */

extern void _registerVar(const char * fname, int line, int col,
                        const char * varname, void * addr, size_t size);
extern void _extern_registerVar(const char * fname, int line, int col,
                        const char * varname, void * addr, size_t size);

/* skipped _setScalarUninitTag_* */
/* skipped _setScalarTagPtrToInt_* */
/* skipped _setScalarTag_* */
/* skipped _setScalarInitTag_* */
/* skipped _extern_setScalarTag_* */
/* skipped _replicateTag */
/* skipped _extern_replicateTag */
/* skipped _promoteTag */

extern void _typecheck_free_partial(void * ptr);
extern void * _typecheck_malloc_zero(size_t size);
extern void * _typecheck_calloc_zero(size_t nelem, size_t elsize);
extern void * _typecheck_realloc_zero(void * ptr, size_t size);
extern void * _typecheck_memalign_zero(size_t alignment, size_t size);
extern void * _typecheck_valloc_zero(size_t size);

extern void * malloc_zero(size_t size);
extern void * calloc_zero(size_t nelem, size_t elsize);
extern void * realloc_zero(void * ptr, size_t size);
extern void * memalign_zero(size_t alignment, size_t size);
extern void * valloc_zero(size_t size);

extern void _processReturn(const char * fname, int line, int col,
                void * scaf_start, void * scaf_end, void * agrf_start, void * agrf_end,
                _addr_and_size_t ** aargaddrs, const void * addr, size_t size);

/* skipped _processArgTag_* */

extern void _reportStaticCounts(const char * fname, const char * descr, int count);


/**************************************************/
#else /* } if !defined (TC_PREINSTR) { */
/**************************************************/

#define _addr_and_size_t	void
#define _ctype_t		void
#define _ctype_void_invalid	0
#define _ctype_int		0
#define _ctype_char		0
#define _ctype_short		0
#define _ctype_long		0
#define _ctype_longlong		0
#define _ctype_float		0
#define _ctype_double		0
#define _ctype_longdouble	0
#define _ctype_pointer		0
#define _ctype_aggregate	0

/* ignore registerExterns */
#define _registerExtern(f,l,c,a,t,s)

/* ignore registerFunctions */
#define _registerFunction(f,l,c,a)

#if defined(TC_HISTO) /* { */

extern void _tcptr_histo_verifyPtr(const char * file, int line, int col, const char * exp,
				const void * addr, size_t size);
extern void _tcptr_histo_setTags(const char * file, int line, int col,
				const void * addr, size_t size);
extern void _tcptr_histo_clearTags(const char * file, int line, int col,
				const void * addr, size_t size);


#define _verifyPtr(f,l,c,e,a,s)                 _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_int(f,l,c,e,a,s)             _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_char(f,l,c,e,a,s)            _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_short(f,l,c,e,a,s)           _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_long(f,l,c,e,a,s)            _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_longlong(f,l,c,e,a,s)        _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_float(f,l,c,e,a,s)           _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_double(f,l,c,e,a,s)          _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_longdouble(f,l,c,e,a,s)      _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_pointer(f,l,c,e,a,s)         _tcptr_histo_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_aggregate(f,l,c,e,a,s)       _tcptr_histo_verifyPtr(f,l,c,e,a,s)

#define _setByteTags(f,l,c,a,s,st,ct)   _tcptr_histo_setTags(f,l,c,a,s)
#define _setStringTag(f,l,c,a,sl)       _tcptr_histo_setTags(f,l,c,a,(sl)+1)
#define _setUninitTag(f,l,c,a,s)        _tcptr_histo_setTags(f,l,c,a,s)
#define _setInitTag(f,l,c,a,s)          _tcptr_histo_setTags(f,l,c,a,s)

#define _clearTag(f,l,c,a,s)            _tcptr_histo_clearTags(f,l,c,a,s)

#define _registerVar(f,l,c,v,a,s)
#define _extern_registerVar(f,l,c,v,a,s)
#define _processReturn(f,l,c,ss,se,as,ae,aa,a,s)	0
#define _reportStaticCounts(fname, descr, count)

/** ERROR: cannot use because macros expansion fails on prototypes!
extern void * _tcptr_histo_malloc(const char * file, int line, size_t size);
extern void * _tcptr_histo_calloc(const char * file, int line, size_t nelem, size_t elsize);
extern void _tcptr_histo_free(const char * file, int line, void * ptr);
extern void * _tcptr_histo_realloc(const char * file, int line, void * ptr, size_t size);
extern void * _tcptr_histo_memalign(const char * file, int line, size_t align, size_t size);
extern void * _tcptr_histo_valloc(const char * file, int line, size_t size);

#define _typecheck_free(p)		_tcptr_histo_free(__FILE__,__LINE__,p)
#define _typecheck_free_partial(p)	_tcptr_histo_free(__FILE__,__LINE__,p)
#define _typecheck_malloc(s)		_tcptr_histo_malloc(__FILE__,__LINE__,s)
#define _typecheck_malloc_zero(s)	_tcptr_histo_malloc(__FILE__,__LINE__,s)
#define _typecheck_calloc(n,e)		_tcptr_histo_calloc(__FILE__,__LINE__,n,e)
#define _typecheck_realloc(p,s)		_tcptr_histo_realloc(__FILE__,__LINE__,p,s)
#define _typecheck_realloc_zero(p,s)	_tcptr_histo_realloc(__FILE__,__LINE__,p,s)
#define _typecheck_memalign(a,s)	_tcptr_histo_memalign(__FILE__,__LINE__,a,s)
#define _typecheck_memalign_zero(a,s)	_tcptr_histo_memalign(__FILE__,__LINE__,a,s)
#define _typecheck_valloc(s)		_tcptr_histo_valloc(__FILE__,__LINE__,s)
#define _typecheck_valloc_zero(s)	_tcptr_histo_valloc(__FILE__,__LINE__,s)
**/

extern void _typecheck_free_partial(void * ptr);
extern void * _typecheck_malloc_zero(size_t size);
#define _typecheck_calloc_zero(n,e)	_typecheck_calloc(n,e)
extern void * _typecheck_realloc_zero(void * ptr, size_t size);
extern void * _typecheck_memalign_zero(size_t alignment, size_t size);
extern void * _typecheck_valloc_zero(size_t size);

extern void * malloc_zero(size_t size);
#define calloc_zero(n,e)	calloc(n,e)
#define realloc_zero(p,s)	realloc(p,s)
extern void * memalign_zero(size_t alignment, size_t size);
extern void * valloc_zero(size_t size);

#else /* } !defined(TC_HISTO) { */

extern void _tcptr_verifyPtr(const char * file, int line, int col, const char * exp,
			const void * addr, size_t size);

#if defined(TC_USE_MACROS) /* { */

#if defined(TC_VULNERABLE_VP) /* { */

#define _macro_verifyPtr(file,line,col,exp,addr,size) \
	(((((signed)MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size) \
	  && (GET_TAG_BYTE(addr) == 0)) \
	 || (_tcptr_verifyPtr(file,line,col,exp,addr,size),1))

#else /* } !defined(TC_VULN_VP) { */

/* OPTION 1: one byte only version */
#define _macro_verifyPtr(file,line,col,exp,addr,size) \
	(((((signed)MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size) \
	  && (GET_TAG_BYTE(addr) == 0xff)) \
	 || (_tcptr_verifyPtr(file,line,col,exp,addr,size),1))

/*
	ver 2: masking -- slower on li!
	  && ((GET_TAG_BYTE(addr) | ((unsigned char)~((~(0xffu << (size))) << MOD_TAGS_PER_BYTE(addr)))) == 0xff)) \
*/
#if 0 /* { */
/* OPTION 2: one or two byte version: not frequent enough? */
#define _macro_verifyPtr(file,line,col,exp,addr,size) \
	(((GET_TAG_BYTE(addr) == 0xff) \
	   && ((((signed)MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size) \
		|| (GET_TAG_BYTE(((const char *)addr)+size) == 0xff))) \
	 || (_tcptr_verifyPtr(file,line,col,exp,addr,size),1))
#endif /* } if 0 */

#endif /* } !defined(TC_VULN_VP) */

#else /* } !defined(TC_USE_MACROS) { */

#define _macro_verifyPtr(file,line,col,exp,addr,size) \
		_tcptr_verifyPtr(file,line,col,exp,addr,size)

#endif /* } !defined(TC_USE_MACROS) */

#if defined(TC_NO_VP) /* { */

#define _verifyPtr(f,l,c,e,a,s)			0
#define _verifyPtr_int(f,l,c,e,a,s)		0
#define _verifyPtr_char(f,l,c,e,a,s)		0
#define _verifyPtr_short(f,l,c,e,a,s)		0
#define _verifyPtr_long(f,l,c,e,a,s)		0
#define _verifyPtr_longlong(f,l,c,e,a,s)	0
#define _verifyPtr_float(f,l,c,e,a,s)		0
#define _verifyPtr_double(f,l,c,e,a,s)		0
#define _verifyPtr_longdouble(f,l,c,e,a,s)	0
#define _verifyPtr_pointer(f,l,c,e,a,s)		0
#define _verifyPtr_aggregate(f,l,c,e,a,s)	0

#else /* } !defined(TC_NO_VP) { */

#define _verifyPtr(f,l,c,e,a,s)			_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_int(f,l,c,e,a,s)		_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_char(f,l,c,e,a,s)		_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_short(f,l,c,e,a,s)		_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_long(f,l,c,e,a,s)		_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_longlong(f,l,c,e,a,s)	_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_float(f,l,c,e,a,s)		_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_double(f,l,c,e,a,s)		_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_longdouble(f,l,c,e,a,s)	_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_pointer(f,l,c,e,a,s)		_macro_verifyPtr(f,l,c,e,a,s)
#define _verifyPtr_aggregate(f,l,c,e,a,s)	_macro_verifyPtr(f,l,c,e,a,s)

#endif /* } !defined(TC_NO_VP) */

/* skipped _verifyTag_* */
/* skipped _copyTag_* */

extern void _tcptr_setTags(const void * addr, size_t size);
extern void _tcptr_clearTags(const void * addr, size_t size);

#if defined(TC_USE_MACROS) /* { */

#define _macro_setTags(addr,size) \
	(((((signed)MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size) \
	  && (GET_TAG_BYTE(addr) |= ((~(0xffu << (size))) << MOD_TAGS_PER_BYTE(addr)))) \
	 || (_tcptr_setTags(addr,size),1))
/*
#define _macro_setTags(addr,size) _tcptr_setTags(addr,size)
*/

#else /* } !defined(TC_USE_MACROS) { */

#define _macro_setTags(addr,size) _tcptr_setTags(addr,size)

#endif /* } !defined(TC_USE_MACROS) */

#if defined(TC_NO_STACKTAG) /* { */

#define _setByteTags(f,l,c,a,s,st,ct)	0
#define _setStringTag(f,l,c,a,sl)	0
#define _setUninitTag(f,l,c,a,s)	0
#define _setInitTag(f,l,c,a,s)		0

#else /* } !defined(TC_NO_STACKTAG) { */

#define _setByteTags(f,l,c,a,s,st,ct)	_macro_setTags(a,s)

#if defined(TC_NO_STRINGTAG) /* { */
#define _setStringTag(f,l,c,a,sl)	0
#else /* } !defined(TC_NO_STRINGTAG) { */
#define _setStringTag(f,l,c,a,sl)	_tcptr_setTags(a,(sl)+1)
#endif /* } !defined(TC_NO_STRINGTAG) */

#define _setUninitTag(f,l,c,a,s)	_macro_setTags(a,s)
#define _setInitTag(f,l,c,a,s)		_macro_setTags(a,s)

#endif /* } !defined(TC_NO_STACKTAG) */

#if defined(TC_USE_MACROS) /* { */

#define _macro_clearTags(addr,size) \
	(((((signed)MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size) \
	  && (GET_TAG_BYTE(addr) &= ~((~(0xffu << (size))) << MOD_TAGS_PER_BYTE(addr)),1)) \
	 || (_tcptr_clearTags(addr,size),1))
/*
#define _macro_clearTags(addr,size) _tcptr_clearTags(addr,size)
*/

#else /* } !defined(TC_USE_MACROS) { */

#define _macro_clearTags(addr,size) _tcptr_clearTags(addr,size)

#endif /* } !defined(TC_USE_MACROS) */

#if defined(TC_NO_STACKTAG) /* { */
#define _clearTag(f,l,c,a,s)	0
#else /* } !defined(TC_NO_STACKTAG) { */
#define _clearTag(f,l,c,a,s)		_macro_clearTags(a,s)
#endif /* } !defined(TC_NO_STACKTAG) */

/* skipped _extern_setUninitTag */

#define _registerVar(f,l,c,v,a,s)
#define _extern_registerVar(f,l,c,v,a,s)

/* skipped _setScalarUninitTag_* */
/* skipped _setScalarTagPtrToInt_* */
/* skipped _setScalarTag_* */
/* skipped _setScalarInitTag_* */
/* skipped _extern_setScalarTag_* */
/* skipped _replicateTag */
/* skipped _extern_replicateTag */
/* skipped _promoteTag */

extern void _typecheck_free_partial(void * ptr);
extern void * _typecheck_malloc_zero(size_t size);
#define _typecheck_calloc_zero(n,e)	_typecheck_calloc(n,e)
extern void * _typecheck_realloc_zero(void * ptr, size_t size);
extern void * _typecheck_memalign_zero(size_t alignment, size_t size);
extern void * _typecheck_valloc_zero(size_t size);

extern void * malloc_zero(size_t size);
#define calloc_zero(n,e)	calloc(n,e)
#define realloc_zero(p,s)	realloc(p,s)
extern void * memalign_zero(size_t alignment, size_t size);
extern void * valloc_zero(size_t size);

extern void _tcptr_processReturn(void * scaf_start, void * scaf_end,
				void * agrf_start, void * agrf_end,
				void * local_start);
#if defined(TC_NO_STACKTAG) /* { */
#define _processReturn(f,l,c,ss,se,as,ae,aa,a,s)	0
#else /* } !defined(TC_NO_STACKTAG) /* { */
#define _processReturn(f,l,c,ss,se,as,ae,aa,a,s)	_tcptr_processReturn(ss,se,as,ae,aa)
#endif /* } !defined(TC_NO_STACKTAG) */

/* skipped _processArgTag_* */

#if defined(TC_USE_MACROS) /* { */

#define _reportStaticCounts(fname, descr, count)

#else /* } !defined(TC_USE_MACROS) { */

extern void _reportStaticCounts(const char * fname, const char * descr, int count);

#endif /* } !defined(TC_USE_MACROS) */

#endif /* } !defined(TC_HISTO) */

#endif /* } !defined (TC_PREINSTR) */

#endif /* } _TYPECHECK_VERIFYAPI_PTR_H_ */

