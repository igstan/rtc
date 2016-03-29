#ifndef _TYPECHECK_INTERNAL_H
#define _TYPECHECK_INTERNAL_H

#define TC_INTERNAL

#include <tcapi.h>
#include <stdio.h>

/************************/
/* GDB-usable Functions */
/************************/
extern void _printTagStderr(void * addr, size_t size);
extern void _tcSetBreak(int msgid, int line);
extern void _tcMallocStatus(void * ptr);

extern _mirror_pos_t _getTagPos(const void * addr);
extern void _printConstsStderr();
/*
extern void _tcfn_SET_UNALLOC_TAG(const void * saddr, const void * eaddr);
extern const void * _tcfn_MIRRORMAP_INDEX(const void * addr);
extern const void * _tcfn_MIRRORPAGE_INDEX(const void * addr);
extern int _tcfn_SAME_MIRROR_PAGE(const void * addr1, const void * addr2);
extern unsigned char _tcfn_TAG_BITS(_mirror_pos_t pos);
extern unsigned char _tcfn_CONT_BIT(_mirror_pos_t pos);
extern unsigned char _tcfn_TYPE_BITS(_mirror_pos_t pos);
extern void WRITE_TAG(_mirror_pos_t * posptr, unsigned char tag);
extern _mirror_pos_t _tcfn_get_NEXT_POS(_mirror_pos_t pos);
extern _mirror_pos_t _tcfn_get_PREV_POS(_mirror_pos_t pos);
extern unsigned char _tcfn_ODD_TAG_BITS(unsigned char byte);
extern unsigned char _tcfn_ODD_TYPE_BITS(unsigned char byte);
extern unsigned char _tcfn_EVEN_TAG_BITS(unsigned char byte);
extern unsigned char _tcfn_EVEN_TYPE_BITS(unsigned char byte);
extern unsigned char _tcfn_GET_TAG_BYTE(const void * addr);
extern const void * _tcfn_GET_TAG_BYTE_addr(const void * addr);
extern unsigned char _tcfn_GET_CHAR_TAG(unsigned char byte, int isOdd);
extern void _tcfn_SET_CHAR_TAG(unsigned char * lhsptr, unsigned char * rhsptr, int isOdd);
*/


/**********************/
/* Compile time flags */
/**********************/
/* This is the size of the extra padding we put between malloc'ed blocks */
#define TC_MALLOC_PADDING 64

/* -- verify buffer size for input functions (e.g., fgets) */
#define TC_VERIFY_INPUT_BUFFER_ALLOC

/* -- debugging: trace call stack bounds */
/* #define TC_TRACECALL */

/* -- turn on/off printing of stats */
#define TC_DOSTATS

/* -- turn on/off getrusage timing */
/* #define TC_DO_TIMINGS(x) x */
#define TC_DO_TIMINGS(x)

/* -- if we want multiline (human-readable) output */
#define TC_MULTILINE

/* -- if we want compact (minimal) output */
/* #define TC_TERSE_OUTPUT */

/* -- log if we're copying INTO mismatched memory */
/* #define TC_LOG_DIRTY_COPY */

/* -- warn if setUninitTag overwrites non-zero (non-Unalloc) memory */
/*    - disabled because of locally-scoped aggregates, e.g. for(){char c[];} */
/* #define TC_CHECK_SET_UNALLOC */

/* -- Counts process return "efficiency" */
/* #define TC_COUNT_PROC_RET */

/******************/
/* Run time flags */
/******************/

/* keep track of free'd memory for diagnostic lookup */
extern int tc_flag_trackfree;


/************************/
/* Signal sending stuff */
/************************/

extern int _brk_msgid, _brk_line;

/* This suppresses the sending of a signal each time
 * a warning or error is encountered.
 * Note: overrides run-time flag (-tc-nosig).
 */
/*
#define TC_NOSIGNAL
*/

#ifndef TC_NOSIGNAL
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

#define TCSIG SIGUSR1
#define TCKILL kill(getpid(),TCSIG)

#else /* TC_NOSIGNAL */

#define TCKILL

#endif /* TC_NOSIGNAL */

/******************/
/* Output Streams */
/******************/
enum _tc_stream {_tc_null	= 0,
		 _tc_log	= 1,
		 _tc_warn	= 2,
		 _tc_error	= 3,
		 _tc_debug	= 4,
		 _tc_stat	= 5,
		 _tc_numstreams	= 6};

static char * stream_descr[_tc_numstreams] = { "null", "log", "warn", "error", "debug", "stat" };

/***********/
/* Various */
/***********/
typedef unsigned char uchar;

extern void _printTagPos(FILE * f, _mirror_pos_t tagpos, size_t size);

extern void _printTag(FILE * f, const void * addr, size_t size);

extern int verifyAlloc(const void * addr, size_t size);

extern void overflowTagPos(_mirror_pos_t * tagpos, int offset);

#define GET_TAG_POS(pos,addr) ( \
	  ((pos).ptr = (unsigned char *) mirrormap[MIRRORMAP_INDEX(addr)]) \
	  || (_touchMirrorPage(MIRRORMAP_INDEX(addr)), \
	      (pos).ptr = (unsigned char *) mirrormap[MIRRORMAP_INDEX(addr)]), \
	  (pos).ptr += MIRRORPAGE_INDEX(addr)/2, \
	  (pos).bit = (unsigned long)(addr)%2 * BITS_PER_TAG \
	)

#define SET_UNALLOC_TAG(caller,fname,line,col, start_addr, end_addr) { \
	  _mirror_pos_t spos, epos; \
	  char * saddr = (char *) (start_addr); \
	  char * eaddr = (char *) (end_addr); \
 \
	  if(saddr != eaddr) { \
	    /* Check stray tags to left */ \
	    GET_TAG_POS(spos, saddr); \
	    if(CONT_BIT(spos)){ \
	      _mirror_pos_t tmp; \
	      _output_si(fname,line,col, SUT_STB, caller,0,0,0, 0,0,0,0); \
	      tmp = spos; \
	      PREV_POS(tmp); \
	      while(CONT_BIT(tmp)){ \
	        /* AAL: unalloc or uninit? */ \
	        WRITE_TAG(tmp,_typetag_unalloc); \
	        PREV_POS(tmp); \
	      } \
	      WRITE_TAG(tmp,_typetag_unalloc); \
	    } \
	    /* Align saddr to even address */ \
	    if((unsigned long)saddr & 0x1){ \
	      WRITE_TAG(spos,_typetag_unalloc); \
	      saddr++; \
	    } \
 \
	    /* Check stray tags to right */ \
	    GET_TAG_POS(epos, eaddr); \
	    if(CONT_BIT(epos)){ \
	      _mirror_pos_t tmp; \
	      _output_si(fname,line,col, SUT_STA, caller,0,0,0, 0,0,0,0); \
	      tmp = epos; \
	      while(CONT_BIT(tmp)){ \
	        /* AAL: unalloc or uninit? */ \
	        WRITE_TAG(tmp,_typetag_unalloc) \
	        NEXT_POS(tmp); \
	      } \
	    } \
	    /* Align eaddr to even address */ \
	    if((unsigned long)eaddr & 0x1){ \
	      --eaddr; \
	      GET_TAG_POS(epos, eaddr); /* note: eaddr is (char *) */ \
	      WRITE_TAG(epos,_typetag_unalloc); \
	    } \
 \
	    if(SAME_MIRROR_PAGE(saddr,eaddr-1)){ \
	      memset(&GET_TAG_BYTE(saddr), _typetag_unalloc, (eaddr-saddr)/2); \
	    } else { \
	      int sindex = MIRRORMAP_INDEX(saddr); \
	      int eindex = MIRRORMAP_INDEX(eaddr-1); \
	      memset(&GET_TAG_BYTE(saddr), _typetag_unalloc, \
			(MIRRORPAGE_NUMBYTES - MIRRORPAGE_INDEX(saddr))/2); \
	      for(++sindex; sindex < eindex; sindex++){ \
	        memset(&GET_TAG_BYTE((char *)(sindex << MIRRORPAGE_NUMBITS)), \
			_typetag_unalloc, MIRRORPAGE_NUMBYTES/2); \
	      } \
	      memset(&GET_TAG_BYTE((char *)(sindex << MIRRORPAGE_NUMBITS)), \
			_typetag_unalloc, MIRRORPAGE_INDEX(eaddr)/2); \
	    } \
	  } \
	}

void _copyTagSilent(const void * dstaddr, const void * srcaddr, size_t size);

/***********************************************************************/
/* Output manager (more or less centralizes log/warning/error outputs) */
/***********************************************************************/

/* Note: implicitly, the first "targs" integer arguments correspond to the
         sizes of the tags (should be asserted?) */

#if !defined(TC_DEFINE_MSGS)
  extern
#endif
  struct {
    enum _tc_stream str; /* _tc_null to deactivate */
    int id;
    const char * msg;
    const char * sdesc;
    const char * idesc;
    const char * adesc;
    const char * tdesc;
    int sargs;
    int iargs;
    int aargs;
    int targs;
  } _msgs[]
#if defined(TC_DEFINE_MSGS)
	= {
#define DMSG(s,i,m,sd,id,ad,td,sa,ia,aa,ta) {s,i,m,sd,id,ad,td,sa,ia,aa,ta},
#else /* !defined(TC_DEFINE_MSGS) */
	;
#define DMSG(s,i,m,sd,id,ad,td,sa,ia,aa,ta)
#endif /* defined(TC_DEFINE_MSGS) */

#define	MSGS_0  0
DMSG(_tc_null,	MSGS_0,	"INTERNAL ERROR", "","","","",	0, 0, 0, 0)

#define	SUT_STB   1
DMSG(_tc_warn,	SUT_STB,	"SET_UNALLOC_TAG: cleaning stray tags (before)",
				"function","","","",	1, 0, 0, 0)

#define	SST_ONM	    2
DMSG(_tc_warn,	SST_ONM,	"setScalarTag: Overwrote non-aligned/non-empty memory!",
				"ovr-tag","ovr-size","addr","",
				1, 1, 1, 0)

#define	PEV_IT	    3
DMSG(_tc_debug,	PEV_IT,		"processExternVars: "
			"extern with incomplete type not initialized!",
					"","","","",	0, 0, 0, 0)
#define	RE_PDEF	    4
DMSG(_tc_warn,	RE_PDEF,	"registerExtern: extern previously defined with different type!",
	"deffile:origfile:deftype:origtype",
	"defline:origline:defcol:origcol", "", "",	4, 4, 0, 0)

#define	VP_UNA	    5
DMSG(_tc_error,	VP_UNA,		"verifyPtr: ptr points to unallocated memory!",
	"exp", "",
	"addr", "", 					1, 0, 1, 0)

#define	VT_TMM	    6
DMSG(_tc_error,	VT_TMM,		"verifyTag: type mismatch!",
	"exp",
	"exsize:fndsize",
	"addr",
	"expect:found", 				1, 2, 1, 2)

#define	VT_TSM	    7
DMSG(_tc_error,	VT_TSM,		"verifyTag: type size mismatch!",
	"exp",
	"exsize:fndsize",
	"addr",
	"expect:found", 				1, 2, 1, 2)

#define	SBT_STB	    8
DMSG(_tc_warn,	SBT_STB,	"setByteTags: stray tags (before)!",
					"","","","",	0, 0, 0, 0)

#define	SBT_STA	    9
DMSG(_tc_warn,	SBT_STA,	"setByteTags: stray tags (after)!",
					"","","","",	0, 0, 0, 0)

#define	SBT_NEM     10
DMSG(_tc_warn,	SBT_NEM,	"setByteTags: initializing non-empty memory!",
		"","size:approx-offset","addr","tags",	0, 2, 1, 1)

#define	CT_CFU	    11
DMSG(_tc_error,	CT_CFU,		"copyTag: copying from unallocated memory!",
	"", "",
	"from:to", "",	 				0, 0, 2, 0)

#define	CT_AIU	    12
DMSG(_tc_error,	CT_AIU,		"copyTag: ... and into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	CT_CIU	    13
DMSG(_tc_error,	CT_CIU,		"copyTag: copying into unallocated memory!",
	"", "",
	"from:to", "",	 				0, 0, 2, 0)

#define	CT_TMM	    14
DMSG(_tc_log,	CT_TMM,		"copyTag: type mismatch!",
	"", "exsize:cpsize:insize",
	"src,dst",
	"expect:copying:into", 				0, 3, 2, 3)

#define	CT_TDM	    15
DMSG(_tc_log,	CT_TDM,		"copyTag: type mismatch (dest)!",
	"", "exsize:cpsize:insize",
	"src,dst",
	"expect:copying:into", 				0, 3, 2, 3)

#define	CT_SI	    16
DMSG(_tc_warn,	CT_SI,		"copyTag: source invalid!",
	"", "cpsize:insize",
	"src,dst",
	"copying:into", 				0, 2, 2, 2)

#define	SUT_STA   17
DMSG(_tc_warn,	SUT_STA,	"SET_UNALLOC_TAG: cleaning stray tags (after)",
				"function","","","",	1, 0, 0, 0)


#define	ERV_ENN	    18
DMSG(_tc_error,	ERV_ENN,	"extern_registerVar: externInitFns not null!",
					"","","","",	0, 0, 0, 0)

#define	SSUT_NEM    19
DMSG(_tc_warn,	SSUT_NEM,	"setScalarUninitTag: initializing non-empty memory!",
					"","","","",	0, 0, 0, 0)

#define	PR_TMM	    20
DMSG(_tc_warn,	PR_TMM,		"processReturn: type mismatch!",
	"", "exsize:fndsize",
	"", "expecting:found", 				0, 2, 0, 2)

#define	PR_TSM	    21
DMSG(_tc_warn,	PR_TSM,		"processReturn: type size mismatch!",
	"", "exsize:fndsize",
	"", "expecting:found", 				0, 2, 0, 2)

#define	PR_TMC	    22
DMSG(_tc_warn,	PR_TMC,		"processReturn: type mismatch (corrected)!",
	"", "exsize:fndsize",
	"", "expecting:found", 				0, 2, 0, 2)

#define	PAT_TMA	    23
DMSG(_tc_warn,	PAT_TMA,	"processArgTags: type size mismatch (aggregate)!",
	"", "expecting:found:index",
	"", "", 					0, 3, 0, 0)

#define	PAT_TMS	    24
DMSG(_tc_warn,	PAT_TMS,	"processArgTags: type mismatch (scalar)!",
	"", "exsize:fndsize:index",
	"", "expecting:found", 				0, 3, 0, 2)

#define	PAT_NEA	    25
DMSG(_tc_error,	PAT_NEA,	"processArgTags: "
			"not enough arguments passed to function!",
	"", "expecting:passed",
	"", "", 					0, 2, 0, 0)

#define	TCF_FUM     26
DMSG(_tc_error,	TCF_FUM,	"(tc_free): freeing unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCMC_CFU    27
DMSG(_tc_error,	TCMC_CFU,	"(tc_memcpy): copying from unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCMC_CIU    28
DMSG(_tc_error,	TCMC_CIU,	"(tc_memcpy): copying into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCMM_CFU    29
DMSG(_tc_error,	TCMM_CFU,	"(tc_memmove): copying from unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCMM_CIU    30
DMSG(_tc_error,	TCMM_CIU,	"(tc_memmove): copying into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCMS_SUM    31
DMSG(_tc_error,	TCMS_SUM,	"(tc_memset): setting unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCLIB_CFU   32
DMSG(_tc_error,	TCLIB_CFU,	"(tc library fn): copying from unallocated memory!",
	"function", "copybytes:allocbytes",
	"", "", 					1, 2, 0, 0)

#define	TCLIB_CIU   33
DMSG(_tc_error,	TCLIB_CIU,	"(tc library fn): copying into unallocated memory!",
	"function","copybytes","","",			1, 1, 0, 0)

#define	TCFG_RIUM   34
DMSG(_tc_error,	TCFG_RIUM,	"(tc_fgets): reading into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCG_RIUM    35
DMSG(_tc_error,	TCG_RIUM,	"(tc_gets): reading into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCSP_RIUM   36
DMSG(_tc_error,	TCG_RIUM,	"(tc_sprintf): reading into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCSNP_RIUM  37
DMSG(_tc_error,	TCG_RIUM,	"(tc_snprintf): reading into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCFR_RIUM   38
DMSG(_tc_error,	TCFR_RIUM,	"(tc_[f]read): reading into unallocated memory!",
					"","","","",	0, 0, 0, 0)

#define	TCSC_MFS    39
DMSG(_tc_error,	TCSC_MFS,	"(tc_*scanf): malformed format string!",
					"","","","",	0, 0, 0, 0)

#define	TCSC_POSN   40
DMSG(_tc_warn,	TCSC_POSN,	"(tc_*scanf): mix of positional and non-positional params!",
					"","","","",	0, 0, 0, 0)

#define	TCSC_UM     41
DMSG(_tc_error,	TCSC_UM,	"(tc_*scanf): reading into unallocated memory!",
		"descr","arg#:fmt#","","",	1, 2, 0, 0)

#define	MSGS_LAST   42
DMSG(_tc_null,	MSGS_LAST,	"INTERNAL ERROR", "","","","",	0, 0, 0, 0)

#if defined(TC_DEFINE_MSGS)
    {0}}; /* dummy last entry */
#undef DMSG
#endif /* !defined(TC_DEFINE_MSGS) */

/********************/
/* Output functions */
/********************/

void _output_internal_error(const char * context, const char * msg);

void _output_simple(int msgid);

void _output_flc(const char * file, int line, int col,
		int msgid);

void _output_si(const char * file, int line, int col,
		int msgid,
		const char *s1, const char *s2, const char *s3, const char *c4,
		int i1, int i2, int i3, int i4);

void _output_full(const char * file, int line, int col,
		int msgid,
		const char *s1,
		int i1, int i2, int i3,
		const void *a1, const void *a2,
		const void *t1, const void *t2, const void *t3);

#endif /* ifndef _TYPECHECK_INTERNAL_H */
