#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* memset */
#include <sys/resource.h> /* for getrusage timing stuff */
#include <sys/time.h> /* for gettimeofday timing stuff */
#include <time.h> /* time, ctime */

#include "tcptr_internal.h"
#include "tcmalloc-hash.h" /* for mhash_getsize optimization, and mhash_debugout */

/******************************************/
/* COMPILE_TIME FLAGS */
/******************************************/
/* #define TC_NOSIGNAL */

#define TC_PRINT_TIMINGS

/* #define TC_LINUX_PRINT_PROC_STAT */

/* #define TC_PRINT_STATS */ /* turned on when compiling -g */

/* #define LOG_VP_CALLERS */ /* diagnosis tool: logs histogram of vp callers */

/******************************************/
/* AUXILIARY */
/******************************************/
/* TIMINGS */
#ifdef TC_PRINT_TIMINGS /* { */
#define TC_DO_TIMINGS(x) x
struct rusage ru0, ru1, ru2, ru3, ru4, ru5;
struct timeval tv0, tv1, tv2, tv3, tv4, tv5;
#else /* } !defined TC_PRINT_TIMINGS { */
#define TC_DO_TIMINGS(x)
#endif /* } !defined TC_PRINT_TIMINGS */

#ifdef TC_PRINT_STATS /* { */
#define TC_STAT_COUNT(x) (statcounter[x]++)
#define TC_STAT_COUNT_KB_ADD(x,i) do {	static int tmp = 512;		\
					tmp += i;			\
					if(tmp >= 1024){		\
					  statcounter[x] += tmp/1024;	\
					  tmp = tmp%1024;		\
					}				\
				  } while(0)
#else /* } !defined TC_PRINT_STATS { */
#define TC_STAT_COUNT(x)
#define TC_STAT_COUNT_KB_ADD(x,i)
#endif /* } !defined TC_PRINT_STATS */

/* SIGNALS */
#ifndef TC_NOSIGNAL /* { */
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

#define TCSIG SIGUSR1
#define TCKILL kill(getpid(),TCSIG)

#else /* } TC_NOSIGNAL { */

#define TCKILL

#endif /* } TC_NOSIGNAL */

int tc_flag_hashopt_threshhold = 64; /* hash optimization lookup threshhold */

char * tc_flag_logfile = 0;

static char tc_flag_skipvp = 0;
static char tc_flag_skipst = 0;
static char tc_flag_skipct = 0;

/******************************************/
/* MIRROR SETUP */
/******************************************/
#if defined(TC_STATIC_MIRROR) /* { */
  /**************************************/
  /* STATIC MIRROR */
  /**************************************/

unsigned char _tc_mirror[1 << (sizeof(void *)*8 - TAGS_PER_BYTE_BITS)];

#else /* } !defined(TC_STATIC_MIRROR) { */
  /**************************************/
  /* NOT STATIC MIRROR: MIRRORMAP */
  /**************************************/

/* the mirrormap lookup table */
void * mirrormap[MIRRORMAP_NUMELEMENTS] = {0};

/* invariant: mirrormap_freelist[0] == 0 always, to signify bottom of "stack" */
void * mirrormap_freelist[TAGS_PER_BYTE] = {0};
void ** mirrormap_freelist_top = &mirrormap_freelist[0];

void _touchMirrorPage(unsigned long mapindex)
{
  if(*mirrormap_freelist_top){
    mirrormap[mapindex] = *mirrormap_freelist_top;
    mirrormap_freelist_top--;
 } else {
    int i;
    mirrormap[mapindex] = (void *) memalign(MIRRORPAGE_NUMBYTES, MIRRORPAGE_NUMBYTES);
    if(!mirrormap[mapindex])
      exit((_output_flce(__FILE__,__LINE__,0,"(_touchMirrorPage)","FATAL: memalign out of memory"),-1));
    for(i = 1; i < TAGS_PER_BYTE; ++i)
      *(++mirrormap_freelist_top) = (char *)mirrormap[mapindex] + i * MIRRORPAGE_SIZE;
  }
  /* initialize mirror to 0 */
  bzero(mirrormap[mapindex], MIRRORPAGE_SIZE);
}

#endif /* } !defined(TC_STATIC_MIRROR) */

/******************************************/
/* DIAGNOSTIC */
/******************************************/
/* typechecker output streams */
/* these are initialized in main() */
enum _tc_stream {_tc_null = 0,
		 _tc_stat,
		 _tc_error,
		 _tc_debug,
		 _tc_numstreams
		};
static FILE * streams[_tc_numstreams] = {0};

/* on function return, different modes of
   clearing the stack frame w.r.t. formals */
#define _TC_NONE        0
#define _TC_SEGS_ONLY   1
#define _TC_TO_SCALAR   2
#define _TC_TO_AGGR     3
#define _TC_TO_HIGHEST  4
static int tc_flag_clear = _TC_SEGS_ONLY;

static int tc_flag_signal = 1;

#ifdef TC_PRINT_STATS /* { */

enum statcounter_entries {
  sc_pr_calls,		/* number of processReturn calls */
  sc_vp_array,		/* verifyPtr of array-index expression */
  sc_vp_onebyte,	/* verifyPtr tag fits in one byte */
  sc_debug_vp_onebyte_ff,	/* verifyPtr one-byte is 0xff */
  sc_vp_twobytes,	/* verifyPtr tag fits in two byte */
  sc_vp_multibytes,	/* verifyPtr tag larger than 8 */
  sc_vp_multipage,	/* verifyPtr tag traversing page boundary */
  sc_vp_hash_succ,	/* verifyPtr hash optimization succeeded */
  sc_vp_hash_fail,	/* verifyPtr hash optimization failed */
  sc_st_onebyte,	/* setTag fits in one byte */
  sc_st_twobytes,	/* setTag fits in two byte */
  sc_st_multibytes,	/* setTag larger than 8 */
  sc_st_multipage,	/* setTag traversing page boundary */
  sc_ct_onebyte,	/* clearTag fits in one byte */
  sc_ct_twobytes,	/* clearTag fits in two byte */
  sc_ct_multibytes,	/* clearTag larger than 8 */
  sc_ct_multipage,	/* clearTag traversing page boundary */

  sc_count_vp,		/* count (total) calls, and number of bytes affected */
  sc_count_vp_kbs,
  sc_count_st,
  sc_count_st_kbs,
  sc_count_ct,
  sc_count_ct_kbs,
  sc_max
};
static const char * statdesc[sc_max] = {
  "pr_calls",
  "vp_array",
  "vp_onebyte",
  "debug_vp_onebyte_ff",
  "vp_twobytes",
  "vp_multibytes",
  "vp_multipage",
  "vp_hash_succ",
  "vp_hash_fail",
  "st_onebyte",
  "st_twobytes",
  "st_multibytes",
  "st_multipage",
  "ct_onebyte",
  "ct_twobytes",
  "ct_multibytes",
  "ct_multipage",
  "count_vp",
  "count_vp_kbs",
  "count_st",
  "count_st_kbs",
  "count_ct",
  "count_ct_kbs",
};
unsigned long long statcounter[sc_max] = {0};

struct static_count_node {
  const char * descr;
  unsigned long count;
  struct static_count_node * next;
} * static_count_head = 0;

#endif /* } TC_PRINT_STATS */

void _printConstsStderr()
{
#if !defined(TC_STATIC_MIRROR) /* { */
#define EXPAND_PRINT_STMT(CID) fprintf(stderr, #CID " = %d (0x%08x)\n", CID, CID)
  EXPAND_PRINT_STMT(MIRRORPAGE_NUMBITS);
  EXPAND_PRINT_STMT(MIRRORPAGE_NUMBYTES);
  EXPAND_PRINT_STMT(MIRRORPAGE_MASK);
  EXPAND_PRINT_STMT(MIRRORMAP_NUMBITS);
  EXPAND_PRINT_STMT(MIRRORMAP_NUMELEMENTS);
#undef EXPAND_PRINT_STMT
#endif /* } !defined(TC_STATIC_MIRROR) */
}

int tc_dont_die = 0;

struct msg_entry {
  const char * file;
  int line, col;
  const char * exp;
  int count;
  struct msg_entry * next;
} * msg_head = 0,
 ** msg_tail = &msg_head;

int tc_flag_summarize = 0;

static struct msg_entry * find(const char * file, int line, int col, const char * exp)
{
  struct msg_entry * n;
  if(file && *file){
    for(n = msg_head; n; n = n->next)
      if(n->line == line && n->col == col &&
	 (n->file == file || !strcmp(n->file, file)))
        return n;
  } else { /* empty file: library function; use exp as key */
    for(n = msg_head; n; n = n->next)
      if(n->exp == exp) return n;
  }
  return 0;
}

static int print_error_summary(FILE * os)
{
  int count = 0;
  struct msg_entry * n;
  for(n = msg_head; n; n = n->next){
    fprintf(os, "{%d}[%s:%d.%d](%s)\n", n->count, n->file, n->line, n->col, n->exp);
    count += n->count;
  }
  return count;
}

void _output_flce(const char * file, int line, int col, const char * exp, const char * msg)
{
  FILE * lf = streams[_tc_error];
  if(tc_flag_logfile){
    if((lf = fopen(tc_flag_logfile, "a"))){
      time_t now;
      char * tstr;
      time(&now);
      tstr = ctime(&now);
      fprintf(lf, "%s: ", tstr);
    } else {
      fprintf(streams[_tc_error], "TC Log Failed\n");
      lf = 0;
    }
  }
  if(lf){
    if(tc_flag_summarize){
      struct msg_entry * e = find(file, line, col, exp);
      if(e){
        e->count++;
      } else {
        fprintf(lf, "[%s:%d.%d](%s) %s\n", file, line, col, exp, msg);
        e = (struct msg_entry *) malloc(sizeof(struct msg_entry));
        if(!e){
          fprintf(lf, "_output_flce: malloc out of memory");
        } else {
          e->file = file;
          e->line = line;
          e->col = col;
          e->exp = exp;
          e->count = 1;
          e->next = 0;
          *msg_tail = e;
          msg_tail = &e->next;
        }
      }
    } else {
      fprintf(lf, "[%s:%d.%d](%s) %s\n", file, line, col, exp, msg);
      TCKILL;
    }
    if(tc_flag_logfile){
      fclose(lf);
    }
  }
  if(!tc_dont_die) exit(-1);
}

void _reportStaticCounts(const char * fname, const char * descr, int count)
{
#ifdef TC_PRINT_STATS /* { */
  struct static_count_node ** np;
  for(np = &static_count_head; *np; np = &(*np)->next){
    if(!strcmp(descr, (*np)->descr)){
      (*np)->count += count;
      break;
    }
  }
  if(!*np){
    *np = (struct static_count_node *) malloc(sizeof(struct static_count_node));
    (*np)-> descr = descr;
    (*np)->count = count;
    (*np)->next = 0;
  }
#endif /* } TC_PRINT_STATS */
}

void _printBinary(FILE * os, unsigned char c)
{
  int i;
  for(i = 0; i < _TC_BYTE; ++i, c >>= 1)
    fprintf(os, (c&0x01)?"I":"O");
}

void _printTag(FILE * os, const char * addr, size_t size)
{
  const char * saddr = addr;
  const char * eaddr = addr + size - 1;
  const unsigned char * sbyte = &GET_TAG_BYTE(saddr);
  const unsigned char * ebyte = &GET_TAG_BYTE(eaddr);
  int spos = MOD_TAGS_PER_BYTE(saddr);
  int epos = MOD_TAGS_PER_BYTE(eaddr);
  int mode = 0;
  int outcount = 0;

  if(sbyte == ebyte){ /* same byte */
    int i;
    unsigned char c = *sbyte;

    fprintf(os, "0x%08x: ", (unsigned long)saddr - spos);
    for(i = 0; i < _TC_BYTE; ++i){
      if(i == spos) mode = 1;
      fprintf(os, (mode)?((c & 0x01)?"I":"O"):((c & 0x01)?"i":"o"));
      if(i == epos) mode = 0;
      c >>= 1;
    }
  } else if(SAME_MIRROR_PAGE(saddr, eaddr)){
    int i;
    unsigned char c = *sbyte;
    unsigned long output_addr = (unsigned long)saddr - spos;

    fprintf(os, "0x%08x: ", output_addr);

    for(i = 0; i < _TC_BYTE; ++i){
      if(i == spos) mode = 1;
      fprintf(os, (mode)?((c & 0x01)?"I":"O"):((c & 0x01)?"i":"o"));
      c >>= 1;
    }
    outcount++;
    for(sbyte++; sbyte != ebyte; sbyte++){
      fprintf(os, " ");
      _printBinary(os, *sbyte);
      if(!(++outcount % 8))
        fprintf(os, "\n0x%08x:", output_addr += 64);
    }
    c = *sbyte;
    fprintf(os, " ");
    for(i = 0; i < _TC_BYTE; ++i){
      fprintf(os, (mode)?((c & 0x01)?"I":"O"):((c & 0x01)?"i":"o"));
      if(i == epos) mode = 0;
      c >>= 1;
    }
  } else { /* different mirror page */
#if !defined(TC_STATIC_MIRROR) /* { */
    int i;
    unsigned char c = *sbyte;
    unsigned long output_addr = (unsigned long)saddr - spos;
    const char * nbaddr = saddr;
    const unsigned char * nb_byte;
    const char * blank = " ";

    fprintf(os, "0x%08x: ", output_addr);

    for(i = 0; i < _TC_BYTE; ++i){
      if(i == spos) mode = 1;
      fprintf(os, (mode)?((c & 0x01)?"I":"O"):((c & 0x01)?"i":"o"));
      c >>= 1;
    }
    outcount++;
    sbyte++;
    do {
      nbaddr = (const char *)((unsigned long)nbaddr | MIRRORPAGE_MASK);
      nb_byte = &GET_TAG_BYTE(nbaddr) + 1;

      for(; sbyte != nb_byte; sbyte++){
        fprintf(os, blank);
        blank = " ";
        _printBinary(os, *sbyte);
        if(!(++outcount % 8))
          fprintf(os, "\n0x%08x:", output_addr += 64);
      }

      nbaddr++;
      sbyte = &GET_TAG_BYTE(nbaddr);
      blank = "*";
    } while(!SAME_MIRROR_PAGE(nbaddr, eaddr));

    for(; sbyte != ebyte; sbyte++){
      fprintf(os, blank);
      blank = " ";
      _printBinary(os, *sbyte);
      if(!(++outcount % 8))
        fprintf(os, "\n0x%08x:", output_addr += 64);
    }

    c = *sbyte;
    fprintf(os, blank);
    blank = " ";
    for(i = 0; i < _TC_BYTE; ++i){
      fprintf(os, (mode)?((c & 0x01)?"I":"O"):((c & 0x01)?"i":"o"));
      if(i == epos) mode = 0;
      c >>= 1;
    }
#endif /* } !defined(TC_STATIC_MIRROR) */
  }
  fprintf(os, "\n");
}

void _printTagStderr(const char * addr, size_t size)
{
  _printTag(stderr, addr, size);
}

/*****************************************/
/* print user and system time difference */
/*****************************************/
static void printdelta(FILE * s, struct rusage * r1, struct rusage * r2,
			struct timeval * tv1, struct timeval * tv2)
{
  struct timeval u1 = r1->ru_utime;
  struct timeval u2 = r2->ru_utime;
  struct timeval s1 = r1->ru_stime;
  struct timeval s2 = r2->ru_stime;

  fprintf(s, "%.3f r, %.3f u, %.3f s\n",
	(double)(tv2->tv_sec-tv1->tv_sec)+(double)(tv2->tv_usec-tv1->tv_usec)/1000000,
	(double)(u2.tv_sec-u1.tv_sec)+(double)(u2.tv_usec-u1.tv_usec)/1000000,
	(double)(s2.tv_sec-s1.tv_sec)+(double)(s2.tv_usec-s1.tv_usec)/1000000
	);
}

/******************************************/
/* API FUNCTIONS */
/******************************************/

#ifdef LOG_VP_CALLERS /* { */
static struct flc_count_node {
  const char * file;
  int line, col;
  const char * exp;
  int ncalls, nbytes;
  struct flc_count_node * next;
} * vp_callers = 0;
#endif /* } LOG_VP_CALLERS  */

void _tcptr_verifyPtr(const char * file, int line, int col, const char * exp,
		const void * addr, size_t size)
{
#if defined(TC_VULNERABLE_VP) /* { */
#define VP_VALID_TAG_BYTE 0
#else /* } !defined(TC_VULNERABLE_VP) { */
#define VP_VALID_TAG_BYTE 0xff
#endif /* } !defined(TC_VULNERABLE_VP) */

  if(tc_flag_skipvp) return;

#ifdef LOG_VP_CALLERS /* { */
  {
    struct flc_count_node * np;
    for(np = vp_callers; np; np = np->next){
      if(np->file == file &&
	  np->line == line &&
	  np->col == col){
        np->ncalls++;
        np->nbytes += size;
        break;
      }
    }
    if(!np){
      np = (struct flc_count_node *) malloc(sizeof(struct flc_count_node));
      np->file = file;
      np->line = line;
      np->col = col;
      np->exp = exp;
      np->ncalls = 1;
      np->nbytes = size;
      np->next = vp_callers;
      vp_callers = np;
    }
  }
#endif /* } LOG_VP_CALLERS  */

  TC_STAT_COUNT(sc_count_vp);
  TC_STAT_COUNT_KB_ADD(sc_count_vp_kbs, size);

#ifdef TC_PRINT_STATS /* { */
  /* count array-index expressions -- use exp string to guess */
  if(exp[strlen(exp)-1] == ']'){
    TC_STAT_COUNT(sc_vp_array);
  }
#endif /* } ifdef TC_PRINT_STATS */

  if((signed)(MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size){

    unsigned char mask = ((~(0xffu << (size))) << MOD_TAGS_PER_BYTE(addr));

    /* tag is all in one byte */
    TC_STAT_COUNT(sc_vp_onebyte);

#ifdef TC_PRINT_STATS /* { */
    if(GET_TAG_BYTE(addr) == VP_VALID_TAG_BYTE)
      TC_STAT_COUNT(sc_debug_vp_onebyte_ff);
#endif /* } TC_PRINT_STATS */

#if defined(TC_VULNERABLE_VP) /* { */
    if((mask & GET_TAG_BYTE(addr)) != 0)
      _output_flce(file,line,col,exp, "Vuln-VerifyPtr failed (case 1)");
#else /* } !defined(TC_VULNERABLE_VP) { */
    if((mask & GET_TAG_BYTE(addr)) != mask)
      _output_flce(file,line,col,exp, "VerifyPtr failed (case 1)");
#endif /* } !defined(TC_VULNERABLE_VP) */

  } else {

    const char * saddr = (const char *) addr;
    const char * eaddr = saddr + size;
    unsigned char maskfront = (0xff << MOD_TAGS_PER_BYTE(saddr));
    unsigned char maskback = (0xffu >> (TAGS_PER_BYTE-MOD_TAGS_PER_BYTE(eaddr)));
    unsigned char * head_byte = &GET_TAG_BYTE(saddr);
    unsigned char * tail_byte = &GET_TAG_BYTE(eaddr);

    if(head_byte == tail_byte-1){

      /* tag is split across two successive bytes */
      TC_STAT_COUNT(sc_vp_twobytes);

#if defined(TC_VULNERABLE_VP) /* { */
      if((maskfront & GET_TAG_BYTE(saddr)) != 0 ||
         (maskback  & GET_TAG_BYTE(eaddr)) != 0)
        _output_flce(file,line,col,exp, "Vuln-VerifyPtr failed (case 2)");
#else /* } !defined(TC_VULNERABLE_VP) { */
      if((maskfront & GET_TAG_BYTE(saddr)) != maskfront ||
         (maskback  & GET_TAG_BYTE(eaddr)) != maskback)
        _output_flce(file,line,col,exp, "VerifyPtr failed (case 2)");
#endif /* } !defined(TC_VULNERABLE_VP) */

    } else {

      TC_STAT_COUNT(sc_vp_multibytes);

      /* mhash optimization: disable for vuln-vp */
#if !defined(TC_VULNERABLE_VP) /* { */
      if(size >= tc_flag_hashopt_threshhold){
	if(size <= mhash_getsize(addr)){
          TC_STAT_COUNT(sc_vp_hash_succ);
          return;
        } else {
          TC_STAT_COUNT(sc_vp_hash_fail);
        }
      }
#endif /* } !defined(TC_VULNERABLE_VP) */

      if(SAME_MIRROR_PAGE(saddr, eaddr)){

#if defined(TC_VULNERABLE_VP) /* { */
        if((maskfront & *head_byte) != 0 ||
	   (maskback  & *tail_byte) != 0){
          _output_flce(file,line,col,exp, "Vuln-VerifyPtr failed (case 3a)");
        }
#else /* } !defined(TC_VULNERABLE_VP) { */
        if((maskfront & *head_byte) != maskfront ||
	   (maskback  & *tail_byte) != maskback){
          _output_flce(file,line,col,exp, "VerifyPtr failed (case 3a)");
        }
#endif /* } !defined(TC_VULNERABLE_VP) */
        else {
          for(head_byte++; head_byte != tail_byte; ++head_byte){
            if(*head_byte != VP_VALID_TAG_BYTE){
              _output_flce(file,line,col,exp, "VerifyPtr failed (case 3b)");
              break;
            }
          }
        }


      } else {
#if !defined(TC_STATIC_MIRROR) /* { */

        TC_STAT_COUNT(sc_vp_multipage);

#if defined(TC_VULNERABLE_VP) /* { */
        if((maskfront & *head_byte) != 0 ||
           (maskback  & *tail_byte) != 0){
          _output_flce(file,line,col,exp, "Vuln-VerifyPtr failed (case 3c)");
        }
#else /*  }!defined(TC_VULNERABLE_VP) { */
        if((maskfront & *head_byte) != maskfront ||
           (maskback  & *tail_byte) != maskback){
          _output_flce(file,line,col,exp, "VerifyPtr failed (case 3c)");
        }
#endif /* } !defined(TC_VULNERABLE_VP) */
        else {

          const char * nbaddr = saddr;

          head_byte++;

          do {
            const unsigned char * nb_byte;

            nbaddr = (const char *)((unsigned long)nbaddr | MIRRORPAGE_MASK);
            nb_byte = &GET_TAG_BYTE(nbaddr) + 1;

            for(; head_byte != nb_byte; ++head_byte){
              if(*head_byte != VP_VALID_TAG_BYTE){
                _output_flce(file,line,col,exp, "VerifyPtr failed (case 3d)");
                return;
              }
            }

            nbaddr++;
            head_byte = &GET_TAG_BYTE(nbaddr);

          } while(!SAME_MIRROR_PAGE(nbaddr, eaddr));

          for(; head_byte != tail_byte; ++head_byte){
            if(*head_byte != VP_VALID_TAG_BYTE){
              _output_flce(file,line,col,exp, "VerifyPtr failed (case 3e)");
              return;
            }
          }
        }
#endif /* } !defined(TC_STATIC_MIRROR) */
      }
    }
  }
#undef VP_VALID_TAG_BYTE
}

void _tcptr_setTags(const void * addr, size_t size)
{
  if(tc_flag_skipst) return;

  TC_STAT_COUNT(sc_count_st);
  TC_STAT_COUNT_KB_ADD(sc_count_st_kbs, size);

  if((signed)(MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size){

    unsigned char mask = ((~(0xffu << (size))) << MOD_TAGS_PER_BYTE(addr));

    /* tag is all in one byte */
    TC_STAT_COUNT(sc_st_onebyte);

    GET_TAG_BYTE(addr) |= mask;

  } else {

    const char * saddr = (const char *) addr;
    const char * eaddr = saddr + size;
    unsigned char maskfront = (0xff << MOD_TAGS_PER_BYTE(saddr));
    unsigned char maskback = (0xffu >> (TAGS_PER_BYTE-MOD_TAGS_PER_BYTE(eaddr)));
    unsigned char * head_byte = &GET_TAG_BYTE(saddr);
    unsigned char * tail_byte = &GET_TAG_BYTE(eaddr);

    if(head_byte == tail_byte-1){

      /* tag is split across two successive bytes */
      TC_STAT_COUNT(sc_st_twobytes);

      *head_byte |= maskfront;
      *tail_byte |= maskback;

    } else {

      TC_STAT_COUNT(sc_st_multibytes);

      if(SAME_MIRROR_PAGE(saddr, eaddr)){

        *head_byte |= maskfront;
        memset(head_byte+1, 0xff, tail_byte - head_byte - 1);
        *tail_byte |= maskback;

      } else {
#if !defined(TC_STATIC_MIRROR) /* { */

        const char * nbaddr = saddr;

        TC_STAT_COUNT(sc_st_multipage);

        *head_byte |= maskfront;

        head_byte++;

        do {

          const unsigned char * nb_byte;

          nbaddr = (const char *)((unsigned long)nbaddr | MIRRORPAGE_MASK);
          nb_byte = &GET_TAG_BYTE(nbaddr) + 1;

          memset(head_byte, 0xff, nb_byte - head_byte);

          nbaddr++;
          head_byte = &GET_TAG_BYTE(nbaddr);

        } while(!SAME_MIRROR_PAGE(nbaddr, eaddr));

        memset(head_byte, 0xff, tail_byte - head_byte);

        *tail_byte |= maskback;
#endif /* } !defined(TC_STATIC_MIRROR) */
      }
    }
  }
}

/* should be near-identical to setTags */
void _tcptr_clearTags(const void * addr, size_t size)
{
  if(tc_flag_skipct) return;

  TC_STAT_COUNT(sc_count_ct);
  TC_STAT_COUNT_KB_ADD(sc_count_ct_kbs, size);

  if((signed)(MOD_TAGS_PER_BYTE(addr)) <= TAGS_PER_BYTE - (signed)size){

    unsigned char mask = ((~(0xffu << (size))) << MOD_TAGS_PER_BYTE(addr));

    /* tag is all in one byte */
    TC_STAT_COUNT(sc_ct_onebyte);

    GET_TAG_BYTE(addr) &= ~mask;

  } else {

    const char * saddr = (const char *) addr;
    const char * eaddr = saddr + size;
    unsigned char maskfront = (0xff << MOD_TAGS_PER_BYTE(saddr));
    unsigned char maskback = (0xffu >> (TAGS_PER_BYTE-MOD_TAGS_PER_BYTE(eaddr)));
    unsigned char * head_byte = &GET_TAG_BYTE(saddr);
    unsigned char * tail_byte = &GET_TAG_BYTE(eaddr);

    if(head_byte == tail_byte-1){

      /* tag is split across two successive bytes */
      TC_STAT_COUNT(sc_ct_twobytes);

      *head_byte &= ~maskfront;
      *tail_byte &= ~maskback;

    } else {

      TC_STAT_COUNT(sc_ct_multibytes);

      if(SAME_MIRROR_PAGE(saddr, eaddr)){

        *head_byte &= ~maskfront;
        memset(head_byte+1, 0x00, tail_byte - head_byte - 1);
        *tail_byte &= ~maskback;

      } else {
#if !defined(TC_STATIC_MIRROR) /* { */

        const char * nbaddr = saddr;

        TC_STAT_COUNT(sc_ct_multipage);

        *head_byte &= ~maskfront;

        head_byte++;

        do {

          const unsigned char * nb_byte;

          nbaddr = (const char *)((unsigned long)nbaddr | MIRRORPAGE_MASK);
          nb_byte = &GET_TAG_BYTE(nbaddr) + 1;

          memset(head_byte, 0x00, nb_byte - head_byte);

          nbaddr++;
          head_byte = &GET_TAG_BYTE(nbaddr);

        } while(!SAME_MIRROR_PAGE(nbaddr, eaddr));

        memset(head_byte, 0x00, tail_byte - head_byte);

        *tail_byte &= ~maskback;
#endif /* } !defined(TC_STATIC_MIRROR) */
      }
    }
  }
}

void _tcptr_processReturn(void * scaf_start, void * scaf_end,
			  void * agrf_start, void * agrf_end,
			  void * local_start)
{
  static int debugflag = 0;

  /* mark end of stack frame */
  void * sf_top = &sf_top;

  TC_STAT_COUNT(sc_pr_calls);

  /* Clear the stack frame between sf_bottom and sf_top */

  /* mark start of stack */
  if(local_start > sf_top){ /* stack grows downward */

    void * sf_bottom = local_start;

    switch(tc_flag_clear){
      case _TC_TO_HIGHEST:
           if(scaf_start && scaf_end > sf_bottom) sf_bottom = scaf_end;
           if(agrf_start && agrf_end > sf_bottom) sf_bottom = agrf_end;
           break;
      case _TC_TO_SCALAR:
           if(scaf_start && scaf_end > sf_bottom) sf_bottom = scaf_end;
           if(agrf_start && agrf_end > scaf_end) {
             _tcptr_clearTags(agrf_start, (char *)agrf_end-(char *)agrf_start);
           }
           break;
      case _TC_TO_AGGR:
           if(agrf_start && agrf_end > sf_bottom) sf_bottom = agrf_end;
           if(scaf_start && scaf_end > agrf_end) {
             _tcptr_clearTags(scaf_start, (char *)scaf_end-(char *)scaf_start);
           }
           break;
      case _TC_SEGS_ONLY:
           if(scaf_start) {
             _tcptr_clearTags(scaf_start, (char *)scaf_end-(char *)scaf_start);
           }
           if(agrf_start) {
             _tcptr_clearTags(agrf_start, (char *)agrf_end-(char *)agrf_start);
           }
      case _TC_NONE:
           ;
    }

    _tcptr_clearTags(sf_top, (char *)sf_bottom-(char *)sf_top);

    if(!debugflag++ && streams[_tc_debug])
      fprintf(streams[_tc_debug], "Note: stack grows decreasingly!\n");

  } else {                /* stack grows upward   */

    void * sf_bottom = local_start;

    switch(tc_flag_clear){
      case _TC_TO_HIGHEST:
           if(scaf_start && scaf_start < sf_bottom) sf_bottom = scaf_start;
           if(agrf_start && agrf_start < sf_bottom) sf_bottom = agrf_start;
           break;
      case _TC_TO_SCALAR:
           if(scaf_start && scaf_start < sf_bottom) sf_bottom = scaf_start;
           if(agrf_start && agrf_start < scaf_start) {
             _tcptr_clearTags(agrf_start, (char *)agrf_end-(char *)agrf_start);
           }
           break;
      case _TC_TO_AGGR:
           if(agrf_start && agrf_start < sf_bottom) sf_bottom = agrf_end;
           if(scaf_start && scaf_start < agrf_start) {
             _tcptr_clearTags(scaf_start, (char *)scaf_end-(char *)scaf_start);
           }
           break;
      case _TC_SEGS_ONLY:
           if(scaf_start) {
             _tcptr_clearTags(scaf_start, (char *)scaf_end-(char *)scaf_start);
           }
           if(agrf_start) {
             _tcptr_clearTags(agrf_start, (char *)agrf_end-(char *)agrf_start);
           }
      case _TC_NONE:
           ;
    }

    _tcptr_clearTags(sf_bottom, (char *)sf_top-(char *)sf_bottom);

    if(!debugflag++ && streams[_tc_debug])
      fprintf(streams[_tc_debug], "Note: stack grows increasingly\n");
  }
}

/******************************************/

/******************************************/
/* SETUP - not needed! */
/******************************************/
void processExternVars(void)
{
}

/***********************************/
/* CLEANUP */
/***********************************/
static void cleanup()
{
  FILE * statstream = streams[_tc_stat];

#ifdef TC_PRINT_STATS /* { */
  if(statstream){
    extern void print_histo(FILE * s);	/* defined in tcptrhisto.c */
    tcptr_print_histo(statstream);

    mhash_debugout(statstream);	/* defined in tcmalloc-hash.c */
  }
#endif /* } TC_PRINT_STATS */

#ifdef LOG_VP_CALLERS /* { */
  if(statstream){
    struct flc_count_node * np;
    fprintf(statstream, "VPcounts:\n");
    for(np = vp_callers; np; np = np->next){
      fprintf(statstream, "VPcount(%d,%d): %s:%d.%d (%s)\n", np->ncalls, np->nbytes,
					np->file, np->line, np->col, np->exp);
    }
  }
#endif /* } LOG_VP_CALLERS */

  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru5));
  TC_DO_TIMINGS(gettimeofday(&tv5, 0));
#ifdef TC_PRINT_TIMINGS /* { */
  if(statstream){
    fprintf(statstream, "Total time: ");
    printdelta(statstream, &ru0, &ru5, &tv0, &tv5);

    fprintf(statstream, "Call init functions: ");
    printdelta(statstream, &ru1, &ru2, &tv1, &tv2);

    fprintf(statstream, "Process externs: ");
    printdelta(statstream, &ru2, &ru3, &tv2, &tv3);

    fprintf(statstream, "Prog_main: ");
    printdelta(statstream, &ru4, &ru5, &tv4, &tv5);

    fprintf(statstream, "RU Min PgFaults: %ld\n", ru5.ru_minflt);
    fprintf(statstream, "RU Maj PgFaults: %ld\n", ru5.ru_majflt);

/* These are unfortunately not implemented in current linux
    fprintf(statstream, "RU Max RSS: %ld\n", ru5.ru_maxrss);
    fprintf(statstream, "RU Swaps: %ld\n", ru5.ru_nswap);
*/

#ifdef TC_LINUX_PRINT_PROC_STAT /* { */
    {
      FILE * statf = fopen("/proc/self/stat", "r");
      if(statf){
	int pid;
	char comm[1024];	/* unsafe scanf! :-D */
	char state;
	int ppid, pgrp, session,
	tty, tty_pgrp, flags,
	min_flt, cmin_flt, maj_flt, cmaj_flt,
	utime, stime, cutime, cstime,
	counter, priority, timeout,
	it_real_value,
	start_time,
	vsize, rss, rlim,
	start_code, end_code,
	start_stack, esp, eip,
	signal, blocked, sigign, sigcatch,
	wchan,
	nswap, cnswap,
	exit_signal, processor;

        fscanf(statf, "%d %s %c ""%d%d%d""%d%d%u""%u%u%u%u""%d%d%d%d""%d%d%u""%u""%d"
			"%u%u%u""%u%u""%u%u%u""%d%d%d%d""%u""%d%d""%d%d",
		&pid, comm, &state,
		&ppid, &pgrp, &session,
		&tty, &tty_pgrp, &flags,
		&min_flt, &cmin_flt, &maj_flt, &cmaj_flt,
		&utime, &stime, &cutime, &cstime,
		&counter, &priority, &timeout,	/* or priority/nice/NULL? */
		&it_real_value,
		&start_time,
		&vsize, &rss, &rlim,
		&start_code, &end_code,
		&start_stack, &esp, &eip,
		&signal, &blocked, &sigign, &sigcatch,
		&wchan,
		&nswap, &cnswap,
		&exit_signal, &processor);
        fprintf(statstream, "proc/stat: minflt = %u\n", min_flt);
        fprintf(statstream, "proc/stat: majflt = %u\n", maj_flt);
        fprintf(statstream, "proc/stat: vsize = %u\n", vsize);
        fprintf(statstream, "proc/stat: rss = %u\n", rss);
        fprintf(statstream, "proc/stat: rlim = %u\n", rlim);
        fprintf(statstream, "proc/stat: nswap = %d\n", nswap);
        fclose(statf);
      }
    }
#endif /* } TC_LINUX_PRINT_PROC_STAT */
  }
#endif /* } TC_PRINT_TIMINGS */

#ifdef TC_PRINT_STATS /* { */
  if(statstream){
    int i;
    for(i = 0; i < sc_max; ++i){
      fprintf(statstream, "Dynamic %s = %llu\n", statdesc[i], statcounter[i]);
    }
  }
  if(statstream){
    struct static_count_node * np;
    for(np = static_count_head; np; np = np->next)
      fprintf(statstream, "Static %s = %lu\n", np->descr, np->count);
  }
#endif /* } TC_PRINT_STATS */

  if(tc_flag_summarize && streams[_tc_error]){
    int nerr = print_error_summary(streams[_tc_error]);
    fprintf(streams[_tc_error], "Total errors = %d\n", nerr);
  }
}

/******************************************/
/* MAIN */
/******************************************/
#if !defined(STANDALONE) /* { */
int main(int argc, char * argv[], char * envp[])
{
  extern void callInitFunctions(void);
  extern int _prog_main(int argc, char * argv[], char * envp[]);
  char * argv_array_end;
  int tc_init_argv = 1;
  int tc_init_envp = 1;
  int tc_info = 0;
  int i;

  /* this points to the terminating null in the argv array */
  argv_array_end = argv[argc-1] + strlen(argv[argc-1]);

  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru0));
  TC_DO_TIMINGS(gettimeofday(&tv0, 0));

  /* typechecker output streams */
  /* if set to 0, nothing will be output */
  streams[_tc_stat] = stderr;
  streams[_tc_debug] = 0; /* debug messages are just meaninglessly annoying */
  streams[_tc_error] = stderr;

#ifndef TC_NOSIGNAL /* { */
  signal(TCSIG,SIG_IGN);
#endif /* } !TC_NOSIGNAL */

  /* register the cleanup function with atexit() */
  atexit(cleanup);

  /* intercept TC command-line arguments */
  for(i = 0; i < argc; ++i){
    if(!strncmp(argv[i],"-tc-",4)){
      int j;
      if(!strcmp(argv[i]+4,"sig")) tc_flag_signal = 1;
      else if(!strcmp(argv[i]+4,"nosig")) tc_flag_signal = 0;
      else if(!strcmp(argv[i]+4,"summarize")) tc_flag_summarize = 1;
      else if(!strcmp(argv[i]+4,"skip-vp")) tc_flag_skipvp = 1;
      else if(!strcmp(argv[i]+4,"skip-st")) tc_flag_skipst = 1;
      else if(!strcmp(argv[i]+4,"skip-ct")) tc_flag_skipct = 1;
      else if(!strcmp(argv[i]+4,"dont-die")) tc_dont_die = 1;
      else if(!strcmp(argv[i]+4,"no-argv")) tc_init_argv = 0;
      else if(!strcmp(argv[i]+4,"no-envp")) tc_init_envp = 0;
      else if(!strcmp(argv[i]+4,"info")) tc_info = 1;
      else if(!strncmp(argv[i]+4,"hashopt=",8)){
        tc_flag_hashopt_threshhold = atoi(argv[i]+12);
        fprintf(stderr, "TC: Setting hashopt threshhold to %d\n", tc_flag_hashopt_threshhold);
      } else if(!strncmp(argv[i]+4,"clear=",6)){
        if(!strcmp(argv[i]+10,"none"))
	  tc_flag_clear = _TC_NONE;
        else if(!strcmp(argv[i]+10,"segs"))
	  tc_flag_clear = _TC_SEGS_ONLY;
        else if(!strcmp(argv[i]+10,"scalar"))
	  tc_flag_clear = _TC_TO_SCALAR;
        else if(!strcmp(argv[i]+10,"aggr"))
	  tc_flag_clear = _TC_TO_AGGR;
        else if(!strcmp(argv[i]+10,"highest"))
	  tc_flag_clear = _TC_TO_HIGHEST;
        else fprintf(stderr, "TC: Unrecognized -tc- flag ignored (%s)\n", argv[i]);
      } else if(!strncmp(argv[i]+4,"logfile=",8)){
        FILE * lf;
	tc_flag_logfile = argv[i]+12;
        if((lf = fopen(tc_flag_logfile, "a"))){
          time_t now;
          char * tstr;
          int li;
          time(&now);
          tstr = ctime(&now);
          fprintf(lf, "%s: TCPTR Running :", tstr);
          for(li = 0; li < argc; ++li)
            fprintf(lf, " %s", argv[li]);
          fprintf(lf, "\n");
          fclose(lf);
          fprintf(stderr, "TC: Logging errors to file (%s)\n", tc_flag_logfile);
        } else {
          fprintf(stderr, "TC: Logfile (%s) append failed; no logging...\n", tc_flag_logfile);
	  tc_flag_logfile = 0;
        }
      } else if(!strncmp(argv[i]+4,"stream-",7)){
        enum _tc_stream stream_id = _tc_null;
	const char * stream_id_str = argv[i]+11;

	if(!strncmp(stream_id_str, "stat", 4)) stream_id = _tc_stat;
	else if(!strncmp(stream_id_str, "error", 5)) stream_id = _tc_error;
	else if(!strncmp(stream_id_str, "debug", 5)) stream_id = _tc_debug;
	else fprintf(stderr, "TC: Unrecognized stream in argument %s\n", argv[i]); 

	if(stream_id != _tc_null){
	  const char * fname = strchr(argv[i]+11, '=');
          if(!fname){
	    fprintf(stderr, "TC: Malformed stream argument %s\n", argv[i]);
	  } else {
	    fname++;
	    if(!strcmp(fname,"")){
	      streams[stream_id] = 0;
	      fprintf(stderr, "TC: Disabling stream (%s)\n", stream_id_str);
	    } else if(!strcmp(fname,"-")){
	      streams[stream_id] = stdout;
	      fprintf(stderr, "TC: Setting stream to stdout (%s)\n", stream_id_str);
	    } else if(!strcmp(fname,"=")){
	      streams[stream_id] = stderr;
	      fprintf(stderr, "TC: Setting stream to stderr (%s)\n", stream_id_str);
	    } else {
	      if((streams[stream_id] = fopen(fname, "w")))
	        fprintf(stderr, "TC: Redirecting stream to file %s (%s)\n", fname, stream_id_str);
	      else
	        fprintf(stderr, "TC: Error opening file %s for output (%s)\n", fname, stream_id_str);
	    }
	  }
        }
      } else fprintf(stderr, "TC: Unrecognized -tc- flag ignored (%s)\n", argv[i]);

      /* adjust remaining args */
      {
        int argvilen = strlen(argv[i])+1;

        /* 0. decrement array_end marker by argvilen, argc by one */
        argv_array_end -= argvilen;
        argc--;

        /* 1. copy argv bytes forward by argvilen */
        { char * c;
          for(c = argv[i]; c <= argv_array_end; ++c)
            c[0] = c[argvilen];
        }

        /* 2. subtract argvs by argvilen */
        { int j;
          for(j = i; j < argc; ++j) argv[j] = argv[j+1] - argvilen;
          argv[argc] = 0;
        }
        i--; /* decrement to process next item */
      }
    }
  }

  /* Initialize globals and externs */
  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru1));
  TC_DO_TIMINGS(gettimeofday(&tv1, 0));
  callInitFunctions();
  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru2));
  TC_DO_TIMINGS(gettimeofday(&tv2, 0));
  processExternVars();
  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru3));
  TC_DO_TIMINGS(gettimeofday(&tv3, 0));

#if !defined(TC_VULNERABLE_VP) /* { */
  if(tc_init_argv){
    int count_argv_bytes = 0;

    /* initialize argv */
    for(i = 0; i < argc; ++i){
      _tcptr_setTags(&argv[i],sizeof(char *));
      _tcptr_setTags(argv[i],strlen(argv[i])+1);
      count_argv_bytes += strlen(argv[i])+1;
    }

    /* (terminating null pointer) */
    _tcptr_setTags(&argv[i],sizeof(char *));

#ifdef TC_PRINT_STATS /* { */
    if(streams[_tc_stat]){
      fprintf(streams[_tc_stat] , "Init: %d argvs, %d bytes\n", argc, count_argv_bytes);
    }
#endif /* } TC_PRINT_STATS */
  }
#endif /* } !defined(TC_VULNERABLE_VP) */

#if !defined(TC_VULNERABLE_VP) /* { */
  if(tc_init_envp){
    int count_envp = 0;
    int count_envp_bytes = 0;

    /* initialize envp */
    for(i = 0; envp[i]; ++i){
      _tcptr_setTags(&envp[i],sizeof(char *));
      _tcptr_setTags(envp[i],strlen(envp[i])+1);
      count_envp_bytes += strlen(envp[i])+1;
    }
    count_envp = i;

    /* (terminating null pointer) */
    _tcptr_setTags(&envp[i],sizeof(char *));

#ifdef TC_PRINT_STATS /* { */
    if(streams[_tc_stat]){
      fprintf(streams[_tc_stat] , "Init: %d envps, %d bytes\n", count_envp, count_envp_bytes);
    }
#endif /* } TC_PRINT_STATS */
  }
#endif /* } !defined(TC_VULNERABLE_VP) */

  if(tc_info) return 1;

  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru4));
  TC_DO_TIMINGS(gettimeofday(&tv4, 0));
  return _prog_main(argc, argv, envp);
}

#else /* } if defined(STANDALONE) { */

/*********************************/
/* standalone test main function */
/*********************************/
int main()
{
  streams[_tc_error] = stderr;

  printf("Mirrorpage Numbytes = %d\n", MIRRORPAGE_NUMBYTES);
  printf("Mirrorpage Size = %d\n", MIRRORPAGE_SIZE);
  printf("Mirrorpage Zero Mask = 0x%08x\n", MIRRORPAGE_ZERO_MASK);
  printf("Mirrorpage Mask = 0x%08x\n", MIRRORPAGE_MASK);
  printf("Mirrormap Numbits = %d\n", MIRRORMAP_NUMBITS);
  printf("Mirrormap Numelements = %d\n", MIRRORMAP_NUMELEMENTS);

  /* setTags */
  fprintf(stderr, "SetTags test:\n");

#define SET_TAG_TEST(ADDR,SIZE) \
	fprintf(stderr, "%08x, %d: ", ADDR, SIZE); \
	_tcptr_setTags((unsigned char *) ADDR, SIZE); \
	_printTagStderr((unsigned char *) ADDR, SIZE+8);
  SET_TAG_TEST(0x0000, 0)
  SET_TAG_TEST(0x0100, 1)
  SET_TAG_TEST(0x0200, 7)
  SET_TAG_TEST(0x0300, 8)
  SET_TAG_TEST(0x0400, 9)
  SET_TAG_TEST(0x0500, 15)
  SET_TAG_TEST(0x0600, 16)
  SET_TAG_TEST(0x0700, 17)
  SET_TAG_TEST(0x0800, 23)
  SET_TAG_TEST(0x0900, 24)
  SET_TAG_TEST(0x0a00, 25)

  SET_TAG_TEST(0x1000, 7)
  SET_TAG_TEST(0x1101, 7)
  SET_TAG_TEST(0x1207, 7)
  SET_TAG_TEST(0x1308, 7)
  SET_TAG_TEST(0x1400, 8)
  SET_TAG_TEST(0x1501, 8)
  SET_TAG_TEST(0x1607, 8)
  SET_TAG_TEST(0x1708, 8)
  SET_TAG_TEST(0x1800, 9)
  SET_TAG_TEST(0x1901, 9)
  SET_TAG_TEST(0x1a07, 9)
  SET_TAG_TEST(0x1b08, 9)
  SET_TAG_TEST(0x1c00, 15)
  SET_TAG_TEST(0x1d01, 15)
  SET_TAG_TEST(0x1e07, 15)
  SET_TAG_TEST(0x1f08, 15)

  SET_TAG_TEST(0x000ffff1, 30)
  SET_TAG_TEST(0x001fffff, 2)
#undef SET_TAG_TEST

  /* clearTags */
  fprintf(stderr, "ClearTags test:\n");
#define CLEAR_TAG_TEST(ADDR,SIZE) \
	fprintf(stderr, "%08x, %d: ", ADDR, SIZE); \
	_tcptr_setTags((unsigned char *) ADDR-1, SIZE+2); \
	_tcptr_clearTags((unsigned char *) ADDR, SIZE); \
	_printTagStderr((unsigned char *) ADDR-1, SIZE+8);
  CLEAR_TAG_TEST(0x2000, 0)
  CLEAR_TAG_TEST(0x2100, 1)
  CLEAR_TAG_TEST(0x2200, 7)
  CLEAR_TAG_TEST(0x2300, 8)
  CLEAR_TAG_TEST(0x2400, 9)
  CLEAR_TAG_TEST(0x2500, 15)
  CLEAR_TAG_TEST(0x2600, 16)
  CLEAR_TAG_TEST(0x2700, 17)
  CLEAR_TAG_TEST(0x2800, 23)
  CLEAR_TAG_TEST(0x2900, 24)
  CLEAR_TAG_TEST(0x2a00, 25)

  CLEAR_TAG_TEST(0x3000, 7)
  CLEAR_TAG_TEST(0x3101, 7)
  CLEAR_TAG_TEST(0x3207, 7)
  CLEAR_TAG_TEST(0x3308, 7)
  CLEAR_TAG_TEST(0x3400, 8)
  CLEAR_TAG_TEST(0x3501, 8)
  CLEAR_TAG_TEST(0x3607, 8)
  CLEAR_TAG_TEST(0x3708, 8)
  CLEAR_TAG_TEST(0x3800, 9)
  CLEAR_TAG_TEST(0x3901, 9)
  CLEAR_TAG_TEST(0x3a07, 9)
  CLEAR_TAG_TEST(0x3b08, 9)
  CLEAR_TAG_TEST(0x3c00, 15)
  CLEAR_TAG_TEST(0x3d01, 15)
  CLEAR_TAG_TEST(0x3e07, 15)
  CLEAR_TAG_TEST(0x3f08, 15)
#undef CLEAR_TAG_TEST

  /* verifyPtr */
  fprintf(stderr, "VerifyPtr test:\n");
#define VERIFY_PTR_TEST(ADDR,SIZE) \
	fprintf(stderr, "%08x, %d:\n", ADDR, SIZE); \
	_tcptr_setTags((unsigned char *) ADDR, SIZE); \
	_tcptr_verifyPtr("",0,0,"fit",(unsigned char *) ADDR, SIZE); \
	_tcptr_verifyPtr("",0,0,"<1",(unsigned char *) ADDR-1, SIZE); \
	_tcptr_verifyPtr("",0,0,">1",(unsigned char *) ADDR, SIZE+1);

  VERIFY_PTR_TEST(0x4000, 0)
  VERIFY_PTR_TEST(0x4100, 1)
  VERIFY_PTR_TEST(0x4200, 7)
  VERIFY_PTR_TEST(0x4300, 8)
  VERIFY_PTR_TEST(0x4400, 9)
  VERIFY_PTR_TEST(0x4500, 15)
  VERIFY_PTR_TEST(0x4600, 16)
  VERIFY_PTR_TEST(0x4700, 17)
  VERIFY_PTR_TEST(0x4800, 23)
  VERIFY_PTR_TEST(0x4900, 24)
  VERIFY_PTR_TEST(0x4a00, 25)
#undef VERIFY_PTR_TEST



  return 0;
}

#endif /* defined(STANDALONE) */

