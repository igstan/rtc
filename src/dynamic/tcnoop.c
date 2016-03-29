#include <stdio.h>
#include <stdlib.h>  /* atexit */
#include "tcinternal.h"

/**********************/
/* compile time flags */
/**********************/
int strictPointer = 0;

/****************************************/
/* "private" API data members: useless? */
/****************************************/
/*
FILE * logstream = 0;
FILE * debugstream = 0;
FILE * warnstream = 0;
FILE * errorstream = 0;
*/

/*********************************************************/
/* "private" API functions: needed by tcmalloc, tcstring */
/*********************************************************/
void _printTagPos(FILE * f, _mirror_pos_t tagpos, size_t size)
{ }

int verifyAlloc(const void * addr, size_t size)
{ return 1; }

void overflowTagPos(_mirror_pos_t * tagpos, int offset)
{ }

_mirror_pos_t _getTagPos(const void * addr)
{
  static _mirror_pos_t dummy;
  return dummy;
}

/********************/
/* API data members */
/********************/
char _ini_static_rep[64];
int _int_static_rep[2];
char _char_static_rep[2];
short _short_static_rep[2];
long _long_static_rep[2];
long long _longlong_static_rep[2];
float _float_static_rep[2];
double _double_static_rep[2];
#ifdef LONGDOUBLE /* { */
long double _longdouble_static_rep[2];
#endif /* } LONGDOUBLE */
void * _pointer_static_rep[2];

_addr_and_size_t * _globalArgAddrs = 0;
int _globalArgCount = 0;
void * _globalCallTarget = 0;
void * _dummyAddr = 0;
char * _globalErrlocFile = "<none>";
int _globalErrlocLine = 0, _globalErrlocCol = 0;

/* for bitfields */
long long _dummyInt;

/************/
/* Counters */
/************/

static unsigned long count_all_calls = 0;
static unsigned long count_all_calls_overflow = 0;
static unsigned long count_all_bytes = 0;
static unsigned long count_all_bytes_overflow = 0;
#define COUNT_CALLS(nbytes) do { \
		if(!++count_all_calls) ++count_all_calls_overflow; \
		if(!++count_all_bytes) ++count_all_bytes_overflow; \
		count_all_bytes += nbytes - 1; \
	} while(0)

/****************************/
/* API function definitions */
/****************************/

void _registerExtern_nosize(const char * fname, int line, int col,
		void * addr, _ctype_t type /* , size_t size_aggr */)
{ }

void _verifyPtr(const char * fname, int line, int col, const char * exp,
		const void * addr, size_t size)
{
  COUNT_CALLS(size);
}

void _verifyTag(const char * fname, int line, int col, const char * exp,
		const void * addr, _ctype_t reftype)
{
  COUNT_CALLS(1);
}

void _setByteTags(const char * fname, int line, int col,
		const void * addr, size_t size, int set_tag, int clear_tag)
{
  COUNT_CALLS(size);
}

void _copyTag(const char * fname, int line, int col,
		const void * dst, const void * src, size_t size, _ctype_t type)
{
  COUNT_CALLS(size);
}

void _extern_registerVar(const char * fname, int line, int col,
		const char * varname, void * addr, size_t size)
{ }

void _extern_setUninitTag(const char * fname, int line, int col,
		void * addr, size_t size)
{ }

void _extern_setScalarTag(const char * fname, int line, int col,
		const void * addr, _ctype_t type)
{ }

void _extern_replicateTag(const char * fname, int line, int col,
		void * addr, size_t size, int nelem)
{ }

void _registerVar(const char * fname, int line, int col,
		const char * varname, void * addr, size_t size)
{ }

void _setScalarUninitTag(const char * fname, int line, int col,
		void * addr, _ctype_t type)
{
  COUNT_CALLS(1);
}

void _setScalarTagPtrToInt(const char * fname, int line, int col,
		const void * addr, _ctype_t type)
{
  COUNT_CALLS(1);
}

void _setScalarTag(const char * fname, int line, int col,
		const void * addr, _ctype_t type)
{
  COUNT_CALLS(1);
}

void _replicateTag(const char * fname, int line, int col,
		void * addr, size_t size, int nelem)
{
  COUNT_CALLS(size*nelem);
}

void _promoteTag(const char * fname, int line, int col,
		const void ** addrptr, _ctype_t opnd_type, _ctype_t expr_type,
		void * tmpspace)
{
  COUNT_CALLS(1);
}

void _tcdebug_processCall_func(const char * fnname)
{ }

void _processReturn(const char * fname, int line, int col,
                void * scaf_start, void * scaf_end, void * agrf_start, void * agrf_end,
                _addr_and_size_t ** aargaddrs, const void * addr, size_t size)
{
  COUNT_CALLS(size);
}

void _processArgTag(const char * fname, int line, int col,
		_addr_and_size_t * argaddrs,
		/*int argCount,*/ int index,
		void * addr, _ctype_t type, size_t size_aggr)
{
  COUNT_CALLS((size_aggr)?size_aggr:1);
}

/*****************************************/
/* report static instrumentation numbers */
/*****************************************/
struct static_count_node {
  const char * descr;
  unsigned long count;
  struct static_count_node * next;
} * static_count_head = 0;

void _reportStaticCounts(const char * fname, const char * descr, int count)
{
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
}

static FILE * statstream = 0;

/**********************************/
/* cleanup function, output stats */
/**********************************/
static void cleanup()
{
  if(statstream){
    struct static_count_node * np;
    for(np = static_count_head; np; np = np->next)
      fprintf(statstream, "Static %s = %lu\n", np->descr, np->count);

    fprintf(statstream, "Dynamic inst-calls: %lu + %lu * (%lu+1)\n",
                        count_all_calls,
                        count_all_calls_overflow,
                        (unsigned long)-1);
    fprintf(statstream, "Dynamic inst-call bytes: %lu + %lu * (%lu+1)\n",
                        count_all_bytes,
                        count_all_bytes_overflow,
                        (unsigned long)-1);
  }
}

/************************/
/* "real" main function */
/************************/

int main(int argc, char * argv[], char * envp[])
{
  extern void callInitFunctions(void);
  extern int _prog_main(int argc, char * argv[], char * envp[]);
  char * argv_array_end;
  int i;

  atexit(cleanup);

  /* this points to the terminating null in the argv array */
  argv_array_end = argv[argc-1] + strlen(argv[argc-1]);

  /* intercept TC command-line arguments */
  for(i = 0; i < argc; ++i){
    if(!strncmp(argv[i],"-tc-",4)){
      if(!strncmp(argv[i]+4,"stream-stat=",12)){
        const char * fname = argv[i]+16;
        if(!strcmp(fname,"")){
          statstream = 0;
          fprintf(stderr, "TC: Disabling statstream\n");
        } else if(!strcmp(fname,"-")){
          statstream = stdout;
          fprintf(stderr, "TC: Setting statstream to stdout\n");
        } else if(!strcmp(fname,"=")){
          statstream = stderr;
          fprintf(stderr, "TC: Setting statstream to stderr\n");
        } else {
          if((statstream = fopen(fname, "w")))
            fprintf(stderr, "TC: Redirecting statstream to file %s\n", fname);
          else
            fprintf(stderr, "TC: Error opening file %s for statstream output\n", fname);
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

  callInitFunctions();

  return _prog_main(argc, argv, envp);
}

