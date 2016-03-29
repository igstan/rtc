#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h> /* for getrusage timing stuff */

#define TC_DEFINE_MSGS
#include "tcinternal.h"
#undef TC_DEFINE_MSGS

/******************/
/* Run time flags */
/******************/

/* turn on/off sending of signal on error/warning */
static int signalflags[_tc_numstreams] = {0};

/* limit to this many error messages per stream (0 means no limit) */
/* when limit reached; count number of messages skipped */
static int output_limit[_tc_numstreams] = {0};
static int overflow_count[_tc_numstreams] = {0};

int tc_flag_summarize = 0; /* "summarize" errors and warnings, by line number,
				at end of execution; may not print on crash */

int tc_flag_terse = 0; /* "terse" output of errors and warnings */

int tc_flag_trackfree = 0; /* keep track of free'd memory for diagnostic lookup */

int tc_flag_vtfix = 0; /* fix tags on verifyTag? */

#define _TC_NONE	0
#define _TC_SEGS_ONLY	1
#define _TC_TO_SCALAR	2
#define _TC_TO_AGGR	3
#define _TC_TO_HIGHEST	4
int tc_flag_clear = _TC_SEGS_ONLY; /* on function return, different modes of
				      clearing the stack frame w.r.t. formals */

/*********************************/
/* Instrumentation Call counters */
/*********************************/
static unsigned long tot_dynamic_num_calls = 0;
static unsigned long tot_dynamic_num_calls_overflow = 0;

/****************************/
/* Macro statistic counters */
/****************************/
int  _vpctr = 0, _vpcctr = 0, _vpactr = 0, /* verifyPtr */
    _vpfctr = 0,_vpcfctr = 0,_vpafctr = 0;
int  _vtctr = 0, _vtcctr = 0, _vtactr = 0, /* verifyTag */
    _vtfctr = 0,_vtcfctr = 0,_vtafctr = 0;
int  _ctctr = 0, _ctcctr = 0, _ctactr = 0, /* copyTag */
    _ctfctr = 0,_ctcfctr = 0,_ctafctr = 0;
int  _sstctr = 0, _sstcctr = 0, /* setScalarTag */
    _sstfctr = 0,_sstcfctr = 0;
int  _ssutctr = 0, _ssutcctr = 0, /* setScalarUninitTag */
    _ssutfctr = 0,_ssutcfctr = 0;
int _patctr = 0, _patactr = 0, _patpctr = 0, /* processArgTag */
    _patfctr = 0,_patafctr = 0;

/**********************/
/* Compile time flags */
/**********************/
/* the values for these are or-ed by the initialization functions
   from the instrumented output. Each file in a given compilation
   should have used the same flags; otherwise the behavior can be
   a bit odd. */

int strictPointer = 0;

/********************************/
/* Paged mirror data structures */
/********************************/

/* the mirrormap lookup table */
void * mirrormap[MIRRORMAP_NUMELEMENTS] = {_typetag_unalloc};

/* for now, freelist assumes tags-per-byte ratio is 2.
   - to be more general introduces unnecessary complications? */
void * mirrormap_freelist = 0;

/******************/
/* Auxiliary data */
/******************/

const char * _typeDesc[] = {"UNA", "UNI", "INT",
                            "FLT", "PTR", "INI", "BIT"};

/******************************/
/* "private" API data members */
/******************************/

/* typechecker output streams */
/* these are initialized in main() */
static FILE * streams[_tc_numstreams] = {0};

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
#ifdef LONGDOUBLE
long double _longdouble_static_rep[2];
#endif /* LONGDOUBLE */
void * _pointer_static_rep[2];

_addr_and_size_t * _globalArgAddrs = 0;
int _globalArgCount = 0;
void * _globalCallTarget = 0;
void * _dummyAddr;
char * _globalErrlocFile = "<none>";
int _globalErrlocLine = 0, _globalErrlocCol = 0;

/* for bitfields */
long long _dummyInt;

/***********/
/* Globals */
/***********/
int _tc_init_mode = 0; /* set to true when initializing globals, and to false when program starts */

/***********************/
/* More auxiliary data */
/***********************/

_typeinfo_t typeinfo[] = {

/* note: ordering must coincide with enum _ctype
	 (which will be used to index into this table) */

  { 0, _typetag_unalloc, 0, 0, 0, _ctype_void_invalid, "void" },
  { &_int_static_rep[0], _typetag_int, sizeof(int), 0, 0,
						_ctype_int, "int" },
  { &_char_static_rep[0], _typetag_int, sizeof(char), 0, 0,
						_ctype_int, "char" },
  { &_short_static_rep[0], _typetag_int, sizeof(short), 0, 0,
						_ctype_int, "short" },
  { &_long_static_rep[0], _typetag_int, sizeof(long), 0, 0,
						_ctype_long, "long" },
  { &_longlong_static_rep[0], _typetag_int, sizeof(long long), 0, 0,
						_ctype_longlong, "long long" },
  { &_float_static_rep[0], _typetag_float, sizeof(float), 0, 0,
						_ctype_double, "float" },
  { &_double_static_rep[0], _typetag_float, sizeof(double), 0, 0,
						_ctype_double, "double" },
#ifdef LONGDOUBLE
  { &_longdouble_static_rep[0], _typetag_float, sizeof(long double), 0, 0,
					_ctype_longdouble, "long double" },
#else /* !LONGDOUBLE */
  /* This preserves correspondence between this table and enum ctype_t */
  { 0, _typetag_float, sizeof(long double), 0, 0,
					_ctype_void_invalid, "long double" },
#endif /* !LONGDOUBLE */
  { &_pointer_static_rep[0], _typetag_ptr, sizeof(void *), 0, 0,
						_ctype_pointer, "pointer" },
  { 0, _typetag_unalloc, 0, 0, 0, _ctype_aggregate, "aggregate" }
};

static void initStaticRep()
{
  int i;

  _setByteTags("<init static rep>",0,0, _ini_static_rep, sizeof(_ini_static_rep), _typetag_init, _typetag_unalloc);
  if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */

  for(i = 0; i < sizeof(typeinfo)/sizeof(typeinfo[0]); ++i) {

    if(typeinfo[i].staticrep_ptr){ /* skips void, aggregate */

      /* compute log of size */
      int j;
      unsigned long unum = (unsigned long) typeinfo[i].size;
      for(j = -1; unum; ++j)
          unum >>= 1;
      if(j == -1 || (1 << j != typeinfo[i].size)/* || (j&SIZE_BITS_MASK != j)*/)
	exit(fprintf(stderr,
		     "TC: Fatal error: incompatible scalar size (sizeof(%s) = %d)\n",
		     typeinfo[i].desc, typeinfo[i].size));
      typeinfo[i].logsize = j;

      typeinfo[i].tagbyte = ((CONT_BIT_MASK | j) << BITS_PER_TAG )
		| typeinfo[i].typetag;

      /* set the static tag */
      _setScalarTag("<init static rep>",0,0, typeinfo[i].staticrep_ptr, i);
      if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */
    }
  }
}

/*************************/
/* register extern stuff */
/*************************/

struct fnCallNode {
  enum { setUninitTag, setScalarTag, replicateTag } function;
  union {
    struct { const char * fname; int line; int col;
             void * addr; size_t size; } sut;
    struct { const char * fname; int line; int col;
             const void * addr; _ctype_t type; } sst;
    struct { const char * fname; int line; int col;
             void * addr; size_t size; int nelem; } rt;
  } args;
  struct fnCallNode * next;
} * externInitFns = 0,
 ** externInitFnsIter = &externInitFns;

struct externVarNode {
  /*--errloc--*/
  const char * fname;
  int line;
  int col;

  /*--variable info--*/
  const char * varname;  /* NOTE: not currently used */
  void * addr;
  _ctype_t type;
  struct fnCallNode * initfns;

  /*--next--*/
  struct externVarNode * next;
} * externVarList = 0;

/**********************************/
/* Auxiliary function definitions */
/**********************************/

/* executes the initialization functions encoded in list */
/* also, free memory used up by list */
static void executeFnCalls(struct fnCallNode * list)
{
  struct fnCallNode * iter = list;
  while(iter){
    struct fnCallNode * temp;
    switch(iter->function){
      case setUninitTag:
        _setUninitTag(	iter->args.sut.fname,
			iter->args.sut.line,
			iter->args.sut.col,
			iter->args.sut.addr,
			iter->args.sut.size);
        break;
      case setScalarTag:
        _setScalarTag(	iter->args.sst.fname,
			iter->args.sst.line,
			iter->args.sst.col,
			iter->args.sst.addr,
			iter->args.sst.type);
        break;
      case replicateTag:
        _replicateTag(	iter->args.rt.fname,
			iter->args.rt.line,
			iter->args.rt.col,
			iter->args.rt.addr,
			iter->args.rt.size,
			iter->args.rt.nelem);
        break;
    }
    temp = iter;
    iter = iter->next;
    free(temp);
  }
}

/************************************************/
/* GDB-friendly function for setting breakpoint */
/************************************************/

int _brk_msgid = 0;
int _brk_line = 0;

/* call with 0,0 to clear breakpoints */
void _tcSetBreak(int msgid, int line){
  /* remember signal flags */
  static int old_signalflags[_tc_numstreams] = {0};

  if(_brk_msgid == 0 && _brk_line == 0){
    int i;
    for(i = 0; i < _tc_numstreams; ++i){
      old_signalflags[i] = signalflags[i];
      signalflags[i] = 0;
    }
  } else if(msgid == 0 && line == 0){
    int i;
    for(i = 0; i < _tc_numstreams; ++i)
      signalflags[i] = old_signalflags[i];
  }

  /* output informative message */

  fprintf(stderr, "TC: Changing breakpoint from %d:%d to %d:%d\n",
		_brk_msgid, _brk_line, msgid, line);

  if(msgid == 0){
    fprintf(stderr, "\t(clear breakpoints)\n");
  } else if (msgid < MSGS_LAST){
    fprintf(stderr, "\t(%d-%s)\n", msgid, _msgs[msgid].msg);
  } else {
    fprintf(stderr, "\t(%d-invalid message id)\n", msgid);
  }

  /* update breakpoint */

  _brk_msgid = msgid;
  _brk_line  = line;
}

/***********************************************************************/
/* Output manager (more or less centralizes log/warning/error outputs) */
/***********************************************************************/

#ifdef TC_MULTILINE
#define IF_MULTILINE(x) x
#else
#define IF_MULTILINE(x)
#endif

/********************/
/* Output functions */
/********************/

void _print_simple(FILE * stream, int msgid)
{
  if(tc_flag_terse){
    fprintf(stream, "%d\n", msgid);
  } else {
    fprintf(stream, "%d-%s\n", msgid, _msgs[msgid].msg);
  }
}

void _print_flc(FILE * stream, const char * file, int line, int col,
		int msgid)
{
  if(tc_flag_terse){
    fprintf(stream, "%d[%s:%d.%d]\n", msgid, file,line,col);
  } else {
    fprintf(stream, "[%s:%d.%d] %d-%s\n", file, line, col, msgid, _msgs[msgid].msg);
  }
}

void _print_si(FILE * stream, const char * file, int line, int col,
		int msgid,
		const char *s1, const char *s2, const char *s3, const char *s4,
		int i1, int i2, int i3, int i4)
{
  if(tc_flag_terse){
    fprintf(stream, "%d[%s:%d.%d]\n", msgid, file,line,col);
  } else {
    fprintf(stream, "[%s:%d.%d] %d-%s ", file, line, col, msgid, _msgs[msgid].msg);
    switch(_msgs[msgid].sargs){
      case 4:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%s:%s:%s:%s)", _msgs[msgid].sdesc,s1,s2,s3,s4);
        break;
      case 3:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%s:%s:%s)", _msgs[msgid].sdesc, s1,s2,s3);
        break;
      case 2:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%s:%s)", _msgs[msgid].sdesc, s1,s2);
        break;
      case 1:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%s)", _msgs[msgid].sdesc, s1);
        break;
    }
    switch(_msgs[msgid].iargs){
      case 4:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%i:%i:%i:%i)", _msgs[msgid].idesc,i1,i2,i3,i4);
        break;
      case 3:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%i:%i:%i)", _msgs[msgid].idesc, i1,i2,i3);
        break;
      case 2:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%i:%i)", _msgs[msgid].idesc, i1,i2);
        break;
      case 1:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%i)", _msgs[msgid].idesc, i1);
        break;
    }
    fprintf(stream, "\n");
  }
}

void _print_full(FILE * stream, const char * file, int line, int col,
		int msgid,
		const char *s1,
		int i1, int i2, int i3,
		const void *a1, const void *a2,
		const void *t1, const void *t2, const void *t3)
{
  if(tc_flag_terse){
    fprintf(stream, "%d[%s:%d.%d]\n", msgid, file,line,col);
  } else {
    fprintf(stream, "[%s:%d.%d] %d-%s ", file, line, col, msgid, _msgs[msgid].msg);
    if(_msgs[msgid].sargs){
      IF_MULTILINE(fprintf(stream, "\n "));
      fprintf(stream, "(%s) = (%s)", _msgs[msgid].sdesc, s1);
    }
    switch(_msgs[msgid].iargs){
      case 3:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%i:%i:%i)", _msgs[msgid].idesc, i1,i2,i3);
        break;
      case 2:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%i:%i)", _msgs[msgid].idesc, i1,i2);
        break;
      case 1:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (%i)", _msgs[msgid].idesc, i1);
        break;
    }
    switch(_msgs[msgid].aargs){
      case 2:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (0x%x:0x%x)", _msgs[msgid].adesc, (unsigned int)a1,(unsigned int)a2);
        break;
      case 1:
        IF_MULTILINE(fprintf(stream, "\n "));
        fprintf(stream, "(%s) = (0x%x)", _msgs[msgid].adesc, (unsigned int)a1);
        break;
    }
    if(_msgs[msgid].targs){
      IF_MULTILINE(fprintf(stream, "\n "));
      fprintf(stream, "(%s) = (", _msgs[msgid].tdesc);
      if(_msgs[msgid].targs >= 1){
          _printTag(stream, t1, i1);
      }
      if(_msgs[msgid].targs >= 2){
          fprintf(stream, ")(");
          _printTag(stream, t2, i2);
      }
      if(_msgs[msgid].targs >= 3){
          fprintf(stream, ")(");
          _printTag(stream, t3, i3);
      }
      fprintf(stream, ")");
    }
    fprintf(stream, "\n");
  }
}

/**************************
 * summarize output stuff *
 **************************/

struct msg_entry {
  int msgid;
  int count;
  const char * file;
  int line, col;
  enum { _msg_simple, _msg_flc, _msg_si, _msg_full } mode;
  const char *s1, *s2, *s3, *s4;
  int i1, i2, i3, i4;
  const void *a1, *a2;
  void *t1, *t2, *t3;
  struct msg_entry * hnext; /* "hash next": for msg_store search */
  struct msg_entry * onext; /* "order next": for output ordering */
} * _msg_store[MSGS_LAST] = {0},
  * _msg_head = 0,
 ** _msg_tail = &_msg_head;

/* prints output to stat stream */
/* also frees _msg_entry nodes */
int _print_all_msgs()
{
  FILE * statstream = streams[_tc_stat];
  int msg_count = 0;
  while(_msg_head){
    struct msg_entry * t = _msg_head;
    _msg_head = _msg_head->onext;
    fprintf(statstream, "{%d}%s:", t->count, stream_descr[_msgs[t->msgid].str]);
    msg_count += t->count;
    switch(t->mode){
      case _msg_simple:
          _print_simple(statstream, t->msgid);
          break;
      case _msg_flc:
          _print_flc(statstream, t->file, t->line, t->col, t->msgid);
          break;
      case _msg_si:
          _print_si(statstream, t->file, t->line, t->col, t->msgid,
		t->s1, t->s2, t->s3, t->s4,
		t->i1, t->i2, t->i3, t->i4);
          break;
      case _msg_full:
          _print_full(statstream, t->file, t->line, t->col, t->msgid,
		t->s1, t->i1, t->i2, t->i3,
		t->a1, t->a2, t->t1, t->t2, t->t3);
          break;
    }
    if(t->t3) free(t->t3);
    if(t->t2) free(t->t2);
    if(t->t1) free(t->t1);
    free(t);
  }
  _msg_tail = &_msg_head;
  return msg_count;
}

static struct msg_entry * find(FILE * stream, int msgid, const char * file, int line, int col,
			int i1, int i2, int i3, const void * t1, const void * t2, const void * t3)
{
  struct msg_entry * n;

  for(n = _msg_store[msgid]; n; n = n->hnext){
    if(n->line == line && n->col == col &&
	(n->file == file || !strcmp(n->file, file))){
      char tb, ntb;
      switch(_msgs[msgid].targs){
        case 3: tb = GET_TAG_BYTE(t3);
		ntb = GET_TAG_BYTE(n->t3);
		if((i3 == 1)
		    ? (GET_CHAR_TAG(ntb, (unsigned long)(n->t3)&0x1)
			!= GET_CHAR_TAG(tb, (unsigned long)(t3)&0x1))
		    : (ntb != tb)) continue;
        case 2: tb = GET_TAG_BYTE(t2);
		ntb = GET_TAG_BYTE(n->t2);
		if((i2 == 1)
		    ? (GET_CHAR_TAG(ntb, (unsigned long)(n->t2)&0x1)
			!= GET_CHAR_TAG(tb, (unsigned long)(t2)&0x1))
		    : (ntb != tb)) continue;
        case 1: tb = GET_TAG_BYTE(t1);
		ntb = GET_TAG_BYTE(n->t1);
		if((i1 == 1)
		    ? (GET_CHAR_TAG(ntb, (unsigned long)(n->t1)&0x1)
			!= GET_CHAR_TAG(tb, (unsigned long)(t1)&0x1))
		    : (ntb != tb)) continue;
        case 0: return n;
      }
    }
  }
  return 0;
}

/********************/
/* Output functions */
/********************/

void _output_internal_error(const char * context, const char * msg)
{
  FILE * errstr = (streams[_tc_error])?(streams[_tc_error]):(stderr);
  fprintf(errstr, "TC INTERNAL ERROR (%s): %s\n", context, msg);
  fflush(errstr);
}

/* check whether to output message */
int check_limit(enum _tc_stream stream_id)
{
  if(output_limit[stream_id] >= 0){ /* limit is set */
    if(output_limit[stream_id] >= 1){ /* limit not reached: decrement */
      output_limit[stream_id]--;
      return 1;
    } else { /* limit reached: count */
      overflow_count[stream_id]++;
      return 0;
    }
  } else return 1; /* no limit */
}

void _output_simple(int msgid)
{
  FILE * stream = streams[_msgs[msgid].str];
  if(stream){
    if(tc_flag_summarize){
      struct msg_entry * n = find(stream, msgid, 0,0,0, 0,0,0, 0,0,0);
      if(n) n->count ++;
      else if(check_limit(_msgs[msgid].str)){
        /* print first time */
        _print_simple(stream, msgid);
        if(signalflags[_msgs[msgid].str]) TCKILL;
        else if(_brk_msgid == msgid) TCKILL;

        /* add entry to _msg_store */
        n = (struct msg_entry *)malloc(sizeof(struct msg_entry));
        if(!n){
          _output_internal_error("_output_simple", "Malloc out of memory");
          return;
        }
        n->msgid = msgid;
        n->count = 1;
        n->file = 0; n->line = 0; n->col = 0;
        n->mode = _msg_simple;
        n->s1 = 0; n->s2 = 0; n->s3 = 0; n->s4 = 0;
        n->i1 = 0; n->i2 = 0; n->i3 = 0; n->i4 = 0;
        n->a1 = 0; n->a2 = 0; n->t1 = 0; n->t2 = 0; n->t3 = 0;
        n->hnext = _msg_store[msgid];
        _msg_store[msgid] = n;
        n->onext = 0;
        *_msg_tail = n;
        _msg_tail = &n->onext;
      }
    } else if(check_limit(_msgs[msgid].str)){
      _print_simple(stream, msgid);
      if(signalflags[_msgs[msgid].str]) TCKILL;
      else if(_brk_msgid == msgid) TCKILL;
    }
  }
}

void _output_flc(const char * file, int line, int col,
		int msgid)
{
  FILE * stream = streams[_msgs[msgid].str];
  if(stream){
    if(tc_flag_summarize){
      struct msg_entry * n = find(stream, msgid, file,line,col, 0,0,0, 0,0,0);
      if(n) n->count ++;
      else if(check_limit(_msgs[msgid].str)){
        /* print first time */
        _print_flc(stream, file, line, col, msgid);
        if(signalflags[_msgs[msgid].str]) TCKILL;
        else if(_brk_msgid == msgid && _brk_line == line) TCKILL;

        /* add entry to _msg_store */
        n = (struct msg_entry *)malloc(sizeof(struct msg_entry));
        if(!n){
          _output_internal_error("_output_flc", "Malloc out of memory");
          return;
        }
        n->msgid = msgid;
        n->count = 1;
        n->file = file; n->line = line; n->col = col;
        n->mode = _msg_flc;
        n->s1 = 0; n->s2 = 0; n->s3 = 0; n->s4 = 0;
        n->i1 = 0; n->i2 = 0; n->i3 = 0; n->i4 = 0;
        n->a1 = 0; n->a2 = 0; n->t1 = 0; n->t2 = 0; n->t3 = 0;
        n->hnext = _msg_store[msgid];
        _msg_store[msgid] = n;
        n->onext = 0;
        *_msg_tail = n;
        _msg_tail = &n->onext;
      }
    } else if(check_limit(_msgs[msgid].str)){
      _print_flc(stream, file, line, col, msgid);
      if(signalflags[_msgs[msgid].str]) TCKILL;
      else if(_brk_msgid == msgid && _brk_line == line) TCKILL;
    }
  }
}

void _output_si(const char * file, int line, int col,
		int msgid,
		const char *s1, const char *s2, const char *s3, const char *s4,
		int i1, int i2, int i3, int i4)
{
  FILE * stream = streams[_msgs[msgid].str];
  if(stream){
    if(tc_flag_summarize){
      struct msg_entry * n = find(stream, msgid, file,line,col, 0,0,0, 0,0,0);
      if(n) n->count ++;
      else if(check_limit(_msgs[msgid].str)){
        /* print first time */
        _print_si(stream, file, line, col, msgid,
		s1,s2,s3,s4, i1,i2,i3,i4);
        if(signalflags[_msgs[msgid].str]) TCKILL;
        else if(_brk_msgid == msgid && _brk_line == line) TCKILL;

        /* add entry to _msg_store */
        n = (struct msg_entry *)malloc(sizeof(struct msg_entry));
        if(!n){
          _output_internal_error("_output_si", "Malloc out of memory");
          return;
        }
        n->msgid = msgid;
        n->count = 1;
        n->file = file; n->line = line; n->col = col;
        n->mode = _msg_si;
        n->s1 = s1;   n->i1 = i1;
        n->s2 = s2;   n->i2 = i2;
        n->s3 = s3;   n->i3 = i3;
        n->s4 = s4;   n->i4 = i4;
        n->a1 = 0; n->a2 = 0; n->t1 = 0; n->t2 = 0; n->t3 = 0;
        n->hnext = _msg_store[msgid];
        _msg_store[msgid] = n;
        n->onext = 0;
        *_msg_tail = n;
        _msg_tail = &n->onext;
      }
    } else if(check_limit(_msgs[msgid].str)){
      _print_si(stream, file, line, col, msgid,
		s1,s2,s3,s4, i1,i2,i3,i4);
      if(signalflags[_msgs[msgid].str]) TCKILL;
      else if(_brk_msgid == msgid && _brk_line == line) TCKILL;
    }
  }
}

void _output_full(const char * file, int line, int col,
		int msgid,
		const char *s1,
		int i1, int i2, int i3,
		const void *a1, const void *a2,
		const void *t1, const void *t2, const void *t3)
{
  FILE * stream = streams[_msgs[msgid].str];
  if(stream){
    if(tc_flag_summarize){
      struct msg_entry * n = find(stream, msgid, file,line,col, i1,i2,i3, t1,t2,t3);
      if(n) n->count ++;
      else if(check_limit(_msgs[msgid].str)){
        /* print first time */
        _print_full(stream, file, line, col, msgid,
		s1, i1,i2,i3, a1,a2, t1,t2,t3);
        if(signalflags[_msgs[msgid].str]) TCKILL;
        else if(_brk_msgid == msgid && _brk_line == line) TCKILL;

        /* add entry to _msg_store */
        n = (struct msg_entry *)malloc(sizeof(struct msg_entry));
        if(!n){
          _output_internal_error("_output_full", "Malloc out of memory");
          return;
        }
        n->msgid = msgid;
        n->count = 1;
        n->file = file; n->line = line; n->col = col;
        n->mode = _msg_full;
        n->s1 = s1;
        n->s2 = 0;
        n->s3 = 0;
        n->s4 = 0;
        n->i1 = i1; n->i2 = i2; n->i3 = i3;
        n->i4 = 0;
        n->a1 = a1; n->a2 = a2;
        n->t1 = 0; n->t2 = 0; n->t3 = 0;
        switch(_msgs[msgid].targs){
          case 3: n->t3 = (void *)malloc(i3+1);
                  _copyTagSilent(n->t3, t3, i3);
          case 2: n->t2 = (void *)malloc(i2+1);
                  _copyTagSilent(n->t2, t2, i2);
          case 1: n->t1 = (void *)malloc(i1+1);
                  _copyTagSilent(n->t1, t1, i1);
          case 0: ;
        }
        n->hnext = _msg_store[msgid];
        _msg_store[msgid] = n;
        n->onext = 0;
        *_msg_tail = n;
        _msg_tail = &n->onext;
      }
    } else if(check_limit(_msgs[msgid].str)){
      _print_full(stream, file, line, col, msgid,
		s1, i1,i2,i3, a1,a2, t1,t2,t3);
      if(signalflags[_msgs[msgid].str]) TCKILL;
      else if(_brk_msgid == msgid && _brk_line == line) TCKILL;
    }
  }
}

/*************************/
/* tagpos functions, etc */
/*************************/

/* temporary hack, to get the address associated with tagpos
   NOTE: does the reverse of _getTagPos */
static char * getAddrForPos(_mirror_pos_t tagpos)
{
  unsigned long i;
  void * mapaddr = (void *)((unsigned long)tagpos.ptr & ~(MIRRORPAGE_MASK>>1));
  unsigned long pageaddr = ((unsigned long)tagpos.ptr & (MIRRORPAGE_MASK>>1));

  for(i = 0; i < MIRRORMAP_NUMELEMENTS; ++i){
    if(mirrormap[i] == mapaddr){
      return (char *)(i << MIRRORPAGE_NUMBITS)
	+ pageaddr * 2
	+ tagpos.bit/BITS_PER_TAG;
    }
  }
  _output_internal_error("getAddForPos", "failed!");
  return 0;
}

void overflowTagPos(_mirror_pos_t * tagpos, int offset)
{
  char * tpaddr = getAddrForPos(*tagpos) + offset;
  GET_TAG_POS(*tagpos, tpaddr); //- remember, GET_TAG_POS is a macro, args cannot have side effect!!
}


/* used by "setScalarTag"
 * stray tags should be cleared to "_typetag_uninit"
 * and warn (only if tag wasn't uninit [or unalloc?] before)
 */
static void initTag(const char * fname, int line, int col,
		    void * addr, _ctype_t type)
{
  int i;
  /* Get the position of the tag for the address. */
  _mirror_pos_t tagpos;

  /* First nibble has tag 0xxx, where xxx is the typetag;
     second nibble is 1yyy, where yyy is the log of the size;
     third nibble is 0zzz, where zzz is unalloc typetag (really 0). */
  uchar first_nibble = typeinfo[type].typetag;
  uchar size_nibble = CONT_BIT_MASK | typeinfo[type].logsize;
  uchar subsequent_nibble = CONT_BIT_MASK | _typetag_unalloc;

  GET_TAG_POS(tagpos, addr);

  /* Clear off stray tags to the left */
  if(CONT_BIT(tagpos)){
    _mirror_pos_t iter = tagpos;
    int overwritten = 1;

    PREV_POS(iter);
    while(CONT_BIT(iter)){
      WRITE_TAG(iter,_typetag_uninit);
      PREV_POS(iter);
      overwritten++;
    }

    if(TAG_BITS(iter) != _typetag_uninit){
        _output_full(fname, line, col, SST_ONM,
		_typeDesc[TYPE_BITS(iter)],
		overwritten,0,0,   addr,0,   0,0,0);
    }

    WRITE_TAG(iter,_typetag_uninit);
  }

  /* write tags for initial byte */
  WRITE_TAG(tagpos, first_nibble);
  if (typeinfo[type].size > 1) {
    NEXT_POS(tagpos);
    WRITE_TAG(tagpos, size_nibble);
  }

  for(i = 2; i < typeinfo[type].size; ++i){
    /* write tag for subsequent bytes */
    NEXT_POS(tagpos);
    WRITE_TAG(tagpos, subsequent_nibble);
  }

  /* Clear off stray tags to the right */
  NEXT_POS(tagpos);
  while(CONT_BIT(tagpos)){
    WRITE_TAG(tagpos,_typetag_uninit)
    NEXT_POS(tagpos);
  }
}

/* If addr's tag is init, return 1 (and set to reftype if fix==1).
 * Otherwise, return 0;
 */
int _checkIfInit(const void * addr, _ctype_t reftype /* must be scalar */, int fix)
{
  _mirror_pos_t tpi;
  int i;

  GET_TAG_POS(tpi, addr);

  for(i = 0; i < typeinfo[reftype].size; ++i){
    if(TAG_BITS(tpi) != _typetag_init)
      return 0;
    NEXT_POS(tpi);
  }

  if(fix){
    _setScalarTag("(tc:_checkIfInit)", 0, 0, addr, reftype);
    if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */
  }

  return 1;
}

/* Equivalent to _verifyTag, except instead of reporting an error,
   it returns status (1 = verify succeeded, 0 = verify failed).
   - used by _promoteTag, _processArgTag/_processRetTag
   - not part of API
   - Note: reftype cannot be aggregate
*/
int _verifyTagSilent(const void * addr, _ctype_t reftype)
{
  _mirror_pos_t tpi;
  _mirror_pos_t stpi;

  GET_TAG_POS(tpi, addr);
  GET_TAG_POS(stpi, typeinfo[reftype].staticrep_ptr);

  /* Compare tags */
  while(TAG_BITS(stpi) != _typetag_unalloc){
    if(TAG_BITS(tpi) != TAG_BITS(stpi)
	&& !_checkIfInit(addr, reftype, 0))
      return 0; /* type mismatch */
    NEXT_POS(tpi);
    NEXT_POS(stpi);
  }

  /* Make sure tagpos has "ended" */
  if(CONT_BIT(tpi))
    return 0; /* type size mismatch */

  /* Verify succeeded, return 1 */
  return 1;
}

/* plain uncomplicated copyTag:
    - don't check alloc
    - don't complain
   but DO clear stray tags to left and right
 */
void _copyTagSilent(const void * dstaddr, const void * srcaddr, size_t size)
{
  int i;
  _mirror_pos_t dst;
  _mirror_pos_t src;

  GET_TAG_POS(dst, dstaddr);
  GET_TAG_POS(src, srcaddr);

  /* Clear off stray tags to the left */
  if(CONT_BIT(dst)){
    _mirror_pos_t iter = dst;
    PREV_POS(iter);
    while(CONT_BIT(iter)){
      /* AAL: unalloc or uninit? */
      WRITE_TAG(iter,_typetag_uninit);
      PREV_POS(iter);
    }
    WRITE_TAG(iter,_typetag_uninit);
  }

  /* Copy tags */
  for(i = 0; i < size; ++i){
    WRITE_TAG(dst,TAG_BITS(src))
    NEXT_POS(src);
    NEXT_POS(dst);
  }

  /* Clear off stray tags to the right */
  while(CONT_BIT(dst)){
    /* AAL: unalloc or uninit? */
    WRITE_TAG(dst,_typetag_uninit)
    NEXT_POS(dst);
  }
}

/* stuff used by verifyAlloc, copyTag, and possibly other functions;
   will eventually want to move to appropriate header file or something */
/* AAL: Macrofy! - after NEXT_POS must somehow PREV_POS! */
#if 0 /* { */
#define TAG_SIZE(pos) ( \
	CONT_BIT(pos) ? -1 : \
		(NEXT_POS(pos), CONT_BIT(pos) ? (1 << TYPE_BITS(pos)) : 1) \
	)
#endif /* } 0 */
int TAG_SIZE(_mirror_pos_t pos)
{
  /* If the CONT_BIT is set on pos, then we're not even at the start
     of a tag.  Return -1.
     If the CONT_BIT is set on the next pos, return the size as encoded
     in the type bits for the next pos.  Otherwise, it is 1. */
  return CONT_BIT(pos) ? -1 :
    (NEXT_POS(pos), CONT_BIT(pos) ? (1 << TYPE_BITS(pos)) : 1);
}

/* verify that memory at tagpos, for size size, is not _typetag_unalloc.
 */
int verifyAlloc(const void * addr, size_t size)
{
  _mirror_pos_t tp;
  _mirror_pos_t tpi;
  int i;

  if(!mirrormap[MIRRORMAP_INDEX(addr)])
    return 0; /* unallocated page */

  GET_TAG_POS(tp, addr);
  tpi = tp;

  if (TAG_BITS(tp) == _typetag_unalloc)
    return 0; /* unallocated memory */
  else if (!CONT_BIT(tp) && /* optimize for scalars: */
	   (NEXT_POS(tp), CONT_BIT(tp) && (1 << TYPE_BITS(tp) >= size)))
    return 1;
  else {
    for(i = 0; i < size; ++i){
      if(TAG_BITS(tpi) == _typetag_unalloc)
	return 0; /* unallocated memory */
      NEXT_POS(tpi);
    }
  }

  /* Verify succeeded, return 1 */
  return 1;
}

/* called by "our" main() after calling the various init
   functions, to process the collected externVarList */
void processExternVars(void)
{
  struct externVarNode * eviter;

  eviter = externVarList;
  while(eviter){
    _mirror_pos_t tagpos;
    struct externVarNode * evcurr = eviter;

    eviter = eviter->next;

    /*--see if tagpos is already initialized */
    GET_TAG_POS(tagpos, evcurr->addr);
    if(TAG_BITS(tagpos) == _typetag_unalloc){
      /*--tagpos has not been allocated; initialize*/
      if(evcurr->type == _ctype_aggregate){
        /* Aggregate type: call initfns */
        if(evcurr->initfns){
          /* note: executeFnCalls will free fnCallNode memory */
          executeFnCalls(evcurr->initfns);
        } else {
          /* Incomplete type: warn? log for now */
	  /* This message is kinda useless; temporarily set aside */
          _output_flc(evcurr->fname, evcurr->line, evcurr->col, PEV_IT);
        }
      } else {
        /* Scalar type: just initialize the tag */
        _setScalarTag(evcurr->fname, evcurr->line, evcurr->col,
			evcurr->addr, evcurr->type);
      }
    } else {
      /*--tagpos has been initialized */
      /*  TODO: may want to verify; do nothing for now */
    }

    free(evcurr);
  }
}

/**************************************/
/* "private" API function definitions */
/**************************************/

/* Summarize if size is bigger than, say, 50, and limit to output of 50 "items" */
void _printTagPos(FILE * f, _mirror_pos_t tagpos, size_t size)
{
  if(size == 0){
    fprintf(f, "<void>");
  } else if(size > 50) { /* summarize */
    int output_count = 0;

    char memo_type = 0;
    int  memo_size = 0;
    char temp_type = 0;
    char temp_size = 0;
    int  memo_count = 0;

    int i = 0;

    for(; i < size; ++i){ /* left-strays */
      if(!CONT_BIT(tagpos)) break;
      else fprintf(f, ":...");
      NEXT_POS(tagpos);
    }
    temp_type = TYPE_BITS(tagpos);
    NEXT_POS(tagpos);
    i++;
    temp_size = 1;
    for(; i < size; ++i){ /* first tag */
      if(!CONT_BIT(tagpos)) break;
      else temp_size++;
      NEXT_POS(tagpos);
    }
    for(; i < size+1; ++i){ /* main loop - go one more step */
      if(!CONT_BIT(tagpos)){
        if(temp_type == memo_type && temp_size == memo_size){
          memo_count++;
        } else {
          if(memo_count){
            if(memo_count == 1)
              fprintf(f, "<%s,%d>", _typeDesc[memo_type], memo_size);
            else
              fprintf(f, "(<%s,%d>x%d)", _typeDesc[memo_type], memo_size, memo_count);
            if(++output_count >= 50){
              fprintf(f, "...remaining %d bytes suppressed...", size-i-temp_size);
              return;
            }
          }
          memo_type = temp_type;
          memo_size = temp_size;
          memo_count = 1;
        }
        temp_type = TYPE_BITS(tagpos);
        temp_size = 1;
      } else {
        temp_size++;
      }
      NEXT_POS(tagpos);
    }
    if(memo_count == 1)
      fprintf(f, "<%s,%d>", _typeDesc[memo_type], memo_size);
    else
      fprintf(f, "(<%s,%d>x%d)", _typeDesc[memo_type], memo_size, memo_count);
    /* unfinished business */
    PREV_POS(tagpos);
    if(CONT_BIT(tagpos)){
      fprintf(f, "<%s,%d+...>", _typeDesc[temp_type], temp_size-1);
    }
  } else {
    int i;
    const char * type_desc = "...";
    for(i = 0; i < size; ++i){
      fprintf(f, "%c%s", CONT_BIT(tagpos)?':':'<',
	      CONT_BIT(tagpos) ? type_desc : (type_desc = _typeDesc[TYPE_BITS(tagpos)]));
      NEXT_POS(tagpos);
    }
    fprintf(f, "%s", CONT_BIT(tagpos)?":...":">");
  }
}

void _printTag(FILE * f, const void * addr, size_t size)
{
  _printTagPos(f, _getTagPos(addr), size);
}

void _printTagStdout(void * addr, size_t size)
{
  _printTagPos(stdout, _getTagPos(addr), size);
}

void _printTagStderr(void * addr, size_t size)
{
  _printTagPos(stderr, _getTagPos(addr), size);
  fprintf(stderr, "\n");
}

/**********************************************/
/* Wrappers for macros - useful for debugging */
/**********************************************/

_mirror_pos_t _getTagPos(const void * addr)
{
  _mirror_pos_t ret;

  GET_TAG_POS(ret,addr);
/*
  unsigned long mapindex = MIRRORMAP_INDEX(addr);
  unsigned long pageindex = MIRRORPAGE_INDEX(addr);

  if(!mirrormap[mapindex])
    _touchMirrorPage(mapindex);

  ret.ptr = (unsigned char *)mirrormap[mapindex] + pageindex/2;
  ret.bit = pageindex%2 * BITS_PER_TAG;
*/
  return ret;
}

void _printConstsStderr()
{
#define EXPAND_PRINT_STMT(CID) fprintf(stderr, #CID " = %d (0x%08x)\n", CID, CID)
  EXPAND_PRINT_STMT(MIRRORPAGE_NUMBITS);
  EXPAND_PRINT_STMT(MIRRORPAGE_NUMBYTES);
  EXPAND_PRINT_STMT(MIRRORPAGE_MASK);
  EXPAND_PRINT_STMT(MIRRORMAP_NUMBITS);
  EXPAND_PRINT_STMT(MIRRORMAP_NUMELEMENTS);
  EXPAND_PRINT_STMT(BITS_PER_TAG);
  EXPAND_PRINT_STMT(LOG_BITS_PER_TAG);
  EXPAND_PRINT_STMT(CONT_BIT_MASK);
  EXPAND_PRINT_STMT(TYPE_BITS_MASK);
  EXPAND_PRINT_STMT(TAG_BITS_MASK);
  EXPAND_PRINT_STMT(EVEN_TAG_MASK);
  EXPAND_PRINT_STMT(EVEN_TYPE_MASK);
  EXPAND_PRINT_STMT(EVEN_CONT_MASK);
  EXPAND_PRINT_STMT(ODD_TAG_MASK);
  EXPAND_PRINT_STMT(ODD_TYPE_MASK);
  EXPAND_PRINT_STMT(ODD_CONT_MASK);
#undef EXPAND_PRINT_STMT
}

void _tcfn_SET_UNALLOC_TAG(const void * start_addr, const void * end_addr)
	{ SET_UNALLOC_TAG("_tcfn_SET_UNALLOC_TAG",__FILE__,__LINE__,0, start_addr, end_addr); }
const void * _tcfn_MIRRORMAP_INDEX(const void * addr) { return (const void *) MIRRORMAP_INDEX(addr); }
const void * _tcfn_MIRRORPAGE_INDEX(const void * addr) { return (const void *) MIRRORPAGE_INDEX(addr); }
int _tcfn_SAME_MIRROR_PAGE(const void * addr1, const void * addr2) { return SAME_MIRROR_PAGE(addr1, addr2); }
unsigned char _tcfn_TAG_BITS(_mirror_pos_t pos) { return TAG_BITS(pos); }
unsigned char _tcfn_CONT_BIT(_mirror_pos_t pos) { return CONT_BIT(pos); }
unsigned char _tcfn_TYPE_BITS(_mirror_pos_t pos) { return TYPE_BITS(pos); }
void _tcfn_WRITE_TAG(_mirror_pos_t * posptr, unsigned char tag) { WRITE_TAG(*posptr,tag); }
_mirror_pos_t _tcfn_get_NEXT_POS(_mirror_pos_t pos) { NEXT_POS(pos); return pos; }
_mirror_pos_t _tcfn_get_PREV_POS(_mirror_pos_t pos) { PREV_POS(pos); return pos; }
unsigned char _tcfn_ODD_TAG_BITS(unsigned char byte) { return ODD_TAG_BITS(byte); }
unsigned char _tcfn_ODD_TYPE_BITS(unsigned char byte) { return ODD_TYPE_BITS(byte); }
unsigned char _tcfn_EVEN_TAG_BITS(unsigned char byte) { return EVEN_TAG_BITS(byte); }
unsigned char _tcfn_EVEN_TYPE_BITS(unsigned char byte) { return EVEN_TYPE_BITS(byte); }
unsigned char _tcfn_GET_TAG_BYTE(const void * addr) { return GET_TAG_BYTE(addr); }
const void * _tcfn_GET_TAG_BYTE_addr(const void * addr) { return &GET_TAG_BYTE(addr); }
unsigned char _tcfn_GET_CHAR_TAG(unsigned char byte, int isOdd) { return GET_CHAR_TAG(byte, isOdd); }
void _tcfn_SET_CHAR_TAG(unsigned char * lhsptr, unsigned char * rhsptr, int isOdd)
	{ SET_CHAR_TAG(*lhsptr, *rhsptr, isOdd); }

/****************************/
/* API function definitions */
/****************************/

/* function version of SET_UNALLOC_TAG macro */
static void _fn_SET_UNALLOC_TAG(const char * caller, const char * fname, int line, int col,
			const void * start_addr, const void * end_addr)
{ SET_UNALLOC_TAG(caller,fname,line,col, start_addr, end_addr); }

/* NOTE: (type == _ctype_aggregate && externInitFns == 0) means
	 type is incomplete, so don't initialize (?) */
void _registerExtern_nosize(const char * fname, int line, int col,
		void * addr, _ctype_t type)
{
  struct externVarNode * evcurr;
  struct externVarNode ** eviter;

  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

  /*--FIRST: look for addr in externVarList */
  eviter = &externVarList;
  while(*eviter && (*eviter)->addr < addr)
    eviter = &(*eviter)->next;

  evcurr = *eviter;

  if(evcurr && evcurr->addr == addr){

    /*--addr found: check for matching types*/
    if(evcurr->type != type){

      /*--type mismatch; warn */
      _output_si(fname, line, col,
		RE_PDEF, 
		fname, evcurr->fname,
		typeinfo[type].desc, typeinfo[evcurr->type].desc,
		line, evcurr->line,
		col, evcurr->col);

    } else if (type == _ctype_aggregate){

      /*--if type matches and is aggregate */

      /*--if new version is complete, and old version is incomplete,
	  adopt new version */
      if(externInitFns && !evcurr->initfns){

        evcurr->fname = fname;
        evcurr->line = line;
        evcurr->col = col;
        evcurr->addr = addr;
        evcurr->type = type;
        evcurr->initfns = externInitFns;
        externInitFns = 0;
        externInitFnsIter = &externInitFns;

      } else if(externInitFns && evcurr->initfns){

        /*--we have two complete aggregate initfns; compare them */
        /* TODO */

      }
    }

    /*--discard init functions (if any) */
    while(externInitFns){
      struct fnCallNode * temp = externInitFns;
      externInitFns = externInitFns->next;
      free(temp);
    }
    externInitFnsIter = &externInitFns;

  } else {

    /*--addr not found; insert info */
    (*eviter) = (struct externVarNode *)malloc(sizeof(struct externVarNode));
    (*eviter)->fname = fname;
    (*eviter)->line = line;
    (*eviter)->col = col;
    (*eviter)->addr = addr;
    (*eviter)->type = type;
    (*eviter)->initfns = externInitFns;
    (*eviter)->next = evcurr;

    /*--reset externInitFns */
    externInitFns = 0;
    externInitFnsIter = &externInitFns;
  }
}

void _touchMirrorPage(unsigned long mapindex)
{
    if(mirrormap_freelist){ /*see comment about freelist above*/
      mirrormap[mapindex] = mirrormap_freelist;
      mirrormap_freelist = 0;
    } else {
      mirrormap[mapindex] = (void *) memalign(MIRRORPAGE_NUMBYTES, MIRRORPAGE_NUMBYTES);
      if(!mirrormap[mapindex])
        exit((_output_internal_error("_touchMirrorPage/FATAL", "memalign out of memory"),-1));
      mirrormap_freelist = (char *)mirrormap[mapindex] + MIRRORPAGE_NUMBYTES/2;
    }
    /* initialize mirror to 0 (_typetag_unalloc) */
    memset(mirrormap[mapindex], _typetag_unalloc, MIRRORPAGE_NUMBYTES/2);
}

/* dereference addr and see if it is allocated (for up to size bytes)
 */
void _verifyPtr(const char * fname, int line, int col, const char * exp,
		const void * addr, size_t size)
{
  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

  /* for now a function call, optimize later when stable */
  if(!verifyAlloc(addr, size)){
    _output_full(fname, line, col, VP_UNA,
		exp, 0,0,0, addr,0, 0,0,0);
	/* prepare to die!  Could return another value from
	   verifyAlloc to say that there is no allocated page
	   in the region.  Could try to exit instead in some cases? */
  }
}

/* Note: we don't fully verify aggregate types;
	 to implement this in the future, we'd need
	 to pass in more info (e.g. size_aggr) */
void _verifyTag(const char * fname, int line, int col, const char * exp,
		const void * addr, _ctype_t reftype /*, size_t size_aggr */)
{
  _mirror_pos_t tpi, stpi;
  _mirror_pos_t staticpos; /* formerly a param */
  _mirror_pos_t tagpos;
  int size;

  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

  /* verifying aggregate: currently just verifyPointer on first byte */
  if(reftype == _ctype_aggregate){
    _verifyPtr(fname,line,col, exp, addr, 1 /* size_aggr */);
    if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */
    return;
  }

  GET_TAG_POS(tagpos, addr);
  GET_TAG_POS(staticpos, typeinfo[reftype].staticrep_ptr);

  size = typeinfo[reftype].size;

  /* Compare tags */
  tpi = tagpos;
  stpi = staticpos;

  if (size > 1) {
    /* AAL: assert(!tpi.bit && !stpi.bit); */
    /* Compare the first bytes of the tags.  The first
       byte of a tag contains both the type and the size. */
    if (*(char *)tpi.ptr != *(char *)stpi.ptr
	&& !_checkIfInit(addr, reftype, 1)) { /*SY: do we really need to fix??*/

      _output_full(fname, line, col, VT_TMM,
		exp, size,size,0, addr,0,
		typeinfo[reftype].staticrep_ptr,addr,0);

      /* if vtfix, or if destination is unallocated,
         suppress future messages by setting to declared type */
      /* note: should check for all = unalloc, instead of any = unalloc? */
      if(tc_flag_vtfix || (!verifyAlloc(addr,size))){
        _copyTagSilent(addr,typeinfo[reftype].staticrep_ptr,size);
      }

      return;
    }
  }
  else { /* A single byte. */
    if(TAG_BITS(tpi) != TAG_BITS(stpi)
	&& !_checkIfInit(addr, reftype, 1)) { /* could optimize by inlining? */ /*SY: do we really need to fix??*/
      /* type mismatch; verify fails */

      _output_full(fname, line, col, VT_TMM,
		exp, size,size,0, addr,0,
		typeinfo[reftype].staticrep_ptr,addr,0);

      /* if destination is unallocated,
         suppress future messages by setting to declared type */
      if(tc_flag_vtfix || (TAG_BITS(tpi) == _typetag_unalloc)){
        _copyTagSilent(addr,typeinfo[reftype].staticrep_ptr,size);
      }

      return;
    }

    NEXT_POS(tpi);
    /* Make sure tagpos has "ended" */
    if(CONT_BIT(tpi)){
      /* tagpos is longer than staticpos */
      _output_full(fname, line, col, VT_TSM,
		exp, size,2*size,0, addr,0, 
		typeinfo[reftype].staticrep_ptr,addr,0);
    }
  }
}

#if !defined(TC_CHECK_SET_UNALLOC) /* { */

#define SBT_NEM_TEST(cond,posn,size,addr)

#else /* } if defined(TC_CHECK_SET_UNALLOC) { */
/* if setByteTags overwrites non-zero (non-Unalloc) memory, issue SBT_NEM warning */

static int _mc; /* used by SBT_NEM_TEST conditionals */

/* _tc_init_mode prevents check for multiple definitions of same location */
/* clear_tag is an argument to setByteTags that indicates whether NEM-check should be done */
#define SBT_NEM_TEST(cond,posn,size,addr) \
	if(!clear_tag && !_tc_init_mode && cond) { \
	  _output_full(fname,line,col, SBT_NEM, 0, size,posn,0, addr,0, addr,0,0); \
	}

/* check a block of (mirror) memory for non-zero (non-Unalloc) memory. */
static int memcheck(void * saddr, size_t n)
{
  if(n <= sizeof(int)){
    char * cp = (char *) saddr;
    int i;
    for(i = 0; i < n; ++i)
      if(cp[i]) return i*2+1;
  } else {
    /* walk up to first int boundary */
    char * cp = (char *) saddr;
    while(((unsigned long)cp) % sizeof(int)){
      if(*cp) return (cp-(char *)saddr)*2+1;
      cp++;
      n--;
    }

    /* check last bytes up to int boundary */
    while(n % sizeof(int))
      if(cp[--n]) return (cp-(char *)saddr)*2+1;

    /* walk in int-sized strides; could go bigger (double-sized strides)? */
    {
      int * ip = (int *) cp;
      int * ep = (int *) (cp + n);
      while(ip < ep){
        if(*ip) return (((char *)ip)-(char *)saddr)*2+1;
        ip++;
      }
    }
  }
  return 0;
}

#endif /* } defined(TC_CHECK_SET_UNALLOC) */

void _setByteTags(const char * fname, int line, int col,
		const void * addr, size_t size, int set_tag, int clear_tag)
{
  _mirror_pos_t spos, epos;
  char * saddr = (char *)addr;
  char * eaddr = (char *)addr + size;
  unsigned char set_tag_pair = set_tag|(set_tag<<BITS_PER_TAG);

  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

  if(saddr != eaddr) {
    /* Check stray tags to left */
    GET_TAG_POS(spos, saddr);
    if(CONT_BIT(spos)){
      _mirror_pos_t tmp;

      _output_flc(fname,line,col, SBT_STB);

      tmp = spos;
      PREV_POS(tmp);
      while(CONT_BIT(tmp)){
        WRITE_TAG(tmp,clear_tag);
        PREV_POS(tmp);
      }
      WRITE_TAG(tmp,_typetag_unalloc);
    }

    /* Align saddr to even address */
    if((unsigned long)saddr & 0x1){
      SBT_NEM_TEST(TAG_BITS(spos),0,size,addr);
      WRITE_TAG(spos,set_tag);
      saddr++;
    }

    /* Check stray tags to right */
    GET_TAG_POS(epos, eaddr);
    if(CONT_BIT(epos)){
      _mirror_pos_t tmp;

      _output_flc(fname,line,col, SBT_STA);

      tmp = epos;
      while(CONT_BIT(tmp)){
        WRITE_TAG(tmp,clear_tag)
        NEXT_POS(tmp);
      }
    }
    /* Align eaddr to even address */
    if((unsigned long)eaddr & 0x1){
      --eaddr;
      GET_TAG_POS(epos, eaddr); /* note: eaddr is (char *) */
      SBT_NEM_TEST(TAG_BITS(epos),size,size,addr);
      WRITE_TAG(epos,set_tag);
    }

    if(SAME_MIRROR_PAGE(saddr,eaddr-1)){
      SBT_NEM_TEST((_mc = memcheck(&GET_TAG_BYTE(saddr), (eaddr-saddr)/2)),
			_mc + (saddr-(char *)addr),size,addr);
      memset(&GET_TAG_BYTE(saddr), set_tag_pair, (eaddr-saddr)/2);
    } else {
      int sindex = MIRRORMAP_INDEX(saddr);
      int eindex = MIRRORMAP_INDEX(eaddr-1);
      SBT_NEM_TEST((_mc = memcheck(&GET_TAG_BYTE(saddr),
		(MIRRORPAGE_NUMBYTES - MIRRORPAGE_INDEX(saddr))/2)),
		_mc + (saddr-(char *)addr),size,addr);
      memset(&GET_TAG_BYTE(saddr), set_tag_pair,
		(MIRRORPAGE_NUMBYTES - MIRRORPAGE_INDEX(saddr))/2);
      for(++sindex; sindex < eindex; sindex++){
        SBT_NEM_TEST((_mc = memcheck(&GET_TAG_BYTE((char *)(sindex << MIRRORPAGE_NUMBITS)),
		MIRRORPAGE_NUMBYTES/2)),_mc + (saddr-(char *)addr),size,addr);
        memset(&GET_TAG_BYTE((char *)(sindex << MIRRORPAGE_NUMBITS)),
		set_tag_pair, MIRRORPAGE_NUMBYTES/2);
      }
      SBT_NEM_TEST((_mc = memcheck(&GET_TAG_BYTE((char *)(sindex << MIRRORPAGE_NUMBITS)),
		MIRRORPAGE_INDEX(eaddr)/2)),_mc + (saddr-(char *)addr),size,addr);
      memset(&GET_TAG_BYTE((char *)(sindex << MIRRORPAGE_NUMBITS)),
		set_tag_pair, MIRRORPAGE_INDEX(eaddr)/2);
    }
  }
}

#if 0 /* { */
#ifdef TC_LOG_DIRTY_COPY /* { */
#define COPYTAG_TYPE_MISMATCH(dst,src) \
( \
  TYPE_BITS(dst) != _typetag_uninit && \
  (( \
     strictPointer && \
     !CONT_BIT(src) && /* AAL: unnecessary now? */ \
     (( \
        TAG_SIZE(src) != TAG_SIZE(dst) \
      )||( \
        TAG_BITS(src) != TAG_BITS(dst) && \
        !(TYPE_BITS(src) == _typetag_int && TYPE_BITS(dst) == _typetag_ptr) && \
        !(TYPE_BITS(src) == _typetag_ptr && TYPE_BITS(dst) == _typetag_int) \
     )) \
   )||( \
     !strictPointer && \
     TAG_BITS(src) != TAG_BITS(dst) \
  )) \
)
#endif /* } TC_LOG_DIRTY_COPY */
#endif /* } if 0 */

/* stray tags are cleared to "uninit", since we're merely corrupting
 * it, not unallocating it.
 */
void _copyTag(const char * fname, int line, int col,
		const void * dstaddr, const void * srcaddr,
		size_t size, _ctype_t type)
{
  int log_written = 0, warn_written = 0;
  int i;
  _mirror_pos_t dst;
  _mirror_pos_t src;

  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

  GET_TAG_POS(dst, dstaddr);
  GET_TAG_POS(src, srcaddr);

  if(!verifyAlloc(srcaddr, size)){
    /* copying from unallocated memory! */
    _output_full(fname,line,col, CT_CFU,
		0, 0,0,0, srcaddr,dstaddr, 0,0,0);
    
    if(type != _ctype_aggregate){
      /* if static type is scalar, we can continue */

      GET_TAG_POS(src, typeinfo[type].staticrep_ptr);

    } else {
      /* if static type is aggregate, do nothing */

      if(!verifyAlloc(dstaddr, size)){
        /* copying into unallocated memory! */
        _output_flc(fname,line,col, CT_AIU);
      }
      return;
    }
  }

  if(!verifyAlloc(dstaddr, size)){
    /* copying into unallocated memory! */
    _output_full(fname,line,col, CT_CIU,
		0, 0,0,0, srcaddr,dstaddr, 0,0,0);
  }

  /* -- Check src against static_type and warn on mismatch */
  if(streams[_msgs[CT_TMM].str]
	&& (type != _ctype_aggregate)
	&& ((TAG_SIZE(src) != typeinfo[type].size)
	    || (TAG_BITS(src) != typeinfo[type].typetag))
	&& !_checkIfInit(srcaddr, type, 0)){
    /* WARN mismatch */ 
    _output_full(fname,line,col, CT_TMM,
		0, size,size,size, srcaddr,dstaddr,
		typeinfo[type].staticrep_ptr,srcaddr,dstaddr);
    log_written = 1;
  }

#ifdef TC_LOG_DIRTY_COPY /* { */
  /* -- Check dst against static_type and warn on mismatch */
  if(streams[_msgs[CT_TDM].str]
	&& (type != _ctype_aggregate)
	&& (TAG_BITS(dst) != _typetag_uninit)
	&& ((TAG_SIZE(dst) != typeinfo[type].size)
	    || (TAG_BITS(dst) != typeinfo[type].typetag))){
    /* WARN mismatch */ 
    _output_full(fname,line,col, CT_TDM,
		0, size,size,size, srcaddr,dstaddr,
		typeinfo[type].staticrep_ptr,srcaddr,dstaddr);
    log_written = 1;
  }
#endif /* } TC_LOG_DIRTY_COPY */

  /* Clear off stray tags to the left */
  if(CONT_BIT(dst)){
    _mirror_pos_t iter = dst;
    PREV_POS(iter);
    while(CONT_BIT(iter)){
      /* AAL: unalloc or uninit? */
      WRITE_TAG(iter,_typetag_uninit);
      PREV_POS(iter);
    }
    WRITE_TAG(iter,_typetag_uninit);
  }

  /* Main copying loop. */
  for(i = 0; i < size; ){

    size_t src_size;

    /* Is this the start of a tag; does it fit within the size to be copied? */
    if(!CONT_BIT(src) && (src_size = TAG_SIZE(src)) <= size - i) {

      int j;

#if 0 /* { */ /* This will catch aggregate mismatches - pointless? */
      /* Warn if we haven't yet and there is a type or size mismatch.
	 The destination being uninit does not count as a mismatch. */
      /* AAL: May not want to warn if size doesn't match when dst is uninit. */
#ifdef TC_LOG_DIRTY_COPY /* { */
      if(streams[_msgs[CT_TDM].str] && !log_written && 
	 (COPYTAG_TYPE_MISMATCH(dst,src) || TAG_SIZE(dst) != TAG_SIZE(src)) &&
	 TYPE_BITS(dst) != _typetag_uninit){
	/* copying into memory initialized with different type */
        _output_full(fname,line,col, CT_TDM,
		0, typeinfo[type].size,size,size, srcaddr,dstaddr,
		typeinfo[type].staticrep_ptr,srcaddr,dstaddr);
	log_written = 1;
      }
#endif /* } TC_LOG_DIRTY_COPY */
#endif /* } if 0 */

      /* Now do the actual tag copy. */
      /* AAL: Should do this in one shot if possible. */
      for(j = 0; j < src_size; ++j){
	WRITE_TAG(dst, TAG_BITS(src));
	NEXT_POS(src);
	NEXT_POS(dst);
      }
      i += src_size;

    } else {  /* AAL: Copying from the middle of src!  Warn! */

      if (!warn_written) {
	/* This is not the start of src or
	   it is longer than the data to be copied? */
        _output_full(fname,line,col, CT_SI,
		0, size,size,0, srcaddr,dstaddr, srcaddr,dstaddr,0);
	warn_written = 1;
      }

      /* Set dst's type to uninit. */
      /* SY: New change: set to INIT instead. */
      do {
	WRITE_TAG(dst, _typetag_init);
	NEXT_POS(src);
	NEXT_POS(dst);
	i++;
      } while (CONT_BIT(src) && i < size);
    }
  }

  /* Clear off stray tags to the right */
  while(CONT_BIT(dst)){
    /* AAL: unalloc or uninit? */
    WRITE_TAG(dst,_typetag_uninit);
    NEXT_POS(dst);
  }
}

/* SY: currently unused: discard? Don't remember its intended purpose -27feb02 */
void _extern_registerVar(const char * fname, int line, int col,
		const char * varname, void * addr, size_t size)
{
  if(externInitFns)
    _output_flc(fname,line,col, ERV_ENN);
  externInitFns = 0;
  externInitFnsIter = &externInitFns;
}

void _extern_setUninitTag(const char * fname, int line, int col,
			void * addr, size_t size)
{
  *externInitFnsIter = (struct fnCallNode *)malloc(sizeof(struct fnCallNode));
  (*externInitFnsIter)->function = setUninitTag;
  (*externInitFnsIter)->args.sut.fname = fname;
  (*externInitFnsIter)->args.sut.line = line;
  (*externInitFnsIter)->args.sut.col = col;
  (*externInitFnsIter)->args.sut.addr = addr;
  (*externInitFnsIter)->args.sut.size = size;
  (*externInitFnsIter)->next = 0;
  externInitFnsIter = &(*externInitFnsIter)->next;
}

void _extern_setScalarTag(const char * fname, int line, int col,
			const void * addr, _ctype_t type)
{
  *externInitFnsIter = (struct fnCallNode *)malloc(sizeof(struct fnCallNode));
  (*externInitFnsIter)->function = setScalarTag;
  (*externInitFnsIter)->args.sst.fname = fname;
  (*externInitFnsIter)->args.sst.line = line;
  (*externInitFnsIter)->args.sst.col = col;
  (*externInitFnsIter)->args.sst.addr = addr;
  (*externInitFnsIter)->args.sst.type = type;
  (*externInitFnsIter)->next = 0;
  externInitFnsIter = &(*externInitFnsIter)->next;
}

void _extern_replicateTag(const char * fname, int line, int col,
			void * addr, size_t size, int nelem)
{
  *externInitFnsIter = (struct fnCallNode *)malloc(sizeof(struct fnCallNode));
  (*externInitFnsIter)->function = replicateTag;
  (*externInitFnsIter)->args.rt.fname = fname;
  (*externInitFnsIter)->args.rt.line = line;
  (*externInitFnsIter)->args.rt.col = col;
  (*externInitFnsIter)->args.rt.addr = addr;
  (*externInitFnsIter)->args.rt.size = size;
  (*externInitFnsIter)->args.rt.nelem = nelem;
  (*externInitFnsIter)->next = 0;
  externInitFnsIter = &(*externInitFnsIter)->next;
}

/* SY: currently unused: discard? Don't remember its intended purpose -27feb02 */
void _registerVar(const char * fname, int line, int col,
		const char * varname, void * addr, size_t size)
{
}

/* AAL: Produces uninits of byte size only. */
void _setScalarUninitTag(const char * fname, int line, int col,
			 void * addr, _ctype_t type)
{
  /* AAL: was initTag(fname, line, col, addr, _typetag_uninit); */

  int i;
  /* Get the position of the tag for the address. */
  _mirror_pos_t tagpos;

  /* First nibble has tag 0xxx, where xxx is _typetag_uninit;
     second nibble is 1yyy, where yyy is the log of the size;
     third nibble is 0zzz, where zzz is unalloc typetag (really 0). */
  uchar first_nibble = _typetag_uninit;
  uchar size_nibble = CONT_BIT_MASK | typeinfo[type].logsize;
  uchar subsequent_nibble = CONT_BIT_MASK | _typetag_unalloc;

  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

  GET_TAG_POS(tagpos, addr);

  /* Clear off stray tags to the left */
  if(CONT_BIT(tagpos)){
    _mirror_pos_t iter = tagpos;

    _output_flc(fname,line,col, SSUT_NEM);

    PREV_POS(iter);
    while(CONT_BIT(iter)){
      /* AAL: unalloc or uninit? */
      WRITE_TAG(iter,_typetag_unalloc);
      PREV_POS(iter);
    }
    WRITE_TAG(iter,_typetag_unalloc);
  }

  /* write tags for initial byte */
  WRITE_TAG(tagpos, first_nibble);
  if (typeinfo[type].size > 1) {
    NEXT_POS(tagpos);
    WRITE_TAG(tagpos, size_nibble);
  }

  for(i = 2; i < typeinfo[type].size; ++i){
    /* write tag for subsequent bytes */
    NEXT_POS(tagpos);
    WRITE_TAG(tagpos, subsequent_nibble);
  }

  /* Clear off stray tags to the right */
  NEXT_POS(tagpos);
  while(CONT_BIT(tagpos)){
    /* AAL: unalloc or uninit? */
    WRITE_TAG(tagpos,_typetag_unalloc)
    NEXT_POS(tagpos);
  }
}

/* if type is pointer, initialize to int */
void _setScalarTagPtrToInt(const char * fname, int line, int col,
		const void * addr, _ctype_t type)
{
  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */
  if(type == _ctype_pointer){
    initTag(fname, line, col, (void *)addr, _ctype_int);
  } else {
    initTag(fname, line, col, (void *)addr, type);
  }
}

void _setScalarTag(const char * fname, int line, int col,
		const void * addr, _ctype_t type)
{
  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */
  initTag(fname, line, col, (void *)addr, type);
}

void _replicateTag(const char * fname, int line, int col,
		void * addr, size_t size, int nelem)
{
  int i;
  char * dst = addr;

  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

/* temporary ASSERT */
  if(nelem == 0){
    fprintf(stderr, "TC: replicating 0 elements!\n");
    return;
  }

  for(i = 1; i < nelem; ++i)
    _copyTagSilent(dst += size, addr, size);
}

/*
 * if opnd_size == expr_size
 * then do nothing //-- memcopy => no change
 * else if verifyTag(*addrptr, opnd_staticpos)
 *      then *addrptr = expr_staticpos //-- `enforce' mode
 *      else do truncation/expansion //-- the messy case
 *
 * Note: tmpspace must be of size >= exptype. tmpspace's mirror is used
 *       to return the promoted tags only if it's a funny case.
 */
void _promoteTag(const char * fname, int line, int col,
		const void ** addrptr, _ctype_t optype, _ctype_t exptype,
		void * tmpspace)
{
  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */
  if(typeinfo[optype].size != typeinfo[exptype].size){
    if(_verifyTagSilent(*addrptr, optype)){
      *addrptr = typeinfo[exptype].staticrep_ptr;
    } else {
      /* do dirty work here */
      *addrptr = typeinfo[exptype].staticrep_ptr;
    }
  }
}

#ifdef TC_COUNT_PROC_RET /* { */
int _numEmptyProcessReturn = 0;
int _numCommonProcessReturn = 0;
int _numProcessReturn = 0;
#endif /* } TC_COUNT_PROC_RET */

/* debugging stuff to trace calls */
#ifdef TC_TRACECALL /* { */
#define TC_FNSTACK_SIZE 2048
const char * _tc_fnstack[TC_FNSTACK_SIZE];
int _tc_fnstack_i = 0;
#endif /* } TC_TRACECALL */

void _tcdebug_processCall_func(const char * fnname)
{
#ifdef TC_TRACECALL /* { */
  fprintf(stderr, "<<%d>>CALLED %s\n", _tc_fnstack_i, fnname);

  if(_tc_fnstack_i < TC_FNSTACK_SIZE)
    _tc_fnstack[_tc_fnstack_i] = fnname;

  _tc_fnstack_i++;
#endif /* } TC_TRACECALL */
}

/* Note: aargaddrs serves two purposes:
      2 - (void *)aargaddrs marks the start of the stack frame
      2 - *aargaddrs points to the argaddrs array, if any
*/
/* addr == null indicates void return */
void _processReturn(const char * fname, int line, int col,
		void * scaf_start, void * scaf_end, void * agrf_start, void * agrf_end,
		_addr_and_size_t ** aargaddrs, const void * addr, size_t size)
{
  static int debugflag = 0;

  /* mark end of stack frame */
  void * sf_top = &sf_top;

#ifdef TC_TRACECALL /* { */
  {
    const char * fnname = "-overflow-";

    _tc_fnstack_i--;
    if(_tc_fnstack_i < TC_FNSTACK_SIZE)
      fnname = _tc_fnstack[_tc_fnstack_i];

    fprintf(stderr, "<<%d>>RETURNING %s\n", _tc_fnstack_i, fnname);

    if(agrf_start){
      fprintf(stderr, "\tagrf_end   = %p (%6d)\n", agrf_end  , ((char *)agrf_end  ) - (char *)sf_top);
      fprintf(stderr, "\tagrf_start = %p (%6d)\n", agrf_start, ((char *)agrf_start) - (char *)sf_top);
    }
    if(scaf_start){
      fprintf(stderr, "\tscaf_end   = %p (%6d)\n", scaf_end  , ((char *)scaf_end  ) - (char *)sf_top);
      fprintf(stderr, "\tscaf_start = %p (%6d)\n", scaf_start, ((char *)scaf_start) - (char *)sf_top);
    }
    fprintf(stderr, "\taargaddrs  = %p (%6d)\n", aargaddrs , ((char *)aargaddrs ) - (char *)sf_top);
    fprintf(stderr, "\tsf_top     = %p (%6d)\n", sf_top    , ((char *)sf_top    ) - (char *)sf_top);
  }
#endif /* } TC_TRACECALL */

  /* Process Return Tag */
  _processReturnNoClear(fname,line,col,*aargaddrs,addr,size);

  /* Clear the stack frame between sf_bottom and sf_top */

  /* mark start of stack */
  if(((void *)aargaddrs) > sf_top){ /* stack grows downward */

    void * sf_bottom = aargaddrs;

    switch(tc_flag_clear){
      case _TC_TO_HIGHEST:
           if(scaf_start && scaf_end > sf_bottom) sf_bottom = scaf_end;
           if(agrf_start && agrf_end > sf_bottom) sf_bottom = agrf_end;
           break;
      case _TC_TO_SCALAR:
           if(scaf_start && scaf_end > sf_bottom) sf_bottom = scaf_end;
           if(agrf_start && agrf_end > scaf_end) {
             _fn_SET_UNALLOC_TAG("processReturn/toScalar",fname,line,col, agrf_start, agrf_end);
           }
           break;
      case _TC_TO_AGGR:
           if(agrf_start && agrf_end > sf_bottom) sf_bottom = agrf_end;
           if(scaf_start && scaf_end > agrf_end) {
             _fn_SET_UNALLOC_TAG("processReturn/toAggr",fname,line,col, scaf_start, scaf_end);
           }
           break;
      case _TC_SEGS_ONLY:
           if(scaf_start) {
             _fn_SET_UNALLOC_TAG("processReturn/Segs",fname,line,col, scaf_start, scaf_end);
           }
           if(agrf_start) {
             _fn_SET_UNALLOC_TAG("processReturn/Segs",fname,line,col, agrf_start, agrf_end);
           }
      case _TC_NONE:
           ;
    }

    SET_UNALLOC_TAG("processReturn", fname,line,col, sf_top, sf_bottom);

    if(!debugflag++ && streams[_tc_debug])
      fprintf(streams[_tc_debug], "Note: stack grows decreasingly!\n");

  } else {                /* stack grows upward   */

    void * sf_bottom = aargaddrs;

    switch(tc_flag_clear){
      case _TC_TO_HIGHEST:
           if(scaf_start && scaf_start < sf_bottom) sf_bottom = scaf_start;
           if(agrf_start && agrf_start < sf_bottom) sf_bottom = agrf_start;
           break;
      case _TC_TO_SCALAR:
           if(scaf_start && scaf_start < sf_bottom) sf_bottom = scaf_start;
           if(agrf_start && agrf_start < scaf_start) {
             _fn_SET_UNALLOC_TAG("processReturn/toScalar", fname,line,col, agrf_start, agrf_end);
           }
           break;
      case _TC_TO_AGGR:
           if(agrf_start && agrf_start < sf_bottom) sf_bottom = agrf_end;
           if(scaf_start && scaf_start < agrf_start) {
             _fn_SET_UNALLOC_TAG("processReturn/toAggr", fname,line,col, scaf_start, scaf_end);
           }
           break;
      case _TC_SEGS_ONLY:
           if(scaf_start) {
             _fn_SET_UNALLOC_TAG("processReturn/Segs", fname,line,col, scaf_start, scaf_end);
           }
           if(agrf_start) {
             _fn_SET_UNALLOC_TAG("processReturn/Segs", fname,line,col, agrf_start, agrf_end);
           }
      case _TC_NONE:
           ;
    }

    SET_UNALLOC_TAG("processReturn", fname,line,col, sf_bottom, sf_top);

    if(!debugflag++ && streams[_tc_debug])
      fprintf(streams[_tc_debug], "Note: stack grows increasingly\n");

  }
}

/* addr == null and size == 0 indicates void return */
/* addr == null and size != 0 indicates copyTag should be elided */
void _processReturnNoClear(const char * fname, int line, int col,
		_addr_and_size_t * argaddrs, const void * addr, size_t size)
{
  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */

  /* process return tag: return value is in <addr,size>,
     the caller's expectation is in argaddrs[0].<addr,size>. */
#ifdef TC_COUNT_PROC_RET /* { */
_numProcessReturn++;
#endif /* } TC_COUNT_PROC_RET */

  if((!(size && !addr)) /* (size && !addr) indicates tag-copy should be elided */
     && (argaddrs && argaddrs[0].size)){ /* indicates caller is instrumented, and expects a return value */
					 /* (else do nothing) */
    if(size == argaddrs[0].size){
      /* -- same size -- */

      /* -- common case: if  - size > 1, and     - even addresses, and
                             - tags match, and   - tag size matches size
            then we're done checking.
       */
      if(!(size > 1
	 && !(((unsigned long)addr | (unsigned long)argaddrs[0].addr) & 0x1)
	 && GET_TAG_BYTE(addr) == GET_TAG_BYTE(argaddrs[0].addr)
	 && (1 << (ODD_TYPE_BITS(GET_TAG_BYTE(addr)))) == size)){

        /* else: either size = 1,         or non-aligned addresses,
                     or tags don't match, or tag size doesn't match size
		     or addr is <INI>
           do things slowly */

        size_t i;
        _mirror_pos_t tpi;
        _mirror_pos_t rtpi;

        GET_TAG_POS(tpi, addr);
        GET_TAG_POS(rtpi, argaddrs[0].addr);

        /* Compare tags */
        for(i = 0; i < size; ++i){
          if(TAG_BITS(tpi) != TAG_BITS(rtpi)
		&& TAG_BITS(tpi) != _typetag_init){ /* INI comparison */
            /* type mismatch */

            _output_full(fname,line,col, PR_TMM,
			0, size,size,0, 0,0, argaddrs[0].addr,addr,0);
            break;
          }
          NEXT_POS(tpi);
          NEXT_POS(rtpi);
        }
        if(i < size){ /* mismatch occurred: copy */
          _copyTagSilent(argaddrs[0].addr, addr, size);
        }
        /* No mismatch: Make sure tagpos has "ended" */
        if(i == size && CONT_BIT(tpi)){
          /* type size mismatch */
          _output_full(fname,line,col, PR_TSM,
			0, size,2*size,0, 0,0, argaddrs[0].addr,addr,0);
        }
#ifdef TC_COUNT_PROC_RET
      } else {
_numCommonProcessReturn++;
#endif /* TC_COUNT_PROC_RET */
      }
    } else {
      /* -- size mismatch -- */

      /* OUTPUT WARNING HERE */
      _output_full(fname,line,col, PR_TMC,
		0, argaddrs[0].size,size,0, 0,0, argaddrs[0].addr,addr,0);

      /* -- leave declared type -- */
    }
#ifdef TC_COUNT_PROC_RET
  } else {
_numEmptyProcessReturn++;
#endif /* TC_COUNT_PROC_RET */
  }
}

void _processArgTag(const char * fname, int line, int col,
		_addr_and_size_t * argaddrs,
		/*int argCount,*/ int index,
		void * addr, _ctype_t type, size_t size_aggr)
{
  if(!++tot_dynamic_num_calls) ++tot_dynamic_num_calls_overflow; /* count */
  if(index <= _globalArgCount){

    if(type == _ctype_aggregate){
      /* -- Aggregate type -- */

      /** same size, just copy **/
      if(argaddrs[index].size == size_aggr){
        /*NOTE: copyTag won't cough _typetag_unalloc; memory is pre-initialized*/
        _copyTag(fname,line,col,
		addr, argaddrs[index].addr, argaddrs[index].size, type);
        if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */

      /** size mismatch, output warning and leave declared type? **/
      } else {
        /* OUTPUT WARNING HERE */
        _output_si(fname,line,col, PAT_TMA,
		0,0,0,0, size_aggr,argaddrs[index].size,index,0);

        /* leave declared type, or attempt partial copy? */
      }
    } else {
      /* -- Scalar type -- */

      /** same size, just copy **/
      if(argaddrs[index].size == typeinfo[type].size){
        /* NOTE: copyTag won't cough _typetag_unalloc; memory is pre-initialized */
        /* NOTE: if arg is INI, we'll just maintain INI tag (copyTag won't complain) */
        _copyTag(fname,line,col,
		addr, argaddrs[index].addr, argaddrs[index].size, type);
        if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */

      /** type matches promotion, set to declared type **/
      } else if(argaddrs[index].size == typeinfo[typeinfo[type].promo].size &&
		_verifyTagSilent(argaddrs[index].addr, typeinfo[type].promo)){
        /* note: verifyTagSilent fixes INI types */
        _setScalarTag(fname,line,col, addr, type);
        if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */

      /** size/type mismatch, output warning and leave declared type? **/
      } else {
        /* OUTPUT WARNING HERE */
        _output_full(fname,line,col, PAT_TMS,
		0, typeinfo[type].size,argaddrs[index].size,index,
		0,0, typeinfo[type].staticrep_ptr,argaddrs[index].addr,0);

        /* leave declared type; or attempt partial copy? */
      }
    }
  } else {
    /* we're out of bounds */
    /* WARN: not enough arguments passed to function, set uninit (?) */
    _output_si(fname,line,col, PAT_NEA,
		0,0,0,0, index,_globalArgCount,0,0);

    _setUninitTag(fname, line, col, addr, typeinfo[type].size);
    if(!tot_dynamic_num_calls--) tot_dynamic_num_calls_overflow--; /* adjust count */
    /* alt: leave declared type */
  }
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

/*****************************************/
/* print user and system time difference */
/*****************************************/
static void printdelta(FILE * s, struct rusage * r1, struct rusage * r2)
{
  struct timeval u1 = r1->ru_utime;
  struct timeval u2 = r2->ru_utime;
  struct timeval s1 = r1->ru_stime;
  struct timeval s2 = r2->ru_stime;

  fprintf(s, "%.3f u, %.3f s\n",
	(double)(u2.tv_sec-u1.tv_sec)+(double)(u2.tv_usec-u1.tv_usec)/1000000,
	(double)(s2.tv_sec-s1.tv_sec)+(double)(s2.tv_usec-s1.tv_usec)/1000000
	);
}

struct rusage ru0, ru1, ru2, ru3, ru4, ru5;
/***********************************/
/* cleanup: registered with atexit */
/***********************************/
static void cleanup()
{
  FILE * statstream = streams[_tc_stat];
  int msg_count;

#ifdef TC_DOSTATS
  getrusage(RUSAGE_SELF, &ru5);
#endif /* TC_DOSTATS */

  msg_count = _print_all_msgs();

  /* output overflow count */
  {
    int i;
    for(i = 0; i < _tc_numstreams; ++i){
      if(overflow_count[i]){
        fprintf(statstream, "Messages skipped for stream %s: %d\n",
		stream_descr[i], overflow_count[i]);
      }
    }
  }

  /* report static stats & dynamic numbers of instrumentation calls */
  if(statstream){
    struct static_count_node * np;
    for(np = static_count_head; np; np = np->next)
      fprintf(statstream, "Static %s = %lu\n", np->descr, np->count);

    fprintf(statstream, "Dynamic inst-calls: %lu + %lu * (%lu+1)\n",
			tot_dynamic_num_calls,
			tot_dynamic_num_calls_overflow,
			(unsigned long)-1);
    fprintf(statstream, "Error/warning/log messages: %d\n", msg_count);
  }

#ifdef TC_DOSTATS
  if(statstream){

    fprintf(statstream, "Total time: ");
    printdelta(statstream, &ru0, &ru5);

    fprintf(statstream, "Call init functions: ");
    printdelta(statstream, &ru1, &ru2);

    fprintf(statstream, "Process externs: ");
    printdelta(statstream, &ru2, &ru3);

    fprintf(statstream, "Prog_main: ");
    printdelta(statstream, &ru4, &ru5);

#ifdef TC_COUNT_PROC_RET /* { */
    fprintf(statstream, "processReturns : tot %d, empty %d, common %d\n",
			_numProcessReturn,
			_numCommonProcessReturn,
			_numEmptyProcessReturn);
#endif /* } TC_COUNT_PROC_RET */

    if(_vpctr || _vpcctr || _vpactr
	|| _vtctr || _vtcctr || _vtactr
	|| _ctctr || _ctcctr || _ctactr
	|| _sstctr || _sstcctr
	|| _ssutctr || _ssutcctr
	|| _patctr || _patpctr || _patactr){

      fprintf(statstream, "Macro counts (function counts):\n");

      fprintf(statstream,
	"verifyPtr : char %d(%d), aggr %d(%d), etc %d(%d)\n",
	_vpcctr, _vpcfctr, _vpactr, _vpafctr, _vpctr, _vpfctr);

      fprintf(statstream,
	"verifyTag : char %d(%d), aggr %d(%d), etc %d(%d)\n",
	_vtcctr, _vtcfctr, _vtactr, _vtafctr, _vtctr, _vtfctr);

      fprintf(statstream,
	"  copyTag : char %d(%d), aggr %d(%d), etc %d(%d)\n",
	_ctcctr, _ctcfctr, _ctactr, _ctafctr, _ctctr, _ctfctr);

      fprintf(statstream,
	"      setScalarTag : char %d(%d), etc %d(%d)\n",
	_sstcctr, _sstcfctr, _sstctr, _sstfctr);

      fprintf(statstream,
	"setScalarUninitTag : char %d(%d), etc %d(%d)\n",
	_ssutcctr, _ssutcfctr, _ssutctr, _ssutfctr);

      fprintf(statstream,
	"processArgTag : scalar %d(%d) (promo %d), aggr %d(%d)\n",
	_patctr, _patfctr, _patpctr, _patactr, _patafctr);
    }
  }
#endif /* TC_DOSTATS */
}

static int stringToStreamId(const char * cp, const char ** endptr)
{
  int i;
  for(i = 0; i < _tc_numstreams; ++i){
    int slen = strlen(stream_descr[i]);
    if(!strncmp(cp,stream_descr[i],slen)){
      if(endptr) *endptr = cp + slen;
      return i;
    }
  }
  return -1;
}


/************************/
/* "real" main function */
/************************/
#ifndef STANDALONE /* { */

int main(int argc, char * argv[], char * envp[])
{
  extern void callInitFunctions(void);
  extern int _prog_main(int argc, char * argv[], char * envp[]);
  int i;

  /* typechecker output streams */
  /* if set to 0, nothing will be output */
  streams[_tc_stat] = stderr;
  streams[_tc_log] = stderr;
  streams[_tc_debug] = 0; /* debug messages are just meaninglessly annoying */
  streams[_tc_warn] = stderr;
  streams[_tc_error] = stderr;

  /* initialize signal flags */
  signalflags[_tc_log] = 1;
  signalflags[_tc_warn] = 1;
  signalflags[_tc_error] = 1;

  /* Initialize output limits */
  output_limit[_tc_error] = 100;
  output_limit[_tc_warn] = 100;
  output_limit[_tc_log] = 100;

  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru0));

#ifndef TC_NOSIGNAL
  signal(TCSIG,SIG_IGN);
#endif

  /* register the cleanup function with atexit() */
  atexit(cleanup);

  /* intercept TC command-line arguments */
  for(i = 0; i < argc; ++i){
    if(!strncmp(argv[i],"-tc-",4)){
      int j;
      if(!strcmp(argv[i]+4,"summarize")) tc_flag_summarize = 1;
      else if(!strcmp(argv[i]+4,"terse")) tc_flag_terse = 1;
      else if(!strcmp(argv[i]+4,"trackfree")) tc_flag_trackfree = 1;
      else if(!strcmp(argv[i]+4,"vtfix")) tc_flag_vtfix = 1;
      else if(!strcmp(argv[i]+4,"no-vtfix")) tc_flag_vtfix = 0;
      else if(!strcmp(argv[i]+4,"allsig"))
		signalflags[_tc_log] = signalflags[_tc_warn] = signalflags[_tc_error] = 1;
      else if(!strcmp(argv[i]+4,"clearsig")){
             int i;
             for(i = 0; i < _tc_numstreams; ++i)
               signalflags[i] = 0;
      } else if(!strncmp(argv[i]+4,"sig-",4)){
             int stream_id;
             stream_id = stringToStreamId(argv[i]+8, 0);
             if(stream_id != -1){
               signalflags[stream_id] = 1;
               fprintf(stderr, "TC: Turning on signal for %s stream\n", stream_descr[stream_id]);
             } else fprintf(stderr, "TC: Malformed -tc- flag ignored (%s)\n", argv[i]);
      } else if(!strncmp(argv[i]+4,"nosig-",6)){
             int stream_id;
             stream_id = stringToStreamId(argv[i]+10, 0);
             if(stream_id != -1){
               signalflags[stream_id] = 0;
               fprintf(stderr, "TC: Turning off signal for %s stream\n", stream_descr[stream_id]);
             } else fprintf(stderr, "TC: Malformed -tc- flag ignored (%s)\n", argv[i]);
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
      } else if(!strncmp(argv[i]+4,"limit-",6)){
             int stream_id;
             const char * stream_limit_str = 0;
             int stream_limit;

             stream_id = stringToStreamId(argv[i]+10, &stream_limit_str);
             if(stream_limit_str && *stream_limit_str == '=')
               stream_limit = atoi(stream_limit_str+1);
             if(stream_id != -1){
               output_limit[stream_id] = stream_limit;
               fprintf(stderr, "TC: setting %s stream limit to %d\n", stream_descr[stream_id], stream_limit);
             } else fprintf(stderr, "TC: Malformed -tc- flag ignored (%s)\n", argv[i]);
      } else if(!strncmp(argv[i]+4,"send-",5)){
             int stream_id;
             const char * msgid_str = 0;
	     int msgid = 0;

             stream_id = stringToStreamId(argv[i]+9, &msgid_str);
             if(msgid_str && *msgid_str == '=')
               msgid = atoi(msgid_str+1);
             if(stream_id != -1 && msgid > MSGS_0 && msgid < MSGS_LAST){
               _msgs[msgid].str = stream_id;
               fprintf(stderr, "TC: sending message %d to %s stream (%d:%s)\n", msgid, stream_descr[stream_id], msgid, _msgs[msgid].msg);
             } else fprintf(stderr, "TC: Malformed -tc- flag ignored (%s)\n", argv[i]);
      } else if(!strncmp(argv[i]+4,"stream-",7)){
             int stream_id;
             const char * fname = 0;

             stream_id = stringToStreamId(argv[i]+11, &fname);
             if(fname && *fname == '=') fname++;
             if(stream_id != -1 && stream_id != _tc_null){
               if(!strcmp(fname,"")){
                 streams[stream_id] = 0;
                 fprintf(stderr, "TC: Disabling %s stream\n", stream_descr[stream_id]);
               } else if(!strcmp(fname,"-")){
                 streams[stream_id] = stdout;
                 fprintf(stderr, "TC: Setting %s stream to stdout\n", stream_descr[stream_id]);
               } else if(!strcmp(fname,"=")){
                 streams[stream_id] = stderr;
                 fprintf(stderr, "TC: Setting %s stream to stderr\n", stream_descr[stream_id]);
               } else {
                 if((streams[stream_id] = fopen(fname, "w")))
                   fprintf(stderr, "TC: Redirecting %s stream to file %s\n", stream_descr[stream_id], fname);
                 else
                   fprintf(stderr, "TC: Error opening file %s for %s output\n", fname, stream_descr[stream_id]);
               }
             } else fprintf(stderr, "TC: Malformed -tc- flag ignored (%s)\n", argv[i]);
      } else fprintf(stderr, "TC: Unrecognized -tc- flag ignored (%s)\n", argv[i]);

      for(j = i+1; j < argc; ++j) argv[j-1] = argv[j];
      argv[--argc] = 0;
      i--;
    }
  }

  /* Initialize static reps */
  initStaticRep();

  /* Initialize globals and externs */
  _tc_init_mode = 1;
  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru1));
  callInitFunctions();
  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru2));
  processExternVars();
  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru3));
  _tc_init_mode = 0;

  /* debug output: compile time flags */
  if(streams[_tc_debug])
    fprintf(streams[_tc_debug], "Note: strictPointer mode is %s\n",
			 strictPointer?"ON":"OFF");

  /* initialize argv */
  for(i = 0; i < argc; ++i){
    _setScalarTag(__FILE__,__LINE__,0,&argv[i],_ctype_pointer);
    _setStringTag(__FILE__,__LINE__,0,argv[i],strlen(argv[i]));
  }
  /* (terminating null pointer) */
  _setScalarTag(__FILE__,__LINE__,0,&argv[i],_ctype_pointer); /*TODO: set null tag*/

  /* initialize envp */
  for(i = 0; envp[i]; ++i){
    _setScalarTag(__FILE__,__LINE__,0,&envp[i],_ctype_pointer);
    _setStringTag(__FILE__,__LINE__,0,envp[i],strlen(envp[i]));
  }
  /* (terminating null pointer) */
  _setScalarTag(__FILE__,__LINE__,0,&envp[i],_ctype_pointer); /*TODO: set null tag*/

  TC_DO_TIMINGS(getrusage(RUSAGE_SELF, &ru4));
  return _prog_main(argc, argv, envp);
}

#else /* } ifdef STANDALONE { */
/****************/
/* Debug main() */
/****************/

int main()
{
  long tmpspace;
  _mirror_pos_t  pos1, pos2, pos3, pos4;
  void		*adr1,*adr2,*adr3,*adr4;

  initStaticRep();

  printf("Mirrorpage Numbytes = %d\n", MIRRORPAGE_NUMBYTES);
  printf("Mirrorpage Mask = 0x%08x\n", MIRRORPAGE_MASK);
  printf("Mirrormap Numbits = %d\n", MIRRORMAP_NUMBITS);
  printf("Mirrormap Numelements = %d\n", MIRRORMAP_NUMELEMENTS);

  fprintf(stderr, "SetScalarUninitTag 82-ffc:\n");
  _setScalarUninitTag("test", __LINE__, 0, (void *)0x82ffffc, _ctype_double);
  fprintf(stderr, "Print 82-ff0 16:\n");
  _printTagStderr((void *)0x82ffff0, 16);
  fprintf(stderr, "Print 83-000 16:\n");
  _printTagStderr((void *)0x8300000, 16);
  fprintf(stderr, "Done\n");

  return 0;
}
#endif /* } ifdef STANDALONE */
