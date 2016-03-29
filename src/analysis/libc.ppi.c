# 1 "libc.tmp.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "libc.tmp.c"
# 1 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h" 1
# 19 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h"
# 1 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h" 1
# 11 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
typedef unsigned int size_t;




typedef enum _ctype {
  _ctype_void_invalid = 0,
  _ctype_int = 1,
  _ctype_char = 2,
  _ctype_short = 3,
  _ctype_long = 4,
  _ctype_longlong = 5,
  _ctype_float = 6,
  _ctype_double = 7,




  _ctype_pointer = 9,
  _ctype_aggregate = 10
} _ctype_t;
# 111 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void _registerFunction(const char * fname, int line, int col, void * addr);
# 120 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void _registerExtern(const char * fname, int line, int col,
                        void * addr, _ctype_t type, size_t size);
# 132 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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
# 253 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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




extern void _verifyTag_pointer(const char * fname, int line, int col,
                        const char * exp, const void * addr);
extern void _verifyTag_aggregate(const char * fname, int line, int col,
                        const char * exp, const void * addr);
# 366 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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




extern void _copyTag_pointer(const char * fname, int line, int col,
                        const void * dst, const void * src, size_t size);
extern void _copyTag_aggregate(const char * fname, int line, int col,
                        const void * dst, const void * src, size_t size);
# 535 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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
# 558 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void _extern_setUninitTag(const char * fname, int line, int col,
                        void * addr, size_t size);
# 570 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void _registerVar(const char * fname, int line, int col,
                        const char * varname, void * addr, size_t size);
extern void _extern_registerVar(const char * fname, int line, int col,
                        const char * varname, void * addr, size_t size);
# 585 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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




extern void _setScalarUninitTag_pointer(const char * fname,
                        int line, int col, void * addr);
# 728 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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




extern void _setScalarTagPtrToInt_pointer(const char * fname,
                        int line, int col, const void * addr);
# 779 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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




extern void _extern_setScalarTag_pointer(const char * fname,
                        int line, int col, const void * addr);
# 1012 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void _replicateTag(const char * fname, int line, int col,
                        void * addr, size_t size, int nelem);
extern void _extern_replicateTag(const char * fname, int line, int col,
                        void * addr, size_t size, int nelem);
# 1027 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void _promoteTag(const char * fname, int line, int col,
                        const void ** addrptr,
                        _ctype_t opnd_type,
                        _ctype_t expr_type,
                        void * tmpspace);
# 1044 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void * _typecheck_malloc_init(size_t size);
extern void _typecheck_free_partial(void * ptr);
extern void * _typecheck_realloc_init(void * ptr, size_t size);
extern void * _typecheck_memalign_init(size_t alignment, size_t size);
extern void * _typecheck_valloc_init(size_t size);
# 1061 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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


extern long long _dummyInt;





extern void _processReturn(const char * fname, int line, int col,
                void * scaf_start, void * scaf_end, void * agrf_start, void * agrf_end,
                _addr_and_size_t ** aargaddrs, const void * addr, size_t size);

extern void _processReturnNoClear(const char * fname, int line, int col,
                _addr_and_size_t * argaddrs, const void * addr, size_t size);



extern void _tcdebug_processCall(const char * fnname);
# 1110 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
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




extern void _processArgTag_pointer(const char *, int, int,
                _addr_and_size_t *, int, void *, size_t);
extern void _processArgTag_aggregate(const char *, int, int,
                _addr_and_size_t *, int, void *, size_t);
# 1219 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void * _ini_static_repptr;

extern void * _int_static_repptr;
extern void * _char_static_repptr;
extern void * _short_static_repptr;
extern void * _long_static_repptr;
extern void * _longlong_static_repptr;
extern void * _float_static_repptr;
extern void * _double_static_repptr;



extern void * _pointer_static_repptr;
# 1273 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern void _reportStaticCounts(const char * fname, const char * descr, int count);
# 1282 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcapi.h"
extern int strictPointer;
# 20 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h" 2
# 66 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h"
void *malloc(size_t size);
void *calloc(size_t nelem, size_t elsize);
void free(void *ptr);
void *memalign(size_t alignment, size_t size);
void *realloc(void *ptr, size_t size);
void *valloc(size_t size);
# 288 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h"
# 1 "/p/wpis/people/students/suan/TypecheckDebugger/include/gnu-hacks.h" 1
# 12 "/p/wpis/people/students/suan/TypecheckDebugger/include/gnu-hacks.h"
extern int _tc_varg_dummy;







typedef void * __builtin_va_list;
extern int _tc_builtin_varg_dummy;
# 289 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h" 2





int _tc_h_end;
# 2 "libc.tmp.c" 2
# 1 "libc.c"



typedef void FILE;

char * _rtclib_fgets(char * s, int n, FILE * stream)
{
  int unknown;
  *s = unknown;
  *(s+unknown) = unknown;
  return s;
}

char * _rtclib_gets(char * s)
{
  int unknown;
  *s = unknown;
  *(s+unknown) = unknown;
  return s;
}

size_t _rtclib_fread(void * ptr, size_t size, size_t nitems, FILE * stream)
{
  int unknown;
  *(char *)ptr = unknown;
  *(char *)(ptr+unknown) = unknown;
  return unknown;
}

int _rtclib_pctn_printf(const char * format, ...)
{
  int unknown;
  char * args;


  args = (_tc_varg_dummy,args);


  *args = 0;
  *(args+unknown) = 0;
# 49 "libc.c"
  return unknown;
}

int _rtclib_pctn_fprintf(FILE * stream, const char * format, ...)
{
  int unknown;
  char * args;


  args = (_tc_varg_dummy,args);


  *args = 0;
  *(args+unknown) = 0;
# 72 "libc.c"
  return unknown;
}

int _rtclib_sprintf(char * str, const char * format, ...)
{
  int unknown;
  char * args;


  args = (_tc_varg_dummy,args);


  *str = 0;
  *(str+unknown) = 0;


  *args = 0;
  *(args+unknown) = 0;
# 99 "libc.c"
  return unknown;
}

int _rtclib_snprintf(char * str, size_t size, const char * format, ...)
{
  int unknown;
  char * args;


  args = (_tc_varg_dummy,args);


  *str = 0;
  *(str+unknown) = 0;


  *args = 0;
  *(args+unknown) = 0;
# 126 "libc.c"
  return unknown;
}

int _rtclib_scanf(const char * format, ...)
{
  int unknown;
  char * args;


  args = (_tc_varg_dummy,args);

  *args = 0;
  *(args+unknown) = 0;

  if(*format && *(format+unknown));

  return unknown;
}


int _rtclib_fscanf(FILE * stream, const char * format, ...)
{
  int unknown;
  char * args;


  args = (_tc_varg_dummy,args);

  *args = 0;
  *(args+unknown) = 0;

  if(*format && *(format+unknown));

  return unknown;
}

int _rtclib_sscanf(const char * str, const char * format, ...)
{
  int unknown;
  char * args;


  args = (_tc_varg_dummy,args);

  *args = 0;
  *(args+unknown) = 0;

  if(*format && *(format+unknown));
  if(*str && *(str+unknown));

  return unknown;
}
# 191 "libc.c"
void _rtclib_free(void * ptr)
{



}
# 207 "libc.c"
void *_rtclib_memcpy(void * s1, const void * s2, size_t n)
{
  int unknown;
  *(char *)s1 = *(char *)s2;
  *(char *)(s1+unknown) = *(char *)(s2+unknown);
  return s1;
}

void *_rtclib_memmove(void * s1, const void * s2, size_t n)
{
  int unknown;
  *(char *)s1 = *(char *)s2;
  *(char *)(s1+unknown) = *(char *)(s2+unknown);
  return s1;
}

char *_rtclib_strcpy(char * dst, const char * src)
{
  int unknown;
  *dst = *src;
  *(dst+unknown) = *(src+unknown);
  return dst;
}

char *_rtclib_strncpy(char * dst, const char * src, size_t n)
{
  int unknown;
  *dst = *src;
  *(dst+unknown) = *(src+unknown);
  return dst;
}


char *_rtclib_strcat(char * dst, const char * src)
{
  int unknown;
  *dst = *src;
  *(dst+unknown) = *(src+unknown);
  return dst;
}

char *_rtclib_strncat(char * dst, const char * src, size_t n)
{
  int unknown;
  *dst = *src;
  *(dst+unknown) = *(src+unknown);
  return dst;
}

size_t _rtclib_strxfrm(char * dst, const char * src, size_t n)
{
  int unknown;
  *dst = *src;
  *(dst+unknown) = *(src+unknown);
  return unknown;
}

void _rtclib_bcopy(void * s, void * d, size_t n)
{
  int unknown;
  *(char *)d = *(char *)s;
  *(char *)(d+unknown) = *(char *)(s+unknown);
}

void _rtclib_bzero(void * s, size_t n)
{
  int unknown;
  *(char *)s = 0;
  *(char *)(s+unknown) = 0;
}

void *_rtclib_memset(void * s, int c, size_t n)
{
  int unknown;
  *(char *)s = c;
  *(char *)(s+unknown) = c;
  return s;
}



void *_rtclib_memccpy(void * s1, const void * s2, int c, size_t n)
{
  int unknown;
  *(char *)s1 = *(char *)s2;
  *(char *)(s1+unknown) = *(char *)(s2+unknown);
}



char *_rtclib_strdup(const char * s1)
{
  int unknown;
  void * _typecheck_malloc();
  char * ret = (char *)_typecheck_malloc();
  *ret = *s1;
  *(ret+unknown) = *(s1+unknown);
  return ret;
}


char *_rtclib_strchr(char *s, int c)
{
  int unknown;
  return (s+unknown);
}


char *_rtclib_strrchr(char *s, int c)
{
  int unknown;
  return (s+unknown);
}


char *_rtclib_strstr(char *s1, const char *s2)
{
  int unknown;
  return (s1+unknown);
}



char *_rtclib_strtok(char *s1, const char *s2)
{
  static char * sp;
  int unknown;
  sp = s1+unknown;
  return sp;
}



char *_rtclib_strtok_r(char *s1, const char *s2, char **lasts)
{
  int unknown;
  return s1?(s1+unknown):(*lasts+unknown);
}




typedef int nl_item;

char * _rtclib_nl_langinfo(nl_item item)
{
  static char nl_langinfo_static_buffer[1] = {'c'};
  return nl_langinfo_static_buffer;
}




struct _typecheck_stat {
  unsigned long long int st_dev;
  unsigned short int __pad1;
  unsigned long st_ino;
  unsigned int st_mode;
  unsigned int st_nlink;
  unsigned int st_uid;
  unsigned int st_gid;
  unsigned long long int st_rdev;
  unsigned short int __pad2;
  long int st_size;
  long int st_blksize;
  long int st_blocks;
  long int st_atime;
  unsigned long int __unused1;
  long int st_mtime;
  unsigned long int __unused2;
  long int st_ctime;
  unsigned long int __unused3;
  unsigned long int __unused4;
  unsigned long int __unused5;

};

int _rtclib_stat(const char *path, struct _typecheck_stat *buf)
{
  int unknown;
  if(*path);
  buf->st_dev = unknown;
  buf->st_ino = unknown;
  buf->st_mode = unknown;
  buf->st_nlink = unknown;
  buf->st_uid = unknown;
  buf->st_gid = unknown;
  buf->st_rdev = unknown;
  buf->st_size = unknown;
  buf->st_blksize = unknown;
  buf->st_blocks = unknown;
  buf->st_atime = unknown;
  buf->st_mtime = unknown;
  buf->st_ctime = unknown;
  return unknown;
}

int _rtclib_lstat(const char *path, struct _typecheck_stat *buf)
{
  int unknown;
  if(*path);
  buf->st_dev = unknown;
  buf->st_ino = unknown;
  buf->st_mode = unknown;
  buf->st_nlink = unknown;
  buf->st_uid = unknown;
  buf->st_gid = unknown;
  buf->st_rdev = unknown;
  buf->st_size = unknown;
  buf->st_blksize = unknown;
  buf->st_blocks = unknown;
  buf->st_atime = unknown;
  buf->st_mtime = unknown;
  buf->st_ctime = unknown;
  return unknown;
}

int _rtclib_fstat(int fildes, struct _typecheck_stat *buf)
{
  int unknown;
  buf->st_dev = unknown;
  buf->st_ino = unknown;
  buf->st_mode = unknown;
  buf->st_nlink = unknown;
  buf->st_uid = unknown;
  buf->st_gid = unknown;
  buf->st_rdev = unknown;
  buf->st_size = unknown;
  buf->st_blksize = unknown;
  buf->st_blocks = unknown;
  buf->st_atime = unknown;
  buf->st_mtime = unknown;
  buf->st_ctime = unknown;
  return unknown;
}





typedef void time_t;

struct tm {
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
};

char *_rtclib_ctime(const time_t *clock)
{
  static char ctime_static_buffer[1] = {'c'};
  return ctime_static_buffer;
}

struct tm *_rtclib_localtime(const time_t *clock)
{
  static struct tm localtime_static_buffer;
  int unknown;
  localtime_static_buffer.tm_sec = unknown;
  localtime_static_buffer.tm_min = unknown;
  localtime_static_buffer.tm_hour = unknown;
  localtime_static_buffer.tm_mday = unknown;
  localtime_static_buffer.tm_mon = unknown;
  localtime_static_buffer.tm_year = unknown;
  localtime_static_buffer.tm_wday = unknown;
  localtime_static_buffer.tm_yday = unknown;
  localtime_static_buffer.tm_isdst = unknown;
  return &localtime_static_buffer;
}

struct tm *_rtclib_gmtime(const time_t *clock)
{
  static struct tm gmtime_static_buffer;
  int unknown;
  gmtime_static_buffer.tm_sec = unknown;
  gmtime_static_buffer.tm_min = unknown;
  gmtime_static_buffer.tm_hour = unknown;
  gmtime_static_buffer.tm_mday = unknown;
  gmtime_static_buffer.tm_mon = unknown;
  gmtime_static_buffer.tm_year = unknown;
  gmtime_static_buffer.tm_wday = unknown;
  gmtime_static_buffer.tm_yday = unknown;
  gmtime_static_buffer.tm_isdst = unknown;
  return &gmtime_static_buffer;
}

char *_rtclib_asctime(const struct tm *tm)
{
  static char asctime_static_buffer[1] = {'c'};
  return asctime_static_buffer;
}





char * _rtclib_getpass(const char * prompt)
{
  static char getpass_static_buffer[1] = {'c'};
  return getpass_static_buffer;
}
# 533 "libc.c"
struct hostent {
  char *h_name;
  char **h_aliases;
  int h_addrtype;
  int h_length;
  char **h_addr_list;
};

struct hostent *_rtclib_gethostbyname(const char *name)
{
  static struct hostent gethostbyname_static_buffer;
  static char h_name_static_buffer[1] = {'c'};
  static char h_addr_list_string_static_buffer[1] = {'c'};
  static char * h_addr_list_static_buffer[1] = {h_addr_list_string_static_buffer};
  static char h_aliases_string_static_buffer[1] = {'c'};
  static char * h_aliases_static_buffer[1] = { h_aliases_string_static_buffer };
  int unknown;

  gethostbyname_static_buffer.h_name = h_name_static_buffer;
  gethostbyname_static_buffer.h_addr_list = h_addr_list_static_buffer;
  gethostbyname_static_buffer.h_addrtype = unknown;
  gethostbyname_static_buffer.h_length = unknown;
  gethostbyname_static_buffer.h_aliases = h_aliases_static_buffer;

  return &gethostbyname_static_buffer;
}

struct hostent *_rtclib_gethostbyaddr(const void *addr, unsigned int len, int type)
{
  static struct hostent gethostbyaddr_static_buffer;
  static char h_name_static_buffer[1] = {'c'};
  static char h_addr_list_string_static_buffer[1] = {'c'};
  static char * h_addr_list_static_buffer[1] = {h_addr_list_string_static_buffer};
  static char h_aliases_string_static_buffer[1] = {'c'};
  static char * h_aliases_static_buffer[1] = { h_aliases_string_static_buffer };
  int unknown;

  gethostbyaddr_static_buffer.h_name = h_name_static_buffer;
  gethostbyaddr_static_buffer.h_addr_list = h_addr_list_static_buffer;
  gethostbyaddr_static_buffer.h_addrtype = unknown;
  gethostbyaddr_static_buffer.h_length = unknown;
  gethostbyaddr_static_buffer.h_aliases = h_aliases_static_buffer;

  return &gethostbyaddr_static_buffer;
}
