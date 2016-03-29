# 1 "libc.tmp.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "libc.tmp.c"
# 1 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h" 1
# 17 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h"
# 1 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h" 1
# 22 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h"
typedef unsigned int size_t;
# 96 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h"
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



extern void _registerVar(const char * fname, int line, int col,
                        const char * varname, void * addr, size_t size);
extern void _extern_registerVar(const char * fname, int line, int col,
                        const char * varname, void * addr, size_t size);
# 167 "/p/wpis/people/students/suan/TypecheckDebugger/include/tcptr.h"
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



extern void _reportStaticCounts(const char * fname, const char * descr, int count);
# 18 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h" 2
# 51 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h"
extern int printf();
extern int fprintf();
# 65 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h"
void *malloc(size_t size);
void *calloc(size_t nelem, size_t elsize);
void free(void *ptr);
void *memalign(size_t alignment, size_t size);
void *realloc(void *ptr, size_t size);
void *valloc(size_t size);
# 233 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h"
# 1 "/p/wpis/people/students/suan/TypecheckDebugger/include/gnu-hacks.h" 1
# 12 "/p/wpis/people/students/suan/TypecheckDebugger/include/gnu-hacks.h"
extern int _tc_varg_dummy;







typedef void * __builtin_va_list;
extern int _tc_builtin_varg_dummy;
# 234 "/p/wpis/people/students/suan/TypecheckDebugger/include/typecheck.h" 2





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
