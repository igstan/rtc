/*************************/
/******** stdio.h ********/
/*************************/
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

  /* varg dummy hack; be careful if modifying; something like a cast may screw this up */
  args = (_tc_varg_dummy,args);

  /* WRITE: for %n */
  *args = 0;
  *(args+unknown) = 0;

  /* READ: format and args */
/* NOTE: currently not checked */
/*
  if(*format && *(format+unknown));
  if(*args && *(args+unknown));
*/

  return unknown;
}

int _rtclib_pctn_fprintf(FILE * stream, const char * format, ...)
{
  int unknown;
  char * args;

  /* varg dummy hack; be careful if modifying; something like a cast may screw this up */
  args = (_tc_varg_dummy,args);

  /* WRITE: for %n */
  *args = 0;
  *(args+unknown) = 0;

  /* READ: format and args */
/* NOTE: currently not checked */
/*
  if(*stream);
  if(*format && *(format+unknown));
  if(*args && *(args+unknown));
*/

  return unknown;
}

int _rtclib_sprintf(char * str, const char * format, ...)
{
  int unknown;
  char * args;

  /* varg dummy hack; be careful if modifying; something like a cast may screw this up */
  args = (_tc_varg_dummy,args);

  /* write into str */
  *str = 0;
  *(str+unknown) = 0;

  /* WRITE: for %n */
  *args = 0;
  *(args+unknown) = 0;

  /* READ: format and args */
/* NOTE: currently not checked */
/*
  if(*stream);
  if(*format && *(format+unknown));
  if(*args && *(args+unknown));
*/

  return unknown;
}

int _rtclib_snprintf(char * str, size_t size, const char * format, ...)
{
  int unknown;
  char * args;

  /* varg dummy hack; be careful if modifying; something like a cast may screw this up */
  args = (_tc_varg_dummy,args);

  /* write into str */
  *str = 0;
  *(str+unknown) = 0;

  /* WRITE: for %n */
  *args = 0;
  *(args+unknown) = 0;

  /* READ: format and args */
/* NOTE: currently not checked */
/*
  if(*stream);
  if(*format && *(format+unknown));
  if(*args && *(args+unknown));
*/

  return unknown;
}

int _rtclib_scanf(const char * format, ...)
{
  int unknown;
  char * args;

  /* varg dummy hack; be careful if modifying; something like a cast may screw this up */
  args = (_tc_varg_dummy,args);

  *args = 0;
  *(args+unknown) = 0;

  if(*format && *(format+unknown));

  return unknown;
}

/* for now: ignore stream: should read? */
int _rtclib_fscanf(FILE * stream, const char * format, ...)
{
  int unknown;
  char * args;

  /* varg dummy hack; be careful if modifying; something like a cast may screw this up */
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

  /* varg dummy hack; be careful if modifying; something like a cast may screw this up */
  args = (_tc_varg_dummy,args);

  *args = 0;
  *(args+unknown) = 0;

  if(*format && *(format+unknown));
  if(*str && *(str+unknown));

  return unknown;
}

/**************************/
/******** malloc.h ********/
/**************************/

/* bypassed
void * _rtclib_malloc(size_t size);
void * _rtclib_calloc(size_t nelem, size_t elsize);
void * _rtclib_memalign(size_t alignment, size_t size);
void * _rtclib_valloc(size_t size);
*/

/* create AOFunction placeholder */
void _rtclib_free(void * ptr)
{
/* previously used to force instrumentation of all malloc/frees
  *(char *)ptr = 0;
*/
}

/* must do in assign-analysis or tca-addrof
void * _rtclib_realloc(void * ptr, size_t size);
*/


/**************************/
/******** string.h ********/
/**************************/

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

/* extern char *strerror(int); */

void *_rtclib_memccpy(void * s1, const void * s2, int c, size_t n)
{
  int unknown;
  *(char *)s1 = *(char *)s2;
  *(char *)(s1+unknown) = *(char *)(s2+unknown);
}

/* extern char *strsignal(int); */

char *_rtclib_strdup(const char * s1)
{
  int unknown;
  void * malloc();
  char * ret = (char *)malloc();	/* malloc w/ no arguments => "unknown" malloc size */
  *ret = *s1;
  *(ret+unknown) = *(s1+unknown);
  return ret;
}

/* no wrapper: model encodes flowthrough only */
char *_rtclib_strchr(char *s, int c)
{
  int unknown;
  return (s+unknown);
}

/* no wrapper: model encodes flowthrough only */
char *_rtclib_strrchr(char *s, int c)
{
  int unknown;
  return (s+unknown);
}

/* no wrapper: model encodes flowthrough only */
char *_rtclib_strstr(char *s1, const char *s2)
{
  int unknown;
  return (s1+unknown);
}

/* no wrapper: model encodes flowthrough only */
/* TODO: write wrapper */
char *_rtclib_strtok(char *s1, const char *s2)
{
  static char * sp;
  int unknown;
  sp = s1+unknown;
  return sp;
}

/* no wrapper: model encodes flowthrough only */
/* TODO: write wrapper */
char *_rtclib_strtok_r(char *s1, const char *s2, char **lasts)
{
  int unknown;
  return s1?(s1+unknown):(*lasts+unknown);
}

/****************************/
/******** langinfo.h ********/
/****************************/
typedef int nl_item;

char * _rtclib_nl_langinfo(nl_item item)
{
  static char nl_langinfo_static_buffer[1] = {'c'};
  return nl_langinfo_static_buffer;
}

/****************************/
/******** sys/stat.h ********/
/****************************/
struct stat {
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

int _rtclib_stat(const char *path, struct stat *buf)
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

int _rtclib_lstat(const char *path, struct stat *buf)
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

int _rtclib_fstat(int fildes, struct stat *buf)
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


/************************/
/******** time.h ********/
/************************/
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


/**************************/
/******** unistd.h ********/
/**************************/
char * _rtclib_getpass(const char * prompt)
{
  static char getpass_static_buffer[1] = {'c'};
  return getpass_static_buffer;
}

/* TODO
int    _rtclib_brk(void *endds);
void * _rtclib_sbrk(int incr);
*/

/* TODO
typedef void off_t;
struct iovec{
};
ssize_t _rtclib_read(int fildes, void *buf, size_t nbyte);
ssize_t _rtclib_pread(int fildes, void *buf, size_t nbyte, off_t offset);
ssize_t _rtclib_readv(int fildes, struct iovec *iov, int iovcnt);
*/

/*************************/
/******** netdb.h ********/
/****** sys/socket.h *****/
/*************************/
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

