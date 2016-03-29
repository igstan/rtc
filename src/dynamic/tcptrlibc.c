#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <langinfo.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <netdb.h>
#include <sys/socket.h>
#include <unistd.h>

#include "tcptr_internal.h"

/*************************/
/******** stdio.h ********/
/*************************/

char   * _typecheck_fgets(char * s, int n, FILE * stream)
{
  _tcptr_verifyPtr("",0,0,"(fgets)", s,n);
  return fgets(s, n, stream);
}

char   * _typecheck_gets(char * s)
{
  char * ret = gets(s);
  _tcptr_verifyPtr("",0,0,"(gets)", s,strlen(ret)+1);
  return ret;
}

size_t   _typecheck_fread(void * ptr, size_t size, size_t nitems, FILE * stream)
{
  _tcptr_verifyPtr("",0,0,"(fread)", ptr, size*nitems);
  return fread(ptr, size, nitems, stream);
}

/**************************/
/******** string.h ********/
/**************************/

void *_typecheck_memcpy(void * s1, const void * s2, size_t n)
{
  _tcptr_verifyPtr("",0,0,"(memcpy:s1)", s1, n);
#ifndef TC_PTRW /* { */
  _tcptr_verifyPtr("",0,0,"(memcpy:s2)", s2, n);
#endif /* } ifndef TC_PTRW */
  return memcpy(s1, s2, n);
}

void *_typecheck_memmove(void * s1, const void * s2, size_t n)
{
  _tcptr_verifyPtr("",0,0,"(memmove:s1)", s1, n);
#ifndef TC_PTRW /* { */
  _tcptr_verifyPtr("",0,0,"(memmove:s2)", s2, n);
#endif /* } ifndef TC_PTRW */
  return memmove(s1, s2, n);
}

char *_typecheck_strcpy(char * dst, const char * src)
{
  int slen = strlen(src);
#ifndef TC_PTRW /* { */
  _tcptr_verifyPtr("",0,0,"(strcpy:src)", src, slen+1);
#endif /* } ifndef TC_PTRW */
  _tcptr_verifyPtr("",0,0,"(strcpy:dst)", dst, slen+1);
  return strcpy(dst, src);
}

char *_typecheck_strncpy(char * dst, const char * src, size_t n)
{
  int slen = strlen(src);
#ifndef TC_PTRW /* { */
  _tcptr_verifyPtr("",0,0,"(strncpy:src)", src, (slen < n)?(slen+1):n);
#endif /* } ifndef TC_PTRW */
  _tcptr_verifyPtr("",0,0,"(strncpy:dst)", dst, n);
  return strncpy(dst, src, n);
}


char *_typecheck_strcat(char * dst, const char * src)
{
  int slen = strlen(src);
  int dlen = strlen(dst);
#ifndef TC_PTRW /* { */
  _tcptr_verifyPtr("",0,0,"(strcat:src)", src, slen+1);
#endif /* } ifndef TC_PTRW */
  _tcptr_verifyPtr("",0,0,"(strcat:dst)", dst, dlen+slen+1);
  return strcat(dst, src);
}

char *_typecheck_strncat(char * dst, const char * src, size_t n)
{
  int slen = strlen(src);
  int dlen = strlen(dst);
#ifndef TC_PTRW /* { */
  _tcptr_verifyPtr("",0,0,"(strncat:src)", src, (slen < n)?(slen+1):n);
#endif /* } ifndef TC_PTRW */
  _tcptr_verifyPtr("",0,0,"(strncat:dst)", dst, dlen+((slen < n)?(slen+1):(n+1)));
  return strncat(dst, src, n);
}

size_t _typecheck_strxfrm(char *s1, const char * s2, size_t n)
{
  return strxfrm(s1, s2, n);
}


void _typecheck_bzero(void * s, size_t n)
{
  _tcptr_verifyPtr("",0,0,"(bzero)", s,n);
  bzero(s, n);
}


void *_typecheck_memset(void * s, int c, size_t n)
{
  _tcptr_verifyPtr("",0,0,"(memset)", s,n);
  return memset(s, c, n);
}

/* char *_typecheck_strerror(int) */
void *_typecheck_memccpy(void * s1, const void * s2, int c, size_t n)
{
  _tcptr_verifyPtr("",0,0,"(memccpy:s1)", s1,n);
#ifndef TC_PTRW /* { */
  _tcptr_verifyPtr("",0,0,"(memccpy:s2)", s2,n);
#endif /* } ifndef TC_PTRW */
  return memccpy(s1, s2, c, n);
}

/* char *_typecheck_strsignal(int) */
char *_typecheck_strdup(const char * s1)
{
  char * ret = strdup(s1);
/*should insert into malloc mhash?:*/
  if(ret) _tcptr_setTags(ret, strlen(ret)+1);
  return ret;
}



/****************************/
/******** langinfo.h ********/
/****************************/
extern char * _typecheck_nl_langinfo(nl_item item)
{
  char * ret = nl_langinfo(item);
  _tcptr_setTags(ret, strlen(ret)+1);
  return ret;
}


/****************************/
/******** sys/stat.h ********/
/****************************/
extern int _typecheck_stat(const char *path, struct stat *buf)
{
  _tcptr_setTags(buf, sizeof(struct stat));
  return stat(path, buf);
}

extern int _typecheck_lstat(const char *path, struct stat *buf)
{
  _tcptr_setTags(buf, sizeof(struct stat));
  return lstat(path, buf);
}

extern int _typecheck_fstat(int fildes, struct stat *buf)
{
  _tcptr_setTags(buf, sizeof(struct stat));
  return fstat(fildes, buf);
}


/************************/
/******** time.h ********/
/************************/
extern char *_typecheck_ctime(const time_t *clock)
{
  char * ret = ctime(clock);
  _tcptr_setTags(ret, strlen(ret)+1);
  return ret;
}

extern struct tm *_typecheck_localtime(const time_t *clock)
{
  struct tm * ret = localtime(clock);
  _tcptr_setTags(ret, sizeof(struct tm));
  return ret;
}

extern struct tm *_typecheck_gmtime(const time_t *clock)
{
  struct tm * ret = gmtime(clock);
  _tcptr_setTags(ret, sizeof(struct tm));
  return ret;
}

extern char *_typecheck_asctime(const struct tm *tm)
{
  char * ret = asctime(tm);
  _tcptr_setTags(ret, strlen(ret)+1);
  return ret;
}


/**************************/
/******** unistd.h ********/
/**************************/
extern char * _typecheck_getpass(const char * prompt)
{
  char * ret = getpass(prompt);
  _tcptr_setTags(ret, strlen(ret)+1);
  return ret;
}

extern int    _typecheck_brk(void *endds)
{
  return brk(endds);
}

extern void * _typecheck_sbrk(int incr)
{
  return sbrk(incr);
}


extern ssize_t _typecheck_read(int fildes, void *buf, size_t nbyte)
{
  return read(fildes, buf, nbyte);
}

extern ssize_t _typecheck_pread(int fildes, void *buf, size_t nbyte, off_t offset)
{
  return pread(fildes, buf, nbyte, offset);
}

extern ssize_t _typecheck_readv(int fildes, struct iovec *iov, int iovcnt)
{
  return readv(fildes, iov, iovcnt);
}


/*************************/
/******** netdb.h ********/
/****** sys/socket.h *****/
/*************************/
struct hostent *_typecheck_gethostbyname(const char *name)
{
  struct hostent * ret = gethostbyname(name);

  _tcptr_setTags(ret, sizeof(struct hostent));

  /* more here, TODO */

  return ret;
}

struct hostent *_typecheck_gethostbyaddr(const char *addr, int len, int type)
{
  struct hostent * ret = gethostbyaddr(addr, len, type);

  _tcptr_setTags(ret, sizeof(struct hostent));

  /* more here, TODO */

  return ret;
}


