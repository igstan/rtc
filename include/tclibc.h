#ifndef _TYPECHECK_LIBC_H
#define _TYPECHECK_LIBC_H

/**************************/
/******** stdlib.h ********/
/**************************/

/* Include size_t typedef, so we don't have to include stdlib.h. */
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned int    size_t;
#endif

/*************************/
/******** stdio.h ********/
/*************************/

extern char   * _typecheck_fgets(char *, int, FILE *);
extern char   * _typecheck_gets(char *);
extern size_t   _typecheck_fread(void *, size_t, size_t, FILE *);

extern int _typecheck_scanf(const char * format, ...);
extern int _typecheck_fscanf(FILE * stream, const char * format, ...);
extern int _typecheck_sscanf(const char * str, const char * format, ...);
extern int _typecheck_pctn_printf(const char * format, ...);
extern int _typecheck_pctn_fprintf(FILE * f, const char * format, ...);
extern int _typecheck_sprintf(char * str, const char * format, ...);
extern int _typecheck_snprintf(char * str, size_t size, const char * format, ...);

/**************************/
/******** malloc.h ********/
/**************************/

extern void * _typecheck_malloc(size_t size);
extern void * _typecheck_calloc(size_t nelem, size_t elsize);
extern void   _typecheck_free(void * ptr);
extern void   _typecheck_free_partial(void * ptr);
extern void * _typecheck_memalign(size_t alignment, size_t size);
extern void * _typecheck_realloc(void * ptr, size_t size);
extern void * _typecheck_valloc(size_t size);


/**************************/
/******** string.h ********/
/**************************/

extern void *_typecheck_memcpy(void *, const void *, size_t);
extern void *_typecheck_memmove(void *, const void *, size_t);
extern char *_typecheck_strcpy(char *, const char *);
extern char *_typecheck_strncpy(char *, const char *, size_t);

extern char *_typecheck_strcat(char *, const char *);
extern char *_typecheck_strncat(char *, const char *, size_t);
extern size_t _typecheck_strxfrm(char *, const char *, size_t);

extern void _typecheck_bzero(void *, size_t);

extern void *_typecheck_memset(void *, int, size_t);
/* extern char *_typecheck_strerror(int); */
extern void *_typecheck_memccpy(void *, const void *, int, size_t);
/* extern char *_typecheck_strsignal(int); */
extern char *_typecheck_strdup(const char *);


/****************************/
/******** langinfo.h ********/
/****************************/
#include <langinfo.h>
extern char * _typecheck_nl_langinfo(nl_item item);

/****************************/
/******** sys/stat.h ********/
/****************************/
#include <sys/stat.h>
extern int _typecheck_stat(const char *path, struct stat *buf);
extern int _typecheck_lstat(const char *path, struct stat *buf);
extern int _typecheck_fstat(int fildes, struct stat *buf);

/************************/
/******** time.h ********/
/************************/
#include <time.h>
extern char *_typecheck_ctime(const time_t *clock);
extern struct tm *_typecheck_localtime(const time_t *clock);
extern struct tm *_typecheck_gmtime(const time_t *clock);
extern char *_typecheck_asctime(const struct tm *tm);

/**************************/
/******** unistd.h ********/
/**************************/
extern char * _typecheck_getpass(const char * prompt);
extern int    _typecheck_brk(void *endds);
extern void * _typecheck_sbrk(int incr);

#include <sys/types.h>
#include <sys/uio.h>
extern ssize_t _typecheck_read(int fildes, void *buf, size_t nbyte);
extern ssize_t _typecheck_pread(int fildes, void *buf, size_t nbyte, off_t offset);
extern ssize_t _typecheck_readv(int fildes, struct iovec *iov, int iovcnt);

/*************************/
/******** netdb.h ********/
/****** sys/socket.h *****/
/*************************/
#include <netdb.h>
#include <sys/socket.h>
struct hostent *_typecheck_gethostbyname(const char *name);
struct hostent *_typecheck_gethostbyaddr(const char *addr, int len, int type);

#endif /* #ifndef TYPECHECK_LIBC_H */
