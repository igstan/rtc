/* - This file describes function renamings,
 *   for main, and for library functions that
 *   have typecheck versions written.
 * - This file is to be included by each to-be-
 *   instrumented file BEFORE being instrumented.
 * - This file is NOT to be included by any
 *   runtime code.
 */

#ifndef _TYPECHECK_H
#define _TYPECHECK_H

/***************************************/
/************ typecheck API ************/
/***************************************/
#ifdef TC_VERIFYPTR_CHECK /* { */
#include <tcptr.h>
#else /* } ifndef TC_VERIFYPTR_CHECK { */
#include <tcapi.h>
#endif /* } !define TC_VERIFYPTR_CHECK */

/**********************************************/
/************ Rename main function ************/
/**********************************************/
#ifdef TC_PREINSTR /* { */
#define main _prog_main 
#endif /* } TC_PREINSTR */

/********************/
/***** stdio.h *****/
/********************/
#ifdef TC_PREINSTR /* { */
#ifdef TC_STDIO
#define fgets _typecheck_fgets 
#define gets _typecheck_gets 
#define fread _typecheck_fread 
#endif /* TC_STDIO */

#ifdef TC_SCANF
#define scanf _typecheck_scanf 
#define fscanf _typecheck_fscanf 
#define sscanf _typecheck_sscanf 
#endif /* TC_SCANF */

#ifdef TC_PRINTF
#define sprintf _typecheck_sprintf 
#define snprintf _typecheck_snprintf 
#endif /* TC_PRINTF */

#ifdef TC_PCTN_PRINTF
extern int printf();
extern int fprintf();

#define printf _typecheck_pctn_printf 
#define fprintf _typecheck_pctn_fprintf 
#endif /* TC_PCTN_PRINTF */

#endif /* } TC_PREINSTR */

/********************/
/***** malloc.h *****/
/********************/
#ifdef TC_PREINSTR /* { */
#ifdef TC_MALLOC
/* uninstrumented versions! */
void *malloc(size_t size);
void *calloc(size_t nelem, size_t elsize);
void free(void *ptr);
void *memalign(size_t alignment, size_t size);
void *realloc(void *ptr, size_t size);
void *valloc(size_t size);

#define malloc _typecheck_malloc 
#define calloc _typecheck_calloc 
#define free _typecheck_free 
#define memalign _typecheck_memalign 
#define realloc _typecheck_realloc 
#define valloc _typecheck_valloc 
#endif /* TC_MALLOC */
#endif /* } TC_PREINSTR */

/*******************/
/***** ctype.h *****/
/*******************/
/* this works only on Linux; no portable anti-macro mechanism? */
#define __NO_CTYPE

/********************/
/***** string.h *****/
/********************/
#ifdef TC_PREINSTR /* { */
#ifdef TC_STRING
/* note: the following (bcopy) is a hack to map bcopy to memcpy.
         In practice, bcopy should occur in two places:
         1) a function prototype, in which case the '*' makes
            the signature match that of memcpy;
         2) a function call in a void context, in which case
            the '*' will at most generate a "void value not
            ignored" warning (right?).
*/
#define bcopy(s,d,z) *_typecheck_memcpy(d,s,z)

#if defined(TC_INLINE_BZERO) /* { */
#define _tc_bzero(s,n) do { size_t i, nn = (n); for(i = 0; i < nn; ++i) ((char *)(s))[i] = 0; } while(0)
#else /* } !defined(TC_INLINE_BZERO) { */
#define _tc_bzero _typecheck_bzero
#endif /* } !defined(TC_INLINE_BZERO) */

#define bzero _typecheck_bzero
#define memcpy _typecheck_memcpy
#define memmove _typecheck_memmove
#define strcpy _typecheck_strcpy
#define strncpy _typecheck_strncpy
#define strcat _typecheck_strcat
#define strncat _typecheck_strncat
#define strxfrm _typecheck_strxfrm
#define memset _typecheck_memset
/* #define strerror _typecheck_strerror */
#define memccpy _typecheck_memccpy
/* #define strsignal _typecheck_strsignal */
#define strdup _typecheck_strdup
#endif /* TC_STRING */
#endif /* } TC_PREINSTR */

/*******************/
/***** wchar.h *****/
/*******************/
#ifdef TC_PREINSTR /* { */
/*
#define wcscpy  _typecheck_wcscpy 
#define wcpcpy  _typecheck_wcpcpy 
#define wcscat  _typecheck_wcscat 
#define wcsdup  _typecheck_wcsdup 
#define wmemcpy _typecheck_wmemcpy 
*/
#endif /* } TC_PREINSTR */

/****************************/
/***** misc library fns *****/
/****************************/
#ifdef TC_PREINSTR /* { */
/*#ifdef TC_LIBC*/
#define nl_langinfo _typecheck_nl_langinfo 
#define stat _typecheck_stat 
#define lstat _typecheck_lstat 
#define fstat _typecheck_fstat 
#define ctime _typecheck_ctime
#define localtime _typecheck_localtime
#define gmtime _typecheck_gmtime
#define asctime _typecheck_asctime
#define getpass _typecheck_getpass
/*
#define brk _typecheck_brk
#define sbrk _typecheck_sbrk
*/
#define read _typecheck_read
#define pread _typecheck_pread
/*
#define readv _typecheck_readv
*/
#define gethostbyname _typecheck_gethostbyname
#define gethostbyaddr _typecheck_gethostbyaddr
/*#endif*/ /* TC_LIBC */
#endif /* } TC_PREINSTR */

/********************************/
/* undo library instrumentation */
/********************************/

#if !defined(TC_PREINSTR) /* { */

#if defined(TC_NO_HEAPTAG) /* { */
#define _typecheck_malloc	malloc
#define _typecheck_calloc	calloc
#define _typecheck_free		free
#define _typecheck_memalign	memalign
#define _typecheck_realloc	realloc
#define _typecheck_valloc	valloc
#endif /* } defined(TC_NO_HEAPTAG) */

/* Time to cheat: in vuln mode, we will allow a	*/
/* sensitive location to be overwritten with 0	*/
#if defined(TC_NO_LIB) || defined(TC_VULNERABLE_VP) /* { */
#define _typecheck_bzero	bzero
#endif /* } defined(TC_NO_LIB) || defined(TC_VULNERABLE_VP) */

#if defined(TC_NO_LIB) /* { */

#define _typecheck_fgets	fgets
#define _typecheck_gets		gets
#define _typecheck_fread	fread
#define _typecheck_scanf	scanf
#define _typecheck_fscanf	fscanf
#define _typecheck_sscanf	sscanf
#define _typecheck_sprintf	sprintf
#define _typecheck_snprintf	snprintf
#define _typecheck_memcpy	memcpy
#define _typecheck_memmove	memmove
#define _typecheck_strcpy	strcpy
#define _typecheck_strncpy	strncpy
#define _typecheck_strcat	strcat
#define _typecheck_strncat	strncat
#define _typecheck_strxfrm	strxfrm
#define _typecheck_memset	memset
#define _typecheck_strerror	strerror
#define _typecheck_memccpy	memccpy
#define _typecheck_strsignal	strsignal
#define _typecheck_strdup	strdup
#define _typecheck_wcscpy	wcscpy 
#define _typecheck_wcpcpy	wcpcpy 
#define _typecheck_wcscat	wcscat 
#define _typecheck_wcsdup	wcsdup 
#define _typecheck_wmemcpy	wmemcpy
#define _typecheck_nl_langinfo	nl_langinfo
#define _typecheck_stat		stat
#define _typecheck_lstat	lstat
#define _typecheck_fstat	fstat
#define _typecheck_ctime	ctime
#define _typecheck_localtime	localtime
#define _typecheck_gmtime	gmtime
#define _typecheck_asctime	asctime
#define _typecheck_getpass	getpass
#define _typecheck_brk		brk
#define _typecheck_sbrk		sbrk
#define _typecheck_read		read
#define _typecheck_pread	pread
#define _typecheck_readv	readv
#define _typecheck_gethostbyname	gethostbyname
#define _typecheck_gethostbyaddr	gethostbyaddr

#else /* } !defined(TC_NO_LIB) { */

/* declare de-_typecheck_-ed prototypes */
/* see analysis/tca.cpp: TCAstate::interestingLibFnSignatures() */
extern char * fgets();
extern char * gets();
extern size_t fread();
extern int scanf();
extern int fscanf();
extern int sscanf();
extern int sprintf();
extern int snprintf();
extern void bzero();
extern void * memcpy();
extern void * memmove();
extern char * strcpy();
extern char * strncpy();
extern char * strcat();
extern char * strncat();
/* extern size_t strxfrm();	*/
extern void * memset();
/* extern char * strerror();	*/
extern void * memccpy();
/* extern char * strsignal();	*/
/* extern char * strdup();	*/
/* wcscpy  */
/* wcpcpy  */
/* wcscat  */
/* wcsdup  */
/* wmemcpy */
/* extern char * nl_langinfo();	*/
/* extern int stat();		*/
/* extern int lstat();		*/
/* extern int fstat();		*/
/* extern char * ctime();	*/
/* extern struct tm * localtime();	*/
/* extern struct tm * gmtime();	*/
/* extern char * asctime();	*/
/* extern char * getpass();	*/
/* extern int brk();		*/
/* extern void * sbrk();	*/
/* extern ssize_t read();	*/
/* extern ssize_t pread();	*/
/* extern ssize_t readv();	*/
/* extern struct hostent * gethostbyname();	*/
/* extern struct hostent * gethostbyaddr();	*/

#endif /* } !defined(TC_NO_LIB) */

#endif /* } !defined(TC_PREINSTR) */

/*************************************************/

/**************************************/
/* GNU Hacks for stdarg, gnu keywords */
/**************************************/
#ifdef TC_PREINSTR /* { */
#include "gnu-hacks.h"
#endif /* } TC_PREINSTR */

/* Used as the marker for AST instrumentation to
   disregard the contents of typecheck.h. */
#ifdef TC_PREINSTR /* { */
int _tc_h_end;
#endif /* } TC_PREINSTR */

#endif	/* _TYPECHECK_H */
