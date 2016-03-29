#ifndef _GNU_HACKS_H
#define _GNU_HACKS_H

/********************/
/***** stdarg.h *****/
/********************/
/*
 * tc -va_arg will convert va_arg to TCvarg
 * here, we convert TCvarg back to va_arg, but
 * also pre-insert a dummy indicator.
 */
extern int _tc_varg_dummy;
#define TCvarg(x,y) (_tc_varg_dummy,va_arg(x,y))

/*
 * New annoyance: redhat GCC includes a reference
 * to __builtin_va_list, so here's a hack to try
 * to get rid of it 
 */
typedef void * __builtin_va_list;
extern int _tc_builtin_varg_dummy;
#define __builtin_va_arg(x,t) __builtin_va_arg(x,(t)_tc_builtin_varg_dummy)

/*************************/
/* DISABLE GNUC KEYWORDS */
/*************************/
#ifdef __GNUC__ /* { */

#undef __GNUC__
#define __GNUC__ 1

/*
#define __attribute__(x)
*/
#define __extension__
#define __inline
#define __restrict

#define __THROW

#define __P(args)     args
#define __PMT(args)   args
#define __const       const
#define __signed      signed
#define __volatile    volatile
#define __DOTS        , ...

#endif /* } __GNUC__ */

#endif	/* _GNU_HACKS_H */
