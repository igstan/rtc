#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "tcptr_internal.h"
#include <tclibc.h>

#define TCSC_LONG    0x001   /* l: long or double */
#define TCSC_LONGDBL 0x002   /* L: long long or long double */
#define TCSC_SHORT   0x004   /* h: short */
#define TCSC_CHAR    0x200   /* hh: char */

struct tcpf_arg_info {
  int posn;
  void * address;
  int elsize;
  struct tcpf_arg_info * next;
};

#define TC_PRINTF_BUFFER_SIZE 20

int read_positional_param(const char ** cp)
{
  if(isdigit(**cp)){
    const char * d = *cp;
    int posn = 0;
    while(isdigit(*d)) posn = posn * 10 + (*d++ - '0');
    if(*d == '$'){
      *cp = d+1;
      return posn;
    }
  }
  return 0;
}

static struct tcpf_arg_info * process_format_string(const char * format)
{
  static struct tcpf_arg_info * listhead = 0;
  static struct tcpf_arg_info * freelist = 0;
  int sc_plain = 0;
  int sc_positional = 0;
  int curposn = 0;
  const char * c = format;

  /* clear listhead */
  while(listhead){
    struct tcpf_arg_info * hnode = listhead;
    listhead = listhead->next;
    hnode->next = freelist;
    freelist = hnode;
  }

  curposn = 0;
  c = format;
  while(*c){
    if(*c++ == '%'){

      int mod_flags = 0;
      
      { /* Check for a positional parameter specification.  */
        int posn = read_positional_param(&c);
        if(posn){
          sc_positional = 1;
          curposn = posn;
        } else {
          sc_plain = 1;
          curposn++;
        }
      }

      /* Check for flag */
      switch(*c){
        case '-':
        case '+':
        case ' ':
        case '#':
        case '0':
          c++;
      }

      /* field width. */
      if(*c == '*'){
        int posn;
        c++;
        posn = read_positional_param(&c);
        if(posn){
          sc_positional = 1;
        } else {
          sc_plain = 1;
	  curposn++;
        }
      } else {
        while (isdigit (*c)) c++;
      }

      /* precision. */
      if(*c == '.'){
        c++;
        if(*c == '*'){
          int posn;
          c++;
          posn = read_positional_param(&c);
          if(posn){
            sc_positional = 1;
          } else {
            sc_plain = 1;
	    curposn++;
          }
        } else {
          while (isdigit (*c)) c++;
        }
      }

      /* Check for type modifiers.  */
      switch (*c++) {
	case 'h': /* ints are short ints or chars.  */
	  if (*c == 'h') {
	      ++c;
	      mod_flags |= TCSC_CHAR;
	  } else
	    mod_flags |= TCSC_SHORT;
	  break;
	case 'l':
	  if (*c == 'l') { /* A double `l' is equivalent to an `L'.  */
	      ++c;
	      mod_flags |= TCSC_LONGDBL;
	  } else /* ints are long ints.  */
	    mod_flags |= TCSC_LONG;
	  break;
	case 'L':
	  /* doubles are long doubles, and ints are long long ints.  */
	  mod_flags |= TCSC_LONGDBL;
	  break;
	default:
	  /* Not a recognized modifier.  Back up.  */
	  --c;
	  break;
      }

      /* End of the format string?  */
      if(!*c){
	/* ERROR: malformed format string */
        _output_flce(__FILE__,__LINE__,0,"(_typecheck_printf0:process_format_string)","malformed format string");

        return listhead;
      }

      /* Find the conversion specifier.  */
      switch (*c++){
	case '%':
	  break;

	case 'n': {
	    struct tcpf_arg_info * newnode;
	    if(freelist){
	      newnode = freelist;
	      freelist = freelist->next;
            } else {
	      newnode = (struct tcpf_arg_info *) malloc(sizeof(struct tcpf_arg_info));
            }
	    newnode->posn = curposn;
	    newnode->address = 0;
	    newnode->elsize = 0;
	    newnode->next = listhead;
	    listhead = newnode;

	    if (mod_flags & TCSC_LONGDBL)
              newnode->elsize = sizeof(long long);
	    else if (mod_flags & TCSC_LONG)
              newnode->elsize = sizeof(long);
	    else if (mod_flags & TCSC_SHORT)
              newnode->elsize = sizeof(short);
	    else
              newnode->elsize = sizeof(int);
            
	  } break;

	case 'c':	/* Match characters.  */
	case 'C':
	case 's':	/* Read a string.  */
	case 'S':

	case 'x':	/* Hexadecimal integer.  */
	case 'X':	/* Ditto.  */
	case 'o':	/* Octal integer.  */
	case 'u':	/* Unsigned decimal integer.  */
	case 'd':	/* Signed decimal integer.  */
	case 'i':	/* Generic number.  */

	case 'e':	/* Floating-point numbers.  */
	case 'E':
	case 'f':
	case 'g':
	case 'G':
	case 'a':
	case 'A':

	case 'p':	/* Generic pointer.  */

	default:
          break;
      } /* end switch (*c++) */
    } /* end if(*c++ == '%') */
  }
  if(sc_positional && sc_plain){
    /* WARNING: both positional and non-positional params given */
    _output_flce(__FILE__,__LINE__,0,"(_typecheck_printf:process_format_string)", "positional and non-positional mix");
  }
  return listhead;
}

void printf_preprocess(va_list ap, struct tcpf_arg_info * in_ailist)
{
  struct tcpf_arg_info * ailist = in_ailist;

  /* do selection sort */
  {
    struct tcpf_arg_info * newlist = 0;

    while(ailist){
      struct tcpf_arg_info ** maxai = &ailist;
      struct tcpf_arg_info ** iter;

      for(iter = &ailist; *iter; iter = &(*iter)->next){
        if((*maxai)->posn < (*iter)->posn)
          maxai = iter;
      }
      {
        struct tcpf_arg_info * tmp;
        tmp = *maxai;
        *maxai = tmp->next;
        tmp->next = newlist;
        newlist = tmp;
      }
    }

    ailist = newlist;
  }

  { /* collect arg addresses */
    struct tcpf_arg_info * iter;
    int posn = 1;
    for(iter = ailist; iter; iter = iter->next){
      while(posn++ < iter->posn){
        int ignore = va_arg(ap, int);
      }
      iter->address = va_arg(ap, void *);
      _tcptr_verifyPtr("",0,0,"(*printf)", iter->address, iter->elsize);
    }
  }
}

int _typecheck_pctn_printf(const char * format, ...)
{
  va_list ap;
  int ret;

  struct tcpf_arg_info * ailist;

  ailist = process_format_string(format);

  if(ailist){
    va_start(ap, format);
    printf_preprocess(ap, ailist);
    va_end(ap);
  }

  /* actual call */
  va_start(ap, format);
  ret = vprintf(format, ap);
  va_end(ap);

  return ret;
}

int _typecheck_pctn_fprintf(FILE * f, const char * format, ...)
{
  va_list ap;
  int ret;

  struct tcpf_arg_info * ailist;

  ailist = process_format_string(format);
      
  if(ailist){
    va_start(ap, format);
    printf_preprocess(ap, ailist);
    va_end(ap);
  }

  /* actual call */
  va_start(ap, format);
  ret = vfprintf(f, format, ap);
  va_end(ap);

  return ret;
}

int _typecheck_sprintf(char * str, const char * format, ...)
{
  va_list ap;
  int ret;

  struct tcpf_arg_info * ailist;

  ailist = process_format_string(format);
      
  if(ailist){
    va_start(ap, format);
    printf_preprocess(ap, ailist);
    va_end(ap);
  }

  /* actual call */
  va_start(ap, format);
  ret = vsprintf(str, format, ap);
  va_end(ap);

  if(ret > 0) _tcptr_verifyPtr("",0,0,"(sprintf)", str, strlen(str)+1);

  return ret;
}

int _typecheck_snprintf(char * str, size_t size, const char * format, ...)
{
  va_list ap;
  int ret;

  struct tcpf_arg_info * ailist;

  ailist = process_format_string(format);
      
  if(ailist){
    va_start(ap, format);
    printf_preprocess(ap, ailist);
    va_end(ap);
  }

  /* actual call */
  va_start(ap, format);
  ret = vsnprintf(str, size, format, ap);
  va_end(ap);

  if(ret > 0) _tcptr_verifyPtr("",0,0,"(snprintf)", str, strlen(str)+1);

  return ret;
}

