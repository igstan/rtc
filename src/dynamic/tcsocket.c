#include <netdb.h>
#include <sys/socket.h>

#include "tcinternal.h"
#include <tclibc.h>

static struct hostent * init_hostent(struct hostent * he)
{
  if(he){
    int i;
    _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &he->h_name, _ctype_pointer);
    _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, he->h_name, strlen(he->h_name));

    _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &he->h_addr_list, _ctype_pointer);
    for(i = 0; he->h_addr_list[i]; ++i){
      _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		&he->h_addr_list[i], _ctype_pointer);
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		he->h_addr_list[i], strlen(he->h_addr_list[i]));
    }
    _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &he->h_addr_list[i], _ctype_pointer);

    _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &he->h_addrtype, _ctype_int);
    _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &he->h_length, _ctype_int);

    _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &he->h_aliases, _ctype_pointer);
    for(i = 0; he->h_aliases[i]; ++i){
      _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		&he->h_aliases[i], _ctype_pointer);
      _setStringTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol,
		he->h_aliases[i], strlen(he->h_aliases[i]));
    }
    _setScalarTag(_globalErrlocFile,_globalErrlocLine,_globalErrlocCol, &he->h_aliases[i], _ctype_pointer);
  }
  return he;
}
struct hostent *_typecheck_gethostbyname(const char *name)
{
  return init_hostent(gethostbyname(name));
}
struct hostent *_typecheck_gethostbyaddr(const char *addr, int len, int type)
{
  return init_hostent(gethostbyaddr(addr, len, type));
}



