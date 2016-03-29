#include <stdio.h>
#include "edge.h"
#include "ao.h"

//------------------------------------------------------

void TCassignEdge::debug_dump(FILE * os)
{
  fprintf(os, "ASSIGN[");
  this->getType().debug_dump(os);
  fprintf(os, "]:(");
  this->getFrom().debug_dump(os);
  fprintf(os, ") -> (");
  this->getTo().debug_dump(os);
  fprintf(os, ")\n");
}

int TCassignEdge::compare(TCassignEdge * le, TCassignEdge * re)
{
  long fromcmp = (long)&le->getFrom() - (long)&re->getFrom();
  if(fromcmp) return fromcmp;
  else {
    long tocmp = (long)&le->getTo() - (long)&re->getTo();
    if(tocmp) return tocmp;
    else return (long)&le->getType() - (long)&re->getType();
  }
}

//------------------------------------------------------
