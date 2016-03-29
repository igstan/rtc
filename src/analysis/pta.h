#ifndef TC_PTA_H /* { */
#define TC_PTA_H

#include "ao.h"

//------------------------------------------------------

class PTanalysis {
  public:
    static void initializeECRbasic(AO& ao);
    static void initializeECRaggregate_collapseAlways(AO& ao);
    static void initializeECRaggregate_collapseOnCast(AO& ao);
    static void initializeECRpointers(AO& ao);

    static void analyzeAssigns_collapseAlways(suco_llist<TCassignEdge *>& assigns);
    static void analyzeAssigns_collapseOnCast(suco_llist<TCassignEdge *>& assigns);
};

//------------------------------------------------------

#endif /* } ifdef TC_PTA_H */
