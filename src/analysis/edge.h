#ifndef TC_EDGE_H /* { */
#define TC_EDGE_H

#include <stdio.h>

class AO;
class TCtype;

//------------------------------------------------------

class TCassignEdge {
  public:
    friend class TCassignEdgeSet;
    TCassignEdge(AO& t, AO& f, TCtype& ty) : to(t), from(f), type(ty) {}
    
    AO& getTo() const { return to; }
    AO& getFrom() const { return from; }
    TCtype& getType() const { return type; }

    void debug_dump(FILE * os);

    static int compare(TCassignEdge * le, TCassignEdge * re); // used by AO/suco_set

  private:
    TCassignEdge();
    AO& to;
    AO& from;
    TCtype& type;
};

//------------------------------------------------------

#endif /* } ifdef TC_EDGE_H */
