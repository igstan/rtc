#ifndef TC_INTERVAL_H /* { */
#define TC_INTERVAL_H

#include <limits.h> //- for INT_MIN, INT_MAX
#include <stdio.h>

class AO;

class Interval
{
  public:

    static const int PLUS_INF = INT_MAX;
    static const int MINUS_INF = INT_MIN;

    Interval()
	: tgt_ao(0), tgt_elty(0), tgt_numelts(0), min(MINUS_INF), max(PLUS_INF),
	  has_zero(false), tgt_readonly(false) {}
    Interval(int c)
	: tgt_ao(0), tgt_elty(0), tgt_numelts(0), min(c), max(c),
	  has_zero(false), tgt_readonly(false) {}
    Interval(int mn, int mx)
	: tgt_ao(0), tgt_elty(0), tgt_numelts(0), min(mn), max(mx),
	  has_zero(false), tgt_readonly(false) {}
    Interval(AO& ao, int mn = 0, int mx = 0)
	: tgt_ao(0), tgt_elty(0), tgt_numelts(0), min(mn), max(mx),
	  has_zero(false), tgt_readonly(false)
	  { setTgt(ao.normalize()); }
    Interval(Interval& iv, int mn, int mx) // This is a specialized constructor, used only by RANfact::evalPredExpr: case 'S'/'V'
	: tgt_ao(iv.tgt_ao), tgt_elty(iv.tgt_elty), tgt_numelts(iv.tgt_numelts), min(mn), max(mx),
	  has_zero(iv.has_zero), tgt_readonly(iv.tgt_readonly) {}

    bool operator==(Interval iv) const
	{ return (this->tgt_ao == iv.tgt_ao) &&
		 (this->tgt_elty == iv.tgt_elty) &&
		 (this->tgt_numelts == iv.tgt_numelts) &&
		 (this->min == iv.min) &&
		 (this->max == iv.max) &&
		 (this->has_zero == iv.has_zero) &&
		 (this->tgt_readonly == iv.tgt_readonly) ;
	}

    bool isNullTgted() const { return (tgt_elty == 0); }
    AO * tgtAO() const { return tgt_ao; }
    int Min() const { return min; }
    int Max() const { return max; }

    void debug_dump(FILE * outf) const;

    bool inBounds(bool iswrite) const;
    void adjustStructOffset(suco_llist<TCtype *>& tylist);

    bool disableIfTargetIsPartOf(AO& ao);

    bool isTop() const;
    bool isBottom() const;

    bool isTargetedFinite() const;
    bool isTargetedHalfFinite() const;

    static const Interval Top;
    static const Interval Bottom;

    void setTop();
    void setBottom();

    bool widen(Interval iv);
    bool narrow(Interval iv);
    bool meet(Interval iv); //- "superset of union"
    bool join(Interval iv); //- "subset of intersect"
    bool cond_intersect(Interval iv); //- "superset of intersect"

    void slide_if_constant(); //- A[s]+[i,i] ==> A[s-i]+[0,0]  (if i < s)

    Interval plus(Interval iv, TCtype * elty) const;  //- > elty=0 means "use underlying type"
    Interval minus(Interval iv, TCtype * elty) const; //- > elty=tcVoid means plain (non-pointer) arithmetic
    Interval ptrs_minus(Interval iv, TCtype& elty) const; //- ptr minus ptr (currently not handled precisely)
    Interval times(Interval iv) const;
    Interval divide(Interval iv) const;
    Interval modulo(Interval iv) const;

  private:
    AO * tgt_ao;	//- used only if flag_range_target_aos == true
    TCtype * tgt_elty;
    int tgt_numelts;

    int min;
    int max;
    bool has_zero;	//- used only when flag_range_zero_special is on; meaningful only if tgt_ao/tgt_elty != 0
    bool tgt_readonly;	//- indicates if the target is readonly, specifically string literals

    void setTgt(AO& ao); //- used by constructor
    void setTgt(TCtype& ty);

    bool minmax_union(int min, int max); //- returns true if changed

    //- return true if OK; false if failed
    static bool adjustTypes(Interval& iv1, Interval& iv2, TCtype * elty, bool addition);

    int maxpos() const;
    int maxneg() const;
    int minpos() const;
    int minneg() const;
    bool inclzero() const;
    bool iszero() const;
};

#endif /* } ifndef TC_INTERVAL_H */
