#ifndef TC_DIAG_H /* { */
#define TC_DIAG_H

#include <stdio.h>
#include "ao.h"
#include "edge.h"
#include "suco.h"

//------------------------------------------------------

class TCstats
{
  public:

    static int new_ty_nodes;
    static int recy_ty_nodes;
    static int new_la_nodes;
    static int recy_la_nodes;
    static int num_pt_constraints;
    static int num_constraint_solving_visits;

    static int cde_incl_num_collapses;
    static int cde_incl_nodes_collapsed;
    static int cde_pt_num_collapses;
    static int cde_pt_nodes_collapsed;

    static int dfa_useless_expr_node_visits;
    static int dfa_useful_expr_node_visits;
    static int dfa_useless_cfg_node_visits;
    static int dfa_useful_cfg_node_visits;

    static int ranc_verify_visits;
    static int ranc_verify_visit_factsizes;
    static int ranc_verify_visit_max_factsize;

    static int mbuc_verify_visits;
    static int mbuc_verify_visit_factsizes;
    static int mbuc_verify_visit_max_factsize;

    static void doAO(AO& ao);
    static void doAssigns(FILE * outf, suco_llist<TCassignEdge *>& assigns);
    static void print(FILE * outf);

    static void timer(const char * descr); //- nonzero descr = start timer
					   //- null descr = end timer, print delta
    static void TCstats::print_now_time(FILE * outf); // prints the current time
  private:
    TCstats();
    static int	num_aos,
		num_addrof,
		num_deref,
		num_unsafe,
		num_tracked;
    static int	num_malloc,
		num_malloc_constsize,
		num_malloc_evalsize;
    static int	num_poss_bottom,
		num_poss_scalar,
		num_poss_zero,
		num_poss_top;
    static int	num_safe_ptr,
		num_unsafe_ptr,
		num_safe_struct_ptr,
		num_unsafe_struct_ptr,
		num_safe_written_ptr,
		num_unsafe_written_ptr,
		num_safe_struct_written_ptr,
		num_unsafe_struct_written_ptr,
		num_safe_fnptr,
		num_unsafe_fnptr,
		num_safe_struct_fnptr,
		num_unsafe_struct_fnptr,
		num_tracked_loc,
		num_untracked_loc,
		num_tracked_fnptr,
		num_untracked_fnptr,
		num_tracked_struct_fnptr,
		num_untracked_struct_fnptr;
};

//------------------------------------------------------

#endif /* } ifndef TC_DIAG_H */

