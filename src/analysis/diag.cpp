#include <stdio.h>
#include <string.h>
#include <sys/resource.h> /* for getrusage timing stuff */
#include <time.h> // for time functions used in print_now_time()

#include "flags.h"
#include "diag.h"
#include "ao.h"
#include "ecr.h"
#include "ty.h"
#include "cfg.h"

//------------------------------------------------------
//- TCstats

int TCstats::new_la_nodes = 0,
    TCstats::recy_la_nodes = 0;

int TCstats::num_pt_constraints = 0,
    TCstats::num_constraint_solving_visits = 0;

int TCstats::cde_incl_num_collapses = 0,
    TCstats::cde_incl_nodes_collapsed = 0,
    TCstats::cde_pt_num_collapses = 0,
    TCstats::cde_pt_nodes_collapsed = 0;

int TCstats::dfa_useless_expr_node_visits = 0,
    TCstats::dfa_useful_expr_node_visits = 0,
    TCstats::dfa_useless_cfg_node_visits = 0,
    TCstats::dfa_useful_cfg_node_visits = 0;

int TCstats::ranc_verify_visits = 0,
    TCstats::ranc_verify_visit_factsizes = 0,
    TCstats::ranc_verify_visit_max_factsize = 0;

int TCstats::mbuc_verify_visits = 0,
    TCstats::mbuc_verify_visit_factsizes = 0,
    TCstats::mbuc_verify_visit_max_factsize = 0;

int TCstats::num_aos = 0,
    TCstats::num_addrof = 0,
    TCstats::num_deref = 0,
    TCstats::num_unsafe = 0,
    TCstats::num_tracked = 0;

int TCstats::num_malloc = 0,
    TCstats::num_malloc_constsize = 0,
    TCstats::num_malloc_evalsize = 0;

int TCstats::num_poss_bottom = 0,
    TCstats::num_poss_scalar = 0,
    TCstats::num_poss_zero = 0,
    TCstats::num_poss_top = 0;

int TCstats::num_safe_ptr = 0,
    TCstats::num_unsafe_ptr = 0,
    TCstats::num_safe_struct_ptr = 0,
    TCstats::num_unsafe_struct_ptr = 0,
    TCstats::num_safe_written_ptr = 0,
    TCstats::num_unsafe_written_ptr = 0,
    TCstats::num_safe_struct_written_ptr = 0,
    TCstats::num_unsafe_struct_written_ptr = 0,
    TCstats::num_safe_fnptr = 0,
    TCstats::num_unsafe_fnptr = 0,
    TCstats::num_safe_struct_fnptr = 0,
    TCstats::num_unsafe_struct_fnptr = 0,
    TCstats::num_tracked_loc = 0,
    TCstats::num_untracked_loc = 0,
    TCstats::num_tracked_fnptr = 0,
    TCstats::num_untracked_fnptr = 0,
    TCstats::num_tracked_struct_fnptr = 0,
    TCstats::num_untracked_struct_fnptr = 0;

void TCstats::doAO(AO& ao)
{
  if(ao.isLib()) return; //- skip library objects

  num_aos++;
  if(ao.getKind() == AO::aoAddrOf) num_addrof++;
  if(ao.getKind() == AO::aoStar) num_deref++;
  switch(ao.getTScode()){
    case 'U': num_unsafe++; break;
    case 'T': num_tracked++; break;
  }
  if(ao.getKind() == AO::aoMalloc){
    num_malloc++;
    if(ao.getStaticType() && ao.getStaticType()->getKind() != TCtype::tcVoid)
      num_malloc_constsize++;
    if(((AOMalloc&)ao).getMallocType()
	&& ((AOMalloc&)ao).getMallocType()->getKind() != TCtype::tcVoid)
      num_malloc_evalsize++;
  }

  switch(ao.getECR().possType().getKind()){
    case TClatType::tclBottom:
	 num_poss_bottom++; break;
    case TClatType::tclZero:
	 num_poss_zero++; break;
    case TClatType::tclTop:
	 num_poss_top++; break;
    default: //- scalar type
	 num_poss_scalar++; break;
  }

  switch(ao.getKind()){
    case AO::aoSDot:
    case AO::aoUDot: {
	 //- if ao is a pointer, and it is a direct decendent of an Id,
	 //  then count as pointer declaration
	 AO * dao = ao.find(AO::aoStar);
	 if(dao){ //- this indicates that ao is a pointer (note: static type not set for sdot/udot aos)
	   //- find root container
	   AO * pao = &ao;
	   do {
	     pao = (pao->getKind() == (AO::aoSDot)) ? &((AOSDot *)pao)->getParent()
						    : &((AOUDot *)pao)->getParent();
	   } while(pao->getKind() == AO::aoSDot || pao->getKind() == AO::aoUDot);

	   if(pao->getKind() == AO::aoId){

	     //- identify fnptr by presence of return ao
	     if(dao->find(AO::aoReturn)){
	       //-- count safe/unsafe
	       if(dao->getTSC() == AO::TSC_POSS_INVALID)
	         num_unsafe_struct_fnptr++;
	       else
	         num_safe_struct_fnptr++;
	       //-- count tracked/untracked
	       // NOTE: this count is technically inaccurate in current implementation,
	       //       since any tracked member of a struct will force the whole thing
	       //       to be tracked -- but this should be a measure of "potential" benefit
	       if(ao.getTSC() == AO::TSC_SAFE)
		 num_untracked_struct_fnptr++;
	       else
		 num_tracked_struct_fnptr++;
	     } else { //- non-function pointer
	       //- non-function pointer
	       if(dao->getTSC() == AO::TSC_POSS_INVALID)
	         num_unsafe_struct_ptr++;
	       else
	         num_safe_struct_ptr++;
	       if(dao->isAssigned()){
	         if(dao->getTSC() == AO::TSC_POSS_INVALID)
	           num_unsafe_struct_written_ptr++;
	         else
	           num_safe_struct_written_ptr++;
	       }
	     }
	   }
	 }
      } break;
    case AO::aoId: {
	 //- categorize pointers
	 AO * dao = ao.find(AO::aoStar);
	 if(dao){ //- this indicates that ao is a pointer (note: no need to use static type)
	   if(!ao.find(AO::aoFunction)){
	     //- identify fnptr by presence of return ao
	     if(dao->find(AO::aoReturn)){
	       //-- count safe/unsafe
	       if(dao->getTSC() == AO::TSC_POSS_INVALID)
	         num_unsafe_fnptr++;
	       else
	         num_safe_fnptr++;
	       //-- count tracked/untracked
	       if(ao.getTSC() == AO::TSC_SAFE)
		 num_untracked_fnptr++;
	       else {
		 num_tracked_fnptr++;
/**/fprintf(stderr, "TRACKED FNPTR: ");
/**/ao.dump_descr(stderr);
/**/fprintf(stderr, "\n");
               }
	       break; // function pointer: don't fallthrough!
	     } else { //- non-function pointer
	       //- non-function pointer
	       if(dao->getTSC() == AO::TSC_POSS_INVALID)
	         num_unsafe_ptr++;
	       else
	         num_safe_ptr++;
	       if(dao->isAssigned()){
	         if(dao->getTSC() == AO::TSC_POSS_INVALID)
	           num_unsafe_written_ptr++;
	         else
	           num_safe_written_ptr++;
	       }
	     }
	   } else { // regular function: don't fallthrough!
	     break;
	   }
	 }
      } // fallthrough (if id is not function or function pointer)
    case AO::aoStringLit:
    case AO::aoMalloc: { // count tracked/untracked locations
	 if(ao.getTSC() == AO::TSC_SAFE)
	   num_untracked_loc++;
	 else
	   num_tracked_loc++;
      } break;
/*
    case AO::aoReturn:
    case AO::aoArg:
    case AO::aoStar:
    case AO::aoValue:
    case AO::aoOp:
    case AO::aoExt:
    case AO::aoAddrOf:
    case AO::aoFunction:
*/
    default: break;	//- NOP
  }
}

void TCstats::doAssigns(FILE * outf, suco_llist<TCassignEdge *>& assigns)
{
  //------- tabulate per undefined function
  struct entry {
    const char * fnname;
    int literal,
	tracked, untracked,
	pt_tracked, pt_untracked;
    struct entry * next;
  } * list = 0;
  //---------------------------------------
  suco_iterator<TCassignEdge *> aei(assigns);
  while(aei.Iterate()){
    if(aei.Current()->getTo().getKind() == AO::aoArg){
      AOArg& aao = (AOArg&)aei.Current()->getTo();
      if(aao.getParent().getKind() == AO::aoStar){
	AO& tgtao = ((AOStar&)aao.getParent()).getTarget();
	if(tgtao.getKind() == AO::aoId){
	  AOId& idao = (AOId&) tgtao;
	  AOFunction * fao = (AOFunction *) idao.find(AO::aoFunction);
	  if(fao && (!fao->getLargNo() || fao->isLib())){ // id is defined and non-library
	    ID& pid = idao.getPid();
	    const char * fnname = pid.getname();
	    // find entry, or add if not found
	    struct entry ** lp = &list;
	    int cf = 1;
	    while((*lp) && (cf = strcmp((*lp)->fnname, fnname)) < 0)
	      lp = &(*lp)->next;
	    if(cf){ //- entry not found
	      struct entry * tail = *lp;
	      *lp = new struct entry;
	      (*lp)->fnname = fnname;
	      (*lp)->literal = 0;
	      (*lp)->tracked = 0;
	      (*lp)->untracked = 0;
	      (*lp)->pt_tracked = 0;
	      (*lp)->pt_untracked = 0;
	      (*lp)->next = tail;
	    }

	    //- categorize fromAO
	    AO& fromao = aei.Current()->getFrom();
	    if(fromao.isVal()){
	      (*lp)->literal++;
	    } else {
	      //- determine trackedness
	      if(fromao.getTSC() == AO::TSC_SAFE){
	        (*lp)->untracked++;
	      } else {
	        (*lp)->tracked++;
	      }
	      //- determine pointer-target trackedness
	      if(aei.Current()->getType().getKind() == TCtype::tcPointer){
		ExposedStatus es = fromao.pointsToExposed();
		if(es.isSome() || es.isAll()){
		  (*lp)->pt_tracked++;
		} else {
		  (*lp)->pt_untracked++;
		}
	      }
	    }
	  } // else function is true fnptr, or implicit (not declared)
	} // else function is not regular fn or fnptr
/**/  } else if(aao.getParent().getKind() == AO::aoId){
/**/    fprintf(stderr, "Assignment into Arg Id _\n");
      }
    }
  }
  //------- print undefined function summary
  struct entry * ep;
  if(list)
    fprintf(outf, "# undef-fns: (tracked/untracked/pt-tracked/pt-untracked/literal)\n");
  for(ep = list; ep; ep = ep->next)
    fprintf(outf, "# undef-fn %s: %d/%d/%d/%d/%d\n",
			ep->fnname, ep->tracked, ep->untracked,
				    ep->pt_tracked, ep->pt_untracked,
				    ep->literal);
}

void TCstats::print(FILE * outf)
{
  fprintf(outf, "# AOs = %d\n"
		"# AddrOf = %d\n"
		"# Deref = %d\n"
		"# Unsafe = %d\n"
		"# Tracked = %d\n"
	, num_aos, num_addrof, num_deref, num_unsafe, num_tracked);

  fprintf(outf, "# Malloc = %d\n"
		"# Malloc-evalsize = %d\n"
		"# Malloc-constsize = %d\n"
	, num_malloc, num_malloc_evalsize, num_malloc_constsize);

  fprintf(outf, "# poss-type bottom = %d\n"
		"# poss-type scalar = %d\n"
		"# poss-type zero = %d\n"
		"# poss-type top = %d\n"
		, num_poss_bottom, num_poss_scalar,
		  num_poss_zero, num_poss_top);

  fprintf(outf, "# safe ptrs = %d\n"
		"# unsafe ptrs = %d\n"
		"# safe ptrs in struct = %d\n"
		"# unsafe ptrs in struct = %d\n"
		"# safe written-ptrs = %d\n"
		"# unsafe written-ptrs = %d\n"
		"# safe written-ptrs in struct = %d\n"
		"# unsafe written-ptrs in struct = %d\n"
		"# safe fnptrs = %d\n"
		"# unsafe fnptrs = %d\n"
		"# safe fnptrs in struct = %d\n"
		"# unsafe fnptrs in struct = %d\n"
		"# tracked locs = %d\n"
		"# untracked locs = %d\n"
		"# tracked fnptrs = %d\n"
		"# untracked fnptrs = %d\n"
		"# tracked fnptrs in struct = %d\n"
		"# untracked fnptrs in struct = %d\n"
	, num_safe_ptr, num_unsafe_ptr,
	  num_safe_struct_ptr, num_unsafe_struct_ptr,
	  num_safe_written_ptr, num_unsafe_written_ptr,
	  num_safe_struct_written_ptr, num_unsafe_struct_written_ptr,
	  num_safe_fnptr, num_unsafe_fnptr,
	  num_safe_struct_fnptr, num_unsafe_struct_fnptr,
	  num_tracked_loc, num_untracked_loc,
	  num_tracked_fnptr, num_untracked_fnptr,
	  num_tracked_struct_fnptr, num_untracked_struct_fnptr);

  if(flag_recycle){
    fprintf(outf, "# Recycle LocAid Nodes: New = %d, Recycled = %d, Freelist = %d\n"
		, new_la_nodes, recy_la_nodes, RDAfact::freelistLength());
    suco_diag_wrapper::suco_write_recycle_stats(outf);
  }

  fprintf(outf, "# Number of poss-type Constraints = %d\n", num_pt_constraints);
  fprintf(outf, "# Constraint solving: number of node visits = %d\n", num_constraint_solving_visits);

  if(flag_collapse_inclto_cycle)
    fprintf(outf, "# Collapsed incl-to %d cycles, %d nodes\n"
		, cde_incl_num_collapses, cde_incl_nodes_collapsed);
  if(flag_collapse_ptsto_cycle)
    fprintf(outf, "# Collapsed pts-to %d cycles, %d nodes\n"
		, cde_pt_num_collapses, cde_pt_nodes_collapsed);

  fprintf(outf, "# DFA (useless/useful) node visits: expr (%d/%d), cnode (%d/%d)\n",
		dfa_useless_expr_node_visits, dfa_useful_expr_node_visits,
		dfa_useless_cfg_node_visits, dfa_useful_cfg_node_visits);

  fprintf(outf, "# range analysis collecting: %d facts / %d nodes = %.2f average; %d max\n",
		ranc_verify_visit_factsizes,
		ranc_verify_visits,
		(float)ranc_verify_visit_factsizes/(float)ranc_verify_visits,
		ranc_verify_visit_max_factsize);

  if(flag_may_be_uninit)
    fprintf(outf, "# mbu analysis collecting: %d facts / %d nodes = %.2f average; %d max\n",
		mbuc_verify_visit_factsizes,
		mbuc_verify_visits,
		(float)mbuc_verify_visit_factsizes/(float)mbuc_verify_visits,
		mbuc_verify_visit_max_factsize);
}

// prints the current time
void TCstats::print_now_time(FILE * outf)
{
  time_t now;
  struct tm * tp;

  time(&now);
  tp = localtime(&now);
  fprintf(outf, "[%02d:%02d:%02d]", tp->tm_hour, tp->tm_min, tp->tm_sec);
}

void TCstats::timer(const char * descr)
{
#define RDS_SIZE 10
  struct rudesc {
    struct rusage ru;
    const char * desc;
  };
  static struct rudesc rdstack[RDS_SIZE];
  static int rdsndx = 0;
  static int rdsndx_overflow = 0;

  if(descr){ //- start timer
    if(rdsndx >= RDS_SIZE){
      fprintf(stderr, "RTC: timer stack overflow; skipping timer for %s\n", descr);
      rdsndx_overflow++;
    } else {
      getrusage(RUSAGE_SELF, &rdstack[rdsndx].ru);
      rdstack[rdsndx].desc = descr;
      fprintf(stderr, "Start %s...", rdstack[rdsndx].desc);
      TCstats::print_now_time(stderr);
      fprintf(stderr, "\n");
      rdsndx++;
    }
  } else { //- end timer
    struct rusage ru_end;
    getrusage(RUSAGE_SELF, &ru_end);

    if(rdsndx_overflow){
      rdsndx_overflow--;
    } else if(rdsndx == 0){
      fprintf(stderr, "RTC: timer stack underflow!!\n");
    } else {
      rdsndx--;

      struct timeval u1 = rdstack[rdsndx].ru.ru_utime;
      struct timeval u2 = ru_end.ru_utime;
      struct timeval s1 = rdstack[rdsndx].ru.ru_stime;
      struct timeval s2 = ru_end.ru_stime;

      fprintf(stderr, "... done %s (%.3f u, %.3f s)\n", rdstack[rdsndx].desc,
	(double)(u2.tv_sec-u1.tv_sec)+(double)(u2.tv_usec-u1.tv_usec)/1000000,
	(double)(s2.tv_sec-s1.tv_sec)+(double)(s2.tv_usec-s1.tv_usec)/1000000);
    }
  }
#undef RDS_SIZE
}

//------------------------------------------------------

