#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h> // for strtoul

#include "flags.h"
#include "diag.h"
#include "ao.h"
#include "tca.h"
#include "pta.h"
#include "ecr.h"
#include "cfg.h"
#include "dfa.h"
#include "ran.h"
#include "pmbu.h"

//------------------------------------------------------
//- Global flags

//- -ptr or -ptrw mode?
bool flag_readwrite = true;

//- fix nargs: use when there is clash in number of function arguments
bool flag_use_maxnargs = true;

//- do address-taken analysis (no points-to analysis)
bool flag_do_addr_taken = false;

//- (ptr/w mode): all dereferences are unsafe (i.e., only compute tracked)
bool flag_all_deref_unsafe = false;

//- verbose output level
int flag_verbose = 0;

//- recycle nodes whenever possible
bool flag_recycle = true;

//- output readable names rather than Tid/Pid
bool flag_readable_output = false;

//- compute GREF and filter call nodes
bool flag_gref_filter = true;

//- GMOD/GREF should skip locals
bool flag_gmodref_skip_locals = true;

//- treat string literals as read-only
bool flag_strlit_readonly = true;

//- tsl analysis: mark stringlit as invalid:
//  this is set to flag_strlit_readonly in -ptrw mode
bool flag_mark_strlit_invalid = false;

//- ptr(w): 0=ignore heap/stack, 1=freed-heap unsafe, 2=heap+stack unsafe
int flag_heapstack = 0;

//- mbu analysis level
int flag_may_be_uninit = 0;

//- mbu intra-procedural or inter-procedural
static bool flag_mbu_inter = true;	//- keep local to this file

//- pmbu flow-sensitive or flow-insensitive
static bool flag_pmbu_sensitive = false;	//- keep local to this file

//- do reaching-defs analysis
bool flag_reaching_defs = false;

//- treat verifyTag as fixing types on mismatch? (for nt:mbu,red)
bool flag_vtfix = false;

//- keep iref sets around (currently needed only by mbu)
bool flag_compute_iref = false;

//- do redundant-check analysis
bool flag_redundant = false;

//- red: handle parallel 2 rounds?
bool flag_red_paral2r = true;

//- do range analysis
bool flag_range = false;

//- range do intra-procedural or inter-procedural
static bool flag_range_inter = false;	//- keep local to this file

//- range analysis: do narrowing phase
bool flag_range_do_narrow = true;

//- range analysis: do widen/narrow always or on backedge only
bool flag_range_widen_always = false;
bool flag_range_narrow_always = true;

//- range analysis: compute backedge filter for better widening
//  ~~ separate flag to control (inter-procedural) loop backedge
//     and callgraph backedge
//  ~~ Notice different default behaviors!
bool flag_range_filter_loop_backedge = true;
bool flag_range_filter_call_backedge = false;

//- range analysis: treat zero specially, i.e. use "has_zero" flag in Interval class
bool flag_range_zero_special = true;

//- range analysis: representation choices (default is "DO"):
bool flag_range_must_have_ao = false;	// true: ao=null implies bottom
bool flag_range_ty_can_change = true;	// true: ty can be adjusted
bool flag_range_do_sliding = true;	// true: allow sliding (only if ty-can-change)
bool flag_range_handle_preds = true;	// true: try to improve predicates
bool flag_range_skip_targeted_preds = false;	// true: don't intersect at predicates if either side is targeted
bool flag_range_exact_sizes = false;	// true: assume exact sizeof info
//- a note on must-have-ao vs. ty-can-change (quite confusing):
//  . if must-have-ao == true,  then ty-can-change==false => cannot change types at all
//  . if must-have-ao == false, then ty-can-change==false => just cannot do xform(1a')
//
//- SO: | must-have-ao | ty-can-change | is
//  ----+--------------+---------------+----------------
//   A. |     true     |     true      | LO + ty-change + xform(1a')
//   B. |     true     |     false     | LO
//   C. |     false    |     true      | DO
//   D. |     false    |     false     | DO - xform(1a')
//- C-D = xform(1a')
//- C-A = unknown-target

//- range analysis: collect stats
bool flag_range_collect_stats = false;

//- vuln mode: write vulnerable locations and dereferences
bool flag_vuln = true;

//- range analysis: set this to true when entering "collecting" phase.
bool flag_debug_range_collect_phase = false;

//- range analysis: account for malloc sizes (whenever possible)
bool flag_range_handle_malloc = true;

//- points-to analysis: do online cycle detection and collapsing
//- NOTE: as implemented, it's unclear whether the incl-to and
//        pts-to cycle-elimination will clash, so be warned.
//	  empirically, at least, they survived with each other.
//  Note that as of nov 2003, -cde-incl appears buggy, while
//	 -cde-pt looks ok.
bool flag_collapse_inclto_cycle = true;
bool flag_collapse_ptsto_cycle = true;

//- dfa worklist mode (see flags.h for modes)
enum wl_mode flag_worklist_mode = WL_MODE_LEAF_FIRST;

//- if p is exposed, *p is unsafe
//  (intended for ptr/w; may work for nt also?)
bool flag_exposed_deref_is_unsafe = false;

//- instantiate all struct/union members during setStaticType (declaration)
bool flag_instantiate_structunion = true;

//- dfa filter relevant AOs during analysis (used by MBU, RAN)
bool flag_filter_relevant_aos = false;

//- dfa worklist initialize depth first, or by traverse id (more or less topological)
bool flag_worklist_init_depth_first = false;

//- mbu: perform lowerThan by comparing size only
bool flag_mbu_lowerthan_by_size = false;

//- dfa: on function call, ignore pre-call facts?
//  (optimistic; unsafe when recursive? -- recursion OK now)
bool flag_callsite_top = true;

//- dfa: use basic blocks
bool flag_use_bblocks = false;

//------------------------------------------------------
//- TCAstate

void TCAstate::processFile(char * filename)
{
  FILE * inf = fopen(filename, "r");
  if(!inf){
    fprintf(stderr, "Error opening input file %s\n", filename);
  } else {
    static int nlctr = 0;
    if(!((nlctr++)%5)) fprintf(stderr, "\n");
    fprintf(stderr, "(%s)", filename);

    char buf[TCA_BUF_SIZE];

    IDmap aidmap, pidmap;
    InputState istate(inf, aidmap, pidmap, values);

    TCtype * tgtty = 0;
    AO * aotgt = 0;

    fgets(buf, TCA_BUF_SIZE, inf);
    while(!feof(inf)){
      switch(*buf){
        case ':': { //- assign target
            char * cp = &buf[2];
	    tgtty = TCtype::stringToTy(cp, &cp);
            aotgt = AO::stringToAO(cp, &cp, aidmap, pidmap, values);
            if(aotgt && !tgtty) tgtty = &TCtype::tcVoidType; //-- error
          } break;
        case '=':   //- 'true' assign source
        case '-': { //- 'pseudo' assign source
            char * cp = &buf[2];
            AO * aosrc = AO::stringToAO(cp, &cp, aidmap, pidmap, values);
            if(aosrc && aotgt){
              TCassignEdge * e = aosrc->assignTo(*aotgt, *tgtty);
              if(e) assigns.Append(e);
              if(*buf == '=') aosrc->setRequiredType(*tgtty); //-- add assignment type to reqd-type
            }
          } break;
        case '@': { //- aid entry
            char sc = buf[2];
            char * cp = &buf[4];
            unsigned int aidno = strtoul(cp, &cp, 10);
            //- skip leading and trailing spaces
            while(isspace(*cp)) cp++;
            char * ep = cp + strlen(cp) - 1;
            while(isspace(*ep)) ep--;
            *(ep+1) = 0;
            //- get id, insert into aidmap
            ID& aid = aidtab.getID(cp,sc);
            aidmap.map(aidno, aid);
          } break;
        case '%': { //- pid entry
            char sc = buf[2];
            char * cp = &buf[4];
            unsigned int pidno = strtoul(cp, &cp, 10);
            //- skip leading and trailing spaces
            while(isspace(*cp)) cp++;
            char * ep = cp + strlen(cp) - 1;
            while(isspace(*ep)) ep--;
            *(ep+1) = 0;
            //- get id, insert into pidmap
            ID& pid = pidtab.getID(cp,sc);
            pidmap.map(pidno, pid);
          } break;
        case '+': { //- static type
            char * cp = &buf[2];
	    TCtype * ty = TCtype::stringToTy(cp, &cp);
            AO * ao = AO::stringToAO(cp, &cp, aidmap, pidmap, values);
            if(ao && ty) ao->setStaticType(*ty);
	  } break;
        case '}': { //- verify tag
            char * cp = &buf[2];
	    TCtype * ty = TCtype::stringToTy(cp, &cp);
            AO * ao = AO::stringToAO(cp, &cp, aidmap, pidmap, values);
            if(ao && ty) ao->setRequiredType(*ty); //- initialize reqd-type
	  } break;
        case ']': { //- verify pointer
            char * cp = &buf[2];
	    TCtype * ty = TCtype::stringToTy(cp, &cp);
            AO * ao = AO::stringToAO(cp, &cp, aidmap, pidmap, values);
            if(ao && ty) ao->setVerifyPtrType(*ty);
	  } break;
        case 'f':   //- normal function definition
        case 'v': { //- varg function definition
            char * cp = &buf[2];
            unsigned int largno = strtoul(cp, &cp, 10);
            AO * ao = AO::stringToAO(cp, &cp, aidmap, pidmap, values);
            if(ao && ao->getKind() == AO::aoFunction)
              ((AOFunction *)ao)->setAttributes(largno, *buf == 'v');
            else {
	      fprintf(stderr, "Invalid f directive, for object: ");
	      if(ao) ao->dump_descr(stderr);
	      else fprintf(stderr, "(null)");
	      fprintf(stderr, "\n");
	    }
	  } break;
        case '!': {
	    char * filestem = 0;
	    int i;
            for(i = strlen(filename) - 1; i >= 0 && filename[i] != '.'; --i);
            if(i >= 0){
              filestem = new char[i+1];
              strncpy(filestem, filename, i);
              filestem[i] = 0;
            } else filestem = filename;
	    cfg.read(istate, buf, filestem);
	  } break;
        case '$': //- ao alias -- not yet implemented
        case '~': //- type-safety level -- ignore (for now?)
        case '#': //- comment
        default : //- treat as comment
	  ;
      }
      fgets(buf, TCA_BUF_SIZE, inf);
    }
    fclose(inf);
  }
}

void TCAstate::traverseAOs(void (*fp)(AO&))
{
  values.traverseAOs(fp);
  aidtab.traverseAOs(fp);
  pidtab.traverseAOs(fp);
}

TCAstate::relaos_mode TCAstate::msp_relaos_mode;
DependencyMap TCAstate::msp_misc_dependencies;
suco_set<ECR *> TCAstate::msp_starting_ecrs;
int TCAstate::crao_total_aos = 0;
int TCAstate::crao_marked_aos = 0;
TClatRootSet TCAstate::possTypeConstraintRoots;
int TCAstate::poss_type_init_counter = 0;
int TCAstate::poss_type_assign_counter = 0;
int TCAstate::poss_type_incl_counter = 0;

void TCAstate::initPossTypeConstraintsAO(AO& ao)
{
  ECR& e = ao.getECR();
  switch(ao.getKind()){
    case AO::aoValue:{
        TCtype& valty = ((AOValue&)ao).getTy();
        TClatType& latty = possTypeConstraintRoots.getLatType(valty);
        e.possType().constrainLE(latty);
      } break;
    case AO::aoOp:{
        TCtype& opty = ((AOOp&)ao).getTy();
        TClatType& latty = possTypeConstraintRoots.getLatType(opty);
        e.possType().constrainLE(latty);
      } break;
    case AO::aoExt:{
        e.possType().constrainLE(possTypeConstraintRoots.getLatType(TClatRootSet::lrBottom));
      } break;
    case AO::aoAddrOf:{
        e.possType().constrainLE(possTypeConstraintRoots.getLatType(TClatRootSet::lrPointer));
      } break;
/*
    case AO::aoId:
    case AO::aoMalloc:
    case AO::aoSDot:
    case AO::aoUDot:
    case AO::aoReturn:
    case AO::aoArg:
    case AO::aoStar:
    case AO::aoStringLit:
    case AO::aoFunction:
*/
    default: break;	//- NOP
  }
  if(flag_verbose > 1){
    if(++poss_type_init_counter % 100 == 0) 
      fprintf(stderr, "."); 
    if(poss_type_init_counter % 5000 == 0) 
      fprintf(stderr, "\n");
  }     
  initPossTypeConstraintsInclusion(e);
}

void TCAstate::initPossTypeConstraintsInclusion(ECR& ecr)
{
  if(ecr.poss_type_incl_visited) return;
  ecr.poss_type_incl_visited = true;

  if(flag_verbose > 1)
    if(++poss_type_incl_counter % 100 == 0) 
      fprintf(stderr, "*"); 

  suco_iterator<ECR *> ii(ecr.inclToECRs());
  while(ii.Iterate()){
    ECR& recr = *ii.Current();
    ecr.possType().constrainLE(recr.possType());
    recr.writeType().constrainLE(ecr.writeType());
    initPossTypeConstraintsInclusion(recr);
  }
}

void TCAstate::initPossTypeConstraintsAssign(TCassignEdge& edge)
{
  ECR& fromECR = edge.getFrom().getECR();
  ECR& toECR = edge.getTo().getECR();
  if(&fromECR != &toECR){ // harmless optimization?
    ECR::traverseCounter++;
    fromECR.traverseTag = ECR::traverseCounter;

    toECR.writeType().constrainLE(fromECR.possType());

    if(flag_verbose > 1){
      if(++poss_type_assign_counter % 100 == 0) 
        fprintf(stderr, "."); 
      if(poss_type_assign_counter % 5000 == 0) 
        fprintf(stderr, "\n");
    }     
  }
  //- do inclusions
  initPossTypeConstraintsInclusion(fromECR);
}

void TCAstate::ptAnalysis()
{
  //----------------------------------------
  //-- PT analysis

  //- prelude: find max_nargs (if needed)
  if(flag_use_maxnargs){
    if(flag_verbose) TCstats::timer("computing maxnargs");
    traverseAOs(ECRargRet::findMaxNargs);
    if(flag_verbose) TCstats::timer(0);
    if(flag_verbose) fprintf(stderr, "(maxnargs = %d)\n", ECRargRet::getMaxNargs());
  }

  // - initialize ECRs
  if(flag_verbose) TCstats::timer("initializing ECRs");
  traverseAOs(PTanalysis::initializeECRbasic);
  if(flag_verbose) fprintf(stderr, "(basic)");
  traverseAOs(PTanalysis::initializeECRaggregate_collapseAlways);
  if(flag_verbose) fprintf(stderr, "(aggregate)");
  traverseAOs(PTanalysis::initializeECRpointers);
  if(flag_verbose) fprintf(stderr, "(pointers)\n");
  if(flag_verbose) TCstats::timer(0);

  TCstats::timer("points-to analysis collapse-always");
  PTanalysis::analyzeAssigns_collapseAlways(assigns);
  TCstats::timer(0);
}

void TCAstate::markInvalidNode(AO& ao)
{
  //-- initial invalid nodes are those pointed-to by VALUE, STRLIT, op and ext
  switch(ao.getKind()){
    case AO::aoValue:
    case AO::aoStringLit:
    case AO::aoOp:
    case AO::aoExt: {
	  //-- mark ECR invalid, propagate up incl edges, and propagate down derefs
	  ao.getECR().getPointsTo().markInvalid(true);
	} break;
    default:
	break;
  }
}

void TCAstate::markInvalidNodeNonNull(AO& ao)
{
  //-- initial non-null invalid nodes are those pointed-to by
  //	VALUE (excl VALUE_zero), STRLIT, op and ext
  switch(ao.getKind()){
    case AO::aoValue:
	if(((AOValue&)ao).getTy().getKind() == TCtype::tcZero){
	  break;
	} // else fallthrough!
    case AO::aoStringLit:
    case AO::aoOp:
    case AO::aoExt: {
	  //-- mark ECR invalid, propagate up incl edges, and propagate down derefs
	  ao.getECR().getPointsTo().markInvalid(true);
	} break;
    case AO::aoAddrOf: {
	  if(flag_mark_strlit_invalid
	     && (((AOAddrOf&)ao).getTarget().getKind() == AO::aoStringLit)){
	    ao.getECR().getPointsTo().markInvalid(true);
          }
	} break;
    default:
	break;
  }
}

//----------------------------------------------------
//-- insert return-node aliases! ( R D foo --incl--> R foo )

void TCAstate::addReturnNodeAliases(AO& ao)
{
  if(ao.getKind() == AO::aoReturn){
    AO& fpao = ((AOReturn&)ao).getParent();
    suco_set<ECR *> alias_ecrs;
    //- collect alias ecrs
    ECR::getECR(fpao).collectAliasECRs(alias_ecrs);	//- Still not safe to use getAliasECRs?
    //- for each alias ao that's a function, insert return-node alias ( R D foo --incl--> R foo )
    suco_iterator<ECR *> ei(alias_ecrs);
    while(ei.Iterate()){
      suco_iterator<AO *> ai(ei.Current()->getAOset());
      while(ai.Iterate()){
        if(ai.Current()->getKind() == AO::aoFunction){
          AO& ret = ((AOFunction *)ai.Current())->getTarget().get_AOReturn();
          ECR::getECR(ao).includesTo(ECR::getECR(ret));
        }
      }
    }
  }
}

//---------------------------------------
//-- Non-standard PT analysis-related stuff:
//   insert argRet assignment edges
void TCAstate::supplementalPTanalysis()
{
  //-- insert argRet assignment edges
  if(flag_verbose) TCstats::timer("adding Arg-Ret assignments");
  TCAstate::static_arg_ret_assigns = &arg_ret_assigns;
  traverseAOs(TCAstate::insertArgRetAssignEdges);
  if(flag_verbose) TCstats::timer(0);
  if(flag_verbose) fprintf(stderr, "(%d argrets processed)\n", insertArgRetAssignEdges_doneset.Length());

  //-- insert return-node aliases! ( R D foo --incl--> R foo )
  // - needed by flow-sensitive analysis/MBU (at least)
  if(flag_verbose) TCstats::timer("adding return-node aliases");
  traverseAOs(TCAstate::addReturnNodeAliases);
  if(flag_verbose) TCstats::timer(0);

  //-- finalize points-to graph: traverse AOs and
  //   fix ECR forward pointers
  TCstats::timer("finalizing points-to graph ECR pointers");
  ECR::traverseCounter++;
  traverseAOs(ECR::fixECR);
  ECR::garbageCollect();
  TCstats::timer(0);
}

suco_set<ECRargRet *> TCAstate::collectInclToArgRets_ECRs;

bool TCAstate::collectInclToArgRets(ECR& e)
{
  ECRargRet * ear = e.argRet();
  if(ear) collectInclToArgRets_ECRs.Insert(ear);
  return true; //-- true ensures full traversal
}

void TCAstate::addAssignEdges(ECR * lecr, ECR * recr, AO::aoKind kind)
{
  if(lecr && recr){
/**/if(lecr != &lecr->followECR()) fprintf(stderr, "WARNING(addAssignEdges): lecr != lecr->followECR()\n");
/**/if(recr != &recr->followECR()) fprintf(stderr, "WARNING(addAssignEdges): recr != recr->followECR()\n");
    suco_set<AO *>& laos = lecr->followECR().getAOset();
    suco_set<AO *>& raos = recr->followECR().getAOset();
    if(!laos.IsEmpty() && !raos.IsEmpty()){
      suco_iterator<AO *> li(laos); //-- note: laos may contain more than one member: due to unification?
      while(li.Iterate()){
        AO& lao = *li.Current();
        if(lao.getKind() == kind){ //- only do aoArg or aoReturn nodes
          suco_iterator<AO *> ri(raos); //-- note: raos may contain more than one member: due to unification?
          while(ri.Iterate()){
            AO& rao = *ri.Current();
            if(rao.getKind() == kind){
              TCtype * rst = rao.getStaticType();
              if(!rst){
                //- no static type: signal error?
                //~~ not if rao is AOArg[largno]
                if((rao.getKind() != AO::aoArg)
		  || (((AOArg&)rao).getParent().getKind() != AO::aoFunction)
		  || (((AOFunction&)((AOArg&)rao).getParent()).getLargNo() != ((AOArg&)rao).argNo())){
                  fprintf(stderr, "WARNING(addAssignEdges): no static type, setting to void type, in:\n  ");
                  lao.dump_descr(stderr);
                  fprintf(stderr, "\n  --ASSIGN--> ");
                  rao.dump_descr(stderr);
                  fprintf(stderr, "\n");
                }
                rst = &TCtype::tcVoidType;
              }
              TCassignEdge * edge = lao.assignTo(rao, *rst);
              if(edge) static_arg_ret_assigns->Append(edge);
	      lao.setRequiredType(*rst); //- add assignment type to reqd-type

            } // else (if not rao.getKind() == kind ) never happens
          }
        } // else ( if not  lao.getKind() == kind ) never happens
      }
    }
  }
}

suco_llist<TCassignEdge *> * TCAstate::static_arg_ret_assigns = 0;

suco_set<ECRargRet *> TCAstate::insertArgRetAssignEdges_doneset;

void TCAstate::insertArgRetAssignEdges(AO& ao)
{
  static int counter = 0;
  if(static_arg_ret_assigns){
    ECR& ecr = ECR::getECR(ao);
    ECRargRet * ear = ecr.argRet();
    if(ear && !insertArgRetAssignEdges_doneset.Contains(ear)){
      insertArgRetAssignEdges_doneset.Insert(ear);

      ECR& e = ear->getMainParent();

      //- traverse include edges, collect frontier ECRs
      collectInclToArgRets_ECRs.Clear();
      e.traverseAliases(0, collectInclToArgRets);

      //- for each frontier ECR, add assignment edges
      suco_iterator<ECRargRet *> ei(collectInclToArgRets_ECRs); // follow inclusion edges
      while(ei.Iterate()){
	ECRargRet * rar = ei.Current();
	if(ear != rar){ //- skip self-assigns?
	  //-------------------------
	  //- DO ARGS
	  int minArgSize = (ear->getArgSize() < rar->getArgSize()) ? ear->getArgSize() : rar->getArgSize();
	  int i;
	  for(i = 1; i <= minArgSize; ++i)
	    addAssignEdges(ear->get_arg(i), rar->get_arg(i), AO::aoArg);

	  //-------------------------
	  //- DO RET
	  addAssignEdges(rar->get_ret(), ear->get_ret(), AO::aoReturn);
	}
      }
      if(flag_verbose > 1){
        if(++counter % 100 == 0)
          fprintf(stderr, ".");
        if(counter % 5000 == 0)
          fprintf(stderr, "\n");
      }
    }
  }
}

void TCAstate::possTypeAnalysis()
{
  //----------------------------------------
  //-- compute possible-types

  //-- mark (initial) invalid nodes
  traverseAOs(TCAstate::markInvalidNode);

  if(flag_heapstack){
    //-- mark freed-heap locations as invalid
    // - mark *free.arg1 as influential
    ID * free_id = this->pidtab.lookupByName(".free");
    if(free_id){
      AO * fao = free_id->get_AOId().find(AO::aoFunction);
      if(fao && fao->get_AOArg(1)){
        //-- free is a special case: markInvalid must follow inclEdge *forward*
        suco_iterator<ECR *> ei(fao->get_AOArg(1)->getECR().getPointsTo().getAliasECRs());
        while(ei.Iterate()){
          ei.Current()->markInvalid(false); // mark invalid, propagate up incl edges, but don't propagate down derefs
          ei.Current()->propagateExposed(); // propagate exposed forward
        }
      } else fprintf(stderr, "WARNING (-heap): Free Function-AO or AOArg Not Found\n");
    } else fprintf(stderr, "WARNING (-heap): Free ID Not Found\n");

    if(flag_heapstack == 2){
      //-- mark all auto locations as invalid
      this->pidtab.traverseIDs(TCAstate::markAutoInvalidId);
      this->aidtab.traverseIDs(TCAstate::markAutoInvalidId);	//- for alloca $malloc objects!
    }
  }

  //- seed runtime-types
  if(flag_verbose) TCstats::timer("initializing possible-type");
  traverseAOs(initPossTypeConstraintsAO);
  if(flag_verbose) TCstats::timer(0);

  if(flag_verbose) TCstats::timer("building possible-types constraints");
  suco_iterator<TCassignEdge *> ai(assigns);
  while(ai.Iterate())
    initPossTypeConstraintsAssign(*ai.Current());
  suco_iterator<TCassignEdge *> arai(arg_ret_assigns);
  while(arai.Iterate())
    initPossTypeConstraintsAssign(*arai.Current());
  if(flag_verbose) TCstats::timer(0);

  if(flag_verbose > 3) possTypeConstraintRoots.debug_dump(stderr);

  //- Solve possible-type constraints
  if(flag_verbose) TCstats::timer("solving possible-type constraints");
  possTypeConstraintRoots.solveMeet();
  if(flag_verbose) TCstats::timer(0);
}

void TCAstate::assignTSC(AO& ao)
{
  ECR& ecr = ao.getECR();
  AO::ts_categ oldtsc = ao.getTSC();
  //-- possibly-invalid...
  if(oldtsc == AO::TSC_POSS_INVALID)
    return; // skip if already there
  // - rule L1a
  if(ao.isRef() && ecr.isInvalid()){
    ao.setTSC(AO::TSC_POSS_INVALID);
    //-- propagate L3:influential, which propagates L4:exposed
    ecr.propagateInfluential();
    return;
  }
  // - rule L1b
//TODO

  //-- badly-typed...
  if(oldtsc == AO::TSC_BADLY_TYPED)
    return; // skip if already there
  // - rules L2a, L2b
  TClatType::latKind possType = ecr.possType().getKind();
  if(!TClatType::GE(possType, ao.getRequiredType()) ||
	possType == TClatType::tclBottom){
    ao.setTSC(AO::TSC_BADLY_TYPED);
    //-- propagate L3:influential, which propagates L4:exposed
    ecr.propagateInfluential();
    return;
  }
}

void TCAstate::assignAllUnsafeRW(AO& ao)
{
  ECR& ecr = ao.getECR();
  AO::ts_categ oldtsc = ao.getTSC();
  //-- possibly-invalid...
  if(oldtsc == AO::TSC_POSS_INVALID)
    return; // skip if already there
  if(ao.isRef()){
    ao.setTSC(AO::TSC_POSS_INVALID);
    //-- propagate L3:influential, which propagates L4:exposed
    ecr.propagateInfluential();
    return;
  }
}

void TCAstate::assignAllUnsafeW(AO& ao)
{
  ECR& ecr = ao.getECR();
  AO::ts_categ oldtsc = ao.getTSC();
  //-- possibly-invalid...
  if(oldtsc == AO::TSC_POSS_INVALID)
    return; // skip if already there
  if(ao.isRef() && ao.isAssigned()){
    ao.setTSC(AO::TSC_POSS_INVALID);
    //-- propagate L3:influential, which propagates L4:exposed
    ecr.propagateInfluential();
    return;
  }
}

void TCAstate::assignTSCderefWrite(AO& ao)
{
  ECR& ecr = ao.getECR();
  AO::ts_categ oldtsc = ao.getTSC();
  //-- possibly-invalid...
  if(oldtsc == AO::TSC_POSS_INVALID)
    return; // skip if already there
  // - rule L1a
  if(ao.isRef() && ecr.isInvalid() && ao.isAssigned()){
    ao.setTSC(AO::TSC_POSS_INVALID);
    //-- propagate L3:influential, which propagates L4:exposed
    ecr.propagateInfluential();
    return;
  }
  // - rule L1b
//TODO

}

void TCAstate::assignTSCderef(AO& ao)
{
  ECR& ecr = ao.getECR();
  AO::ts_categ oldtsc = ao.getTSC();
  //-- possibly-invalid...
  if(oldtsc == AO::TSC_POSS_INVALID)
    return; // skip if already there
  // - rule L1a
  if(ao.isRef() && ecr.isInvalid()){
    ao.setTSC(AO::TSC_POSS_INVALID);
    //-- propagate L3:influential, which propagates L4:exposed
    ecr.propagateInfluential();
    return;
  }
  // - rule L1b
//TODO

}

void TCAstate::tscAnalysis()
{
  //----------------------------------------
  //-- compute type-safety levels
  if(flag_verbose) TCstats::timer("assigning type-safety categories");
  traverseAOs(assignTSC);
  if(flag_verbose) TCstats::timer(0);
}

void TCAstate::markInvalidLocAO(AO& ao)
{
  if(ao.isLoc()) // mark invalid, propagate up incl edges, but don't propagate down derefs
    ao.getECR().markInvalid(false);
}

void TCAstate::markAutoInvalidId(ID& id)
{
  if(id.getSC() == ID::AUTO)
    id.get_AOId().traverseAOs(TCAstate::markInvalidLocAO);
}

void TCAstate::invalidPtrAnalysis()
{
  //----------------------------------------
  //-- mark non-null invalid nodes
  traverseAOs(markInvalidNodeNonNull);

  if(flag_heapstack){
    //-- mark freed-heap locations as invalid
    // - mark *free.arg1 as influential
    ID * free_id = this->pidtab.lookupByName(".free");
    if(free_id){
      AO * fao = free_id->get_AOId().find(AO::aoFunction);
      if(fao && fao->get_AOArg(1)){
        //-- free is a special case: markInvalid must follow inclEdge *forward*
        suco_iterator<ECR *> ei(fao->get_AOArg(1)->getECR().getPointsTo().getAliasECRs());
        while(ei.Iterate()){
          ei.Current()->markInvalid(false); // mark invalid, propagate up incl edges, but don't propagate down derefs
          ei.Current()->propagateExposed(); // propagate exposed forward
        }
      } else fprintf(stderr, "WARNING (-ptr-heap): Free Function-AO or AOArg Not Found\n");
    } else fprintf(stderr, "WARNING (-ptr-heap): Free ID Not Found\n");

    if(flag_heapstack == 2){
      //-- mark all auto locations as invalid
      this->pidtab.traverseIDs(TCAstate::markAutoInvalidId);
      this->aidtab.traverseIDs(TCAstate::markAutoInvalidId);	//- for alloca $malloc objects!
    }
  }

  //----------------------------------------
  //-- assign type-safety levels, rules L1a and L1b only
  if(flag_verbose) TCstats::timer("assigning type-safety categories");
  traverseAOs(flag_readwrite?assignTSCderef:assignTSCderefWrite);
  if(flag_verbose) TCstats::timer(0);
}

void TCAstate::allDerefUnsafeAnalysis()
{
  traverseAOs(flag_readwrite?assignAllUnsafeRW:assignAllUnsafeW);
}

void TCAstate::tcFlowSensitiveAnalyses()
{
  if(flag_reaching_defs | flag_may_be_uninit | flag_redundant){ //- faster not to short-circuit
    cfg.prepareCFG(false);

    if(flag_reaching_defs){
      TCstats::timer("reaching-defs analysis");
      RDA rda;
      rda.doAnalysis(cfg);
      TCstats::timer(0);
    }
    if(flag_may_be_uninit){
      TCstats::timer("may-be-uninit analysis");
      if(flag_verbose) fprintf(stderr, "(mbu level %d)\n", flag_may_be_uninit);
      MBU mbu(flag_mbu_inter);
      mbu.doAnalysis(cfg);
      TCstats::timer(0);

      if(flag_reaching_defs){ // collect mbu-rda results

        RDMBfactHandler rmh(this->mbu_results);
        MBUc rdmb(rmh, flag_mbu_inter);

        TCstats::timer("collecting reaching-def/may-be-uninit results");
        rdmb.collectResults(cfg);
        TCstats::timer(0);

        //- for each result index object, propagateExposed on its ecr
        this->mbu_results.markAOsExposed();

      } else { // collect mbu-delta results

        TCstats::timer("collecting may-be-uninit/delta results");

        MBUc1factHandler c1h(this->mbu_results);
        MBUc mbuc1(c1h, flag_mbu_inter);
        if(flag_verbose) fprintf(stderr, "(phase 1)");
        mbuc1.collectResults(cfg);

        MBUc2factHandler c2h(this->mbu_results);
        MBUc mbuc2(c2h, flag_mbu_inter);
        if(flag_verbose) fprintf(stderr, "(phase 2)");
        mbuc2.collectResults(cfg);

        TCstats::timer(0);

        //- for each result index object, propagateExposed on its ecr
        this->mbu_results.markAOsExposed();
      }
    }
    if(flag_redundant){
      TCstats::timer("redundant-check (tp) analysis");
      RED red(REDtpFactHandler::handler, flag_red_paral2r);
      red.doAnalysis(cfg);
      TCstats::timer(0);

      if(flag_verbose) TCstats::timer("collecting redundant-check (tp) results");
      REDtpcFactHandler rtpcfh(this->red_results);
      REDc redc(rtpcfh);
      redc.collectResults(cfg);
      if(flag_verbose) TCstats::timer(0);
    }
  }
}

//- Mark starting points for relevant-aos filtering, controlled by flag
//  msp_relaos_mode : MBU or RAN mode
//     - if MBU mode, mark only safe derefs
//     - if RAN mode, mark unsafe derefs and tracked malloc sizes
//  Collect these in TCAstate::msp_starting_ecrs.
//- In RAN mode, also collect in TCAstate::msp_misc_dependencies
//  the following:
//  1. for assignments, dependencies not captured by flow-insensitive
//     assignment edges (e.g. if A is an array, x = A[i] is captured
//     only by x <--assign-- "op A", so we need to add the dependency
//     x <--dep-- i).
//  2. for predicates, dependency sets containing all aos in the
//     predicate (e.g., "x < y" generates the set {x,y} showing that
//     x and y depend on each other).
bool TCAstate::markStartingPoints(PgmExpr& pe)
{
  //- 1. verify ptr/ptrw
  if(pe.getKind() == PgmExpr::fVerify){
    PExprVerify& pev = (PExprVerify&) pe;
    if((pev.getVpKind() == PExprVerify::vpPtrW) ||
	(flag_readwrite && (pev.getVpKind() == PExprVerify::vpPtr))){
      bool is_safexp_deref = false;
      bool is_unsafe_deref = false;
      {
        suco_iterator<AO *> aoi(pev.getDesc().getAOs());
        while(aoi.Iterate()){
          if(aoi.Current()->getTSC() >= AO::TSC_EXPOSED)
            is_safexp_deref = true;
          else
            is_unsafe_deref = true;
        }
      }
      if(((TCAstate::msp_relaos_mode == relaos_MBU) && is_safexp_deref)||  //-MBU MODE: mark affaos of safe derefs
	(((TCAstate::msp_relaos_mode == relaos_RAN) && is_unsafe_deref))){ //-RAN MODE: mark affaos of unsafe deref
        suco_set<ECR *> affecrs;
        pev.getDesc().collectAffectingAliasLocECRs(affecrs);
        suco_iterator<ECR *> aei(affecrs);
        while(aei.Iterate()){
          TCAstate::markRelevantAndCollectInclFromECRs(aei.Current()->getAOset(), TCAstate::msp_starting_ecrs);
        }
      }
    }
  }
  //- 2. RAN mode: collect mallocsize AOs and dependencies
  if(TCAstate::msp_relaos_mode == relaos_RAN){
    //- 2a. malloc decl (future extension: any decl, if size argument may be a variable)
    //      - collect mallocsize affECRs
    if(pe.getKind() == PgmExpr::fDecl){
      PExprDecl& ped = (PExprDecl&) pe;
//TODO: filter out untracked aos? should only arise w/o heapstack, so let's not bother!?!
      if(ped.isMalloc() && ped.getMallocSize()){
        suco_set<ECR *> affecrs;
        ped.getMallocSize()->collectAffectingAliasLocECRs(affecrs); //-TODO: need only deref one level?
        suco_iterator<ECR *> aei(affecrs);
        while(aei.Iterate()){
          TCAstate::markRelevantAndCollectInclFromECRs(aei.Current()->getAOset(), TCAstate::msp_starting_ecrs);
        }
      }
    }
    //- 3. Collect dependencies (RAN mode only)
    if(pe.getKind() == PgmExpr::fAssign){
      PExprAssign& pea = (PExprAssign&) pe;
      suco_set<AO *> dep_aos;
      pea.getRHS().collectDependencies(dep_aos, true);
      if(!dep_aos.IsEmpty()){
        TCAstate::msp_misc_dependencies.addDirectedDependency(pea.getLHS().getAOs(), dep_aos); //- dep_aos is consumed
      }
    }
    if(pe.getKind() == PgmExpr::fPredicate){
      PExprPredicate& pep = (PExprPredicate&) pe;
      suco_set<AO *> dep_aos;
      pep.getDesc().collectDependencies(dep_aos, false);
      if(dep_aos.Length() > 1){ //- skip empty and singleton
        TCAstate::msp_misc_dependencies.addDependencySet(dep_aos); //- dep_aos is consumed
      }
    }
  }
  return true; //- continue traversal
}

void TCAstate::resetRelevantTag(AO& ao)
{
  ao.dfa_relevant = true;
}

void TCAstate::clearRelevantTag(AO& ao)
{
  ao.dfa_relevant = false;
}

void TCAstate::countRelevantAOs(AO& ao)
{
  TCAstate::crao_total_aos++;
  if(ao.dfa_relevant){
    if(flag_verbose){
      fprintf(stderr, "Relevant AO: ");
      ao.dump_descr(stderr);
      fprintf(stderr, "\n");
    }
    TCAstate::crao_marked_aos++;
  }
}
 
void TCAstate::markRelevantAndCollectInclFromECRs(suco_set<AO *>& aoset, suco_set<ECR *>& ecrset)
{
  suco_iterator<AO *> aoi(aoset);
  while(aoi.Iterate()){
    AO& ao = *aoi.Current();
    if(!ao.dfa_relevant && ao.isLocArgRet()){
      ao.dfa_relevant = true;
      ao.getECR().collectInclFromECRs(ecrset);
    }
  }
}

//- Marks relevant loc-AOs for two different (ptr/ptrw) analyses:
//  a. relmode=relaos_MBU : start with afflocs of safe and exposed
//	derefs, and propagate from there.
//  b. relmode=relaos_RAN : start with afflocs of unsafe derefs,
//	and propagate from there.
//  The 'readwrite' flag controls whether to start from vPtr+vPtrW
//  or vPtrW-only nodes.
//- The markings are carried by the AO::dfa_relevant tag.
void TCAstate::markRelevantLocs(bool readwrite, relaos_mode relmode)
{
  TCstats::timer("collect relevant locset");
  //  I. Clear dfa_relevant tags, and msp_starting_ecrs
  this->traverseAOs(TCAstate::clearRelevantTag);
  TCAstate::msp_starting_ecrs.Clear();

  //  II. Mark starting points (see markStartingPoints()).
  TCAstate::msp_relaos_mode = relmode;
  cfg.traverseRootNodes(TCAstate::markStartingPoints);

  // IIa: verbose output
  if(flag_verbose){
    TCAstate::crao_total_aos = 0;
    TCAstate::crao_marked_aos = 0;
    fprintf(stderr, "STARTING POINTS (%s):\n",
			(TCAstate::msp_relaos_mode == relaos_MBU)?"MBU":"RAN");
    this->traverseAOs(TCAstate::countRelevantAOs);
    fprintf(stderr, "--Starting Point AOs: %d, Total AOs: %d\n",
  			TCAstate::crao_marked_aos,
  			TCAstate::crao_total_aos);
    TCAstate::msp_misc_dependencies.debug_dump(stderr);
  }
  fprintf(stderr, "--Starting points: %d ecrs; Dependencies: %d directed, %d sets\n",
			TCAstate::msp_starting_ecrs.Length(),
			TCAstate::msp_misc_dependencies.getDirectedDependencies().Length(),
			TCAstate::msp_misc_dependencies.getDependencySets().Length());

  // III. Follow assignment edges and dependencies backwards:
  //      MBU: assignment edges only; RAN: assignments+dependencies.
  //      (Note: don't filter by relmode, since locs of any classification
  //       may affect the points of interest)
  suco_set<ECR *> pending_ecrs;
  pending_ecrs.Attach(TCAstate::msp_starting_ecrs);
  do {
    suco_set<ECR *> active_ecrs;
    active_ecrs.Attach(pending_ecrs);

/**/fprintf(stderr, "DEBUG relevant: active_ecrs = %d...\n", active_ecrs.Length());

    //- check dependencies (RAN only)
    if(TCAstate::msp_relaos_mode == relaos_RAN){
      suco_set<ECR *> new_ecrs;
      suco_iterator<Dependency *> ddi(TCAstate::msp_misc_dependencies.getDirectedDependencies());
      while(ddi.Iterate()){
        if(active_ecrs.Contains(&ddi.Current()->getKey().getECR())){
          TCAstate::markRelevantAndCollectInclFromECRs(ddi.Current()->getAOs(), pending_ecrs);
          ddi.DeleteCurrent(); //- WARNING: leaking a Dependency
        }
      }
      suco_iterator<suco_set<AO *> *> dsi(TCAstate::msp_misc_dependencies.getDependencySets());
      while(dsi.Iterate()){
        //- check to see if active_ecrs intersects dsi.Current()
        suco_iterator<AO *> daoi(*dsi.Current());
        while(daoi.Iterate()){
          if(active_ecrs.Contains(&daoi.Current()->getECR())){
            //- they intersect
            TCAstate::markRelevantAndCollectInclFromECRs(*dsi.Current(), pending_ecrs);
            dsi.DeleteCurrent(); //- WARNING: leaking an ao-list
            break;
          }
        }
      }
    }

    //- collect incoming edges
    suco_set<TCassignEdge *> edges;
    suco_iterator<ECR *> ecri(active_ecrs);
    while(ecri.Iterate()){
      suco_iterator<AO *> aoi(ecri.Current()->getAOset());
      while(aoi.Iterate()){
        edges.Union(aoi.Current()->getIncomingAssignEdges());
      }
    }

    //- follow edges backwards
    suco_iterator<TCassignEdge *> aei(edges);
    while(aei.Iterate()){
      //- collect affecting ecrs: "from" aliases + de-starred ao aliases
      suco_set<ECR *> from_ecrs;
      aei.Current()->getFrom().getECR().collectAliasECRs(from_ecrs, true);

      //- collect unprocessed back-prop'ed ecrs: for each irrelevant
      //  loc-ao* in from_ecrs, follow incl-edges backwards
      //  (*loc-ao, or all aos?)
      suco_iterator<ECR *> fecri(from_ecrs);
      while(fecri.Iterate()){
        TCAstate::markRelevantAndCollectInclFromECRs(fecri.Current()->getAOset(), pending_ecrs);
      }
    }
  } while(!pending_ecrs.IsEmpty());

  //  V. Diagnostic: count relevant aos
  TCAstate::crao_total_aos = 0;
  TCAstate::crao_marked_aos = 0;
  this->traverseAOs(TCAstate::countRelevantAOs);
  fprintf(stderr, "%s Relevant AOs: %d, Total AOs: %d\n",
			(TCAstate::msp_relaos_mode == relaos_MBU)?"MBU":"RAN",
  			TCAstate::crao_marked_aos,
  			TCAstate::crao_total_aos);

  TCstats::timer(0);
}

void TCAstate::ptrFlowSensitiveAnalyses()
{
  if(flag_may_be_uninit | flag_redundant | flag_range){
    cfg.prepareCFG(flag_range);

    if(flag_may_be_uninit){
      //- collect relevant aos: safe pointers that would otherwise be checked
//OBSOLESCE!
//      if(flag_filter_relevant_aos){
//        this->markRelevantLocs(flag_readwrite, relaos_MBU);
//      }

      pMBUfactHandler::readwrite = flag_readwrite;

      if(flag_pmbu_sensitive){ //- flow-sensitive

/**/fprintf(stderr, "WARNING: pmbu/flow-sensitive not yet supported.\n");

//--OBSOLETE/TODO: flow-sensitive pMBU analysis
// - currently in non-correct condition: must rewrite
      //- (ptr/ptrw)-mode mbu analysis
//      TCstats::timer("may-be-uninit analysis");
//      pMBUfactHandler pmbuh();
//      MBU pmbu(false, pmbuh);
//      pmbu.doAnalysis(cfg, true);	//-true=skip_libfns
//      if(flag_verbose) TCstats::timer(0);

      } else { //- flow-insensitive

        TCstats::timer("may-be-uninit analysis (flow-insensitive)");
        pMBUfactHandler::doInsensitiveAnalysis(this->cfg);
        TCstats::timer(0);

      }

      this->traverseAOs(pMBUfactHandler::collectUnsafeTracked);

      fprintf(stderr, "pMBU: %d locs, %d unsafe, %d tracked\n",
			pMBUfactHandler::results.Length(),
			pMBUfactHandler::unsafe.Length(),
			pMBUfactHandler::tracked.Length());

      //- done: reset relevant tags
//      if(flag_filter_relevant_aos){
//        this->traverseAOs(TCAstate::resetRelevantTag);
//      }
    }

    if(flag_redundant){
      //- REDUNDANT CHECK ANALYSIS
      if(flag_readwrite){ //- ptr

        TCstats::timer("redundant-check (p) analysis");
        RED red(REDpFactHandler::handler, flag_red_paral2r);
        red.doAnalysis(cfg);
        TCstats::timer(0);

        if(flag_verbose) TCstats::timer("collecting redundant-check (p) results");
        REDpcFactHandler rcfh(this->red_results);
        REDc redc(rcfh);
        redc.collectResults(cfg);
        if(flag_verbose) TCstats::timer(0);

      } else { //- ptrw

        TCstats::timer("redundant-check (pw) analysis");
        RED red(REDpwFactHandler::handler, flag_red_paral2r);
        red.doAnalysis(cfg);
        TCstats::timer(0);

        if(flag_verbose) TCstats::timer("collecting redundant-check (pw) results");
        REDpwcFactHandler rcfh(this->red_results);
        REDc redc(rcfh);
        redc.collectResults(cfg);
        if(flag_verbose) TCstats::timer(0);
      }
    }

    if(flag_range){
      //- RANGE ANALYSIS
      if(flag_filter_relevant_aos){
        this->markRelevantLocs(flag_readwrite, relaos_RAN);
      }
      {
        TCstats::timer("range analysis - widening");
        if(flag_verbose) fprintf(stderr, "(widening %s)\n", flag_range_widen_always?"always":"on backedge");
        RAN ranw(RANfactHandler::handler, flag_range_inter, RAN::WN_WIDEN, flag_range_widen_always);
        ranw.doAnalysis(cfg);
        TCstats::timer(0);
      }
      if(flag_range_do_narrow){
        TCstats::timer("range analysis - phase 2: narrowing");
        if(flag_verbose) fprintf(stderr, "(narrowing %s)\n", flag_range_narrow_always?"always":"on backedge");
        RAN rann(RANfactHandler::handler, flag_range_inter, RAN::WN_NARROW, flag_range_narrow_always);
        rann.doAnalysis(cfg);
        TCstats::timer(0);
      }
      {
        TCstats::timer("collecting range-analysis results");
        RANcFactHandler rcfh(this->ran_results, flag_readwrite);
        RANc ranc(rcfh, flag_range_inter);
        flag_debug_range_collect_phase = true; //- debug flag, to output known-pred info
        ranc.collectResults(cfg);
        TCstats::timer(0);
        if(flag_range_collect_stats){
          this->ran_num_all_vps = rcfh.countAllVPs(false);
          this->ran_num_finite_vps = rcfh.countFiniteVPs(false);
          this->ran_num_half_finite_vps = rcfh.countHalfFiniteVPs(false);

          this->ran_num_array_all_vps = rcfh.countAllVPs(true);
          this->ran_num_array_finite_vps = rcfh.countFiniteVPs(true);
          this->ran_num_array_half_finite_vps = rcfh.countHalfFiniteVPs(true);
          this->ran_num_array_inbounds_vps = rcfh.countInboundsVPs(true);

          this->ran_num_known_preds = rcfh.countKnownPreds();
        }
        //- inb-vps: do always
        this->ran_num_inbounds_vps = rcfh.countInboundsVPs(false);
        this->ran_num_inbounds_sdotarrows = rcfh.countInboundsSdotArrows();
      }
      if(flag_filter_relevant_aos){
        this->traverseAOs(TCAstate::resetRelevantTag);
      }
    }
  }
}

void TCAstate::setAddrofDerefUDotUnsafe(AO& ao)
{
  //-----------------------------------------------
  //-- find all addr-of objects, mark target unsafe
  if(ao.getKind() == AO::aoAddrOf){
    AOAddrOf& ado = (AOAddrOf&) ao;
    ado.getTarget().setTS_UNSAFE(); //- propagates along assign edges
  }

  //---------------------------------------------
  //-- find all dereference nodes, mark as unsafe
  if(ao.getKind() == AO::aoStar){
    ao.setTS_UNSAFE(); //- propagates along assign edges
  }

  //----------------------------------------
  //-- find all union-dot nodes, mark as unsafe
  if(ao.getKind() == AO::aoUDot){
    ao.setTS_UNSAFE(); //- propagates along assign edges
    //- set parent unsafe also
    ((AOUDot &)ao).getParent().setTS_UNSAFE();
  }
}

void TCAstate::markDerefUnsafeAddrofTracked(AO& ao)
{
  if(ao.isRef())
    ao.setTSC(AO::TSC_POSS_INVALID);

  if(ao.getKind() == AO::aoMalloc)
    ao.setTSC(AO::TSC_EXPOSED);

  if(ao.getKind() == AO::aoAddrOf)
    ((AOAddrOf&)ao).getTarget().setTSC(AO::TSC_EXPOSED);
}

void TCAstate::addrAssignAnalysis(bool ptrptrw)
{
  traverseAOs(ptrptrw?markDerefUnsafeAddrofTracked:setAddrofDerefUDotUnsafe);
}

bool TCAstate::isVulnFunction(const char * cp)
{
  const char * fnlist[] = {
		".execl",
		".execlp",
		".execle",
		".execv",
		".execvp",
		".execve",
		".system",
		".popen",
		".longjmp",
		"._longjmp",
		".siglongjmp",
	};
  for(int i = 0; i < (sizeof(fnlist)/sizeof(fnlist[0])); ++i){
    if(!strcmp(cp, fnlist[i]))
      return true;
  }
  return false;
}

// Search for and mark vulnerable locations:
//  1. Function pointers are vulnerable
//  2. Vulnerable function arguments are vulnerable
//     (vulnerable functions: exec, system, popen, longjmp,...
//      - see isVulnFunction() above for full list)
//     *NOTE*: currently coded to assume the vulnerable
//     locations are:
//      a. the first argument of the vulnerable function
//      b. what the first argument points to
//     e.g., system(p) ==> p and what p points to are vulnerable.
//     This fits the currently-recognized vulnerable functions,
//     but would need to be changed for future extensions.
bool TCAstate::collectVulnerable(PgmExpr& expr)
{
  if(expr.getKind() == PgmExpr::fCall){
    PExprCall& call_expr = (PExprCall&)expr;

    //- A. mark vulnerable function pointers
    if(!call_expr.isDirectCall()){
      suco_iterator<AO *> faoi(call_expr.getFaos());
      while(faoi.Iterate()){
        AO& fao = *faoi.Current();
        if(fao.isLoc()){
          fao.markVulnerableLocAndPropagate("Function Pointer");
        } else {
          suco_iterator<ECR *> ecri(fao.getECR().getAliasECRs());
          while(ecri.Iterate()){
            suco_iterator<AO *> aoi(ecri.Current()->getAOset());
            while(aoi.Iterate()){
              if(aoi.Current()->isLoc()){
                aoi.Current()->markVulnerableLocAndPropagate("Function Pointer");
              }
            }
          }
        }
      }
    }

    //- B. look for vulnerable functions
    const char * vuln_fn_name = 0;
    { //-1. search TargetFns -- currently vuln fns should be undefined, so this block should yield nothing
      suco_iterator<CFGfunction *> tfi(call_expr.getTargetFns());
      while(tfi.Iterate()){
        const char * idname = tfi.Current()->getId().getPid().getname();
        if(TCAstate::isVulnFunction(idname)){
          vuln_fn_name = idname;
          break;
        }
      }
    }
    if(!vuln_fn_name){
      //-2. search undefTargetFns
      suco_iterator<AOId *> utfi(call_expr.getUndefTargetFns());
      while(utfi.Iterate()){
        const char * idname = utfi.Current()->getPid().getname();
        if(TCAstate::isVulnFunction(idname)){
          vuln_fn_name = idname;
          break;
        }
      }
    }
    //- if this is a vulnerable call, do stuff
    if(vuln_fn_name){
      PExprArg * arg1_expr = call_expr.getArg(1);
      if(arg1_expr){
        suco_iterator<AO *> arg1_aoi(arg1_expr->getDesc().getAOs());
        while(arg1_aoi.Iterate()){
          AO& arg1_ao = *arg1_aoi.Current();

          //- mark as vulnerable location:
          //  - if arg1_ao is a location, then arg1_ao
          //  - else, loc-aliases(arg1_ao)
          if(arg1_ao.isLoc()){
            arg1_ao.markVulnerableLocAndPropagate(vuln_fn_name);
          } else {
            suco_iterator<ECR *> ecri(arg1_ao.getECR().getAliasECRs());
            while(ecri.Iterate()){
              suco_iterator<AO *> aoi(ecri.Current()->getAOset());
              while(aoi.Iterate()){
                if(aoi.Current()->isLoc()){
                  aoi.Current()->markVulnerableLocAndPropagate(vuln_fn_name);
                }
              }
            }
          }
          //- also, mark as vulnerable locations pt-set(arg1_ao)
          {
            suco_iterator<ECR *> ecri(arg1_ao.getECR().getPointsTo().getAliasECRs());
            while(ecri.Iterate()){
              suco_iterator<AO *> aoi(ecri.Current()->getAOset());
              while(aoi.Iterate()){
                if(aoi.Current()->isLoc()){
                  aoi.Current()->markVulnerableLocAndPropagate(vuln_fn_name);
                }
              }
            }
          }
        }
      } else fprintf(stderr, "WARNING(collectVulnerable): call to %s missing argument 1\n", vuln_fn_name);
    }
  }
  return true; //- true ensures full traversal
}

void TCAstate::computeVulnerable()
{
  cfg.prepareCFG(false);
  cfg.traverseRootNodes(TCAstate::collectVulnerable);
}

void TCAstate::writeVulnerable(FILE * outf)
{
  AO::aoWriteStream = outf;
  traverseAOs(AO::writeVulnerable);
  AO::aoWriteStream = 0;
}

//NOTE: the signatures capture the behavior of our wrappers,
//	which doesn't always match the actual behavior!
//	(in particular, our wrappers were writting with
//	-ptrw mode in mind, and doesn't always check reads?)
//TODO: for the retval-sensitive functions (those that return
//	a static buffer), I currently just comment them out.
//	Don't uncomment them unless handling them; else may
//	erroneously de-_typecheck_ some calls.
const char * TCAstate::interestingLibFnSignature(ID * fnid)
{
  //-These describe the behavior of _typecheck_ versions: see dynamic/tcptrlibc.c
  //  r=read
  //  w=write
  //  x=no
  //  v=vararg: vr/vw/vx
  //-0th spot = retval (currently unused)
  //-1st spot = arg 1, ...
  static struct {
    const char * fnname;
    const char * signature;
  } libfns[] = {
	{".fgets","_w"},
	{".gets","_w"},
	{".fread","_w"},
	{".scanf","_xvw"},
	{".fscanf","_xxvw"},
	{".sscanf","_xxvw"},
				//NOTE (sprintf,snprintf): current signatures ("_w","_w") are unsafe;
				//	should actually be ("_wvw","_wxxvw"), plus some coordination with
				//	pctn (not currently done for sprintf,sprintf -- TODO).
				//	HOWEVER, current version is "conservative" in that it doesn't
				//	allow any "%n" to write into a vulnerable location.
	{".sprintf","_w"},
	{".snprintf","_w"},
//	{".printf,"_"},		//- "xvw" ~~> handled by "pctn"?
//	{".fprintf,"_"},	//- "xxvw" ~~> handled by "pctn"?
	{".free","_w"},
//	{".realloc","_?x"},
	{".bzero","_w"},
	{".memcpy","_wr"},
	{".memmove","_wr"},
	{".strcpy","_wr"},
	{".strncpy","_wr"},
	{".strcat","_wr"},
	{".strncat","_wr"},
//	{".strxfrm","_wrx"},
	{".memset","_w"},
//	{".strerror","_"},
	{".memccpy","_wr"},
//	{".strsignal","_"},
//	{".strdup","_r"},
//	{".nl_langinfo","_x"},
//	{".stat","_rw"},
//	{".lstat","_rw"},
//	{".fstat","_xw"},
//	{".ctime","_r"},
//	{".localtime","_r"},
//	{".gmtime","_r"},
//	{".asctime","_r"},
//	{".getpass","_r"},
//	{".read","_xwx"},
//	{".pread","_xwxx"},
//	{".readv","_xwx"},
//	{".gethostbyname","_r"},
//	{".gethostbyaddr","_rxx"},
  };

  if(fnid){
    int i;
    for(i = 0; i < (sizeof(libfns)/sizeof(libfns[0])); ++i){
      if(!strcmp(fnid->getname(), libfns[i].fnname)){
        return libfns[i].signature;
      }
    }
  }
  return 0;
}

//-------------------------------------------
// For each "interesting" function (described by
// TCAstate::interestingLibFnSignature() above)
// determine whether we should call the instrumented
// or uninstrumented version of the function by checking
// whether the relevant argument points to a tracked/
// vulnerable location.
// NOTE/TODO: currently includes "free", which sort of
//  doubles the functionality of "/" computed elsewhere.
//  The difference is that "/" also computes the status
//  in "limit_malloc" mode, which isn't done here.
//  So for now, continue to use "/" instead of this.
//  (Even though, technically, "limit_malloc" mode is
//  unsafe.)
bool TCAstate::writeLibfnTrackStatus(PgmExpr& expr)
{
  if(expr.getKind() == PgmExpr::fCall){
    PExprCall& call_expr = (PExprCall&)expr;

    //- look for interesting library functions
    const char * libfn_signature = interestingLibFnSignature(call_expr.isDirectCall());
    if(libfn_signature){
      ExposedStatus es_ptr = ExposedStatus::NA;
      ExposedStatus es_vuln = ExposedStatus::NA;
      int i;
      bool varg_mode = false;
      PExprArg * arg_expr;
      for(i = 1; arg_expr = call_expr.getArg(i); ++i){
        if(!varg_mode && !libfn_signature[i]){
          break; //-done
        }
        if(!varg_mode && libfn_signature[i] == 'v'){
          if((libfn_signature[i+1] == 'w')
		|| (libfn_signature[i+1] == 'r' && flag_readwrite == false)){
            varg_mode = true;
          } else {
            break; //-done
          }
        }
        if(varg_mode
		|| (libfn_signature[i] == 'w')
		|| (libfn_signature[i] == 'r' && flag_readwrite == false)){
          suco_iterator<AO *> aoi(arg_expr->getDesc().getAOs());
          while(aoi.Iterate()){
            es_ptr.combine(aoi.Current()->pointsToExposed(false, flag_range, false));
            es_vuln.combine(aoi.Current()->pointsToExposed(false, flag_range, true));
          }
        }
        if(es_ptr.isSome() && es_vuln.isSome()) break; //- short circuit; no need to check further
      }
      AID& call_aid = call_expr.getAid();
      fprintf(AO::aoWriteStream, "! %d f p %c %d\n", call_aid.filestem_id, es_ptr.exposedCode(), call_aid.aid);
      fprintf(AO::aoWriteStream, "! %d f v %c %d\n", call_aid.filestem_id, es_vuln.exposedCode(), call_aid.aid);
    }
  }
  return true; //- true ensures full traversal
}

void TCAstate::outputLibfnTrackStatus(FILE * outf)
{
  cfg.prepareCFG(false);
  AO::aoWriteStream = outf; //- borrow this stream pointer :-D
  cfg.traverseRootNodes(TCAstate::writeLibfnTrackStatus);
  AO::aoWriteStream = 0;
}

void TCAstate::writeTSoutput(FILE * outf, bool tsl)
{
  //-- assign unique IDs to aidtab and pidtab entries
  unsigned int naids = aidtab.assignUIDs(1);
  unsigned int npids = pidtab.assignUIDs(naids);

  if(flag_verbose) fprintf(stderr, "#Aids = %d, #Pids = %d\n", naids-1, npids-naids);

  fprintf(stderr, (tsl)?"---Outputting ts_levels\n":"---Outputting ts_categs\n");

  fprintf(outf, "# %s\n", (tsl)?"(addr-taken TSLs)":((flag_all_deref_unsafe)?"(tracked-only TSCs)":"(flow-insens TSCs)"));

  //----------------------------------------
  //-- collect stats on number of different types of nodes
  TCstats::doAssigns(outf, assigns);
  traverseAOs(TCstats::doAO);

  //----------------------------------------
  //-- output stats
  TCstats::print(outf);

  //-- first: for address-taken analysis, default ts-level is SAFE
  fprintf(outf, "^ S\n");

  //-- next, output aliases
  aidtab.output_aliases(outf, '@');
  pidtab.output_aliases(outf, '%');

  //- traverse abstract objects to assign ecrnos, and output inclTo lists
  if(!tsl){
    AO::aoWriteStream = outf;
    traverseAOs(AO::assignEcrNosAndWriteToFile);
    AO::aoWriteStream = 0;
  }

  //- traverse abstract objects to output safety level
  AO::aoWriteStream = outf;
  traverseAOs((tsl)?(AO::writeTSlevel):(AO::writeTScateg));
  AO::aoWriteStream = 0;

  //- A. traverse argument to free()
  //     check to see if points-to target is all tracked,
  //     all untracked, or a mix (incl untouched under -ran)
  //     NOTE: assumes direct calls only!  TODO: handle indirect calls
  //- B. traverse argument to vulnerable functions (longjmp, exec*, system, popen)
  //     output comment summarizing their trackedness/touchedness
  //     NOTE: again, assume direct calls only!  TODO: handle indirect calls
  if(!tsl){
    suco_iterator<TCassignEdge *> aei(assigns);
    while(aei.Iterate()){
      if(aei.Current()->getTo().getKind() == AO::aoArg){
	AOArg& aao = (AOArg &) aei.Current()->getTo();
        if(aao.argNo() == 1 && aao.getParent().getKind() == AO::aoStar){
          AOStar& sao = (AOStar &)aao.getParent();
          if(sao.getTarget().getKind() == AO::aoId){
            ID& fid = ((AOId &)sao.getTarget()).getPid();
            if(!strcmp(fid.getname(),".free")){
              //-- found an argument to free
              AO& fao = aei.Current()->getFrom();
	      if(!fao.isVal()){ //- skip value AOs
                if(flag_do_addr_taken) fprintf(outf, "/ A A ");
                else fprintf(outf, "/ %c %c ",  fao.pointsToExposed(false, flag_range, false).exposedCode(),
						fao.pointsToExposed(true, flag_range, false).exposedCode());
                fao.write_string_rep(outf, flag_readable_output);
                fprintf(outf, "\n");

                //- vuln mode output
                if(flag_vuln){ //-TODO: should only output in ptr/ptrw mode!!!
                  fprintf(outf, "/V%c %c ", fao.pointsToExposed(false, flag_range, true).exposedCode(),
					    fao.pointsToExposed(true, flag_range, true).exposedCode());
                  fao.write_string_rep(outf, flag_readable_output);
                  fprintf(outf, "\n");
                }
              }
            }
            if(TCAstate::isVulnFunction(fid.getname())){
              //-- found an argument to vuln function
              AO& fao = aei.Current()->getFrom();
	      if(!fao.isVal()){ //- skip value AOs
                fprintf(outf, "# vuln%s(", fid.getname());
                fao.write_string_rep(outf, true);
                fprintf(outf, "):\n");
                //-----------------------------------------------------
                // Hackery Alert:
                //  What we're trying to do here is figure out the
                //  tracked-ness status of first argument of vuln fns,
		//  in particular setjmp buffers.
                //  Here, fao is the first argument to longjmp.
                //  Question: is fao the buffer, or a pointer to the
                //  buffer?
                //  Answer: looks like it's implementation dependent:
                //  fao is of type "jmp_buf" which (on my Linux gcc)
                //  is an array of size one which gets translated to
                //  a pointer, but another implementation may choose
                //  not to pass-by-reference.
                //  Therefore: I'll look up the static type of the
                //  argument (aao), and if it is a pointer, then
                //  look at fao's points-to set; otherwise look at
                //  fao's alias set.
                //-----------------------------------------------------
                bool getpointsto = true; //- assume pass-by-reference by default
                if(aao.getStaticType()){
                  getpointsto = (aao.getStaticType()->getKind() == TCtype::tcPointer);
                } else fprintf(stderr, "ERROR(writeTSoutput): %s argument static type unknown\n", fid.getname());
                suco_iterator<ECR *> ei((getpointsto) ? fao.getECR().getPointsTo().getAliasECRs()
						      : fao.getECR().getAliasECRs()
					);
                while(ei.Iterate()){
                  suco_iterator<AO *> aoi(ei.Current()->getAOset());
                  while(aoi.Iterate()){
                    if(aoi.Current()->isLoc() &&
			aoi.Current()->getKind() != AO::aoStringLit && //- filter out string literals!?
			(&aoi.Current()->getEnclosingStruct()) == aoi.Current()){ //- only output outermost struct
										  //  TODO: this is a cheap test that
										  //  only works with collapseAlways ECR mapping!
                      fprintf(outf, "#  arg%s(%c/%c): ", fid.getname(),
				ei.Current()->exposed?'E':'S',	//- exploitation of friendship :-D
				ei.Current()->touched?'T':'U');
                      aoi.Current()->write_string_rep(outf, true);
                      fprintf(outf, "\n");
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  //- output DFA results
  AID::writeFileStemMap(outf);

  //- outputs per-aid tracked-status for interesting library functions
  //  (free, strcpy, etc.)
  //  NOTE: for free, duplicates (?) "/" output above
  TCAstate::outputLibfnTrackStatus(outf);

  if(flag_may_be_uninit){
    this->mbu_results.writeResults(outf);
    pMBUfactHandler::writeResults(this->cfg, outf);
  }

  //- output RED results
  if(flag_redundant){
    this->red_results.writeResults(outf);
  }

  //- output RAN results
  if(flag_range){
    if(flag_range_collect_stats){
      fprintf(outf, "# RAN-count vps-all = %d\n", this->ran_num_all_vps);
      fprintf(outf, "# RAN-count vps-finite = %d\n", this->ran_num_finite_vps);
      fprintf(outf, "# RAN-count vps-half-finite = %d\n", this->ran_num_half_finite_vps);
      fprintf(outf, "# RAN-count vps-array-all = %d\n", this->ran_num_array_all_vps);
      fprintf(outf, "# RAN-count vps-array-finite = %d\n", this->ran_num_array_finite_vps);
      fprintf(outf, "# RAN-count vps-array-half-finite = %d\n", this->ran_num_array_half_finite_vps);
      fprintf(outf, "# RAN-count vps-array-inbounds = %d\n", this->ran_num_array_inbounds_vps);
      fprintf(outf, "# RAN-count known-preds = %d\n", this->ran_num_known_preds);
    }
    //- inb-vps: do always
    fprintf(outf, "# RAN-count vps-inbounds = %d\n", this->ran_num_inbounds_vps);
    fprintf(outf, "# RAN-count vps-inb-sdotarrow = %d\n", this->ran_num_inbounds_sdotarrows);
    suco_iterator<PExprVerify *> bci(this->ran_results);
    while(bci.Iterate()){
      AID& aid = bci.Current()->getAid();
      fprintf(outf, "! %d b %d\n", aid.filestem_id, aid.aid);
    }
  }

  //- output if exposed AOs are untouched
  if(flag_range){ //- future extension: other analyses may touch also
    AO::aoWriteStream = outf;
    traverseAOs(AO::writeUntouchedExposed);
    AO::aoWriteStream = 0;
  }
}

void TCAstate::printDerefAliases(AO& ao)
{
  if(ao.isRef()){
    ao.write_string_rep(AO::aoWriteStream, true);
    fprintf(AO::aoWriteStream, ": ");
    suco_set<AO *> alias_aos;
    suco_iterator<ECR *> ei(ao.getECR().getAliasECRs());
    while(ei.Iterate())
      alias_aos.Union(ei.Current()->getAOset());
    AO::write_list_string_rep(AO::aoWriteStream, alias_aos, true);
    fprintf(AO::aoWriteStream, "\n");
  }
}

void TCAstate::writeAliases(FILE * outf)
{
  AO::aoWriteStream = outf;
  traverseAOs(TCAstate::printDerefAliases);
  AO::aoWriteStream = 0;
}

void TCAstate::writeCallgraph(FILE * outf, bool dot, char * trace_fn)
{
  cfg.writeCallgraph(outf, dot, trace_fn);
}

FILE * daf_outf = 0;
void dump_ao_fun(AO& ao) { ao.debug_dump(daf_outf); fprintf(daf_outf, "\n"); }

void TCAstate::summary_dump(FILE * outf)
{
  daf_outf = outf;
  fprintf(stderr, " --Dumping Value-AOs...\n");
  fprintf(outf, "--VALUE-AOs:\n");
  values.traverseAOs(dump_ao_fun);

  fprintf(stderr, " --Dumping Aid-AOs...\n");
  fprintf(outf, "--AID-AOs:\n");
  aidtab.traverseAOs(dump_ao_fun);

  fprintf(stderr, " --Dumping Pid-AOs...\n");
  fprintf(outf, "--PID-AOs:\n");
  pidtab.traverseAOs(dump_ao_fun);
  daf_outf = 0;

  fprintf(stderr, "---Done Dumping AOs\n");

  fprintf(stderr, " --Dumping Assignments...\n");
  fprintf(outf, "--ASSIGNS:\n");
  suco_iterator<TCassignEdge *> ai(assigns);
  while(ai.Iterate()){
    ai.Current()->getTo().write_string_rep(outf, true);
    fprintf(outf, "= ");
    ai.Current()->getFrom().write_string_rep(outf, true);
    fprintf(outf, "\n");
  }

  fprintf(stderr, " --Dumping Arg-Ret Assignments...\n");
  fprintf(outf, "--ARG-RET-ASSIGNS:\n");
  suco_iterator<TCassignEdge *> arai(arg_ret_assigns);
  while(arai.Iterate()){
    arai.Current()->getTo().write_string_rep(outf, true);
    fprintf(outf, "= ");
    arai.Current()->getFrom().write_string_rep(outf, true);
    fprintf(outf, "\n");
  }

}

void TCAstate::debug_dump(FILE * outf)
{
  fprintf(stderr, "---Dumping State:\n");

  fprintf(stderr, " --Dumping Aidtab...\n");
  fprintf(outf, "--AIDTAB:\n");
  aidtab.debug_dump(outf);

  fprintf(stderr, " --Dumping Pidtab...\n");
  fprintf(outf, "--PIDTAB:\n");
  pidtab.debug_dump(outf);

  daf_outf = outf;
  fprintf(stderr, " --Dumping Value-AOs...\n");
  fprintf(outf, "--VALUE-AOs:\n");
  values.traverseAOs(dump_ao_fun);

  fprintf(stderr, " --Dumping Aid-AOs...\n");
  fprintf(outf, "--AID-AOs:\n");
  aidtab.traverseAOs(dump_ao_fun);

  fprintf(stderr, " --Dumping Pid-AOs...\n");
  fprintf(outf, "--PID-AOs:\n");
  pidtab.traverseAOs(dump_ao_fun);
  daf_outf = 0;

  fprintf(stderr, " --Dumping Assignments...\n");
  fprintf(outf, "--ASSIGNS:\n");
  suco_iterator<TCassignEdge *> ai(assigns);
  while(ai.Iterate()) ai.Current()->debug_dump(outf);

  fprintf(stderr, " --Dumping Arg-Ret Assignments...\n");
  fprintf(outf, "--ARG-RET-ASSIGNS:\n");
  suco_iterator<TCassignEdge *> arai(arg_ret_assigns);
  while(arai.Iterate()) arai.Current()->debug_dump(outf);

  fprintf(stderr, " --Dumping ECRs/Points-to Graph...\n");
  fprintf(outf, "--ECR/PT Graph:\n");
  AO::aoWriteStream = outf;
  traverseAOs(AO::writeECR);
  AO::aoWriteStream = 0;

  fprintf(stderr, " --Dumping CFG...\n");
  fprintf(outf, "--CFG:\n");
  cfg.debug_dump(outf);
  fprintf(stderr, "---Done Dumping State\n");

  if(flag_may_be_uninit){
    fprintf(stderr, " --Dumping MBU results...\n");
    fprintf(outf, "--MBU results:");
    this->mbu_results.debug_dump(outf, false);
    fprintf(stderr, " --Done Dumping MBU results\n");

    fprintf(stderr, " --Dumping pMBU results...\n");
    fprintf(outf, "--pMBU aos:\n");
    suco_iterator<AO *> pmi(pMBUfactHandler::results);
    while(pmi.Iterate()){
      pmi.Current()->dump_descr(outf);
      fprintf(outf, "\n");
    }
    fprintf(stderr, " --Done Dumping pMBU results\n");
  }

  if(flag_redundant){
    fprintf(stderr, " --Dumping RED results...\n");
    this->red_results.debug_dump(outf);
    fprintf(stderr, " --Done Dumping RED results\n");
  }

  if(flag_range){
    fprintf(stderr, " --Dumping RAN results...\n");
    
    fprintf(outf, "--RAN results:");
    suco_iterator<PExprVerify *> bci(this->ran_results);
    while(bci.Iterate())
      bci.Current()->debug_dump(outf, 3, true);

    fprintf(stderr, " --Done Dumping RAN results\n");
  }
}

//------------------------------------------------------

int main(int argc, char * argv[])
{
  const char * outfile = 0;
  enum { tcm_pta, tcm_callgraph, tcm_ptr, tcm_ptrw, tcm_full } mode = tcm_full;
  bool debug_mode = false;
  bool summary_mode = false;
  bool no_output = false;
  bool callgraph_dot = false;
  char * calltrace = 0;
  int local_flag_range_collect_stats = -1;

  if(argc <= 1)
    return fprintf(stderr, "Usage: %s [options] files\n"
			   "   [options] are:\n"
			   "     -o outfile   output filename (default a.tc_tsls)\n"
			   "     -addr        address-taken analysis\n"
			   "     -pta         points-to analysis only (no TS-level output)\n"
			   "     -callgraph   dump callgraph info only\n"
			   "      -callgraph-dot  output in dot format\n"
			   "     -calltrace=<fn_name>\n"
			   "                  dump non-recursive call trace(s) for function fn_name\n"
			   "     -nt          full RTC(newtags) type-safety analysis (default action)\n"
			   "     -ptr         mark invalid non-null pointer only (for security tool)\n"
			   "     -ptrw        mark invalid deref-written non-null pointer only\n"
			   "       -strlit-writable in ptrw mode: are strlits writable? (default no)\n"
			   "       -unsafe-derefs   all derefs unsafe; only compute tracked (default no)\n"
			   "    [-no]-vuln    output vulnerable locations and dereferences (default yes)\n"
			   "     -heap        unsafe to point to freed-heap (default off)\n"
			   "     -heapstack   unsafe to point to freed-heap or stack (default off)\n"
			   "                  (NOTE: these were previously -ptr-heap/-ptr-heapstack)\n"
			   "\n"
			   "     -dfa         dataflow (flow-sensitive) analysis\n"
			   "                  turns on -mbu1\n"
			   "    [-no]-vtfix   treat verifyTag as fixing type on mismatch (default no)\n"
			   "     -mbu[1234]   may-be-uninit analysis level\n"
			   "                  1: full (default)\n"
			   "                  2: don't assign return values\n"
			   "                  3: don't assign args/return values\n"
			   "                  4: don't propagate via asssignments at all\n"
			   "     -mbu-intra/-mbu-inter\n"
			   "                  do may-be-uninit intra/inter-procedurally (default inter)\n"
			   "     -mbu-intra-iref\n"
			   "                  do may-be-uninit intra-procedurally, use iref (default no)\n"
			   "     -pmbu-sensitive/-pmbu-insensitive\n"
			   "                  vp may-be-uninit flow-insensitive (default) or sensitive\n"
			   "     -rda         do reaching-defs analysis\n"
			   "     -red         do redundant-check analysis\n"
			   "      -red-par, red-no-par\n"
			   "                  parallels do two rounds (default yes)\n"
			   "     -range       do range analysis (-ptr/-ptrw only)\n"
			   "      -ran-intra/-ran-inter\n"
			   "                  do range analysis intra/inter-procedurally (default intra)\n"
			   "     -ran-[DO|LO] interval use DO or LO representation (default DO)\n"
			   "       -ran-must-ao/-ran-must-ao-no\n"
			   "                  must have AO?  (LO=>yes, DO=>no)\n"
			   "       -ran-tychange-[yes|no]\n"
			   "                  allow descriptor type change?  (LO=>no, DO=>yes)\n"
			   "       -ran-sliding-[yes|no]\n"
			   "                  do sliding if possible?  (only if tychange; default=yes)\n"
			   "       -ran-dopreds-[yes|no]\n"
			   "                  try to improve predicate ranges?  (default=yes)\n"
                           "       -ran-skip-tgted-preds\n"
			   "                  don't improve targeted predicate ranges\n"
			   "       -ran-exactsizes[-no]\n"
			   "                  assume exact sizes of types?  (default=no)\n"
			   "      -ran-widen-[always|backedge]\n"
			   "                  widen always or only on backedge (default=backedge)\n"
			   "      -ran-no-narrow\n"
			   "      -ran-narrow[|-always|-backedge]\n"
			   "                  range analysis: do narrowing? (default=yes)\n"
			   "                  if so, always or only on backedge (default=always)\n"
			   "      -ran[-no]-filter-loop-backedge\n"
			   "                  compute backedge filter on loops? (default=yes)\n"
			   "      -ran[-no]-filter-call-backedge\n"
			   "                  compute backedge filter in callgraph? (default=no)\n"
			   "      -ran[-no]-malloc\n"
			   "                  handle malloc objects (default=yes)\n"
			   "      -ran[-no]-zero-special\n"
			   "                  treat zero specially (default=yes)\n"
			   "     -ran[-no]-collect-stats\n"
			   "                  collect finite/half-finite/known-pred stats (default=ptr)\n"
			   "     -ltsize      mbu: perform lower-than by size comparison (experimental)\n"
			   "     -callmeet    at non-empty callsites, always merge with pre-call facts\n"
			   "    [-no]-gref    compute GREF and filter at callsites (default=yes)\n"
			   "    [-no]-bblocks use basic blocks (default=off)\n"
			   "     -gmodref-skip-locals\n"
			   "     -gmodref-include-locals\n"
			   "                  GMOD/GREF should include/skip locals (default=skip)\n"
			   "     -wlmode[012] } worklist traversal order (default=leaf):\n"
			   "      -fifo       } 0: fifo\n"
			   "      -lifo       } 1: lifo\n"
			   "      -leaf       } 2: leaf-first in call-graph\n"
			   "     -wli-topo/-wli-depth\n"
			   "                  worklist initialize traversal order (default=topo)\n"
                           "     -exposed-unsafe\n"
                           "                  if p is exposed, *p is unsafe (default=no)\n"
			   "    [-no]-relevant-aos\n"
			   "                  filter relevant aos during dfa (default=false)\n"
			   "    [-no]-inst-fields\n"
			   "                  instantiate struct/union field AOs at decl (default=true)\n"
			   "\n"
			   "     -debug       dump full debug output, turns on -readable\n"
			   "     -readable    output readable names instead of pid/aid\n"
			   "     -summary     dump AO summary output\n"
			   "     -no-output   don't output to file\n"
			   "\n"
			   "    [-no]-fixnargs  Precompute max number of function args (default=yes)\n"
			   "    [-no]-cde-incl  Cycle detection/elimination: incl-to (default=yes)\n"
			   "    [-no]-cde-pt    Cycle detection/elimination: points-to (default=yes)\n"
			   "\n"
			   "    [-no]-recycle Recycle list nodes (default=yes)\n"
			   "\n"
			   "     -v[12345]    verbose execution mode (-v == -v2)\n"
			   "                  1: info on stage only\n"
			   "                  2: progress dots\n"
			   "                  3: full stats\n"
			   "                  4: higher diagnostic debug stuff\n"
			   "                  5: DFA full trace\n"
			, argv[0]);

  suco_llist<const char *> infiles;

  for(int i = 1; i < argc; ++i){
    if(argv[i][0] == '-'){
      if(!strcmp(argv[i],"-o")){
        if(i+1 < argc) outfile = argv[++i];
      } else if(!strcmp(argv[i],"-addr")){
        flag_do_addr_taken = true;
      } else if(!strcmp(argv[i],"-pta")){
        mode = tcm_pta;
      } else if(!strcmp(argv[i],"-callgraph")){
        mode = tcm_callgraph;
      } else if(!strcmp(argv[i],"-callgraph-dot")){
        mode = tcm_callgraph;
        callgraph_dot = true;
      } else if(!strncmp(argv[i],"-calltrace=", 11)){
        mode = tcm_callgraph;
        calltrace = &argv[i][11];
      } else if(!strcmp(argv[i],"-ptr")){
        mode = tcm_ptr;
        flag_readwrite = true;
      } else if(!strcmp(argv[i],"-ptrw")){
        mode = tcm_ptrw;
        flag_readwrite = false;
      } else if(!strcmp(argv[i],"-nt")){
        mode = tcm_full;
      } else if(!strcmp(argv[i],"-vuln")){
        flag_vuln = true;
      } else if(!strcmp(argv[i],"-no-vuln")){
        flag_vuln = false;
      } else if(!strcmp(argv[i],"-heap")){
        flag_heapstack = 1;
      } else if(!strcmp(argv[i],"-heapstack")){
        flag_heapstack = 2;
      } else if(!strcmp(argv[i],"-ptr-heap")){
        flag_heapstack = 1;
      } else if(!strcmp(argv[i],"-ptr-heapstack")){
        flag_heapstack = 2;
      } else if(!strcmp(argv[i],"-strlit-writable")){
        flag_strlit_readonly = false;
      } else if(!strcmp(argv[i],"-unsafe-derefs")){
        flag_all_deref_unsafe = true;
      } else if(!strcmp(argv[i],"-dfa")){
        flag_may_be_uninit = 1;
      } else if(!strcmp(argv[i],"-vtfix")){
        flag_vtfix = true;
      } else if(!strcmp(argv[i],"-no-vtfix")){
        flag_vtfix = false;
      } else if(!strcmp(argv[i],"-rda")){
        flag_reaching_defs = true;
      } else if(!strcmp(argv[i],"-red")){
        flag_redundant = true;
      } else if(!strcmp(argv[i],"-red-par")){
        flag_redundant = true;
        flag_red_paral2r = true;
      } else if(!strcmp(argv[i],"-red-no-par")){
        flag_redundant = true;
        flag_red_paral2r = false;
      } else if(!strcmp(argv[i],"-range")){
        flag_range = true;
      } else if(!strcmp(argv[i],"-ran-intra")){
        flag_range = true;
        flag_range_inter = false;
      } else if(!strcmp(argv[i],"-ran-inter")){
        flag_range = true;
        flag_range_inter = true;
      } else if(!strcmp(argv[i],"-ran-widen-always")){
        flag_range_widen_always = true;
      } else if(!strcmp(argv[i],"-ran-widen-backedge")){
        flag_range_widen_always = false;
      } else if(!strcmp(argv[i],"-ran-DO")){
        flag_range_must_have_ao = false;
        flag_range_ty_can_change = true;
        flag_range_do_sliding = true;
      } else if(!strcmp(argv[i],"-ran-LO")){
        flag_range_must_have_ao = true;
        flag_range_ty_can_change = false;
        flag_range_do_sliding = false;
      } else if(!strcmp(argv[i],"-ran-must-ao")){
        flag_range_must_have_ao = true;
      } else if(!strcmp(argv[i],"-ran-must-ao-no")){
        flag_range_must_have_ao = false;
      } else if(!strcmp(argv[i],"-ran-tychange-yes")){
        flag_range_ty_can_change = true;
      } else if(!strcmp(argv[i],"-ran-tychange-no")){
        flag_range_ty_can_change = false;
      } else if(!strcmp(argv[i],"-ran-sliding-yes")){
        flag_range_do_sliding = true;
      } else if(!strcmp(argv[i],"-ran-sliding-no")){
        flag_range_do_sliding = false;
      } else if(!strcmp(argv[i],"-ran-dopreds-yes")){
        flag_range_handle_preds = true;
      } else if(!strcmp(argv[i],"-ran-dopreds-no")){
        flag_range_handle_preds = false;
      } else if(!strcmp(argv[i],"-ran-skip-tgted-preds")){
        flag_range_skip_targeted_preds = true;
      } else if(!strcmp(argv[i],"-ran-exactsizes")){
        flag_range_exact_sizes = true;
      } else if(!strcmp(argv[i],"-ran-exactsizes-no")){
        flag_range_exact_sizes = false;
      } else if(!strcmp(argv[i],"-ran-narrow")){
        flag_range_do_narrow = true;
      } else if(!strcmp(argv[i],"-ran-no-narrow")){
        flag_range_do_narrow = false;
      } else if(!strcmp(argv[i],"-ran-narrow-always")){
        flag_range_do_narrow = true;
        flag_range_narrow_always = true;
      } else if(!strcmp(argv[i],"-ran-narrow-backedge")){
        flag_range_do_narrow = true;
        flag_range_narrow_always = false;
      } else if(!strcmp(argv[i], "-ran-malloc")){
        flag_range_handle_malloc = true;
      } else if(!strcmp(argv[i], "-ran-no-malloc")){
        flag_range_handle_malloc = false;
      } else if(!strcmp(argv[i], "-ran-zero-special")){
        flag_range_zero_special = true;
      } else if(!strcmp(argv[i], "-ran-no-zero-special")){
        flag_range_zero_special = false;
      } else if(!strcmp(argv[i], "-ran-collect-stats")){
        local_flag_range_collect_stats = 1;
      } else if(!strcmp(argv[i], "-ran-no-collect-stats")){
        local_flag_range_collect_stats = 0;
      } else if(!strcmp(argv[i], "-ran-filter-loop-backedge")){
        flag_range_filter_loop_backedge = true;
      } else if(!strcmp(argv[i], "-ran-no-filter-loop-backedge")){
        flag_range_filter_loop_backedge = false;
      } else if(!strcmp(argv[i], "-ran-filter-call-backedge")){
        flag_range_filter_call_backedge = true;
      } else if(!strcmp(argv[i], "-ran-no-filter-call-backedge")){
        flag_range_filter_call_backedge = false;
      } else if(!strcmp(argv[i],"-mbu-intra")){
        flag_mbu_inter = false;
        if(!flag_may_be_uninit) flag_may_be_uninit = 1;
      } else if(!strcmp(argv[i],"-mbu-intra-iref")){
        flag_compute_iref = true;
        flag_mbu_inter = false;
        if(!flag_may_be_uninit) flag_may_be_uninit = 1;
      } else if(!strcmp(argv[i],"-mbu-inter")){
        flag_mbu_inter = true;
        if(!flag_may_be_uninit) flag_may_be_uninit = 1;
      } else if(!strncmp(argv[i],"-mbu",4)){
        flag_may_be_uninit = atoi(argv[i]+4);
        if(flag_may_be_uninit < 1)
           flag_may_be_uninit = 1;
      } else if(!strcmp(argv[i],"-pmbu-sensitive")){
        flag_pmbu_sensitive = true;
        if(!flag_may_be_uninit) flag_may_be_uninit = 1;
      } else if(!strcmp(argv[i],"-pmbu-insensitive")){
        flag_pmbu_sensitive = false;
        if(!flag_may_be_uninit) flag_may_be_uninit = 1;
      } else if(!strncmp(argv[i],"-wlmode", 7)){
        flag_verbose = (isdigit(argv[i][7]))
			? (argv[i][7] - '0')
			: 0;
      } else if(!strcmp(argv[i],"-fifo")){
        flag_worklist_mode = WL_MODE_FIFO;
      } else if(!strcmp(argv[i],"-lifo")){
        flag_worklist_mode = WL_MODE_LIFO;
      } else if(!strcmp(argv[i],"-leaf")){
        flag_worklist_mode = WL_MODE_LEAF_FIRST;
      } else if(!strcmp(argv[i],"-wli-topo")){
        flag_worklist_init_depth_first = false;
      } else if(!strcmp(argv[i],"-wli-depth")){
        flag_worklist_init_depth_first = true;
      } else if(!strcmp(argv[i],"-exposed-unsafe")){
        flag_exposed_deref_is_unsafe = true;
      } else if(!strcmp(argv[i],"-relevant-aos")){
        flag_filter_relevant_aos = true;
      } else if(!strcmp(argv[i],"-no-relevant-aos")){
        flag_filter_relevant_aos = false;
      } else if(!strcmp(argv[i],"-inst-fields")){
        flag_instantiate_structunion = true;
      } else if(!strcmp(argv[i],"-no-inst-fields")){
        flag_instantiate_structunion = false;
      } else if(!strcmp(argv[i],"-ltsize")){
        flag_mbu_lowerthan_by_size = true;
      } else if(!strcmp(argv[i],"-callmeet")){
        flag_callsite_top = false;
      } else if(!strcmp(argv[i],"-bblocks")){
        flag_use_bblocks = true;
      } else if(!strcmp(argv[i],"-no-bblocks")){
        flag_use_bblocks = false;
      } else if(!strcmp(argv[i],"-fixnargs")){
        flag_use_maxnargs = true;
      } else if(!strcmp(argv[i],"-no-fixnargs")){
        flag_use_maxnargs = false;
      } else if(!strcmp(argv[i],"-cde-incl")){
        flag_collapse_inclto_cycle = true;
      } else if(!strcmp(argv[i],"-no-cde-incl")){
        flag_collapse_inclto_cycle = false;
      } else if(!strcmp(argv[i],"-cde-pt")){
        flag_collapse_ptsto_cycle = true;
      } else if(!strcmp(argv[i],"-no-cde-pt")){
        flag_collapse_ptsto_cycle = false;
      } else if(!strcmp(argv[i],"-recycle")){
        flag_recycle = true;
        suco_recycle = true;
      } else if(!strcmp(argv[i],"-no-recycle")){
        flag_recycle = false;
        suco_recycle = false;
      } else if(!strcmp(argv[i],"-debug")){
        debug_mode = true;
        flag_readable_output = true;
      } else if(!strcmp(argv[i],"-readable")){
        flag_readable_output = true;
      } else if(!strcmp(argv[i],"-gref")){
        flag_gref_filter = true;
      } else if(!strcmp(argv[i],"-no-gref")){
        flag_gref_filter = false;
      } else if(!strcmp(argv[i],"-gmodref-include-locals")){
        flag_gmodref_skip_locals = false;
      } else if(!strcmp(argv[i],"-gmodref-skip-locals")){
        flag_gmodref_skip_locals = true;
      } else if(!strcmp(argv[i],"-summary")){
        summary_mode = true;
      } else if(!strcmp(argv[i],"-no-output")){
        no_output = true;
      } else if(argv[i][0] == '-' && argv[i][1] == 'v'){
        flag_verbose = (isdigit(argv[i][2]))
			? (argv[i][2] - '0')
			: 2;
      } else {
        fprintf(stderr, "Unrecognized option [%s] ignored\n", argv[i]);
      }
    } else {
      infiles.Append(argv[i]); //-- collect input files
    }
  }

  if(mode == tcm_ptrw){
    flag_mark_strlit_invalid = flag_strlit_readonly;
  }
  if(local_flag_range_collect_stats == -1){
    if(mode == tcm_ptr) flag_range_collect_stats = true;
  } else {
    flag_range_collect_stats = local_flag_range_collect_stats;
  }
  TCAstate state;

  TCstats::timer("reading files");
  suco_iterator<const char *> ifi(infiles); //-- read in from input files
  while(ifi.Iterate())
    state.processFile((char *)ifi.Current());
  fprintf(stderr, "\n");
  TCstats::timer(0);

  if(!outfile)
    outfile = (debug_mode || summary_mode)?"a.debug_dump":"a.tc_tsls";

  fprintf(stderr, "Doing analysis\n");
  switch(mode){
    case tcm_ptr:
    case tcm_ptrw:
      if(flag_do_addr_taken){
        state.addrAssignAnalysis(true);
      } else {
        TCstats::timer("flow-insensitive invalid-ptr analysis");
        state.ptAnalysis();
        state.supplementalPTanalysis();
        if(flag_all_deref_unsafe) state.allDerefUnsafeAnalysis();
        else state.invalidPtrAnalysis();
        TCstats::timer(0);
        state.ptrFlowSensitiveAnalyses();
      }
      if(flag_vuln) state.computeVulnerable();
      break;

    case tcm_pta:
    case tcm_callgraph:
      state.ptAnalysis();
      state.supplementalPTanalysis();
      break;

    default:
    case tcm_full:
      if(flag_do_addr_taken){
        state.addrAssignAnalysis(false);
      } else {
        TCstats::timer("flow-insensitive TSC analysis");
        state.ptAnalysis();
        state.supplementalPTanalysis();
        state.possTypeAnalysis();
        state.tscAnalysis();
        TCstats::timer(0);
        state.tcFlowSensitiveAnalyses();
      }
      break;
  }
  fprintf(stderr, "Analysis Done\n");

  if(!no_output){
    FILE * outf = fopen(outfile, "w");
    if(!outf){
      fprintf(stderr, "Error opening output file %s\n", outfile);
    } else {
      const char * ata = (flag_do_addr_taken)?" - Address Taken Analysis":"";
      fprintf(stderr, "Writing output to file [%s]\n", outfile);
      switch(mode){
        case tcm_ptr:  fprintf(outf, "# Invalid Pointer Analysis%s\n", ata); break;
        case tcm_ptrw: fprintf(outf, "# Invalid Pointer (write-only) Analysis%s\n", ata); break;
        case tcm_pta:  fprintf(outf, "# Points-To Analysis\n"); break;
        case tcm_callgraph:  fprintf(outf, "# Callgraph\n"); break;
        default:
        case tcm_full: fprintf(outf, "# Full (nt) Type-Safty Analysis%s\n", ata); break;
      }

      fprintf(outf, "# Flow-sensitive Analyses:");
      if(flag_reaching_defs) fprintf(outf, " -rda");
      if(flag_may_be_uninit) fprintf(outf, " -mbu%d%s%s%s",
					flag_may_be_uninit,
					flag_mbu_inter?"":"-intra",
					flag_pmbu_sensitive?"-sensitive":"",
					flag_compute_iref?"-iref":"-gref");
      if(flag_heapstack == 1) fprintf(outf, " -heap");
      if(flag_heapstack == 2) fprintf(outf, " -heapstack");

      if(flag_redundant){
        fprintf(outf, " -red");
        if(flag_red_paral2r) fprintf(outf, "(par)");
      }
      if(flag_vtfix) fprintf(outf, " -vtfix");
      if(flag_range){
        fprintf(outf, " -range-%s (filter-backedge:loop=%s,callgraph=%s)(widen %s)",
			(flag_range_inter)?"inter":"intra",
			(flag_range_filter_loop_backedge)?"yes":"no",
			(flag_range_filter_call_backedge)?"yes":"no",
			(flag_range_widen_always)?"always":"on backedge");
        if(flag_range_do_narrow) fprintf(outf, "(narrow %s)", (flag_range_narrow_always)?"always":"on backedge");
      }
      fprintf(outf, "\n");

      if(debug_mode){
        state.debug_dump(outf);
      } else if(summary_mode){
        state.summary_dump(outf);
        fprintf(outf, "--Dereference-AO Aliases--\n");
	state.writeAliases(outf);
      } else {
        switch(mode){
	  case tcm_pta:
	    state.writeAliases(outf);
	    break;
	  case tcm_callgraph:
	    state.writeCallgraph(outf, callgraph_dot, calltrace);
	    break;
	  default:
	    state.writeTSoutput(outf, (mode == tcm_full) && flag_do_addr_taken);
	    break;
        }
      }
      if(flag_vuln && (mode == tcm_ptr || mode == tcm_ptrw)){
        state.writeVulnerable(outf);
      }
      fclose(outf);
    }
  }

  //- verbose: output runtime flags for verification
  if(flag_verbose){
#define WRITE_FLAG_VALUE(flag) fprintf(stderr, #flag " = %d\n", flag)
    WRITE_FLAG_VALUE(flag_do_addr_taken);
    WRITE_FLAG_VALUE(flag_use_maxnargs);
    WRITE_FLAG_VALUE(flag_collapse_inclto_cycle);
    WRITE_FLAG_VALUE(flag_collapse_ptsto_cycle);
    WRITE_FLAG_VALUE(flag_verbose);
    WRITE_FLAG_VALUE(flag_recycle);
    WRITE_FLAG_VALUE(flag_readable_output);
    WRITE_FLAG_VALUE(flag_gref_filter);
    WRITE_FLAG_VALUE(flag_gmodref_skip_locals);
    WRITE_FLAG_VALUE(flag_heapstack);
    WRITE_FLAG_VALUE(flag_may_be_uninit);
    WRITE_FLAG_VALUE(flag_mbu_inter);
    WRITE_FLAG_VALUE(flag_compute_iref);
    WRITE_FLAG_VALUE(flag_pmbu_sensitive);
    WRITE_FLAG_VALUE(flag_reaching_defs);
    WRITE_FLAG_VALUE(flag_redundant);
    WRITE_FLAG_VALUE(flag_red_paral2r);
    WRITE_FLAG_VALUE(flag_instantiate_structunion);
    WRITE_FLAG_VALUE(flag_worklist_mode);
    WRITE_FLAG_VALUE(flag_exposed_deref_is_unsafe);
    WRITE_FLAG_VALUE(flag_worklist_init_depth_first);
    WRITE_FLAG_VALUE(flag_filter_relevant_aos);
    WRITE_FLAG_VALUE(flag_mbu_lowerthan_by_size);
    WRITE_FLAG_VALUE(flag_callsite_top);
    WRITE_FLAG_VALUE(flag_use_bblocks);
    WRITE_FLAG_VALUE(flag_range);
    WRITE_FLAG_VALUE(flag_range_inter);
    WRITE_FLAG_VALUE(flag_range_must_have_ao);
    WRITE_FLAG_VALUE(flag_range_ty_can_change);
    WRITE_FLAG_VALUE(flag_range_do_sliding);
    WRITE_FLAG_VALUE(flag_range_handle_preds);
    WRITE_FLAG_VALUE(flag_range_skip_targeted_preds);
    WRITE_FLAG_VALUE(flag_range_exact_sizes);
    WRITE_FLAG_VALUE(flag_range_do_narrow);
    WRITE_FLAG_VALUE(flag_range_widen_always);
    WRITE_FLAG_VALUE(flag_range_narrow_always);
    WRITE_FLAG_VALUE(flag_range_filter_loop_backedge);
    WRITE_FLAG_VALUE(flag_range_filter_call_backedge);
    WRITE_FLAG_VALUE(flag_range_zero_special);
    WRITE_FLAG_VALUE(flag_range_handle_malloc);
    WRITE_FLAG_VALUE(flag_strlit_readonly);
    WRITE_FLAG_VALUE(flag_vuln);
#undef WRITE_FLAG_VALUE
  }

  return 0;
}

//------------------------------------------------------
