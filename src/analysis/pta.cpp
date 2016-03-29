#include "flags.h"
#include "diag.h" // for timer
#include "ao.h"
#include "ecr.h"
#include "pta.h"

void PTanalysis::initializeECRbasic(AO& ao)
{
  switch(ao.getKind()){
    case AO::aoReturn: {
	  ECR& parecr  = ECR::getECR(((AOReturn&) ao).getParent());
	  parecr.getArgRet().addRet(ECR::getECR(ao));
	} break;
    case AO::aoArg: {
	  ECR& parecr  = ECR::getECR(((AOArg&) ao).getParent());
	  parecr.getArgRet().addArg(((AOArg&) ao).argNo(), ECR::getECR(ao));
	} break;

    case AO::aoSDot:  //- do nothing
    case AO::aoUDot:
	break;

    case AO::aoFunction: {
	  ECR& e = ECR::getECR(ao); //- allocates new ECR
	  if(((AOFunction &)ao).isVarg()){ //- is var-arg: merge argret slots starting from vargno
	    ECRargRet& ear = e.getArgRet();	// TODO1: merge for all functions, as ellipsis may not be explicit?
	    ECR& ve = ECR::new_ECR();		// TODO2: user could walk va_list from earlier than next-to-last formal!?
	    for(int i = ((AOFunction &)ao).getLargNo(); i <= ECRargRet::getMaxNargs(); ++i)
	      ear.addArg(i, ve);
	  }
	} break;

    case AO::aoStar:
    case AO::aoAddrOf:
    case AO::aoOp:
    case AO::aoExt:
    case AO::aoId:
    case AO::aoMalloc:
    case AO::aoValue:
    case AO::aoStringLit:
    default: {
	  ECR::getECR(ao); //- allocates new ECR
	} break;
  }
}

void PTanalysis::initializeECRaggregate_collapseAlways(AO& ao)
{
  switch(ao.getKind()){
    case AO::aoSDot: {
	  ECR::unifyECRs(ao, ((AOSDot&) ao).getParent());
	} break;
    case AO::aoUDot: {
	  ECR::unifyECRs(ao, ((AOUDot&) ao).getParent());
	} break;

    case AO::aoStar:
    case AO::aoAddrOf:
    case AO::aoOp:
    case AO::aoExt:
    case AO::aoReturn:
    case AO::aoArg:
    case AO::aoId:
    case AO::aoMalloc:
    case AO::aoValue:
    case AO::aoStringLit:
    default:
	break;
  }
}

void PTanalysis::initializeECRaggregate_collapseOnCast(AO& ao)
{
  initializeECRaggregate_collapseAlways(ao);
}

void PTanalysis::initializeECRpointers(AO& ao)
{
  switch(ao.getKind()){
    case AO::aoStar: {
// (old approach: p --pt--> *p)
//        ECR::getECR(((AOStar&) ao).getTarget()).pointsTo(ECR::getECR(ao));
// (new appraoch: *p --incl--> a <--pt-- p )
          ECR& a = ECR::getECR(((AOStar&) ao).getTarget()).getPointsTo();
          ECR::getECR(ao).includesTo(a);
	} break;

    case AO::aoAddrOf: {
	  ECR::getECR(ao).pointsTo(ECR::getECR(((AOAddrOf&) ao).getTarget()));
	} break;

    case AO::aoOp: {
// (old approach: +p --pt--> a <--pt--p
//	  ECR::getECR(ao).pointsTo(ECR::getECR(((AOOp&) ao).getTarget()).getPointsTo());
// (new approach: +p --pt--> a --incl--> b <--pt-- p
          ECR& b = ECR::getECR(((AOOp&) ao).getTarget()).getPointsTo();
          ECR::getECR(ao).getPointsTo().includesTo(b);
	} break;
    case AO::aoExt: {
// (old approach: ext p --pt--> a <--pt--p
//	  ECR::getECR(ao).pointsTo(ECR::getECR(((AOExt&) ao).getTarget()).getPointsTo());
// (new approach: ext p --pt--> a --incl--> b <--pt-- p
          ECR& b = ECR::getECR(((AOExt&) ao).getTarget()).getPointsTo();
          ECR::getECR(ao).getPointsTo().includesTo(b);
	} break;

    case AO::aoSDot:
    case AO::aoUDot:
    case AO::aoReturn:
    case AO::aoArg:
    case AO::aoId:
    case AO::aoMalloc:
    case AO::aoValue:
    case AO::aoStringLit:
    default:
	break;
  }
}

void PTanalysis::analyzeAssigns_collapseAlways(suco_llist<TCassignEdge *>& assigns)
{
  // - process assignment edges
  if(flag_verbose){
    TCstats::timer("processing assignment edges");
    if(flag_verbose > 1)
      fprintf(stderr, "(each dot is 100 assignments, 50 dots per row)\n");
  }

  int counter = 0;
  suco_iterator<TCassignEdge *> ei(assigns);
  while(ei.Iterate()){
    TCassignEdge& e = *ei.Current();
    if(!e.getTo().isVal()){ //- don't process assignments into Value nodes
      ECR& rhs = ECR::getECR(e.getFrom());
      ECR& lhs = ECR::getECR(e.getTo());
      lhs.getPointsTo().includesTo(rhs.getPointsTo());
      if(flag_verbose > 1){
        if(++counter % 100 == 0)
          fprintf(stderr, ".");
        if(counter % 5000 == 0)
          fprintf(stderr, "\n");
      }
    }
  }
  if(flag_verbose) TCstats::timer(0);
}

void PTanalysis::analyzeAssigns_collapseOnCast(suco_llist<TCassignEdge *>& assigns)
{
  analyzeAssigns_collapseAlways(assigns);
}

