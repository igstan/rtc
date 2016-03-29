#include <stdio.h>
#include "suco.h"
#include "suco.cpp"

bool suco_recycle = true;

class AO;
class AOId;
class CFGbblock;
class CFGfunction;
class CFGnode;
class Dependency;
class ECR;
class ECRargRet;
class ExpDescr;
class PExprArg;
class PExprAssign;
class PExprCall;
class PExprVerify;
class PExprPredicate;
class PgmExpr;
class PgmStmt;
class TCassignEdge;
class TClatType;
class TCtype;

struct int_triple;

void suco_diag_wrapper::suco_write_recycle_stats(FILE * outf)
{
  if(suco_recycle){

#define write_recy_stats(ty) \
    fprintf(outf, "# \t%8d\t%8d\t%8d\t%s\n", \
		suco_llist<ty>::_suco_node::num_new_nodes, \
		suco_llist<ty>::_suco_node::num_recy_nodes, \
		suco_llist<ty>::_suco_node::freelistLength(), \
		#ty \
		);
    fprintf(outf, "# Recycled Suco Nodes (new/recycled/freelist/type):\n");

    write_recy_stats(AO *)
    write_recy_stats(AOId *)
    write_recy_stats(CFGbblock *)
    write_recy_stats(CFGfunction *)
    write_recy_stats(CFGnode *)
    write_recy_stats(Dependency *)
    write_recy_stats(ECR *)
    write_recy_stats(ECRargRet *)
    write_recy_stats(ExpDescr *)
    write_recy_stats(PExprArg *)
    write_recy_stats(PExprAssign *)
    write_recy_stats(PExprCall *)
    write_recy_stats(PExprVerify *)
    write_recy_stats(PExprPredicate *)
    write_recy_stats(PgmExpr *)
    write_recy_stats(PgmStmt *)
    write_recy_stats(TCassignEdge *)
    write_recy_stats(TClatType *)
    write_recy_stats(TCtype *)
    write_recy_stats(const char *)
    write_recy_stats(int)
    write_recy_stats(suco_llist<PgmExpr *> *)
    write_recy_stats(struct int_triple *)

#undef write_recy_stats
  }
}

void suco_usage_dummy_function()
{
#define instantiate_llist(list) do{\
		list.Contains(0); \
		list.Insert(0); \
		list.Append(0); \
		list.Attach(list); \
		list.Copy(list); \
		list.Head(); \
		list.Last(); \
		list.ElementAt(0); \
		list.Remove(0); \
		list.Clear(); \
		list.RemoveHead(); \
		list.Length(); \
		list.IsEmpty(); \
	} while(0)

  suco_llist<CFGbblock *> lcb;
  suco_llist<CFGfunction *> lcf;
  suco_llist<CFGnode *> lcn;
  suco_llist<PExprCall *> lpc;
  suco_llist<TCassignEdge *> ltae;
  suco_llist<const char *> lstr;
  suco_llist<suco_llist<PgmExpr *> *> lfnl;

  suco_llist<AO *> lao;
  suco_llist<suco_set<AO *> *> laos;
  suco_llist<AOId *> lid;
  suco_llist<Dependency *> ldep;
  suco_llist<ECR *> lecr;
  suco_llist<ECRargRet *> lear;
  suco_llist<ExpDescr *> led;
  suco_llist<PExprArg *> lpa;
  suco_llist<PExprAssign *> lpg;
  suco_llist<PExprVerify *> lpv;
  suco_llist<PExprPredicate *> lpp;
  suco_llist<PgmExpr *> lpe;
  suco_llist<PgmStmt *> lps;
  suco_llist<TCassignEdge *> lae;
  suco_llist<TClatType *> ltlt;
  suco_llist<TCtype *> ltt;
  suco_llist<int> lint;
  suco_llist<struct int_triple *> lit;

  instantiate_llist(lcb);
  instantiate_llist(lcf);
  instantiate_llist(lcn);
  instantiate_llist(lpc);
  instantiate_llist(ltae);
  instantiate_llist(lstr);
  instantiate_llist(lfnl);

  instantiate_llist(lao);
  instantiate_llist(laos);
  instantiate_llist(lid);
  instantiate_llist(ldep);
  instantiate_llist(lecr);
  instantiate_llist(lear);
  instantiate_llist(led);
  instantiate_llist(lpa);
  instantiate_llist(lpg);
  instantiate_llist(lpv);
  instantiate_llist(lpp);
  instantiate_llist(lpe);
  instantiate_llist(lps);
  instantiate_llist(lae);
  instantiate_llist(ltlt);
  instantiate_llist(ltt);
  instantiate_llist(lint);
  instantiate_llist(lit);

#undef instantiate_llist

#define instantiate_queue(queue) do{\
		queue.Enqueue(0); \
		queue.Dequeue(); \
	} while(0)

#define instantiate_stack(stack) do{\
		stack.Push(0); \
		stack.Pop(); \
	} while(0)

#undef instantiate_queue
#undef instantiate_stack

#define instantiate_set(set) do{\
		set.Insert(0); \
		set.GetSingleton(); \
		set.InsertList(set); \
		set.Union(set); \
		set.UnionConsume(set); \
		set.Superset(set); \
		set.Subset(set); \
		set.Intersect(set); \
		set.Subtract(set); \
		set.Intersects(set); \
	} while(0)

  suco_set<AO *> sao;
  suco_set<suco_set<AO *> *> saos;
  suco_set<AOId *> sid;
  suco_set<CFGfunction *> scf;
  suco_set<CFGnode *> scn;
  suco_set<Dependency *> sdep;
  suco_set<ECR *> secr;
  suco_set<ECRargRet *> sear;
  suco_set<ExpDescr *> sed;
  suco_set<PExprArg *> spa;
  suco_set<PExprAssign *> spg;
  suco_set<PExprVerify *> spv;
  suco_set<PExprPredicate *> spp;
  suco_set<PgmExpr *> spe;
  suco_set<PgmStmt *> sps;
  suco_set<TCassignEdge *> sae;
  suco_set<TClatType *> stlt;
  suco_set<int> sint;
  suco_set<struct int_triple *> sit;

  instantiate_set(sao);
  instantiate_set(saos);
  instantiate_set(sid);
  instantiate_set(scf);
  instantiate_set(scn);
  instantiate_set(sdep);
  instantiate_set(secr);
  instantiate_set(sear);
  instantiate_set(sed);
  instantiate_set(spa);
  instantiate_set(spg);
  instantiate_set(spv);
  instantiate_set(spp);
  instantiate_set(spe);
  instantiate_set(sps);
  instantiate_set(sae);
  instantiate_set(stlt);
  instantiate_set(sint);
  instantiate_set(sit);

#undef instantiate_set
}
