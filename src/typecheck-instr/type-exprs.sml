structure TypeExprs : (*TYPE_EXPRS*) INSTR_AST = struct 

(* Top level function. *)
fun instrAst (bundle as {ast as edecls,
			 tidtab,
			 errorCount,
			 warningCount,
			 auxiliaryInfo as {aidtab, implicits, env as symtab}},
	      filestem, errStrm) =
let
  (* If BuildAst.makeAst starts returning errorState, we may want to reuse it. *)
  val errorState = Error.mkErrState errStrm
  val bug = Error.bug errorState

  (* Reconstruct the state out of the AST bundle. *)
  val stateInfo = ParseToAst.progToState bundle

  val globalState as {uidTables={ttab,atab,implicits},...} =
      State.initGlobal(stateInfo, errorState)

  val localState = State.initLocal ()

  val stateFuns = State.stateFuns(globalState, localState)

  val {locFuns =
       {pushLoc, popLoc, getLoc, error, warn},
       tidsFuns =
       {pushTids, resetTids},
       tmpVarsFuns =
       {pushTmpVars, resetTmpVars},
       envFuns =
       {topLevel, pushLocalEnv, popLocalEnv, lookSym, bindSym,
	lookSymGlobal, bindSymGlobal, lookLocalScope, getGlobalEnv},
       uidTabFuns =
       {bindAid, lookAid, bindTid, lookTid},
       funFuns =
       {newFunction, getReturnTy, checkLabels, addLabel, addGoto},
       switchFuns =
       {pushSwitchLabels, popSwitchLabels, addSwitchLabel, addDefaultLabel},
       ...}
      = stateFuns

  (* lookup type of expr in aidtab *)
  fun lookupExprType(Ast.EXPR(_, adorn, _)) =
      case lookAid adorn
        of SOME ct => ct
	 | NONE => (bug ("lookupExprType: no type for expression; aid = "
			 ^ Int.toString adorn);
		    Ast.Void)

(*
  fun lookupExprImplicitType(Ast.EXPR(_,adorn,_)) = Aidtab.find (implicits,adorn)
*)
  (**************************  END OF HELPER FUNCTIONS *************************)
  (************************** START OF INSTR FUNCTIONS *************************)

  fun instrExprs nil = nil
    | instrExprs (head::tail) = (instrExpr head)::(instrExprs tail)

  and instrExpr (outer_exp as (Ast.EXPR (coreExpr,exp_aid,exp_loc))) =
      let val exp_type = lookupExprType outer_exp
      in  case coreExpr
	    of Ast.IntConst li => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
	     | Ast.RealConst r => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
	     | Ast.StringConst s => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
	     | Ast.Call (fexp,exps) =>
		let val coreExpr' = Ast.Call (instrExpr fexp, instrExprs exps)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.QuestionColon (e1,e2,e3) =>
		let val coreExpr' = Ast.QuestionColon (instrExpr e1, instrExpr e2, instrExpr e3)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Assign (e1,e2) =>
		let val coreExpr' = Ast.Assign (instrExpr e1, instrExpr e2)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Comma (e1,e2) =>
		let val coreExpr' = Ast.Comma (instrExpr e1, instrExpr e2)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Sub (e1,e2) =>
		let val coreExpr' = Ast.Sub (instrExpr e1, instrExpr e2)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Member (exp,mem) =>
		let val coreExpr' = Ast.Member (instrExpr exp, mem)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Arrow (exp,mem) =>
		let val coreExpr' = Ast.Arrow (instrExpr exp, mem)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Deref exp =>
		let val coreExpr' = Ast.Deref (instrExpr exp)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.AddrOf exp =>
		let val coreExpr' = Ast.AddrOf (instrExpr exp)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Binop (binop,e1,e2) =>
		let val coreExpr' = Ast.Binop (binop, instrExpr e1, instrExpr e2)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Unop (unop,exp) =>
		let val coreExpr' = Ast.Unop (unop, instrExpr exp)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.SizeOf ty => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
	     | Ast.Cast (ctype,exp) =>
		let val coreExpr' = Ast.Cast (ctype, instrExpr exp)
		    val outer_exp' = Ast.EXPR (coreExpr',exp_aid, exp_loc)
		in
		    Ast.EXPR (Ast.Cast(exp_type,outer_exp'),exp_aid,exp_loc)
		end
	     | Ast.Id pid => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
	     | Ast.EnumId (pid,li) => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
	     | Ast.ExprExt ee => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
	     | Ast.ErrorExpr => Ast.EXPR (Ast.Cast(exp_type,outer_exp),exp_aid,exp_loc)
      end

  fun instrInitExprs nil = nil
    | instrInitExprs (head::tail) = (instrInitExpr head)::(instrInitExprs tail)

  and instrInitExpr iexpr =
      (case iexpr of Ast.Simple exp => Ast.Simple (instrExpr exp)
		   | Ast.Aggregate iexps => Ast.Aggregate (instrInitExprs iexps)
      )

  fun instrDecls nil = nil
    | instrDecls (head::tail) = (instrDecl head)::(instrDecls tail)

  and instrDecl decl =
      ( case decl
	  of Ast.TypeDecl _ => decl
	   | Ast.VarDecl (id, initExprOp) =>
	     ( case initExprOp
		 of SOME iexp => Ast.VarDecl (id, SOME (instrInitExpr iexp))
		  | NONE => decl
	     )
      )

  fun instrStmts nil = nil
    | instrStmts (head::tail) = (instrStmt head)::(instrStmts tail)

  and instrStmt (outer_stmt as (Ast.STMT(coreStmt,stmt_aid,stmt_loc))) =
     (case coreStmt
	of Ast.Expr expOp =>
	    ( case expOp
		of SOME exp => Ast.STMT (Ast.Expr (SOME (instrExpr exp)),stmt_aid,stmt_loc)
		 | NONE => outer_stmt
	    )

	 | Ast.Compound (decls, stmts) =>
		Ast.STMT(Ast.Compound (instrDecls decls, instrStmts stmts),stmt_aid,stmt_loc)

	 | Ast.While (exp,stmt) =>
		Ast.STMT(Ast.While ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)

	 | Ast.Do (exp,stmt) => 
		Ast.STMT(Ast.Do ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)

	 | Ast.For (e1op,e2op,e3op,stmt) =>
		let val e1op' = case e1op of SOME exp1 => SOME (instrExpr exp1)
					   | NONE => NONE
		    val e2op' = case e2op of SOME exp2 => SOME (instrExpr exp2)
					   | NONE => NONE
		    val e3op' = case e3op of SOME exp3 => SOME (instrExpr exp3)
					   | NONE => NONE
		    val stmt' = instrStmt stmt
		in
		    Ast.STMT(Ast.For (e1op',e2op',e3op',stmt'),stmt_aid,stmt_loc)
		end

	 | Ast.Labeled (label,stmt) =>
		Ast.STMT(Ast.Labeled (label,(instrStmt stmt)),stmt_aid,stmt_loc)
	 | Ast.CaseLabel (li,exp,stmt) => 
		Ast.STMT(Ast.CaseLabel (li,exp,(instrStmt stmt)),stmt_aid,stmt_loc)
	 | Ast.DefaultLabel stmt =>
		Ast.STMT(Ast.DefaultLabel (instrStmt stmt),stmt_aid,stmt_loc)
	 | Ast.Goto label => outer_stmt
	 | Ast.Break => outer_stmt
	 | Ast.Continue => outer_stmt
	 | Ast.Return expOp => 
		(case expOp
		   of NONE => outer_stmt
		    | SOME exp => Ast.STMT(Ast.Return (SOME (instrExpr exp)),stmt_aid,stmt_loc)
		)
	 | Ast.IfThen (exp,stmt) => 
		Ast.STMT(Ast.IfThen ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)
	 | Ast.IfThenElse (exp,stmt1,stmt2) => 
		Ast.STMT(Ast.IfThenElse ((instrExpr exp),(instrStmt stmt1),(instrStmt stmt2)),
			 stmt_aid,stmt_loc)
	 | Ast.Switch (exp,stmt) =>
		Ast.STMT(Ast.Switch ((instrExpr exp),(instrStmt stmt)),stmt_aid,stmt_loc)
	 | Ast.StatExt se => outer_stmt
	 | Ast.ErrorStmt => outer_stmt
     )

  fun instrExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
      (case coreEdecl
	 of Ast.ExternalDecl (Ast.TypeDecl _) => edecl
	  | Ast.ExternalDecl (Ast.VarDecl (id,initExprOp)) => edecl
	  | Ast.FunctionDef (id,ids,stmt) =>
		Ast.DECL (Ast.FunctionDef (id,ids,instrStmt stmt),edecl_aid,edecl_loc)
	  | Ast.ExternalDeclExt _ => edecl
      )

  fun instrExternalDecls nil = nil
  |   instrExternalDecls (ed::edecls) =
		(instrExternalDecl ed)::(instrExternalDecls edecls)

  fun instrAst' edecls =
      let val new_edecls = instrExternalDecls (edecls)
	  val ret_bundle =
		{ ast=new_edecls,
		  tidtab=ttab,
		  errorCount=errorCount,
		  warningCount=warningCount,
		  auxiliaryInfo = { aidtab=atab,
				    implicits=implicits,
				    env=getGlobalEnv()}}
      in
	  (* Return AstBundle *)
          ret_bundle
      end

in
    instrAst' edecls
end (* fun instrAst *)


end (* Structure TypeExprs *)
