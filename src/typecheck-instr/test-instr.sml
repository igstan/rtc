structure TestInstr : INSTR_AST =
struct 

  (************************** TEST FUNCTION *************************)
  fun instrAst (bundle as {ast as edecls,
			 tidtab,
			 errorCount,
			 warningCount,
			 auxiliaryInfo as {aidtab, implicits, env as symtab}}, _, _) =
  let

    val (lookup_fn, lookup_children_fn) = OptInterface.addrTakenAnalysis bundle

    fun processExprs (nil) = nil
      | processExprs ((head::tail)) = (processExpr (head)) @ (processExprs (tail))

    and processExpr ((outer_exp as Ast.EXPR (coreExpr,_,_))) =
	let val _ = case coreExpr
	    of Ast.IntConst li => nil
	     | Ast.RealConst r => nil
	     | Ast.StringConst s => nil
	     | Ast.Call (fexp,exps) => (processExpr (fexp)) @ (processExprs (exps))
	     | Ast.QuestionColon (e1,e2,e3) => (processExpr (e1)) @ (processExpr (e2)) @ (processExpr (e3))
	     | Ast.Assign (e1,e2) => (processExpr (e1)) @ (processExpr (e2))
	     | Ast.Comma (e1,e2) => (processExpr (e1)) @ (processExpr (e2))
	     | Ast.Sub (e1,e2) => (processExpr (e1)) @ (processExpr (e2))
	     | Ast.Member (exp,mem) => processExpr (exp)
	     | Ast.Arrow (exp,mem) => processExpr (exp)
	     | Ast.Deref exp => (processExpr (exp))
	     | Ast.AddrOf exp => (processExpr (exp))
	     | Ast.Binop (binop,e1,e2) => (processExpr (e1)) @ (processExpr (e2))
	     | Ast.Unop (unop,exp) => processExpr (exp)
	     | Ast.SizeOf ty => nil
	     | Ast.Cast (ctype,exp) => processExpr (exp)
	     | Ast.Id (id as {uid=pid,ctype=cty,...}) => nil
	     | Ast.EnumId (pid,li) => nil
	     | Ast.ExprExt ee => nil
	     | Ast.ErrorExpr => nil
	    fun getTSLstring aol = Rtc.tsToDescr (lookup_fn aol)
	in
	    (print ("EXPR: " ^ (PPLib.ppToString (PPAst.ppExpression () tidtab) outer_exp)
			^ " -> " ^ (getTSLstring
(*NOTE:This part untested; wrote to allow compilation*)
					  (  (	OptInterface.expToAbsObjects
						(( fn (Ast.EXPR (_,aid,_)) =>
						  Option.getOpt(Aidtab.find (aidtab,aid), Ast.Void)
						 ),tidtab,{deref=true})
					     ) outer_exp
					  )
				   )
			^ "\n");
	    nil)
	end

    fun processDecls nil = nil
      | processDecls (head::tail) = (processDecl head) @ (processDecls tail)

    and processDecl decl =
	( case decl
	  of Ast.TypeDecl _ => nil
	   | Ast.VarDecl (id as {ctype=ty,...}, initExprOp) =>
	     (print ("DECL: " ^ (PPLib.ppToString PPAst.ppId id)
			^ " -T-> " ^ (Rtc.tcTypeToDescr (OptInterface.ctypeToTcType tidtab ty))
			^ "\n");
	      nil)
	)


    fun processStmts nil = nil
      | processStmts (head::tail) = (processStmt head) @ (processStmts tail)

    and processStmt ((Ast.STMT(coreStmt,_,_))) =
     (case coreStmt
	of Ast.Expr expOp =>
	    ( case expOp
		of SOME exp => processExpr (exp)
		 | NONE => nil
	    )

	 | Ast.Compound (decls, stmts) => (processDecls decls) @ (processStmts stmts)

	 | Ast.While (exp,stmt) => (processExpr (exp)) @ (processStmt stmt)

	 | Ast.Do (exp,stmt) => (processExpr (exp)) @ (processStmt stmt)

	 | Ast.For (e1op,e2op,e3op,stmt) =>
		let val e1pids = case e1op of SOME exp1 => processExpr (exp1)
					   | NONE => nil
		    val e2pids = case e2op of SOME exp2 => processExpr (exp2)
					   | NONE => nil
		    val e3pids = case e3op of SOME exp3 => processExpr (exp3)
					   | NONE => nil
		in
		    e1pids @ e2pids @ e3pids @ (processStmt stmt)
		end

	 | Ast.Labeled (label,stmt) => processStmt stmt
	 | Ast.CaseLabel (li,exp,stmt) => processStmt stmt
	 | Ast.DefaultLabel stmt => processStmt stmt
	 | Ast.Goto label => nil
	 | Ast.Break => nil
	 | Ast.Continue => nil
	 | Ast.Return expOp => (case expOp
				  of SOME exp => processExpr (exp)
				   | NONE => nil
				)
	 | Ast.IfThen (exp,stmt) => (processExpr (exp)) @ (processStmt stmt)
	 | Ast.IfThenElse (exp,stmt1,stmt2) =>
			(processExpr (exp)) @ (processStmt stmt1) @ (processStmt stmt2)
	 | Ast.Switch (exp,stmt) => (processExpr (exp)) @ (processStmt stmt)
	 | Ast.StatExt se => nil
	 | Ast.ErrorStmt => nil
     )

    fun processExternalDecl (edecl as Ast.DECL(coreEdecl,edecl_aid,edecl_loc)) =
	(case coreEdecl
	   of Ast.ExternalDecl (Ast.TypeDecl _) => nil
	    | Ast.ExternalDecl (Ast.VarDecl (id as {ctype=ty,...},initExprOp)) =>
	     (print ("DECL: " ^ (PPLib.ppToString PPAst.ppId id)
			^ " -T-> " ^ (Rtc.tcTypeToDescr (OptInterface.ctypeToTcType tidtab ty))
			^ "\n");
	      nil)
	    | Ast.FunctionDef (id,ids,stmt) => processStmt stmt
	    | Ast.ExternalDeclExt _ => nil
	)

    fun processExternalDecls nil = nil
      | processExternalDecls (ed::edecls) =
		(processExternalDecl ed) @ (processExternalDecls edecls)

    val _ = processExternalDecls (edecls)

  in bundle end (* fun instrAst *)


end (* Structure TypeExprs *)
