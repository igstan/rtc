(* Copyright (c) 1998 by Lucent Technologies 
 * pretty-printer which outputs each expression's {aid} before the expression. -SY:10jan2003
 *)

local 
  structure PPAstAdornment : PPASTADORNMENT =
  struct
    type aidinfo = int (* offset that'll be subtracted from aid *)

    fun ppExpressionAdornment ppCoreExpr aidinfo tidtab pps (Ast.EXPR (coreExpr,aid,_)) = 
	( PPLib.addStr pps ("{" ^ (Int.toString (aid - aidinfo)) ^ "}")
	; ppCoreExpr aidinfo tidtab pps coreExpr
	)

    fun ppStatementAdornment ppCoreStmt aidinfo tidtab pps  (Ast.STMT (coreStmt,aid,_)) = 
	((case coreStmt
	   of Ast.Expr _ => ()
	    | _ => ( PPLib.addStr pps ("{" ^ (Int.toString (aid - aidinfo)) ^ "}") )
	  )
	; ppCoreStmt aidinfo tidtab pps coreStmt
	)

    fun ppExternalDeclAdornment ppCoreExternalDecl aidinfo tidtab pps
	  (Ast.DECL (coreExtDecl,_,_)) = 
	ppCoreExternalDecl aidinfo tidtab pps coreExtDecl
  end

in
  structure PPAstDebugAid = PPAstFn(structure PPAstAdornment=PPAstAdornment)
end
