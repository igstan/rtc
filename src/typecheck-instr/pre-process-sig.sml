signature PRE_PROCESS =
sig
  val mapExprs : (Ast.expression -> Ast.expression) -> ParseToAst.astBundle -> ParseToAst.astBundle
  val handlePrintfPctN : ParseToAst.astBundle -> ParseToAst.astBundle
  val fixLibFnRetTys : ParseToAst.astBundle -> ParseToAst.astBundle
end
