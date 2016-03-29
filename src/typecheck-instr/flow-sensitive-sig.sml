signature FLOW_SENSITIVE =
sig
  structure pidset : ORD_SET where type Key.ord_key = Pid.uid
(*
  structure pidaidmap : ORD_MAP where type Key.ord_key = Pid.uid
*)
  structure aidset : ORD_SET where type Key.ord_key = Aid.uid

  val flowSensitiveAnalyses : ParseToAst.astBundle
				* (Ast.expression -> Rtc.absObject list)
				* (Rtc.absObject list -> Rtc.absObject list)
					-> (((Pid.uid -> bool) * (Aid.uid -> bool))
					    * (Aid.uid -> bool)
					    * (Pid.uid -> bool))

  val outputDataflowOperators : ParseToAst.astBundle
				* Aid.uid
				* (Ast.expression -> Ast.ctype)
				* TextIO.outstream -> unit

end
