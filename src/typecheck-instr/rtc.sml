structure Rtc : RTC =
struct 

  datatype ts_categ = TSC_POSS_INVALID | TSC_BADLY_TYPED | TSC_INFLUENTIAL | TSC_EXPOSED | TSC_SAFE

  val TSL_UNSAFE = TSC_POSS_INVALID
  val TSL_TRACKED = TSC_EXPOSED
  val TSL_SAFE = TSC_SAFE

  datatype es_status = ES_ALL | ES_SOME | ES_NONE

  datatype tcType
	= tcVoid
	| tcZero
	| tcInt
	| tcChar
	| tcShort
	| tcLong
	| tcLongLong
	| tcFloat
	| tcDouble
	| tcLongDouble
	| tcPointer
	| tcStruct of tcType list
	| tcUnion of tcType list
	| tcArray of tcType * LargeInt.int
	| tcBitField of tcType * LargeInt.int
	| tcFunction of tcType * tcType list

  datatype absObject
	= aoId of Pid.uid
	| aoMalloc of Aid.uid
	| aoSDot of absObject * tcType list
	| aoUDot of absObject * tcType
	| aoReturn of absObject
	| aoArg of absObject * LargeInt.int
	| aoStar of absObject
	| aoValue of tcType
	| aoStringLit of Aid.uid
	| aoOp of tcType * absObject
	| aoExt of tcType * tcType * absObject
	| aoAddrOf of absObject
	| aoFunction of absObject

  datatype mbuMarker
	= MBU_Assign
(*	| MBU_Call	*)
	| MBU_Verify
	| MBU_Decl
	| MBU_Arg
	| MBU_Return

  datatype redMarker
	= RED_Assign
	| RED_Verify

  exception exTC of string

  fun True _ = true
  fun False _ = false

  (* removes duplicates from a list *)
  fun rmdup _ nil = nil
    | rmdup eqfn (head::tail) =
	if List.exists (fn x => eqfn(x, head)) tail
	then rmdup eqfn tail
	else head::(rmdup eqfn tail)

  fun intToString i = if i >= 0 then Int.toString i
		      else "-" ^ (Int.toString (~i))

  fun largeIntToString li = if li >= 0 then LargeInt.toString li
			    else "-" ^ (LargeInt.toString (~li))

  fun ts_le (l1,l2) =
      case l1 of TSC_POSS_INVALID => true
	       | TSC_BADLY_TYPED => not (l2 = TSC_POSS_INVALID)
	       | TSC_INFLUENTIAL => (l2 = TSC_INFLUENTIAL)
				    orelse (l2 = TSC_SAFE)
	       | TSC_EXPOSED => (l2 = TSC_EXPOSED)
				orelse (l2 = TSC_SAFE)
	       | TSC_SAFE => (l2 = TSC_SAFE)

  (* SY: if l1 and l2 are EXPOSED and INFLUENTIAL; we arbitrarily choose l2
   * Might this be a problem???
   *)
  fun ts_min (l1,l2) =
      if ts_le (l1,l2)
      then l1
      else l2

  fun tsToDescr tsc =
      case tsc
	of TSC_POSS_INVALID => "POSS_INVALID"
	 | TSC_BADLY_TYPED => "BADLY_TYPED"
	 | TSC_INFLUENTIAL => "INFLUENTIAL"
	 | TSC_EXPOSED => "EXPOSED"
	 | TSC_SAFE => "SAFE"

  (* Vanilla output function *)
  fun warning msg = TextIO.output (TextIO.stdErr, msg)

  (* output function with location info *)
  fun warningLoc loc msg =
      let val (fname,lineno,colno) =
	  case loc
	    of SourceMap.LOC{srcFile,beginLine,beginCol,...} => (srcFile,beginLine,beginCol)
	     | SourceMap.UNKNOWN => ("unknown",0,0)
      in warning ("[" ^ fname ^ ":" ^ (Int.toString lineno) ^ "." ^ (Int.toString colno) ^ "] " ^ msg)
      end

  fun tcTypeToDescr tcType =
      (case tcType
	 of tcVoid		=> "void"
	  | tcZero		=> "zero"
	  | tcInt		=> "int"
	  | tcChar		=> "char"
	  | tcShort		=> "short"
	  | tcLong		=> "long"
	  | tcLongLong		=> "longlong"
	  | tcFloat		=> "float"
	  | tcDouble		=> "double"
	  | tcLongDouble	=> "longdouble"
	  | tcPointer		=> "pointer"
	  | tcStruct _		=> "aggregate"
	  | tcUnion _		=> "aggregate"
	  | tcArray _		=> "aggregate"
	  | tcBitField (tcty,_)	=> tcTypeToDescr tcty
	  | tcFunction _	=> "aggregate"
	)

  (************************** TCTYPE TO STRING *************************)
  fun liToStr li = (largeIntToString li) ^ " "
  fun tcTypeToString ty =
      case ty
	of tcVoid => "v "
	 | tcZero => "z "
	 | tcInt => "i "
	 | tcChar => "c "
	 | tcShort => "h "
	 | tcLong => "l "
	 | tcLongLong => "g "
	 | tcFloat => "f "
	 | tcDouble => "d "
	 | tcLongDouble => "e "
	 | tcPointer => "p "
	 | tcStruct tylist => "s " ^ (tcTypeListToString tylist) ^ "; "
	 | tcUnion tylist => "u " ^ (tcTypeListToString tylist) ^ "; "
	 | tcArray (ty,li) => "a " ^ (liToStr li) ^ (tcTypeToString ty)
	 | tcBitField (ty,li) => "b " ^ (liToStr li) ^ (tcTypeToString ty)
	 | tcFunction (ty,tylist) => "x " ^ (tcTypeToString ty) ^ (tcTypeListToString tylist) ^ "; "

  and tcTypeListToString nil = ""
    | tcTypeListToString (ty::tail) = (tcTypeToString ty) ^ (tcTypeListToString tail)

  (************************** ABSTRACT OBJECT TO STRING *************************)
  fun aoToString ao =
  (
      let
	  fun pidToStr p = (Int.toString p) ^ " "
	  fun aidToStr a = (Int.toString a) ^ " "

	  fun aoToStr ao =
	      case ao
		of aoId pid => "I " ^ (pidToStr pid)
		 | aoMalloc aid => "M " ^ (aidToStr aid)
		 | aoSDot (ao,tlist) => "S " ^ (aoToStr ao) ^ (tcTypeListToString tlist) ^ "; "
		 | aoUDot (ao,ty) => "U " ^ (aoToStr ao) ^ (tcTypeToString ty)
		 | aoReturn ao => "R " ^ (aoToStr ao)
		 | aoArg (ao,li) => "F " ^ (liToStr li) ^ (aoToStr ao)
		 | aoStar ao => "D " ^ (aoToStr ao)
		 | aoValue ty => "V " ^ (tcTypeToString ty)
		 | aoStringLit aid => "L " ^ (aidToStr aid)
		 | aoOp (ty,ao) => "O " ^ (aoToStr ao) ^ (tcTypeToString ty)
		 | aoExt (tty,fty,ao) => "E " ^ (aoToStr ao) ^ (tcTypeToString tty) ^ (tcTypeToString fty)
		 | aoAddrOf ao => "A " ^ (aoToStr ao)
		 | aoFunction ao => "X " ^ (aoToStr ao)
      in  aoToStr ao
      end
  ) (* end fun aoToString ao *)

  (************************** STRING TO ABSTRACT OBJECT *************************)
  fun stringToAO (intToAid,intToPid,exceptn) str =
  (
      let
	  fun strToLI nil = ((warning "stringToAO/strToLI: parse error - no more token\n");
			     raise exceptn)
	    | strToLI (head::tail) =
		case (LargeInt.fromString head)
		  of SOME li => (li, tail)
		   | NONE => (warning ("stringToAO/strToLI: parse error - invalid number [" ^ head ^ "]\n");
			      raise exceptn)
	  fun strToAid nil = ((warning "stringToAO/strToAid: parse error - no more token\n");
			      raise exceptn)
	    | strToAid (head::tail) =
		case (Int.fromString head)
		  of SOME i => (intToAid i, tail)
		   | NONE => (warning ("stringToAO/strToAid: parse error - invalid number [" ^ head ^ "]\n");
			      raise exceptn)
	  fun strToPid nil = ((warning "stringToAO/strToPid: parse error - no more token\n");
			      raise exceptn)
	    | strToPid (head::tail) =
		case (Int.fromString head)
		  of SOME i => (intToPid i, tail)
		   | NONE => (warning ("stringToAO/strToPid: parse error - invalid number [" ^ head ^ "]\n");
			      raise exceptn)

	  fun strToType nil = ((warning "stringToAO/strToType: parse error - no more token\n");
			       (tcVoid,nil))
	    | strToType (head::tail) =
	      ( case head
		  of "v" =>	(tcVoid,tail)
		   | "z" =>	(tcZero,tail)
		   | "i" =>	(tcInt,tail)
		   | "c" =>	(tcChar,tail)
		   | "h" =>	(tcShort,tail)
		   | "l" =>	(tcLong,tail)
		   | "g" =>	(tcLongLong,tail)
		   | "f" =>	(tcFloat,tail)
		   | "d" =>	(tcDouble,tail)
		   | "e" =>	(tcLongDouble,tail)
		   | "p" =>	(tcPointer,tail)
		   | "s" =>	let val (tylist,tail) = strToTyList tail
				in (tcStruct tylist, tail) end
		   | "u" =>	let val (tylist,tail) = strToTyList tail
				in (tcUnion tylist, tail) end
		   | "a" =>	let val (li,tail) = strToLI tail
				    val (ty,tail) = strToType tail
				in (tcArray (ty,li), tail) end
		   | "b" =>	let val (li,tail) = strToLI tail
				    val (ty,tail) = strToType tail
				in (tcBitField (ty,li), tail) end
		   | "x" =>	let val (ty,tail) = strToType tail
				    val (tylist,tail) = strToTyList tail
				in (tcFunction (ty,tylist), tail) end
		   | _ => (warning ("stringToAO/strToType: parse error - invalid type [" ^ head ^ "]\n");
			   (tcVoid,tail))
	      )

	  and strToTyList nil = (nil,nil)
	    | strToTyList (head::tail) =
		if head = ";"
		then (nil,tail)
		else let val (ty,tail) = strToType (head::tail)
			 val (tylist,tail) = strToTyList tail
		     in (ty::tylist,tail) end

	  fun strToAO nil = (warning "stringToAO/strToAO: parse error - no more token\n";
			     raise exceptn)
	    | strToAO (head::tail) =
	      ( case head
		  of "I" =>	let val (pid,tail) = strToPid tail
				in (aoId pid, tail) end
		   | "M" =>	let val (aid,tail) = strToAid tail
				in (aoMalloc aid, tail) end
		   | "S" =>	let val (ao,tail) = strToAO tail
				    val (tlist,tail) = strToTyList tail
				in (aoSDot (ao,tlist), tail) end
		   | "U" =>	let val (ao,tail) = strToAO tail
				    val (ty,tail) = strToType tail
				in (aoUDot (ao,ty), tail) end
		   | "R" =>	let val (ao,tail) = strToAO tail
				in (aoReturn ao, tail) end
		   | "F" =>	let val (li,tail) = strToLI tail
				    val (ao,tail) = strToAO tail
				in (aoArg (ao,li), tail) end
		   | "D" =>	let val (ao,tail) = strToAO tail
				in (aoStar ao, tail) end
		   | "V" =>	let val (ty,tail) = strToType tail
				in (aoValue ty, tail) end
		   | "L" =>	let val (aid,tail) = strToAid tail
				in (aoStringLit aid, tail) end
		   | "O" =>	let val (ao,tail) = strToAO tail
				    val (ty,tail) = strToType tail
				in (aoOp (ty,ao), tail) end
		   | "E" =>	let val (ao,tail) = strToAO tail
				    val (tty,tail) = strToType tail
				    val (fty,tail) = strToType tail
				in (aoExt (tty,fty,ao), tail) end
		   | "A" =>	let val (ao,tail) = strToAO tail
				in (aoAddrOf ao, tail) end
		   | "X" =>	let val (ao,tail) = strToAO tail
				in (aoFunction ao, tail) end
		   | _ => (warning ("stringToAO/strToAO: parse error - invalid object [" ^ head ^ "]\n");
			   raise exceptn)
	      )
	  val (ao,_) = strToAO (String.tokens Char.isSpace str)
      in  ao end
  ) (* end fun stringToAO (intToAid,intToPid) str *)

end (* Structure Rtc *)

