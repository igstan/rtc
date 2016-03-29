signature RTC =
sig

  datatype ts_categ = TSC_POSS_INVALID | TSC_BADLY_TYPED | TSC_INFLUENTIAL | TSC_EXPOSED | TSC_SAFE

  val TSL_UNSAFE : ts_categ
  val TSL_TRACKED : ts_categ
  val TSL_SAFE : ts_categ

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

  val True : 'a -> bool
  val False : 'a -> bool

  val rmdup : ('a * 'a -> bool) -> 'a list -> 'a list

  val intToString : int -> string
  val largeIntToString : LargeInt.int -> string

  val ts_le : ts_categ * ts_categ -> bool
  val ts_min : ts_categ * ts_categ -> ts_categ
  val tsToDescr : ts_categ -> string
  val tcTypeToDescr : tcType -> string
  val tcTypeToString : tcType -> string
  val tcTypeListToString : tcType list -> string

  val aoToString : absObject -> string
  val stringToAO : (int -> Aid.uid) * (int -> Pid.uid) * exn -> string -> absObject

end
