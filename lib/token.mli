val keywords : (string, TokenType.tokenType) Hashtbl.t

module Token : sig
  type t
  val token : tokenType:TokenType.tokenType -> lexeme:string -> literal:(string option) -> line:int -> t
  val toString : t -> string
end