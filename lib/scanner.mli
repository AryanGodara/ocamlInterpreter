module Scanner : sig
  type t
  val create : source:string -> t
  val scanTokens : unit -> (Token.Token.t list)
end