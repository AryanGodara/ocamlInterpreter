open TokenType

let keywords = Hashtbl.create 16
let () = 
  Hashtbl.add keywords "and" AND;
  Hashtbl.add keywords "class" CLASS;
  Hashtbl.add keywords "else" ELSE;
  Hashtbl.add keywords "false" FALSE;
  Hashtbl.add keywords "for" FOR;
  Hashtbl.add keywords "fun" FUN;
  Hashtbl.add keywords "if" IF;
  Hashtbl.add keywords "nil" NIL;
  Hashtbl.add keywords "or" OR;
  Hashtbl.add keywords "print" PRINT;
  Hashtbl.add keywords "return" RETURN;
  Hashtbl.add keywords "super" SUPER;
  Hashtbl.add keywords "this" THIS;
  Hashtbl.add keywords "true" TRUE;
  Hashtbl.add keywords "var" VAR;
  Hashtbl.add keywords "while" WHILE;

module Token = struct
  type t = { 
    line: int;
    tokenType: TokenType.tokenType;
    lexeme: string;
    literal: string option;
    }

  let token ~tokenType ~lexeme ~literal ~line = { 
    line = line;
    tokenType = tokenType;
    lexeme = lexeme;
    literal = literal;
    }

  let toString t : string = 
    Printf.sprintf "%s %s %s"
  (match t.tokenType with
  | LEFT_PAREN -> "LEFT_PAREN"
  | RIGHT_PAREN -> "RIGHT_PAREN"
  | LEFT_BRACE -> "LEFT_BRACE"
  | RIGHT_BRACE -> "RIGHT_BRACE"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | BANG -> "BANG"
  | BANG_EQUAL -> "BANG_EQUAL"
  | EQUAL -> "EQUAL"
  | EQUAL_EQUAL -> "EQUAL_EQUAL"
  | GREATER -> "GREATER"
  | GREATER_EQUAL -> "GREATER_EQUAL"
  | LESS -> "LESS"
  | LESS_EQUAL -> "LESS_EQUAL"
  | IDENTIFIER -> "IDENTIFIER"
  | STRING -> "STRING"
  | NUMBER -> "NUMBER"
  | AND -> "AND"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FALSE -> "FALSE"
  | FOR -> "FOR"
  | FUN -> "FUN"
  | IF -> "IF"
  | NIL -> "NIL"
  | OR -> "OR"
  | PRINT -> "PRINT"
  | RETURN -> "RETURN"
  | SUPER -> "SUPER"
  | THIS -> "THIS"
  | TRUE -> "TRUE"
  | VAR -> "VAR"
  | WHILE -> "WHILE"
  | EOF -> "EOF")
  t.lexeme
  (match t.literal with
  | None -> ""
  | Some x -> x)
end