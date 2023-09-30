module Scanner = struct
  type t = {
    source: string;
    mutable tokens: Token.Token.t list;
    mutable start: int ;
    mutable current: int;
    mutable line: int;
  }

  let create (source : string) : t = {
    source;
    tokens = [];
    start = 0;
    current = 0;
    line = 1;
  }


  (*? scanTokens() scans the source file and returns a list of tokens *)
  (* let scanTokens (source : string) : Token.Token.t list *)


  (*? scanToken() scans the next token in the source file *)
  (* let scanToken (scanner : t) : unit = *)


  (*? isAtEnd() returns true if the scanner has reached the end of the source file *)
  let isAtEnd (scanner : t) : bool =
    scanner.current >= String.length scanner.source


  (*? addTokenLiteral adds a token to the tokens  *)
  let addTokenLiteral (scanner : t) (tokenType : TokenType.tokenType) (literal : string) : unit =
    let lexeme = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
    let literalOption = Some literal in
    let token = Token.Token.token ~tokenType ~lexeme ~literal:literalOption ~line:scanner.line in
    scanner.tokens <- token :: scanner.tokens


  let addToken (scanner : t) (tokenType : TokenType.tokenType) : unit =
    addTokenLiteral scanner tokenType ""

 

  (*? advance() consumes the next character in the source file and returns it *)
  let advance (scanner : t ) : char =
    let c = String.get scanner.source scanner.current in
    scanner.current <- scanner.current + 1;
    c
  

  (*? match() consumes the current character in the source file if it matches the expected character, returning true if it did, and advancing the current forward *)
  let matchChar (scanner : t) (expected:char) : bool =
    if isAtEnd scanner then false
    else
      let c = String.get scanner.source scanner.current in
      if c <> expected then false
      else
        begin
          scanner.current <- scanner.current + 1;
          true
        end


  (*? peek() returns the current character in the source file without consuming it *)
  let peek (scanner : t) : char =
    if isAtEnd scanner then '\000'
    else
      String.get scanner.source scanner.current
    

  (*? peekNext() returns the next character in the source file without consuming it *)
  let peekNext (scanner : t) : char =
    if scanner.current + 1 >= String.length scanner.source then '\000'
    else
      String.get scanner.source (scanner.current+1)
  
  (*? string() consumes the next string in the source file and adds it to the tokens slice *)
  let addString (scanner : t) : unit =
    while (peek scanner) <> '"' && not (isAtEnd scanner) do
      if (peek scanner) == '\n' then scanner.line <- scanner.line + 1;
      
      advance scanner |> ignore;
    done;
    (* The Closing *)
    if (isAtEnd scanner) then ErrorLogging.error ~lint:scanner.line ~message:"Unterminated string.";
    
    (* Trim the surrounding quotes *)
    let value = String.sub scanner.source scanner.start (scanner.current - scanner.start - 1) in
    addTokenLiteral scanner TokenType.STRING value;
    ()


  let isDigit (c:char) : bool =
    c >= '0' && c <= '9'

  let isAlpha (c:char) : bool =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

  let isAlphaNumeric (c:char) : bool =
    isAlpha c || isDigit c

  (*? number() consumes the next number in the source file and adds it to the tokens slice *)
  let addNumber (scanner : t) : unit =
    while isDigit (peek scanner) do
      advance scanner |> ignore;
    done;
    (* Look for a fractional part *)
    if peek scanner == '.' && isDigit (peekNext scanner) then
      begin
        (* Consume the "." *)
        advance scanner |> ignore;
        while isDigit (peek scanner) do
          advance scanner |> ignore;
        done;
      end;
    let value = String.sub scanner.source scanner.start (scanner.current - scanner.start) in
    addTokenLiteral scanner TokenType.NUMBER value;
    ()
    
end