let hadError : bool ref = ref false

let report (lint:int) (where:string) (message:string) : unit =
  print_endline ("[line " ^ string_of_int lint ^ "] Error" ^ where ^ ": " ^ message ^ "\n");
  hadError := true;
  ()

let error (lint:int) (message:string) : unit =
  print_endline ("[line " ^ string_of_int lint ^ "] Error : " ^ message ^ "\n");
  report lint "" message;
  ()
