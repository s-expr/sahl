open Sparse;;

let run_buffer buf :  Interp.Val.t = 
try
  let prog = Parser.main Lexer.token buf in
  Interp.eval prog 
with
| Lexer.Error msg ->
   Printf.fprintf stderr "%s%!" msg; exit 1
| Parser.Error ->
   Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start buf); exit 1

let main () = 
  let files = List.tl (Array.to_list Sys.argv) in
  List.fold_left
    (fun acc file ->
      let in_file =  open_in file in
      let buffer = Lexing.from_channel in_file in
      (run_buffer buffer) :: acc)
    []
    files
;;

main ()
