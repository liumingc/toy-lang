(*
let () =
    let l = Lex.new_lexer () in
    let rec lp l =
        match l with
        | Lex.Lno -> ()
        | _ ->
            let tok, l' = Lex.lex l in
            print_string (Lex.to_string tok);
            print_newline ();
            lp l'
    in
    lp l;;
*)

let () =
    let file_name = ref "-" in
    if Array.length Sys.argv > 1 then begin
        file_name := Sys.argv.(1)
    end;
    let l = Lex.new_lexer !file_name in
    let rec lp l =
        begin try
          let e, l' = Parser.pa_expr l in
          print_endline "==AST==> ";
          print_endline (Parser.to_string e);
          print_endline "=INST|=> ";
          let irs = Transl.trans e Env.empty in
          Ir.print_irs irs;
          print_endline "% ";
          lp l'
        with Parser.Noparse ->
          print_endline "! parser error"
        end
    in
    lp l
;;

