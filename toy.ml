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
    let l = Lex.new_lexer () in
    let rec lp l =
        begin try
            let e, l' = Parser.pa_expr l in
            print_endline "=> ";
            print_endline (Parser.to_string e);
            print_endline "% ";
            lp l'
        with Parser.Noparse ->
            print_endline "! parser error"
        end
    in
    lp l
;;

