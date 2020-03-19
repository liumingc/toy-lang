type tok =
    | Ident of string
    | Str of string
    | Num of int (*float*)
    (* Special symbols *)
    | Dot
    | Lbrace
    | Rbrace
    | Lcbrace
    | Rcbrace
    | Lbracket
    | Rbracket
    | Comma
    | Colon
    | Eq
    | Great (* > *)
    | Less (* < *)
    | Larrow (* <- *)
    | Rarrow (* -> *)
    | Unkown of char
    | Semicolon
    | Minus
    | Plus
    | Star  (* * *)
    | Slash (* / *)
    | Bar (* | *)
    | Underline (* _ *)
    | Dddot
    | Eof
    | Unspec
    (* Keyword *)
    | If
    | Then
    | Else
    | Do
    | Begin
    | End
    | While
    | For
    | Fun
    | Fn
    | Let
    | In
    | Match
    | With
    | When
    | Of
    | As
    | And
    | Type
    | Datatype
;;

let to_string t =
    match t with
    | Ident x -> "Ident(" ^ x ^ ")"
    | Str x -> "Str(" ^ x ^ ")"
    | Num x -> "Num(" ^ string_of_int x ^ ")"
    | Dot -> "."
    | Lbrace -> "("
    | Rbrace -> ")"
    | Lcbrace -> "{"
    | Rcbrace -> "}"
    | Semicolon -> "SEMICOLON"
    | Bar -> "|"
    | Slash -> "/"
    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
    | Underline -> "_"
    | Dddot -> "..."
    | Unkown c -> "?" ^ Printf.sprintf "%c" c
    | If -> "IF"
    | Then -> "THEN"
    | Else -> "ELSE"
    | Fun -> "FUN"
    | Fn -> "FN"
    | Do -> "DO"
    | Begin -> "BEGIN"
    | End -> "END"
    | Let -> "LET"
    | In -> "IN"
    | While -> "WHILE"
    | For -> "FOR"
    | Match -> "MATCH"
    | With -> "WITH"
    | When -> "WHEN"
    | Of -> "OF"
    | As -> "AS"
    | And -> "AND"
    | Type -> "TYPE"
    | Datatype -> "DATATYPE"
    | Lbracket -> "["
    | Rbracket -> "]"
    | Larrow -> "=>"
    | Rarrow -> "<-"
    | Comma -> ","
    | Colon -> ":"
    | Eq -> "="
    | Less -> "<"
    | Great -> ">"
    | Eof -> "EOF"
    | Unspec -> "??? unspecified"
;;


let last_char = ref None;;

let kwtab = Hashtbl.of_seq (List.to_seq [
    ("if", If);
    ("then", Then);
    ("else", Else);
    ("fun", Fun);
    ("fn", Fn);
    ("for", For);
    ("do", Do);
    ("begin", Begin);
    ("end", End);
    ("while", While);
    ("let", Let);
    ("in", In);
    ("match", Match);
    ("with", With);
    ("as", As);
    ("when", When);
    ("type", Type);
    ("datatype", Datatype);
]);;


let print_kwtab () =
    Hashtbl.iter (fun k v ->
        print_string k;
        print_string " => ";
        print_string (to_string v);
        print_newline ()
        ) kwtab;;

(* print_kwtab();; *)

let is_kw x =
    match Hashtbl.find_opt kwtab x with
    | None -> false
    | Some _ -> true;;


(* get a token from in_channel *)
let next_tok ic =
    let readc () =
        match !last_char with
        | None -> input_char ic 
        | Some x -> last_char := None; x in
    (*
    let rec skip_spc () =
        let x = readc () in
        match x with
        | ' ' | '\t' -> skip_spc ()
        | _ -> last_char := Some x in
    *)
    let read_until stop initx =
        let buf = Buffer.create 20 in
        let rec lp () =
            let x = readc () in
            if stop x then last_char := Some x
            else begin
                Buffer.add_char buf x;
                lp ()
            end in
        begin match initx with
        | None -> lp ()
        | Some x ->
                Buffer.add_char buf x;
                lp () end;
        Buffer.contents buf in
    let read_str () =
        (* TODO handle the escape sequence *)
        let s = read_until (fun x -> x == '"') None in
        ignore (readc ()); (* eat the last '"' *)
        s in
    let is_alpha x =
        match x with
        | 'a' .. 'z' | 'A' .. 'Z' | '\'' -> true
        | _ -> false in
    let is_digit x =
        match x with
        | '0' .. '9' -> true
        | _ -> false in
    let read_ident x =
        let s = read_until (fun x -> not (is_alpha x)) (Some x) in
        s in
    let read_digits x =
        (* TODO check 3.45.2 as error *)
        let s = read_until (fun x -> not (is_digit x (* || x = '.' *) )) (Some x) in
        s in
    let choice expect a b =
        let y = readc () in
        begin match y with
        | x when x = expect -> a
        | _ -> last_char := Some y; b
        end in
    let rec token () =
        let x = readc () in
        begin match x with
        | '"' -> let s = read_str () in Str s
        | x when is_alpha x ->
                let s = read_ident x in
                begin
                match Hashtbl.find_opt kwtab s with
                | None -> Ident s
                | Some x -> x
                end
        | x when is_digit x ->
                let s = read_digits x in
                (*Num (Scanf.sscanf s "%f" (fun x -> x))*)
                (*Num (float_of_string s)*)
                Num (int_of_string s)
        | ' ' | '\t' -> token ()
        | '\n' -> token ()
        | '.' -> Dot
        | '(' -> Lbrace
        | ')' -> Rbrace
        | '{' -> Lcbrace
        | '}' -> Rcbrace
        | '[' -> Lbracket
        | ']' -> Rbracket
        | ':' -> Colon
        | ',' -> Comma
        | '=' -> Eq
        | ';' -> Semicolon
        | '-' -> choice '>' Rarrow Minus
        | '<' -> choice '-' Larrow Less
        | '>' -> Great
        | '/' -> Slash
        | '*' -> Star
        | '+' -> Plus
        | '_' -> Underline
        | '|' -> Bar
        | _ -> Unkown x
        end in
    token ();;

(* need to wrap lexer, so lex : lex -> tok * lex *)
type t = 
    | Lyes of tok * t
    | Lno (* EOF *)
    | Lpending of int
;;


let tok_cache = Array.make 5000 Unspec;; (* TODO the cache should be increasing automatically *)
let ic = ref stdin;; (* TODO *)

let new_lexer file_name =
    if file_name <> "-" then begin
        try
            let fh = open_in file_name in
            ic := fh
        with _ -> ignore ()
    end;
    Lpending 0;;

let lex l =
    match l with
    | Lno -> Eof, Lno
    | Lyes (tok, l') -> tok, l' (* this case is not used *)
    | Lpending i ->
            match tok_cache.(i) with
            | Unspec ->
                    begin try
                        let x = next_tok !ic in
                        begin tok_cache.(i) <- x;
                        x, Lpending (i+1) end
                    with End_of_file ->
                        (Eof, Lno)
                    end
            | Eof -> Eof, Lno
            | x ->
                    x, Lpending (i+1)
;;

(*
let lex l =
    let (x, _) as a = lex' l in
    Printf.printf "<- token. %s\n" (to_string x);
    a
;;
*)
