(* parser combinator
Some nameing abbreviations:
    p is short for parser
    r is short for result
    i is short for input
 *)

exception Noparse

let (++) p1 p2 input =
    let r1, i1 = p1 input in
    let r2, i2 = p2 i1 in
    (r1, r2), i2;;

let (|||) p1 p2 input =
    try p1 input
    with Noparse -> p2 input ;;

let rec many p input =
    try
        let r1, i1 = p input in
        let rs, is = many p i1 in
        (r1 :: rs), is
    with Noparse -> [], input;;

let handle p f input =
    let res, i = p input in
    f res, i;;

let rec unzip = function
    | [] -> [], []
    | (x,y)::rest ->
            let xs, ys = unzip rest in
            x::xs, y::ys;;

let listof p sep input =
    let p1 = many (sep ++ p) in
    let p2 = handle p1 (fun rs -> snd (unzip rs)) in
    let p3 = p ++ p2 in
    let (r1, r2), rest = p3 input in
    r1::r2, rest
;;

type ast =
    | Var of string
    | Num of float
    | Str of string
    | If of ast * ast * ast
    | Fun of string * string list * ast (* How to specify the first string as Var x? *)
    | Let of string * ast * ast
    | While of ast * ast
    | Begin of ast list
    (* More to add *)
;;

let rec to_string ast =
    let open Printf in
    begin match ast with
    | Var x -> "Var(" ^ x ^ ")"
    | Num x -> "Num(" ^ string_of_float x ^ ")"
    | Str x -> "Str(" ^ x ^ ")"
    | If (e1, e2, e3) ->
            sprintf "IF %s\nTHEN %s\nELSE %s" (to_string e1) (to_string e2) (to_string e3)
    | Fun (x, args, e) ->
            sprintf "FUN %s(%s) =\n\t%s" x (args_to_string args) (to_string e)
    | Let (x, e1, e2) ->
            sprintf "LET %s = %s IN\n%s" x (to_string e1) (to_string e2)
    | While (e1, e2) ->
            sprintf "WHILE %s DO\n%s" (to_string e1) (to_string e2)
    | Begin es ->
            sprintf "BEGIN\n" ^
            List.fold_right (fun x r -> (to_string x) ^ ";\n" ^ r) es "" ^
            "END\n"
    end;
and args_to_string args =
    List.fold_right (fun x r -> (x ^ ", " ^ r)) args ""
;;

(* from here on, parser is closely binded with lexer *)
module L = Lex;;

let pa_atom t l =
    let (t', l') as x = L.lex l in
    if t' = t then x, l'
    else raise Noparse
;;

let rec pa_var l =
    let t', l' = L.lex l in
    match t' with
    | L.Ident i -> Var i, l'
    | _ -> raise Noparse
and pa_name l =
    let t', l' = pa_var l in
    match t' with
    | Var x -> x, l'
    | _ -> raise Noparse
and pa_str l =
    let t', l' = L.lex l in
    match t' with
    | L.Str i -> Str i, l'
    | _ -> raise Noparse
and pa_num l =
    let t', l' = L.lex l in
    match t' with
    | L.Num i -> Num i, l'
    | _ -> raise Noparse
let pa_const =
    pa_str ||| pa_num;;

let rec pa_expr l =
    (* need to handle function call and binary op *)
    begin
    pa_if
    ||| pa_fun
    ||| pa_const
    ||| pa_let
    ||| pa_begin
    ||| pa_var
    end l
and pa_if l =
    let (((((_, e1), _), e2), _), e3), l' =
        begin
    pa_atom L.If ++ pa_expr
    ++ pa_atom L.Then ++
    pa_expr ++ pa_atom L.Else ++ pa_expr
        end l in
    If (e1, e2, e3), l'
and pa_fun l =
    let ((((((_, name), _), arglist), _), _), e2), l' =
        begin
    pa_atom L.Fun ++ pa_name ++ pa_atom L.Lbrace ++ pa_arglist ++ pa_atom L.Rbrace ++
    pa_atom L.Eq ++ pa_expr
        end l in
    Fun (name, arglist, e2), l'
and pa_arglist l =
    listof pa_name (pa_atom L.Comma) l
and pa_let l =
    let (((((_, name), _), e1), _), e2), l' =
        begin
    pa_atom L.Let ++ pa_name ++ pa_atom L.Eq ++ pa_expr ++ pa_atom L.In ++ pa_expr
        end l in
    Let (name, e1, e2), l'
and pa_begin l =
    let rs, l' = listof pa_expr (pa_atom L.Semicolon) l in
    Begin rs, l'
;;

(*
#trace pa_expr;;
#trace pa_var;;
*)

