(* parser combinator
Some nameing abbreviations:
    p is short for parser
    r is short for result
    i is short for input
 *)

open Printf

exception Noparse
exception Done

(* Why use tuple instead of list? because r1 and r2 can have different types *)
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

(*
 * Do i need these things:
   * tuples
   * prim1(~, !), prim2(+, -) etc
   In syntax, need () to force precedence,
   but that should be gone in ast(i think).
 *)
(*
type pr =
  | Prim_plus
  | Prim_minus (* etc *)
;;
*)

type ast =
    | Var of string
    | Num of int
    | Str of string
    | If of ast * ast * ast
    | Fun of string * string list * ast (* How to specify the first string as Var x? *)
    | Fn of string list * ast (* anonymous function *)
    | App of ast * ast list (* Application *)
    | Let of string * ast * ast
    | While of ast * ast
    | Begin of ast list
    | Match of ast * clause list
    | Prim of string
    (* More to add *)
and clause =
    pat * ast option * ast (* pattern [when e] -> e *)
and pat =
    | Pconsti of int
    | Pconsts of string
    | Pvar of string
    | Pany
    | Pctor of pat list (* constructor, or sum type *)
    | Precord of recpat list * bool (* if has ..., then bool is true *)
    | Ptuple of pat list
and recpat = string * pat
;;

let rec to_string ast =
    (*let open Printf in*)
    begin match ast with
    | Var x -> "Var(" ^ x ^ ")"
    | Num x -> "Num(" ^ string_of_int x ^ ")"
    | Str x -> "Str(" ^ x ^ ")"
    | If (e1, e2, e3) ->
            sprintf "IF %s\nTHEN %s\nELSE %s" (to_string e1) (to_string e2) (to_string e3)
    | Fun (x, args, e) ->
            sprintf "FUN %s(%s) =\n%s" x (args_to_string args) (to_string e)
    | Fn (args, e) ->
            sprintf "FN (%s) => %s" (args_to_string  args) (to_string e)
    | App (f, args) ->
        sprintf "%s(%s)" (to_string f) (list_to_string to_string args)
    | Let (x, e1, e2) ->
            sprintf "LET %s = %s IN\n%s" x (to_string e1) (to_string e2)
    | While (e1, e2) ->
            sprintf "WHILE %s DO\n%s" (to_string e1) (to_string e2)
    | Begin es ->
            sprintf "BEGIN\n" ^
            List.fold_right (fun x r -> (to_string x) ^ ";\n" ^ r) es "" ^
            "END\n"
    | Match (e, cls) ->
            sprintf "MATCH %s WITH\n" (to_string e) ^
            List.fold_right (fun x r -> (cla_to_string x) ^ "\n" ^ r) cls ""
    | Prim pr ->
        sprintf "Prim(%s)" pr
    end
and args_to_string args =
    List.fold_right (fun x r -> (x ^ ", " ^ r)) args ""
and list_to_string f ls =
    List.fold_right (fun x r -> (f x ^ ", " ^ r)) ls ""
and cla_to_string = function (pat, g, e) ->
    let opt_str = begin match g with
    | None -> ""
    | Some e -> "WHEN " ^ to_string e end in
    sprintf "| %s%s => %s" (pat_to_string pat) opt_str (to_string e)
and pat_to_string = function
    | Pconsti i -> sprintf "Pconst %d" i
    | Pconsts s -> sprintf "Pconst %s" s
    | Pvar s -> sprintf "Pvar %s" s
    | Pany -> "Pany"
    | Pctor ps ->
            "Pctor(" ^
            List.fold_right (fun x r -> pat_to_string x ^ "," ^ r) ps ")"
    | Precord (_, _) -> "Precord"
    | Ptuple ps -> List.fold_right (fun x r -> pat_to_string x ^ "," ^ r) ps ""
;;

(* from here on, parser is closely binded with lexer *)
module L = Lex;;

let pa_atom t l =
  let (t', l') as x = L.lex l in
  if t' = t then x, l'
  else if t' = L.Eof then raise Done
  else raise Noparse
;;

let rec pa_var l =
    let t', l' = L.lex l in
    match t' with
    | L.Ident i -> Var i, l'
    | L.Eof -> raise Done
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
    | L.Eof -> raise Done
    | _ -> raise Noparse
and pa_num l =
    let t', l' = L.lex l in
    match t' with
    | L.Num i -> Num i, l'
    | L.Eof -> raise Done
    | _ -> raise Noparse
and pa_end l =
    let t', _ = L.lex l in
    print_endline ("parser trying pa_end:" ^ L.to_string t');
    match t' with
    | L.Eof -> raise Done
    | _ -> raise Noparse
let pa_const =
    pa_str ||| pa_num;;

let rec pa_expr l =
    (* need to handle function call and binary op
     * how to handle left recursion? *)
    begin
    pa_if
    ||| pa_fun
    ||| pa_let
    ||| pa_begin
    ||| pa_brace
    (*||| pa_const*)
    ||| pa_var_or_app
    ||| pa_match
    ||| pa_end
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
and pa_fn l =
    let p1 =
      pa_atom L.Fn ++ pa_atom L.Lbrace ++ pa_arglist ++ pa_atom L.Rbrace ++
      pa_atom L.Rarrow ++ pa_expr in
    let ((((_, arglist), _), _), e), l' = p1 l in
    Fn (arglist, e), l'
and pa_let l =
    let (((((_, name), _), e1), _), e2), l' =
        begin
    pa_atom L.Let ++ pa_name ++ pa_atom L.Eq ++ pa_expr ++ pa_atom L.In ++ pa_expr
        end l in
    Let (name, e1, e2), l'
and pa_begin l =
  let comb_p = pa_atom L.Begin ++
  (listof pa_expr (pa_atom L.Semicolon)) ++ pa_atom L.End in
  let ((_, rs), _), l' = comb_p l in
    Begin rs, l'
and pa_brace l =
    let comb_p = pa_atom L.Lbrace ++ pa_expr ++ pa_atom L.Rbrace in
    let ((_, e), _), l' = comb_p l in
    e, l'
and pa_oprand l =
    (pa_var ||| pa_const ||| pa_fn) l
and pa_var_or_app l =
    let f, l1 = pa_oprand l (* or pa_fn? *) in
    let next, l2 = L.lex l1 in
    match next with
    | L.Lbrace ->
            (* Application. So f (a b) is ambiguous for (a b).
             * need to use another symbol to force precedence.
         * I think there is a reason for distinguishing expr and stmt.
         *)
            begin
                let p1 = listof pa_expr (pa_atom L.Comma) in
                let p2 = p1 ++ pa_atom L.Rbrace in
                let (rs, _), l3 = p2 l2 in
                App (f, rs), l3
        end
    | L.Plus | L.Minus | L.Eq | L.Less | L.Great ->
            (*let op2, l3 = pa_oprand l2 in*)
            let op2, l3 = pa_var_or_app l2 in
            App (Prim (L.to_string next), [f; op2]), l3
            (* should define a pa_bin to enhance this
             * BUG: can't handle `a + 2 + 3` for now. *)
    | _ -> f, l1
and pa_match l =
    let comb_p =
        pa_atom L.Match ++ pa_expr ++ pa_atom L.With ++
        listof pa_cla (pa_atom L.Bar) in
    let (((_, e1), _), clas), l' = comb_p l in
    Match (e1, clas), l'
and pa_cla l =
    let comb_p =
        (* TODO handle when clause *)
        pa_pat ++ pa_atom L.Rarrow ++ pa_expr in
    let ((p, _), e2), l' = comb_p l in
    (p, None, e2), l'
and pa_pat l =
    let t', l' = L.lex l in
    let p = begin match t' with
    | L.Num i -> Pconsti i
    | L.Str s -> Pconsts s
    | L.Ident i -> Pvar i
    | L.Underline -> Pany
    (* TODO *)
    | _ -> raise Noparse
    end in
    p, l'
;;

(*
#trace pa_expr;;
#trace pa_var;;
*)

