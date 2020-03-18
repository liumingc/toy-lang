open Printf

type instr =
    | Const of int
    | Sconst of string
    | Acc of int
    | Envacc of int (* to access free variables *)
    | Grab of int
    | Restart
    | Push
    | Pop
    | Apply of int
    | Appterm of int
    | Getglobal of int (* these are for the primitives etc *)
    | Setglobal of int
    | Closure of int * int (* label * free-var num *)
    | Label of int
    | Br of int
    | Brif of cond * int
    (* More to add, like block etc *)
and cond =
    | Ctrue
    | Cfalse
    | Ceq
    | Cgt
    | Cge
    | Clt
    | Cle
;;

(* Primitives *)
let primitives = [
    ("=", 0);
    ("<", 1);
    (">", 2);
    ("+", 3);
    ("-", 4);
    ("*", 5);
    ("/", 6);
    ("%", 7);
    ("print", 8);
    ("read", 9);
    ("stop", 10);
]

let get_prim_nr pr =
    let rec lp prs =
        match prs with
        | [] -> 100
        | (x, n)::xs ->
                if x = pr then n
                else lp xs
    in
    lp primitives
;;

let print_ir = function
    | Const i ->
            printf "\tconst %d\n" i
    | Sconst s ->
            printf "\tsconst %s\n" s
    | Acc i ->
            printf "\tacc %d\n" i
    | Envacc i ->
            printf "\tenvacc %d\n" i
    | Grab i ->
            printf "\tgrab %d\n" i
    | Restart ->
            printf "\trestart\n"
    | Push ->
            printf "\tpush\n"
    | Pop ->
            printf "\tpop\n"
    | Apply i ->
            printf "\tapply %d\n" i
    | Appterm i ->
            printf "\tappterm %d\n" i
    | Getglobal i ->
            printf "\tgetglobal %d\n" i
    | Setglobal i ->
            printf "\tsetglobal %d\n" i
    | Closure (l, n) ->
            printf "\tclosure (L%d, %d)\n" l n
    | Label i ->
            printf "L%d:\n" i
    | Br i ->
            printf "\tbr L%d\n" i
    | Brif (c, i) ->
            let cc =
                match c with
                | Ctrue -> "true"
                | Cfalse -> "false"
                | _ -> "xx"
            in
            printf "\tbrif %s L%d\n" cc i
;;

let print_irs irs =
    List.iter print_ir irs;;

