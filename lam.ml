type t =
    | Var of string
    | Const of int (* TODO, support boolean, string etc *)
    | Lam of string * t
    | App of t * t;;

(* utils *)
let nx = ref 100
let gen () =
    nx := !nx + 1;
    "G:" ^ (string_of_int !nx);;

(* Beta_conv: [t/x] e *)
let rec subst t x e =
    begin match e with
    | Const _ -> e
    | Var x' ->
            if x' = x then t
            else e
    | Lam (y, e1) ->
            if x = y then e
            else
                (* There is a subtle bug in #1 version
                 * need to rename to avoid name capture problem
                 * The rename is only necessary when
                 * FreeVars(t) /\ BoundVars(e1) is not empty
                 * *)
                (*Lam(y, subst t x e1)*)
                Lam(y, subst t x (rename e1))
    | App(e1, e2) ->
            App (subst t x e1, subst t x e2) end
(* Alpha_conv *)
and rename e =
    match e with
    | Const _ -> e
    | Var _ -> e
    | Lam(x, e1) ->
            let g = gen () in
            Lam(g, rename (subst (Var g) x e1))
    | App(e1, e2) ->
            App (rename e1, rename e2) ;;
