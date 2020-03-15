(* Translate from parser tree to ir *)

let lbln = ref 0;;

let genlabel () =
    lbln := !lbln + 1;
    !lbln;;

open Parser;;

let rec trans expr env =
    (* TODO first cons the instructions
     * and to a reverse in the end to speedup *)
    match expr with
    | Num i -> [Ir.Const i]
    | Str i -> [Ir.Sconst i]
    | Var i ->
            begin match Env.lookup env i with
            | x -> [Ir.Acc x]
            | exception _ -> failwith "Var not found" end
    | If(e1, e2, e3) ->
            let ir1 = trans e1 env in
            let ir2 = trans e2 env in
            let ir3 = trans e3 env in
            let lfls = genlabel () in
            let lend = genlabel () in
            ir1
            @ [Ir.Brif (Ir.Cfalse, lfls)] 
            @ (ir2 @ [Ir.Br lend])
            @ [Ir.Label lfls] @ ir3 @ [Ir.Label lend]
    | Fun(f, args, e) ->
            (* should use string label instead of int? *)
            let fl = genlabel () in
            let env1 = Env.extend env f fl in
            let env2 =
                List.fold_right
                (fun a r ->
                    let al = genlabel () in
                    Env.extend r a al) args env1 in
            trans e env2
    | App(e1, args) ->
            let ins1 = [] in
            let ins2 =
                List.fold_right
                (fun a ins ->
                    let xs = trans a env in
                    xs @ [Ir.Push] @ ins) args ins1 in
            let nargs = List.length args in
            let ei = trans e1 env in
            ins2 @ ei @ [Ir.Apply nargs]
    | Begin es ->
            List.fold_right
            (fun a ins ->
                (trans a env) @ ins) es []
    | Prim pr ->
            [Ir.Getglobal (Ir.get_prim_nr pr)] (* TODO *)
    | _ -> failwith "Not handled"
;;
