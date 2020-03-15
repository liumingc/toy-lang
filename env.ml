
let extend env name i =
    (name, i) :: env;;

let rec lookup env name =
    match env with
    | [] -> failwith (name ^ " Not found")
    | (x, i)::xs ->
            if name = x
            then i
            else lookup xs name
;;

let empty = [];;
