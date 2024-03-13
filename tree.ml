type id = string
type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

let rec maxargs : stm -> int =
 fun stm ->
  let rec expaux = function
    | IdExp _ | NumExp _ -> 0
    | OpExp (e1, _, e2) -> max (expaux e1) (expaux e2)
    | EseqExp (s, e) -> max (maxargs s) (expaux e)
  in
  match stm with
  | CompoundStm (s1, s2) -> max (maxargs s1) (maxargs s2)
  | AssignStm (_, e) -> expaux e
  | PrintStm el ->
      List.fold_left (fun acc e -> max acc (expaux e)) (List.length el) el

let interp : stm -> unit =
 fun stm ->
  let open struct
    type table = (id * int) list
  end in
  let update t id v = (id, v) :: t in
  let lookup t id = snd (List.find (fun (id', _) -> id = id') t) in
  let rec interpStm : stm -> table -> table =
   fun stm t ->
    match stm with
    | CompoundStm (s1, s2) -> t |> interpStm s1 |> interpStm s2
    | AssignStm (id, e) ->
        let v, t' = interpExp e t in
        update t' id v
    | PrintStm el ->
        List.fold_left
          (fun t e ->
            let v, t' = interpExp e t in
            print_int v;
            print_newline ();
            t')
          t el
  and interpExp : exp -> table -> int * table =
   fun exp t ->
    match exp with
    | IdExp id -> (lookup t id, t)
    | NumExp n -> (n, t)
    | OpExp (e1, op, e2) ->
        let v1, t' = interpExp e1 t in
        let v2, t'' = interpExp e2 t' in
        ( (match op with
          | Plus -> v1 + v2
          | Minus -> v1 - v2
          | Times -> v1 * v2
          | Div -> v1 / v2),
          t'' )
    | EseqExp (s, e) -> t |> interpStm s |> interpExp e
  in
  ignore (interpStm stm [])
