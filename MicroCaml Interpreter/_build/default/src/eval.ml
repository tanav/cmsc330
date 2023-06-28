open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(* type values = Int of int|Bool of bool|String of string *)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)

let rec eval_expr env e = match e with 
  | Value(i) -> i

  | ID(i) -> ref_lookup env i

  | Fun(x, y) -> Closure(env, x, y)

  | FunctionCall(x, y) -> 
    let temp1 = eval_expr env x in let temp2 = eval_expr env y in (match temp1 with 
      | Closure (a, b, c) -> let e = ref_extend a b temp2 in eval_expr e c
      | _ -> raise (TypeError "ERROR!"))

  | Not(i) -> 
    let temp = eval_expr env i in (match temp with
      | Bool(x) -> Bool(not x)
      | _ -> raise (TypeError "ERROR!"))

  | Let(x, condition, y, z) ->
    if condition == false then
      let temp1 = eval_expr env y in let environ = ref_extend env x temp1 in let temp2 = eval_expr environ z in temp2
    else 
      let e = ref_extend_tmp env x in let temp1 = eval_expr e y in ref_update e x temp1; let temp2 = eval_expr e z in temp2
      
  | If(x, y, z) -> 
    let condition = eval_expr env x in (match condition with
      | Bool(x) -> if x then eval_expr env y else eval_expr env z
      | _ -> raise (TypeError "ERROR!"))

  | Binop(x, y, z) -> (match x with
    | NotEqual -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Bool(a), Bool(b) -> Bool (a != b)
        | Int(a), Int(b) -> Bool (a != b)
        | String(a), String(b) -> Bool (a != b)
        | _ -> raise (TypeError "ERROR!"))
    | Equal -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Bool(a), Bool(b) -> Bool (a == b)
        | Int(a), Int(b) -> Bool (a == b)
        | String(a), String(b) -> Bool (a == b)
        | _ -> raise (TypeError "ERROR!"))
    | GreaterEqual -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> Bool (a >= b)
        | _ -> raise (TypeError "ERROR!"))
    | LessEqual -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> Bool (a <= b)
        | _ -> raise (TypeError "ERROR!"))
    | Greater -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> Bool (a > b)
        | _ -> raise (TypeError "ERROR!"))
    | Less -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> Bool (a < b)
        | _ -> raise (TypeError "ERROR!"))
    | Sub ->
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> Int (a - b)
        | _ -> raise (TypeError "ERROR!"))
    | Add ->
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> Int (a + b)
        | _ -> raise (TypeError "ERROR!"))
    | Div -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> if b != 0 then Int (a / b) else raise DivByZeroError
        | _ -> raise (TypeError "ERROR!"))
    | Mult ->
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Int(a), Int(b) -> Int (a * b)
        | _ -> raise (TypeError "ERROR!"))
    | Or ->
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Bool(a), Bool(b) -> Bool (a || b)
        | _ -> raise (TypeError "ERROR!"))
    | And ->
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | Bool(a), Bool(b) -> Bool (a && b)
        | _ -> raise (TypeError "ERROR!"))
    | Concat -> 
      let temp1 = eval_expr env y in let temp2 = eval_expr env z in (match (temp1, temp2) with 
        | String(a), String(b) -> String (a ^ b)
        | _ -> raise (TypeError "ERROR!")))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

 let eval_mutop env m = match m with 
  | NoOp -> env, None
  | Def(a, b) -> 
    let e = ref_extend_tmp env a in let temp = eval_expr e b in 
    ref_update e a temp; e, Some temp
  | Expr(i) -> let temp = eval_expr env i in env, Some temp