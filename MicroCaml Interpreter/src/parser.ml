open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_if toks = match lookahead toks with
  | Some Tok_If -> let x = match_token toks Tok_If in
      let x1, temp1 = parse_expr x in
      let x2 = match_token x1 Tok_Then in
      let x3, temp2 = parse_expr x2 in
      let x4 = match_token x3 Tok_Else in
      let x5, temp3 = parse_expr x4 in
      x5, If(temp1, temp2, temp3)
  | _ -> raise (InvalidInputException "ERROR!")

and parse_primary toks = match lookahead toks with
  | Some Tok_LParen -> let x = match_token toks Tok_LParen in
      let x1, temp = parse_expr x in
      let x2 = match_token x1 Tok_RParen in x2, temp
  | Some Tok_ID i -> let x = match_token toks (Tok_ID i) in x, ID(i)
  | Some Tok_String i -> let x = match_token toks (Tok_String i) in x, Value(String(i))
  | Some Tok_Bool i -> let x = match_token toks (Tok_Bool i) in x, Value(Bool(i))
  | Some Tok_Int i -> let x = match_token toks (Tok_Int i) in x, Value(Int(i))
  | _ -> raise (InvalidInputException "ERROR!")

and parse_let toks =
  let x = match_token toks Tok_Let in match lookahead x with
    | Some Tok_ID i -> (let x1, i = parse_primary x in
        let x2 = match_token x1 Tok_Equal in
        let x3, temp1 = parse_expr x2 in
        let x4 = match_token x3 Tok_In in
        let x5, temp2 = parse_expr x4 in
        match i with
          | ID(str) -> x5, Let(str, false, temp1, temp2)
          | _ -> raise (InvalidInputException "ERROR!"))
    | Some Tok_Rec -> (let x1 = match_token x Tok_Rec in
        let x2, i = parse_primary x1 in
        let x3 = match_token x2 Tok_Equal in
        let x4, temp1 = parse_expr x3 in
        let x5 = match_token x4 Tok_In in
        let x6, temp2 = parse_expr x5 in
        match i with 
          | ID(str) -> x6, Let(str, true, temp1, temp2)
          | _ -> raise (InvalidInputException "ERROR!"))
    | _ -> raise (InvalidInputException "ERROR!")

and parse_fun toks = match lookahead toks with
  | Some Tok_Fun -> (let x = match_token toks Tok_Fun in
      let x1, i = parse_primary x in
      let x2 = match_token x1 Tok_Arrow in
      let x3, temp = parse_expr x2 in
    match i with
      | ID(str) -> x3, Fun(str, temp)
      | _ -> raise (InvalidInputException "ERROR!"))
  | _ -> raise (InvalidInputException "ERROR!")

and parse_fun_call toks =
  let x, temp = parse_primary toks in match lookahead x with
    | Some Tok_LParen -> let x1, temp2 = parse_primary x in x1, FunctionCall(temp, temp2)
    | Some Tok_ID i -> let x1, temp2 = parse_primary x in x1, FunctionCall(temp, temp2)
    | Some Tok_String i -> let x1, temp2 = parse_primary x in x1, FunctionCall(temp, temp2)
    | Some Tok_Bool i -> let x1, temp2 = parse_primary x in x1, FunctionCall(temp, temp2)
    | Some Tok_Int i -> let x1, temp2 = parse_primary x in x1, FunctionCall(temp, temp2) 
    | _ -> x, temp

and parse_unary toks = match lookahead toks with
  | Some Tok_Not -> let x = match_token toks Tok_Not in
      let x1, temp = parse_unary x in x1, Not(temp)
  | _ -> let x, temp = parse_fun_call toks in x, temp

and parse_concat toks =
  let x, temp = parse_unary toks in match lookahead x with
    | Some Tok_Concat -> let x1 = match_token x Tok_Concat in
        let x2, temp2 = parse_concat x1 in x2, Binop(Concat, temp, temp2)
    | _ -> x, temp

and parse_mult_div toks =
  let x, temp = parse_concat toks in match lookahead x with
    | Some Tok_Div -> let x1 = match_token x (Tok_Div) in
        let x2, temp2 = parse_mult_div x1 in x2, Binop(Div, temp, temp2)
    | Some Tok_Mult -> let x1 = match_token x Tok_Mult in
        let x2, temp2 = parse_mult_div x1 in x2, Binop(Mult, temp, temp2)
    | _ -> x, temp

and parse_add_sub toks =
  let x, temp = parse_mult_div toks in match lookahead x with
    | Some Tok_Sub -> let x1 = match_token x Tok_Sub in
        let x2, temp2 = parse_add_sub x1 in x2, Binop(Sub, temp, temp2)
    | Some Tok_Add -> let x1 = match_token x Tok_Add in
        let x2, temp2 = parse_add_sub x1 in x2, Binop(Add, temp, temp2)
    | _ -> x, temp

and parse_inequality toks =
  let x, temp = parse_add_sub toks in match lookahead x with
    | Some Tok_Greater -> let x1 = match_token x Tok_Greater in
        let x2, temp2 = parse_inequality x1 in x2, Binop(Greater, temp, temp2)
    | Some Tok_Less -> let x1 = match_token x Tok_Less in
        let x2, temp2 = parse_inequality x1 in x2, Binop(Less, temp, temp2)
    | Some Tok_GreaterEqual -> let x1 = match_token x Tok_GreaterEqual in
        let x2, temp2 = parse_inequality x1 in x2, Binop(GreaterEqual, temp, temp2)
    | Some Tok_LessEqual -> let x1 = match_token x Tok_LessEqual in
        let x2, temp2 = parse_inequality x1 in x2, Binop(LessEqual, temp, temp2)
    | _ -> x, temp

and parse_equal toks =
  let x, temp = parse_inequality toks in match lookahead x with
    | Some Tok_NotEqual -> let x1 = match_token x Tok_NotEqual in
        let x2, temp2 = parse_equal x1 in x2, Binop(NotEqual, temp, temp2)
    | Some Tok_Equal -> let x1 = match_token x Tok_Equal in
        let x2, temp2 = parse_equal x1 in x2, Binop(Equal, temp, temp2)
    | _ -> x, temp

and parse_and toks = 
  let x, temp = parse_equal toks in match lookahead x with
    | Some Tok_And -> let x1 = match_token x Tok_And in
        let x2, temp2 = parse_and x1 in (x2, Binop(And, temp, temp2))
    | _ -> x, temp

and parse_or toks =
  let x, temp = parse_and toks in match lookahead x with
    | Some Tok_Or -> let x1 = match_token x Tok_Or in
        let x2, temp2 = parse_or x1 in x2, Binop(Or, temp, temp2)
    | _ -> x, temp

and parse_expr toks = match lookahead toks with
  | Some Tok_If -> let x, temp = parse_if toks in x, temp
  | Some Tok_Let -> let x, temp = parse_let toks in x, temp
  | Some Tok_Fun -> let x, temp = parse_fun toks in x, temp
  | _ -> let x, temp = parse_or toks in x, temp

let rec parse_defmutop toks = match lookahead toks with
  | Some Tok_Def -> (let x = match_token toks Tok_Def in
    let x1, temp = parse_primary x in
    let x2 = match_token x1 Tok_Equal in
    let x3, temp2 = parse_expr x2 in
    let x4 = match_token x3 Tok_DoubleSemi in
    match temp with
      | ID(str) -> x4, Def(str, temp2)
      |  _ -> raise (InvalidInputException "ERROR!"))
  | _ -> raise (InvalidInputException "ERROR!")

let rec parse_mutop toks = match lookahead toks with
  | Some Tok_DoubleSemi -> let x = match_token toks Tok_DoubleSemi in x, NoOp
  | Some Tok_Def -> let x, temp = parse_defmutop toks in x, temp
  | _ -> let x, temp = parse_expr toks in let x1 = match_token x Tok_DoubleSemi in x1, Expr(temp)