open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions *)

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

(* Part 2: Parsing expressions *)

(* PrimaryExpr -> Tok_Int | Tok_Bool | Tok_String | Tok_ID | ( Expr ) *)
let rec parse_PrimaryExpr toks =
  match lookahead toks with
  | Some (Tok_Int i) ->
      let toks' = match_token toks (Tok_Int i) in
      (toks', Int i)
  | Some (Tok_Bool b) ->
      let toks' = match_token toks (Tok_Bool b) in
      (toks', Bool b)
  | Some (Tok_String s) ->
      let toks' = match_token toks (Tok_String s) in
      (toks', String s)
  | Some (Tok_ID id) ->
      let toks' = match_token toks (Tok_ID id) in
      (toks', ID id)
  | Some Tok_LParen ->
      let toks1 = match_token toks Tok_LParen in
      let (toks2, expr) = parse_expr toks1 in
      let toks3 = match_token toks2 Tok_RParen in
      (toks3, expr)
  | _ -> raise (InvalidInputException "Expected primary expression")

(* FunctionCallExpr -> PrimaryExpr PrimaryExpr | PrimaryExpr *)
and parse_FunctionCallExpr toks =
  let (toks1, primary) = parse_PrimaryExpr toks in
  match lookahead toks1 with
  | Some (Tok_Int _) | Some (Tok_Bool _) | Some (Tok_String _)
  | Some (Tok_ID _) | Some Tok_LParen ->
      let (toks2, arg) = parse_PrimaryExpr toks1 in
      (toks2, FunctionCall(primary, arg))
  | _ -> (toks1, primary)

(* UnaryExpr -> not UnaryExpr | FunctionCallExpr *)
and parse_UnaryExpr toks =
  match lookahead toks with
  | Some Tok_Not ->
      let toks1 = match_token toks Tok_Not in
      let (toks2, expr) = parse_UnaryExpr toks1 in
      (toks2, Not expr)
  | _ -> parse_FunctionCallExpr toks

(* ConcatExpr -> UnaryExpr ^ ConcatExpr | UnaryExpr *)
and parse_ConcatExpr toks =
  let (toks1, unary) = parse_UnaryExpr toks in
  match lookahead toks1 with
  | Some Tok_Concat ->
      let toks2 = match_token toks1 Tok_Concat in
      let (toks3, concat) = parse_ConcatExpr toks2 in
      (toks3, Binop(Concat, unary, concat))
  | _ -> (toks1, unary)

(* MultiplicativeExpr -> ConcatExpr (Mult | Div) MultiplicativeExpr | ConcatExpr *)
and parse_MultiplicativeExpr toks =
  let (toks1, concat) = parse_ConcatExpr toks in
  match lookahead toks1 with
  | Some Tok_Mult ->
      let toks2 = match_token toks1 Tok_Mult in
      let (toks3, mult) = parse_MultiplicativeExpr toks2 in
      (toks3, Binop(Mult, concat, mult))
  | Some Tok_Div ->
      let toks2 = match_token toks1 Tok_Div in
      let (toks3, mult) = parse_MultiplicativeExpr toks2 in
      (toks3, Binop(Div, concat, mult))
  | _ -> (toks1, concat)

(* AdditiveExpr -> MultiplicativeExpr (+ | -) AdditiveExpr | MultiplicativeExpr *)
and parse_AdditiveExpr toks =
  let (toks1, mult) = parse_MultiplicativeExpr toks in
  match lookahead toks1 with
  | Some Tok_Add ->
      let toks2 = match_token toks1 Tok_Add in
      let (toks3, add) = parse_AdditiveExpr toks2 in
      (toks3, Binop(Add, mult, add))
  | Some Tok_Sub ->
      let toks2 = match_token toks1 Tok_Sub in
      let (toks3, add) = parse_AdditiveExpr toks2 in
      (toks3, Binop(Sub, mult, add))
  | _ -> (toks1, mult)

(* RelationalExpr -> AdditiveExpr (< | > | <= | >=) RelationalExpr | AdditiveExpr *)
and parse_RelationalExpr toks =
  let (toks1, add) = parse_AdditiveExpr toks in
  match lookahead toks1 with
  | Some Tok_Less ->
      let toks2 = match_token toks1 Tok_Less in
      let (toks3, rel) = parse_RelationalExpr toks2 in
      (toks3, Binop(Less, add, rel))
  | Some Tok_Greater ->
      let toks2 = match_token toks1 Tok_Greater in
      let (toks3, rel) = parse_RelationalExpr toks2 in
      (toks3, Binop(Greater, add, rel))
  | Some Tok_LessEqual ->
      let toks2 = match_token toks1 Tok_LessEqual in
      let (toks3, rel) = parse_RelationalExpr toks2 in
      (toks3, Binop(LessEqual, add, rel))
  | Some Tok_GreaterEqual ->
      let toks2 = match_token toks1 Tok_GreaterEqual in
      let (toks3, rel) = parse_RelationalExpr toks2 in
      (toks3, Binop(GreaterEqual, add, rel))
  | _ -> (toks1, add)

(* EqualityExpr -> RelationalExpr (= | <>) EqualityExpr | RelationalExpr *)
and parse_EqualityExpr toks =
  let (toks1, rel) = parse_RelationalExpr toks in
  match lookahead toks1 with
  | Some Tok_Equal ->
      let toks2 = match_token toks1 Tok_Equal in
      let (toks3, eq) = parse_EqualityExpr toks2 in
      (toks3, Binop(Equal, rel, eq))
  | Some Tok_NotEqual ->
      let toks2 = match_token toks1 Tok_NotEqual in
      let (toks3, eq) = parse_EqualityExpr toks2 in
      (toks3, Binop(NotEqual, rel, eq))
  | _ -> (toks1, rel)

(* AndExpr -> EqualityExpr && AndExpr | EqualityExpr *)
and parse_AndExpr toks =
  let (toks1, eq) = parse_EqualityExpr toks in
  match lookahead toks1 with
  | Some Tok_And ->
      let toks2 = match_token toks1 Tok_And in
      let (toks3, and_expr) = parse_AndExpr toks2 in
      (toks3, Binop(And, eq, and_expr))
  | _ -> (toks1, eq)

(* OrExpr -> AndExpr || OrExpr | AndExpr *)
and parse_OrExpr toks =
  let (toks1, and_expr) = parse_AndExpr toks in
  match lookahead toks1 with
  | Some Tok_Or ->
      let toks2 = match_token toks1 Tok_Or in
      let (toks3, or_expr) = parse_OrExpr toks2 in
      (toks3, Binop(Or, and_expr, or_expr))
  | _ -> (toks1, and_expr)

(* IfExpr -> if Expr then Expr else Expr *)
and parse_IfExpr toks =
  let toks1 = match_token toks Tok_If in
  let (toks2, cond) = parse_expr toks1 in
  let toks3 = match_token toks2 Tok_Then in
  let (toks4, then_expr) = parse_expr toks3 in
  let toks5 = match_token toks4 Tok_Else in
  let (toks6, else_expr) = parse_expr toks5 in
  (toks6, If(cond, then_expr, else_expr))

(* FunctionExpr -> fun Tok_ID -> Expr *)
and parse_FunctionExpr toks =
  let toks1 = match_token toks Tok_Fun in
  match lookahead toks1 with
  | Some (Tok_ID id) ->
      let toks2 = match_token toks1 (Tok_ID id) in
      let toks3 = match_token toks2 Tok_Arrow in
      let (toks4, body) = parse_expr toks3 in
      (toks4, Fun(id, body))
  | _ -> raise (InvalidInputException "Expected identifier after fun")

(* LetExpr -> let Recursion Tok_ID = Expr in Expr *)
and parse_LetExpr toks =
  let toks1 = match_token toks Tok_Let in
  let (toks2, is_rec) = match lookahead toks1 with
    | Some Tok_Rec -> (match_token toks1 Tok_Rec, true)
    | _ -> (toks1, false)
  in
  match lookahead toks2 with
  | Some (Tok_ID id) ->
      let toks3 = match_token toks2 (Tok_ID id) in
      let toks4 = match_token toks3 Tok_Equal in
      let (toks5, expr1) = parse_expr toks4 in
      let toks6 = match_token toks5 Tok_In in
      let (toks7, expr2) = parse_expr toks6 in
      (toks7, Let(id, is_rec, expr1, expr2))
  | _ -> raise (InvalidInputException "Expected identifier after let")

(* Expr -> LetExpr | IfExpr | FunctionExpr | OrExpr *)
and parse_expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunctionExpr toks
  | _ -> parse_OrExpr toks
