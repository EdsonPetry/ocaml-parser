open TokenTypes
open String

(*type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Equal
  | Tok_NotEqual
  | Tok_Greater
  | Tok_Less
  | Tok_GreaterEqual
  | Tok_LessEqual
  | Tok_Or
  | Tok_And
  | Tok_Not
  | Tok_If
  | Tok_Then
  | Tok_Else
  | Tok_Add
  | Tok_Sub
  | Tok_Mult
  | Tok_Div
  | Tok_Concat
  | Tok_Let
  | Tok_Rec
  | Tok_In
  | Tok_Def
  | Tok_Fun
  | Tok_Arrow
  | Tok_Int of int
  | Tok_Bool of bool
  | Tok_String of string
  | Tok_ID of string
  | Tok_DoubleSemi*)


(* We provide the regular expressions that may be useful to your code *)

let re_rparen = Str.regexp ")";;
let re_lparen = Str.regexp "(";;
let re_equal = Str.regexp "=";;
let re_not_equal = Str.regexp "<>";;
let re_greater = Str.regexp ">";;
let re_less = Str.regexp "<";;
let re_greater_equal = Str.regexp ">=";;
let re_less_equal = Str.regexp "<=";;
let re_or = Str.regexp "||";;
let re_and = Str.regexp "&&";;
let re_not = Str.regexp "not";;
let re_if = Str.regexp "if";;
let re_then = Str.regexp "then";;
let re_else = Str.regexp "else";;
let re_add = Str.regexp "+";;
let re_sub = Str.regexp "-";;
let re_mult = Str.regexp "*";;
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^";;
let re_let = Str.regexp "let";;
let re_rec = Str.regexp "rec";;
let re_in = Str.regexp "in";;
let re_def = Str.regexp "def";;
let re_fun = Str.regexp "fun";;
let re_arrow = Str.regexp "->";;
let re_pos_int = Str.regexp "[0-9]+";;
let re_neg_int = Str.regexp "(-[0-9]+)";;
let re_true = Str.regexp "true";;
let re_false = Str.regexp "false";;
let re_string = Str.regexp "\"[^\"]*\"";;
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*";;
let re_double_semi = Str.regexp ";;";;
let re_whitespace = Str.regexp "[ \t\n]+";;

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
  let rec tok pos s =
    if pos >= String.length s then
      []
    else
      if Str.string_match re_whitespace s pos then
        tok (Str.match_end ()) s

      else if Str.string_match re_double_semi s pos then
        Tok_DoubleSemi :: tok (Str.match_end ()) s
      else if Str.string_match re_greater_equal s pos then
        Tok_GreaterEqual :: tok (Str.match_end ()) s
      else if Str.string_match re_less_equal s pos then
        Tok_LessEqual :: tok (Str.match_end ()) s
      else if Str.string_match re_not_equal s pos then
        Tok_NotEqual :: tok (Str.match_end ()) s
      else if Str.string_match re_or s pos then
        Tok_Or :: tok (Str.match_end ()) s
      else if Str.string_match re_and s pos then
        Tok_And :: tok (Str.match_end ()) s
      else if Str.string_match re_arrow s pos then
        Tok_Arrow :: tok (Str.match_end ()) s

      else if Str.string_match re_neg_int s pos then
        let matched = Str.matched_string s in
        let num_str = String.sub matched 1 (String.length matched - 2) in
        let num = int_of_string num_str in
        Tok_Int num :: tok (Str.match_end ()) s

      else if Str.string_match re_pos_int s pos then
        let matched = Str.matched_string s in
        let num = int_of_string matched in
        Tok_Int num :: tok (Str.match_end ()) s

      else if Str.string_match re_string s pos then
        let matched = Str.matched_string s in
        let str_content = String.sub matched 1 (String.length matched - 2) in
        Tok_String str_content :: tok (Str.match_end ()) s

      else if Str.string_match re_id s pos then
        let matched = Str.matched_string s in
        let token = match matched with
          | "let" -> Tok_Let
          | "rec" -> Tok_Rec
          | "in" -> Tok_In
          | "fun" -> Tok_Fun
          | "if" -> Tok_If
          | "then" -> Tok_Then
          | "else" -> Tok_Else
          | "not" -> Tok_Not
          | "true" -> Tok_Bool true
          | "false" -> Tok_Bool false
          | "def" -> Tok_Def
          | id -> Tok_ID id  
        in
        token :: tok (Str.match_end ()) s

      else if Str.string_match re_lparen s pos then
        Tok_LParen :: tok (Str.match_end ()) s
      else if Str.string_match re_rparen s pos then
        Tok_RParen :: tok (Str.match_end ()) s
      else if Str.string_match re_equal s pos then
        Tok_Equal :: tok (Str.match_end ()) s
      else if Str.string_match re_greater s pos then
        Tok_Greater :: tok (Str.match_end ()) s
      else if Str.string_match re_less s pos then
        Tok_Less :: tok (Str.match_end ()) s
      else if Str.string_match re_add s pos then
        Tok_Add :: tok (Str.match_end ()) s
      else if Str.string_match re_sub s pos then
        Tok_Sub :: tok (Str.match_end ()) s
      else if Str.string_match re_mult s pos then
        Tok_Mult :: tok (Str.match_end ()) s
      else if Str.string_match re_div s pos then
        Tok_Div :: tok (Str.match_end ()) s
      else if Str.string_match re_concat s pos then
        Tok_Concat :: tok (Str.match_end ()) s

      else
        raise (InvalidInputException ("Unexpected character at position " ^ string_of_int pos))
  in
  tok 0 input
