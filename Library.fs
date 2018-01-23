module internal FSharp.SExpression
open System
#if NUMERICS
open System.Numerics
#else
type BigInteger = Int64
#endif
// https://github.com/AshleyF/FScheme/blob/master/FScheme.fs

type Token =
    | Open | Close
    | Quote | Unquote
    | Number of string
    | String of string
    | Symbol of string

let private tokenize source =
    let rec string acc = function
        | '\\' :: '"' :: t -> string (acc + "\"") t // escaped quote becomes quote
        | '\\' :: 'b' :: t -> string (acc + "\b") t // escaped backspace
        | '\\' :: 'f' :: t -> string (acc + "\f") t // escaped formfeed
        | '\\' :: 'n' :: t -> string (acc + "\n") t // escaped newline
        | '\\' :: 'r' :: t -> string (acc + "\r") t // escaped return
        | '\\' :: 't' :: t -> string (acc + "\t") t // escaped tab
        | '\\' :: '\\' :: t -> string (acc + "\\") t // escaped backslash
        | '"' :: t -> acc, t // closing quote terminates
        | c :: t -> string (acc + (c.ToString())) t // otherwise accumulate chars
        | _ -> failwith "Malformed string."
    let rec comment = function
        | '\r' :: t | '\n' :: t -> t // terminated by line end
        | [] -> [] // or by EOF
        | _ :: t -> comment t
    let rec token acc = function
        | (')' :: _) as t -> acc, t // closing paren terminates
        | w :: t when Char.IsWhiteSpace(w) -> acc, t // whitespace terminates
        | [] -> acc, [] // end of list terminates
        | c :: t -> token (acc + (c.ToString())) t // otherwise accumulate chars
    let rec tokenize' acc = function
        | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t // skip whitespace
        | '(' :: t -> tokenize' (Open :: acc) t
        | ')' :: t -> tokenize' (Close :: acc) t
        | '\'' :: t -> tokenize' (Quote :: acc) t
        | ',' :: t -> tokenize' (Unquote :: acc) t
        | ';' :: t -> comment t |> tokenize' acc // skip over comments
        | '"' :: t -> // start of string
            let s, t' = string "" t
            tokenize' (Token.String(s) :: acc) t'
        | '-' :: d :: t when Char.IsDigit(d) -> // start of negative number
            let n, t' = token ("-" + d.ToString()) t
            tokenize' (Token.Number(n) :: acc) t'
        | '+' :: d :: t | d :: t when Char.IsDigit(d) -> // start of positive number
            let n, t' = token (d.ToString()) t
            tokenize' (Token.Number(n) :: acc) t'
        | s :: t -> // otherwise start of symbol
            let s, t' = token (s.ToString()) t
            tokenize' (Token.Symbol(s) :: acc) t'
        | [] -> List.rev acc // end of list terminates
    tokenize' [] source

type Expression =
    | Number of BigInteger
    | String of string
    | Symbol of string
    | List of Expression list

let parse (source:string) =
    let map = function
        | Token.Number(n) -> Expression.Number(BigInteger.Parse(n))
        | Token.String(s) -> Expression.String(s)
        | Token.Symbol(s) -> Expression.Symbol(s)
        | _ -> failwith "Syntax error."
    let rec list f t acc =
        let e, t' = parse' [] t
        parse' (List(f e) :: acc) t'
    and parse' acc = function
        | Open :: t -> list id t acc
        | Close :: t -> (List.rev acc), t
        | Quote :: Open :: t -> list (fun e -> [Symbol("quote"); List(e)]) t acc
        | Quote :: h :: t -> parse' (List([Symbol("quote"); map h]) :: acc) t
        | Unquote :: Open :: t -> list (fun e -> [Symbol("unquote"); List(e)]) t acc
        | Unquote :: h :: t -> parse' (List([Symbol("unquote"); map h]) :: acc) t
        | h :: t -> parse' ((map h) :: acc) t
        | [] -> (List.rev acc), []
    let result, _ = parse' [] (tokenize ( source.ToCharArray() |> List.ofArray ))
    result


let rec print = function
    | List(list) -> "(" + String.Join(" ", (List.map print list)) + ")"
    | String(s) -> sprintf "\"%s\"" s
    | Symbol(s) -> s
    | Number(n) -> n.ToString()

let printList parsed=
    parsed |> List.map print |> String.concat "\n"

/// Signature for serializer module 
type Serializer<'T> = 
    abstract member serialize: 'T -> Expression
    abstract member deSerialize: Expression -> 'T option
