(*  1. Implement an F# function {sumOption : int option list -> int option} which returns 
  -None if there is a None anywhere in the list;
  -otherwise returns Some s, where s is the sum of the integers in the list  *)

      //Transforms a world-crossing function into a world-crossing function that works with collections?
      let rec sumOption (xs : int option list) : int option = match xs with
          | [] -> Some 0
          | x::xr -> match x with
                     | None -> None
                     | Some v -> if (sumOption xr) = None then None else Some(v + Option.get(sumOption xr))


(* (1.2)  There is an incomplete implementation of a simple language with integers, booleans, and let-bindings (where variables can only contain integers). 
This language also has some limited support for exceptions.
 -The expression fail means raise an exception. There is only one kind of exception, and there are no exception messages. (In the abstract syntax, this is Fail.)
 -try e1 with e2 evaluates to e1 if e1 does not raise an exception, and otherwise evaluates to e2. (Abstract syntax: Try (e1, e2).)
 -choose { e1 | e2 | ... | en } evaluates to the first expression in the list e1, . . .  en, that does not raise an exception. It does this by evaluating the expressions
  in sequence until it finds the first one that does not raise an exception. If all of the expressions raise an exception, then so does choose { e1 | e2 | ... | en }. 
  (Abstract syntax: Choose [e1; e2; ...; en].)
For example:
 -the expression try (if 1 < (2 + (3 + fail)) then 10 else 11) with 0 evaluates to 0 (the condition of the if raises an exception because of the fail, and this is handled by the try);
 -the expression choose { ((fail < 20) || true) | (30 < fail) } raises an exception, because the two options of the choose both raise exceptions;
 -the expression choose { (true || (fail < 20)) | (30 < fail) } evaluates to true (|| is shortcircuiting, so (fail < 20) and (30 < fail) are not evaluated);
 -the expression in 2(i) below evaluates to 0 if x is negative (it raises an exception that is handled by the try), and evaluates to x otherwise;
 -the expression in 2(ii) below evaluates to lower if x < lower, to upper if upper < x, and to x otherwise (if a case does not apply it raises an exception, causing choose to try the next case).
You can use the function prettyprint : expr -> string to convert abstract syntax to concrete syntax. Most of the rest of the implementation is incomplete, you have to fix it in the problems below. *)

      type expr =
          | Var of string                 // variable
          | Let of string * expr * expr   // let x = e1 in e2
          | Num of int                    // constant integer
          | Plus of expr * expr           // e1 + e2
          | Times of expr * expr          // e1 * e2
          | True                          // true
          | False                         // false
          | LessThan of expr * expr       // e1 < e2
          | Or of expr * expr             // e1 || e2
          | And of expr * expr            // e1 && e2
          | If of expr * expr * expr      // if e1 then e2 else e2
          | Fail                          // fail    (raise exception)
          | Try of expr * expr            // try e1 with e2
          | Choose of expr list           // choose { e1 | e2 | ... | en }

      // An example:
      // let left = e1 in let right = e2 in let sum = left + right in
      //    if (0 < left && sum < right) || (left < 0 && right < sum) then fail else sum
      
      let checkedPlus (e1 : expr) (e2 : expr) : expr =
          Let ("left", e1, Let ("right", e2, Let ("sum", Plus (Var "left", Var "right"),
              If (Or (And (LessThan (Num 0, Var "left"), LessThan (Var "sum", Var "right")),
                      And (LessThan (Var "left", Num 0), LessThan (Var "right", Var "sum"))),
                  Fail, Var "sum"))))

      let prettyprint (e : expr) : string =
          let paren b s = if b then  "(" + s + ")" else s
          let rec prettyprintExpr (e : expr) (acc : int) : string =
              match e with
              | Var x -> x
              | Let (x, erhs, ebody) ->
                   paren (3 <= acc) ("let " + x + " = " + prettyprintExpr erhs 1 + " in " + prettyprintExpr ebody 1)
              | Num i -> string i
              | Plus (e1, e2) ->
                   paren (7 <= acc) (prettyprintExpr e1 6 + " + " + prettyprintExpr e2 7)
              | Times (e1, e2) ->
                   paren (8 <= acc) (prettyprintExpr e1 7 + " * " + prettyprintExpr e2 8)
              | True -> "true"
              | False -> "false"
              | LessThan (e1, e2) ->
                   paren (6 <= acc) (prettyprintExpr e1 5 + " < " + prettyprintExpr e2 6)
              | Or (e1, e2) ->
                   paren (4 <= acc) (prettyprintExpr e1 3 + " || " + prettyprintExpr e2 4)
              | And (e1, e2) ->
                   paren (5 <= acc) (prettyprintExpr e1 4 + " && " + prettyprintExpr e2 5)
              | If (e1, e2, e3) ->
                  paren (2 <= acc) ("if " + prettyprintExpr e1 3 + " then " + prettyprintExpr e2 2 + " else " + prettyprintExpr e3 1)
              | Fail -> "fail"
              | Try (e1, e2) ->
                  paren (3 <= acc) ("try " + prettyprintExpr e1 2 + " with " + prettyprintExpr e2 1)
              | Choose [] ->
                  paren (9 <= acc) ("choose { }")
              | Choose es ->
                  paren (9 <= acc) ("choose { " + prettyprintNonEmptyOptions es + " }")
          and prettyprintNonEmptyOptions (es : expr list) =
              match es with
              | [] -> failwith "expected es to be non-empty"
              | [e] -> prettyprintExpr e 0
              | e::es -> prettyprintExpr e 0 + " | " + prettyprintNonEmptyOptions es
          prettyprintExpr e 0

      type token =
          | NAME of string
          | LET | EQUAL | IN
          | INT of int
          | PLUS | TIMES
          | TRUE | FALSE
          | LT | OR | AND
          | IF | THEN | ELSE
          | FAIL | TRY | WITH | CHOOSE
          | LBRACE | RBRACE | BAR
          | LPAR | RPAR
          | ERROR of char

      let string2Chars (s : string) : char list =
          let rec helper cs i =
              if i = 0 then cs else let i = i - 1 in helper (s.[i] :: cs) i
          helper [] (String.length s)
      let isDigit c = '0' <= c && c <= '9'
      let digit2Int (c : char) = int c - int '0'
      let isLowercaseLetter c = 'a' <= c && c <= 'z'
      let isUppercaseLetter c = 'A' <= c && c <= 'Z'
      let isLetter c = isLowercaseLetter c || isUppercaseLetter c
      
      type value =
          | VNum of int    // integer
          | VBool of bool  // boolean
          | VFail          // exception
      type envir = (string * int) list
      
      let rec lookup x env =
          match env with
          | []          -> failwith (x + " not found")
          | (y, v)::env -> if x = y then v else lookup x env

      type typ =
        | I   // integer
        | B   // boolean
        | IB  // both integer and boolean


(*  2. Convert the following into abstract syntax, by defining F# values of type expr. In these expressions x, lower and upper are names of variables.
 -try (if x < 0 then fail else x) with 0. Call the F# value absTry, by changing the line let absTry : expr = Fail.
 -choose { if x < lower then lower else fail
          | if upper < x then upper else fail
          | x }
  Call the F# value clamp, by changing the line let clamp : expr = Fail *)

      // try (if x < 0 then fail else x) with 0
      let absTry : expr = Try (If (LessThan (Var ("x"), Num 0), Fail, Var ("x")), Num 0);;

      // choose { if x < lower then lower else fail | if upper < x then upper else fail | x }
      let clamp : expr = Choose ([If (LessThan (Var ("x"), Var ("lower")), Var ("lower"), Fail); If (LessThan (Var ("upper"), Var ("x")), Var ("upper"), Fail); Var ("x")])

(*  3. The expression Plus (e1, e2) raises an exception if either of e1 or e2 raises an exception. Implement two F# functions
  -defaultPlusTry : expr -> expr -> expr
  -defaultPlusChoose : expr -> expr -> expr
each of which constructs an expression that adds the two arguments, but never raises an exception. If one of the arguments raises an exception,
that argument should be treated as 0 in the sum of them.
  -defaultPlusTry should use Try but not Choose.
  -defaultPlusChoose should use Choose but not Try.
Your functions should not inspect their arguments, they should just wrap them in some more constructors from the expr type (like checkedPlus does)  *)

      let defaultPlusTry (e1 : expr) (e2 : expr) : expr = Plus (Try (e1, Num 0), Try (e2, Num 0));;
      let defaultPlusChoose (e1 : expr) (e2 : expr) : expr = Plus (Choose ([e1; Num 0]), Choose ([e2; Num 0]));;

(*  4. The implementation of the lexer lex : string -> token list fails to recognize the tokens BAR and TRY (which are | and try in the concrete syntax).
Fix the lexer to that they are recognized. Do not change lex directly, instead change the functions that appear above it. Be careful to ensure that || is still recognized as OR *) 

      let word2Token (s : string) : token =
          match s with
          | "let" -> LET
          | "in"  -> IN
          | "true" -> TRUE
          | "false" -> FALSE
          | "if" -> IF
          | "then" -> THEN
          | "else" -> ELSE
          | "fail" -> FAIL
          | "with" -> WITH
          | "try" -> TRY
          | "choose" -> CHOOSE
          | _     -> NAME s
      
      let rec tokenize (cs : char list) : token list =
          match cs with
          | [] -> []
          | '='::cs -> EQUAL :: tokenize cs
          | '+'::cs -> PLUS :: tokenize cs
          | '*'::cs -> TIMES :: tokenize cs
          | '<'::cs -> LT :: tokenize cs
          | '|'::'|'::cs -> OR :: tokenize cs
          | '|'::cs -> BAR :: tokenize cs
          | '&'::'&'::cs -> AND :: tokenize cs
          | ' '::cs -> tokenize cs
          | '\t'::cs -> tokenize cs
          | '\n'::cs -> tokenize cs
          | '{'::cs -> LBRACE :: tokenize cs
          | '}'::cs -> RBRACE :: tokenize cs
          | '('::cs -> LPAR :: tokenize cs
          | ')'::cs -> RPAR :: tokenize cs
          | '-'::c::cs when isDigit c -> tokenizeInt cs (- digit2Int c)
          | c::cs when isDigit c -> tokenizeInt cs (digit2Int c)
          | c::cs when isLowercaseLetter c -> tokenizeWord cs (string c)
          | c::cs -> ERROR c :: tokenize cs
      and tokenizeInt cs (acc : int) =
          match cs with
          | c::cs when isDigit c -> tokenizeInt cs (acc * 10 + digit2Int c)
          | _ -> INT acc :: tokenize cs
      and tokenizeWord cs (acc : string) =
          match cs with
          | c::cs when isLetter c || isDigit c -> tokenizeWord cs (acc + string c)
          | _ -> word2Token acc :: tokenize cs
      
      let lex (s : string) : token list =
          tokenize (string2Chars s)


(*  5.  The implementation of the parser parse : token list -> expr is incomplete for choose expressions. Fix it by implementing the appropriate case of parseFactor.
The parser should check that list of options begins with LBRACE and ends with RBRACE. The function parseNonEmptyOptions will be useful. *)

      let rec parseExpr (ts : token list) : expr * token list =
        match ts with
        | IF :: ts ->
            let econd, ts = parseDisjunction ts
            match ts with
            | THEN :: ts ->
                let etrue, ts = parseDisjunction ts
                match ts with
                | ELSE :: ts ->
                    let efalse, ts = parseExpr ts
                    If (econd, etrue, efalse), ts
                | _ -> failwith "if without else"
            | _ -> failwith "if without then"
        | _ -> parseDisjunction ts
      and parseDisjunction (ts : token list) : expr * token list =
        let e1, ts = parseConjunction ts
        match ts with
        | OR :: ts ->
            let e2, ts = parseDisjunction ts
            Or (e1, e2), ts
        | _ -> e1, ts
      and parseConjunction (ts : token list) : expr * token list =
        let e1, ts = parseComparison ts
        match ts with
        | AND :: ts ->
            let e2, ts = parseConjunction ts
            And (e1, e2), ts
        | _ -> e1, ts
      and parseComparison (ts : token list) : expr * token list =
        let e1, ts = parseSum ts
        match ts with
        | LT :: ts ->
            let e2, ts = parseComparison ts
            LessThan (e1, e2), ts
        | _ -> e1, ts
      and parseSum (ts : token list) : expr * token list =
        let e1, ts = parseSummand ts
        match ts with
        | PLUS :: ts ->
            let e2, ts = parseSum ts
            Plus (e1, e2), ts
        | _ -> e1, ts
      and parseSummand (ts : token list) : expr * token list =
        let e1, ts = parseFactor ts
        match ts with
        | TIMES :: ts ->
            let e2, ts = parseSummand ts
            Times (e1, e2), ts
        | _ -> e1, ts
      and parseFactor (ts : token list) : expr * token list =
        match ts with
        | NAME x :: ts -> (Var x, ts)
        | LET :: ts ->
            match ts with
            | NAME x :: EQUAL :: ts ->
                let (erhs, ts) = parseExpr ts
                match ts with
                | IN :: ts ->
                    let ebody, ts = parseExpr ts
                    Let (x, erhs, ebody), ts
                | _ -> failwith "let without in"
            | NAME x :: _ -> failwith "let without equals sign"
            | _ -> failwith "let without variable name"
        | INT i :: ts -> Num i, ts
        | FAIL :: ts -> Fail, ts
        | TRUE :: ts -> True, ts
        | FALSE :: ts -> False, ts
        | TRY :: ts ->
            let etry, ts = parseExpr ts
            match ts with
            | WITH :: ts ->
                let ecatch, ts = parseExpr ts
                Try (etry, ecatch), ts
            | _ -> failwith "try without with"
        | CHOOSE :: ts ->
            match ts with
            | LBRACE :: RBRACE :: ts -> Choose [], ts
            | LBRACE :: ts -> 
                let (es, ts) = parseNonEmptyOptions ts
                match ts with
                    | RBRACE :: ts -> Choose es, ts
                    | _ -> failwith "left paren without right paren"
            | _ -> failwith "Not implemented"
        | LPAR :: ts ->
            let e, ts = parseExpr ts
            match ts with
            | RPAR :: ts -> e, ts
            | _ -> failwith "left paren without right paren"
        | t::_ -> failwithf "%A cannot start a factor" t
        | _  -> failwith "unexpected end of expression"
      and parseNonEmptyOptions (ts : token list) : expr list * token list =
        let e, ts = parseExpr ts
        match ts with
        | BAR :: ts ->
            let es, ts = parseNonEmptyOptions ts
            (e :: es, ts)
        | _ -> ([e], ts)
    
      let parse (ts : token list) : expr =
        let e, ts = parseExpr ts
        if ts = [] then e else failwithf "unconsumed tokens %A" ts
      let lexParse (s : string) : expr = parse (lex s)


(* 6. The implementation of the evaluator eval : expr -> envir -> value is also incomplete for choose expressions. Fix it by implementing the appropriate case of eval.
Expressions that raise an exception evaluate to VFail. *)

      let rec eval (e : expr) (env : envir) : value =
          match e with
          | Var x -> VNum (lookup x env)
          | Let (x, erhs, ebody) ->
              let xval = eval erhs env
              match xval with
              | VNum i ->
                  let env1 = (x, i) :: env
                  eval ebody env1
              | _ -> failwith "variables should be integers"
          | Num i -> VNum i
          | Plus (e1, e2) ->
              evalIntegerOp (fun i1 i2 -> VNum (i1 + i2)) e1 e2 env
          | Times (e1, e2) ->
              evalIntegerOp (fun i1 i2 -> VNum (i1 * i2)) e1 e2 env
          | True -> VBool true
          | False -> VBool false
          | LessThan (e1, e2) ->
              evalIntegerOp (fun i1 i2 -> VBool (i1 < i2)) e1 e2 env
          | Or (e1, e2) ->
              match eval e1 env with
              | VNum _ -> failwith "cannot apply a boolean operation to an integer"
              | VBool true -> VBool true  // short circuit
              | VBool false ->
                  match eval e2 env with
                  | VNum _ -> failwith "cannot apply a boolean operation to an integer"
                  | VBool b -> VBool b
                  | VFail -> VFail
              | VFail -> VFail
          | And (e1, e2) ->
              match eval e1 env with
              | VNum _ -> failwith "cannot apply a boolean operation to an integer"
              | VBool false -> VBool false  // short circuit
              | VBool true ->
                  match eval e2 env with
                  | VNum _ -> failwith "cannot apply a boolean operation to an integer"
                  | VBool b -> VBool b
                  | VFail -> VFail
              | VFail -> VFail
          | If (econd, etrue, efalse) ->
              match eval econd env with
              | VNum _ -> failwith "cannot use an integer as a condition"
              | VBool cond -> eval (if cond then etrue else efalse) env
              | VFail -> VFail
          | Fail -> VFail
          | Try (etry, ecatch) ->
              match eval etry env with
              | VFail -> eval ecatch env
              | v -> v
          | Choose [] -> VFail
          | Choose (e :: es) ->
                  match eval e env,es with
                  | VFail,[] -> VFail
                  | VFail, es -> eval(Choose(es)) env
                  | v, es -> v
          // | Choose (e :: es) -> // expression: expression list
          //     match (eval e env), es with
          //     | VFail, [] -> VFail 
          //     | v, es-> v //it's always the first value that evaluates. Disregard the rest of the list
      and evalIntegerOp (op : int -> int -> value) (e1 : expr) (e2 : expr) (env : envir) : value =
          match eval e1 env with
          | VNum i1 ->
              match eval e2 env with
              | VNum i2 -> op i1 i2
              | VBool _ -> failwith "cannot apply an integer operation to a boolean"
              | VFail -> VFail
          | VBool _ -> failwith "cannot apply an integer operation to a boolean"
          | VFail -> VFail
      
      let e =
          Plus (Var "y",
              Choose
                [ If (LessThan (Var "x", Num 0), Fail, Var "x")
                ; If (LessThan (Plus (Var "x", Var "y"), Num 0), Fail, Plus (Var "x", Var "y"))
                ; Choose [ Var "y" ]])



(* 7. This language has three types: I for integers, B for booleans, and IB for expressions (such as fail) that can be used both as integers and booleans.
The function infer : expr -> typ is meant to infer the type of an expression.
 (7.1) Implement the function getCommonType : typ -> typ -> typ. This function is used to infer the type of an If. It is meant to check that the branches have
 compatible types, and to find the common type of the two branches if they are compatible. It should raise an F# exception (use failwith) if the two types are incompatible.
 You need to be careful with IB: the expression if 0 < x then 0 else fail should have type I, and the expression if 0 < x then fail else fail should have type IB  *)

      let getCommonType (t1 : typ) (t2 : typ) : typ =
          match t1, t2 with
          | I, I | I, IB | IB, I-> I
          | I, B -> failwith "branches of different types I and B"
          | B, I -> failwith "branches of different types B and I"
          | B, B | B, IB | IB, B -> B
          | IB, IB -> IB
          
(* (7.2) Implement the Try case of the given infer function. This should check that the two subexpressions have compatible types using getCommonType.   *)
      
      let rec infer (e : expr) : typ =
          match e with
          | Var x -> I
          | Let (x, erhs, ebody) ->
              match infer erhs with
              | I | IB -> infer ebody
              | B -> failwith "variables must be integers"
          | Num i -> I
          | Plus (e1, e2) ->
              match infer e1, infer e2 with
              | I, I | I, IB | IB, I | IB, IB -> I
              | _, _ -> failwith "wrong operand type"
          | Times (e1, e2) ->
              match infer e1, infer e2 with
              | I, I | I, IB | IB, I | IB, IB -> I
              | _, _ -> failwith "wrong operand type"
          | True -> B
          | False -> B
          | LessThan (e1, e2) ->
              match infer e1, infer e2 with
              | I, I | I, IB | IB, I | IB, IB -> B
              | _, _ -> failwith "wrong operand type"
          | Or (e1, e2) ->
              match infer e1, infer e2 with
              | B, B | B, IB | IB, B | IB, IB -> B
              | _, _ -> failwith "wrong operand type"
          | And (e1, e2) ->
              match infer e1, infer e2 with
              | B, B | B, IB | IB, B | IB, IB -> B
              | _, _ -> failwith "wrong operand type"
          | If (econd, etrue, efalse) ->
              match infer econd with
              | B | IB -> getCommonType (infer etrue) (infer efalse)
              | I -> failwith "if condition of type integer"
          | Fail -> IB
          | Try (etry, ecatch) ->
              match getCommonType (infer etry) (infer ecatch) with
              | I -> I
              | B -> B
              | IB -> IB
          | Choose [] -> IB
          | Choose (e :: es) -> getCommonType (infer e) (infer (Choose es))

(* 8. Implement the function desugarChoose : expr -> expr, which should convert the given expression to an equivalent expression that does not use Choose.
You will need to use Try and Fail. Be careful to ensure that you desugar all subexpressions *)

      let rec desugarChoose (e : expr) : expr =
          match e with
          | Choose([]) -> Fail
          | Choose(e::es) -> Try(e, desugarChoose(Choose(es)))
          | ex -> ex
      
      let e2 =
          desugarChoose (Plus (Var "y",
              Choose
                [ If (LessThan (Var "x", Num 0), Fail, Var "x")
                ; If (LessThan (Plus (Var "x", Var "y"), Num 0), Fail, Plus (Var "x", Var "y"))
                ; Choose [ Var "y" ]]))
      
      
