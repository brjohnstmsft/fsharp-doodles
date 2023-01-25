module NNF.DNF

open NNF.NNFExpr

// DNF classification implemented directly with recursion.
let classifyDnfRecursive =
    let rec (|Literal|_|) =
        function
        | Not Literal -> Some Literal
        | Comparison _ -> Some Literal
        | _ -> None

    let rec (|Conjunction|_|) =
        function
        | And (Conjunction, Conjunction) -> Some Conjunction
        | Literal -> Some Conjunction
        | _ -> None

    let rec (|Disjunction|_|) =
        function
        | Or (Disjunction, Disjunction) -> Some Disjunction
        | Conjunction -> Some Disjunction
        | _ -> None

    function
    | Literal -> Literal
    | Conjunction -> Conjunction
    | Disjunction -> Disjunction
    | _ -> Unknown

// DNF classification implemented in terms of xfold.
// NOTE: This is not strictly iterative, but in principle it could be if xfold were internally implemented iteratively.
let classifyDnfIterative expr =
    let andf (lhs, rhs) =
        match lhs with
        | Literal | Conjunction ->
            match rhs with
            | Literal | Conjunction -> Conjunction
            | _ -> Unknown
        | _ -> Unknown
        
    let orf (lhs, rhs) =
        match lhs with
        | Literal | Conjunction | Disjunction ->
            match rhs with
            | Literal | Conjunction | Disjunction -> Disjunction
            | _ -> Unknown
        | _ -> Unknown
        
    let notf _ = function Not (Comparison _) -> Literal | _ -> Unknown
        
    let value v _ _ = v

    let anyf = value Unknown
    let allf = value Unknown
    let compf = value Literal

    let ignoreExpr f t _ = f t

    xfold (ignoreExpr andf) (ignoreExpr orf) notf anyf allf compf expr
