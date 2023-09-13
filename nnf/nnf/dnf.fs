module NNF.DNF

open NNF.NNFExpr

// DNF classification implemented directly with recursion.
// Reference: https://en.wikipedia.org/wiki/Disjunctive_normal_form
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
    let andf =
        function
        | (Literal | Conjunction), (Literal | Conjunction) -> Conjunction
        | _ -> Unknown
        
    let orf =
        function
        | (Literal | Conjunction | Disjunction), (Literal | Conjunction | Disjunction) -> Disjunction
        | _ -> Unknown
        
    let notf _ =
        function
        | Not (Comparison _) -> Literal
        | _ -> Unknown
        
    let value v _ _ = v

    let anyf = value Unknown
    let allf = value Unknown
    let compf = value Literal

    let ignoreExpr f t _ = f t

    xfold (ignoreExpr andf) (ignoreExpr orf) notf anyf allf compf expr
