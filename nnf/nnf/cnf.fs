module NNF.CNF

open NNF.NNFExpr

// CNF classification implemented directly with recursion.
let classifyCnfRecursive =
    let rec (|Literal|_|) =
        function
        | Not Literal -> Some Literal
        | Comparison _ -> Some Literal
        | _ -> None

    let rec (|Disjunction|_|) =
        function
        | Or (Disjunction, Disjunction) -> Some Disjunction
        | Literal -> Some Disjunction
        | _ -> None

    let rec (|Conjunction|_|) =
        function
        | And (Conjunction, Conjunction) -> Some Conjunction
        | Disjunction -> Some Conjunction
        | _ -> None

    function
    | Literal -> Literal
    | Disjunction -> Disjunction
    | Conjunction -> Conjunction
    | _ -> Unknown

// CNF classification implemented in terms of xfold.
// NOTE: This is not strictly iterative, but in principle it could be if xfold were internally implemented iteratively.
let classifyCnfIterative expr =
    let orf =
        function
        | (Literal | Disjunction), (Literal | Disjunction) -> Disjunction
        | _ -> Unknown
        
    let andf =
        function
        | (Literal | Disjunction | Conjunction), (Literal | Disjunction | Conjunction) -> Conjunction
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
