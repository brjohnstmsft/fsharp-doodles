module NNF.NNF

open NNF.NNFExpr

let rec nnf expr =
    match expr with
    | Not (Not x) -> nnf x
    | Not (Or (left, right)) -> And (nnf (Not left), nnf (Not right))
    | Not (And (left, right)) -> Or (nnf (Not left), nnf (Not right))
    | Not (Comparison (left, op, right)) when op <> Equal -> Comparison (left, op.Negative, right)
    | Not x -> Not (nnf x)
    | All x -> All (nnf x)
    | Any x -> Any (nnf x)
    | And (left, right) -> And (nnf left, nnf right)
    | Or (left, right) -> Or (nnf left, nnf right)
    | _ -> expr

let nnfOnePass expr =
    let notCount expr count =
        match expr with
        | Any _ | All _ -> 0    // Start over in lambda bodies
        | Not _ -> count + 1
        | _ -> count

    let odd x = x % 2 <> 0

    let rewrite notCount expr =
        if odd notCount then
            match expr with
            | And (Not left, Not right) -> Not (Or (left, right))
            | Or (Not left, Not right) -> Not (And (left, right))
            | Comparison (left, op, right) when op <> Equal -> Not (Comparison (left, op.Negative, right))
            | Equals (left, right) -> Not (Not (Equals left right))
            | Any expr -> Not (Not (Any expr))
            | _ -> expr
        else
            match expr with
            | Not (Not x) -> x
            | _ -> expr

    replaceAcc rewrite notCount 0 expr

let nnfIterative expr =
    let nnfOneRound expr =
        let rewrite expr =
            match expr with
            | Not (Not x) -> x
            | Not (Or (left, right)) -> And (Not left, Not right)
            | Not (And (left, right)) -> Or (Not left, Not right)
            | Not (Comparison (left, op, right)) when op <> Equal -> Comparison (left, op.Negative, right)
            | _ -> expr

        replace rewrite expr

    let rec loop e =
        let nextExpr = nnfOneRound e
        if nextExpr = e then e
        else loop nextExpr

    loop expr
