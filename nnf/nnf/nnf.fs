module NNF.NNF

open NNF.NNFExpr

// Classical one-pass recursive implementation of NNF based on its Boolean algebra definition
let rec nnf expr =
    match expr with
    | Not (Not x) -> nnf x                                                                          // Double-NOT elimination
    | Not (Or (left, right)) -> And (nnf (Not left), nnf (Not right))                               // DeMorgan's Law
    | Not (And (left, right)) -> Or (nnf (Not left), nnf (Not right))                               // DeMorgan's Law
    | Not (Comparison (left, op, right)) when op <> Equal -> Comparison (left, op.Negative, right)  // Flip operator to consume NOT
    | Not x -> Not (nnf x)
    | All x -> All (nnf x)
    | Any x -> Any (nnf x)
    | And (left, right) -> And (nnf left, nnf right)
    | Or (left, right) -> Or (nnf left, nnf right)
    | _ -> expr

// Classical implementation of NNF based on its recursive definition, but implemented (indirectly) in terms of xfoldAcc.
// NOTE: This is not strictly iterative, but in principle it could be if xfoldAcc were internally implemented iteratively.
// Requires multiple passes over the AST. Each pass moves NOTs one level down towards the leaves, where they will be eliminated
// by flipping comparison operators (or pairing them with Equals).
let nnfIterative expr =
    let nnfOneRound expr =
        let rewrite expr =
            match expr with
            | Not (Not x) -> x                                                                              // Double-NOT eliminiation
            | Not (Or (left, right)) -> And (Not left, Not right)                                           // DeMorgan's Law
            | Not (And (left, right)) -> Or (Not left, Not right)                                           // DeMorgan's Law
            | Not (Comparison (left, op, right)) when op <> Equal -> Comparison (left, op.Negative, right)  // Flip operator to consume NOT
            | _ -> expr

        replace rewrite expr

    // Loop until the AST stops changing.
    let rec loop e =
        let nextExpr = nnfOneRound e
        if nextExpr = e then e
        else loop nextExpr

    loop expr

// Novel (AFAIK) one-pass implementation of NNF, implemented (indirectly) in terms of xfoldAcc.
// NOTE: This is not strictly iterative, but in principle it could be if xfoldAcc were internally implemented iteratively.
// This algorithm is based on the observation that NNF isn't necessarily about pushing NOTs down the AST, but rather ensuring that
// they're eliminated everywhere in the middle (they're ok at the leaves, in the form of NOT Equals or flipped operators, and a single
// NOT at the top of the AST is ok). We can get there by moving NOTs up the tree and relying on double-NOT elimination, which lends itself
// very well to a single depth-first preorder traversal. Whether to move NOTs up at a given node depends on whether there are an even or
// odd number of NOTs above the given sub-tree. If there is an odd number of NOTs, we should pull up a NOT from the sub-tree so that it
// will eventually combine with the "odd NOT out" above to eliminate both. If there is an even number, we should look for double-NOT
// elimination.
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
            | And (Not left, Not right) -> Not (Or (left, right))   // Reverse DeMorgan's Law
            | Or (Not left, Not right) -> Not (And (left, right))   // Reverse DeMorgan's Law
            | Comparison (left, op, right) when op <> Equal -> Not (Comparison (left, op.Negative, right))  // Flip operator to produce NOT
            | Equals (left, right) -> Not (Not (Equals left right)) // Add double-NOT to help consume another NOT further up the tree.
            | Any expr -> Not (Not (Any expr))                      // Add double-NOT to help consume another NOT further up the tree.
            | _ -> expr
        else
            match expr with
            | Not (Not x) -> x  // Double-NOT eliminiation
            | _ -> expr

    replaceAcc rewrite notCount 0 expr
