module NNF.NNFExpr

open System.Collections.Generic

// Prototype for transforming ASTs into nnf in a bottom-up manner.
type Operator =
    | Less
    | LessOrEqual
    | Greater
    | GreaterOrEqual
    | Equal
    | NotEqual
with
    member this.Negative =
        match this with
        | Less -> GreaterOrEqual
        | LessOrEqual -> Greater
        | Greater -> LessOrEqual
        | GreaterOrEqual -> Less
        | NotEqual -> Equal
        | Equal -> failwith "Can't negate equals (just because)."

type Expr =
    | And of (Expr * Expr)
    | Or of (Expr * Expr)
    | Not of Expr
    | Any of Expr
    | All of Expr
    | Comparison of (string * Operator * int)

let (|Equals|_|) expr =
    match expr with
    | Comparison (left, Equal, right) -> Some (Equals (left, right))
    | _ -> None

let (|NotEquals|_|) expr =
    match expr with
    | Comparison (left, NotEqual, right) -> Some (NotEquals (left, right))
    | _ -> None

let Equals s i = Comparison (s, Equal, i)
let NotEquals s i = Comparison (s, NotEqual, i)
let LessThan s i = Comparison (s, Less, i)
let LessOrEqualTo s i = Comparison (s, LessOrEqual, i)
let GreaterThan s i = Comparison (s, Greater, i)
let GreaterOrEqualTo s i = Comparison (s, GreaterOrEqual, i)

// Recursively navigate the AST in depth-first preorder, flowing and updating a given accumulator value to pass to each step,
// and accumulating a final result as the traversal winds back up the three.
let rec private xfoldAcc' andf orf notf anyf allf compf updateAcc acc expr : 'r =
    let newAcc = updateAcc expr acc
    let recurse = xfoldAcc' andf orf notf anyf allf compf updateAcc newAcc
    match expr with
    | And (left, right) -> andf (recurse left, recurse right) expr acc
    | Or (left, right) -> orf (recurse left, recurse right) expr acc
    | Not x -> notf (recurse x) expr acc
    | Any x -> anyf (recurse x) expr acc
    | All x -> allf (recurse x) expr acc
    | Comparison t -> compf t expr acc

// Iteratively navigate the AST in depth-first preorder, flowing and updating a given accumulator value to pass to each step,
// and accumulating a final result as the traversal winds back up the three.
let private xfoldAcc andf orf notf anyf allf compf updateAcc (acc: 'a) expr : 'r =
    let forEachChild callback expr =
        match expr with
        | And (left, right) ->
            callback(left)
            callback(right)
        | Or (left, right) ->
            callback(left)
            callback(right)
        | Not e -> callback(e)
        | Any e -> callback(e)
        | All e -> callback(e)
        | Comparison _ -> ()

    let traversalStack =
        // To avoid recursion, we need to load up a stack of nodes such that they can be popped in post-order.
        // To do this, we use two stacks in a manner similar in spirit to the Shunting Yard algorithm.
        //
        // Here is an example of the main Shunting Yard mechanic. Given a tree like this:
        //
        //        1
        //       / \
        //      2   3
        //     / \
        //    4   5
        //
        // The processing steps look like this, where I is the intermediate stack and F is the final stack:
        //
        // 1.       2.          3.          4.             5.                6.
        // I: [1]   I: [3, 2]   I: [2]      I: [5, 4]      I: [4]            I: []
        // F: []    F: [1]      F: [3, 1]   F: [2, 3, 1]   F: [5, 2, 3, 1]   F: [4, 5, 2, 3, 1]
        let finalStack = Stack<(Expr * 'a)>()
        let intermediateStack = Stack<(Expr * 'a)>()

        intermediateStack.Push((expr, acc));

        while intermediateStack.Count > 0 do
            let (currentExpr, currentAcc) = intermediateStack.Pop()
            finalStack.Push((currentExpr, currentAcc))

            // Get the new state to use for the child nodes.
            let newAcc = updateAcc currentExpr currentAcc

            // Push children of the current child node to the intermediate stack. Note that we push left-to-right,
            // so that they will be popped right-to-left, and therefore pushed on the final stack in right-to-left
            // order, putting the leftmost leaf at the top of the stack (see example above).
            currentExpr |> forEachChild (fun child ->
                intermediateStack.Push((child, newAcc)))

        finalStack

    // This stack will hold values returned by recent visits. When visiting a non-terminal node, the top of
    // the stack will contain results of visiting child nodes for the current node. Values are pushed onto
    // this stack in the same order that the nodes are visited (left-to-right), so consuming code must pop
    // them in the opposite order (right-to-left).
    let visitResultStack = Stack<'r>()

    let iterate currentAcc currentExpr =
        let callback f t = f t currentExpr currentAcc

        match currentExpr with
        | And _ ->
            // Pop in reverse order, since nodes were visited in order.
            let right = visitResultStack.Pop()
            let left = visitResultStack.Pop()
            callback andf (left, right)
        | Or _ ->
            // Pop in reverse order, since nodes were visited in order.
            let right = visitResultStack.Pop()
            let left = visitResultStack.Pop()
            callback orf (left, right)
        | Not _ -> visitResultStack.Pop() |> callback notf
        | Any _ -> visitResultStack.Pop() |> callback anyf
        | All _ -> visitResultStack.Pop() |> callback allf
        | Comparison t -> callback compf t

    // At this point traversalStack contains all the nodes in left-to-right post-order (meaning the leftmost
    // deepest leaf is at the top of the stack), plus state information. We can just iterate through them
    // and visit each one. For example, for a trivial binary tree with parent node P, left leaf L, and
    // right leaf R, traversalStack will contain [L, R, P] where L is the top of the stack.
    while traversalStack.Count > 0 do
        let (currentExpr, currentAcc) = traversalStack.Pop()
        let resultForCurrentExpr = iterate currentAcc currentExpr

        visitResultStack.Push(resultForCurrentExpr)

    let result = visitResultStack.Pop()

    if visitResultStack.Count > 0 then
        // If this happens, iterate has a logic error whereby child nodes are being left behind on the
        // visited node stack.
        invalidOp "iterate left behind child nodes on the visited node stack."

    result

// Recursively navigate the AST in depth-first preorder, accumulating a final result as the traversal winds back up the tree.
let xfold andf orf notf anyf allf compf expr : 'r =
    let ignoreAcc f t e _ = f t e
    let update _ acc = acc

    xfoldAcc (ignoreAcc andf) (ignoreAcc orf) (ignoreAcc notf) (ignoreAcc anyf) (ignoreAcc allf)
        (ignoreAcc compf) update 0 expr

// Recursively navigate the AST in depth-first preorder, flowing and updating a given accumulator value to pass to each step,
// and rewriting AST nodes as the traversal winds back up the three.
let replaceAcc rewrite updateAcc acc expr =
    let copy matchExpr makeExpr args expr =
        match matchExpr expr with
        | Some args' -> if args = args' then expr else makeExpr args
        | None -> failwith "Invalid expression type."

    // These basically assert that the node is of the expected type and return it, or construct a new one if the node's
    // children changed.
    let XAnd = copy (function And t -> Some t | _ -> None) And
    let XOr = copy (function Or t -> Some t | _ -> None) Or
    let XNot = copy (function Not x -> Some x | _ -> None) Not
    let XAny = copy (function Any x -> Some x | _ -> None) Any
    let XAll = copy (function All x -> Some x | _ -> None) All
    let XComparison = copy (function Comparison t -> Some t | _ -> None) Comparison

    let andf t e acc = XAnd t e |> rewrite acc
    let orf t e acc = XOr t e |> rewrite acc
    let notf x e acc = XNot x e |> rewrite acc
    let anyf x e acc = XAny x e |> rewrite acc
    let allf x e acc = XAll x e |> rewrite acc
    let compf t e acc = XComparison t e |> rewrite acc

    xfoldAcc andf orf notf anyf allf compf updateAcc acc expr

// Recursively navigate the AST in depth-first preorder, rewriting AST nodes as the traversal winds back up the three.
let replace rewrite expr =
    let ignoreAcc _ e = rewrite e
    let update _ acc = acc
    replaceAcc ignoreAcc update 0 expr

// Put the given expression in a normal form guaranteed to have no NotEquals and no All operators.
let canonicalize expr =
    let rewrite expr =
        match expr with
        | NotEquals (left, right) -> Not (Equals left right)
        | All x -> Not (Any (Not x))
        | _ -> expr

    replace rewrite expr

type ExprType =
    | Literal
    | Conjunction
    | Disjunction
    | Unknown
