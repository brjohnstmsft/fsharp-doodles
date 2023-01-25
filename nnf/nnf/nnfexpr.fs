module NNF.NNFExpr

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
let rec private xfoldAcc andf orf notf anyf allf compf updateAcc acc expr : 'r =
    let newAcc = updateAcc expr acc
    let recurse = xfoldAcc andf orf notf anyf allf compf updateAcc newAcc
    match expr with
    | And (left, right) -> andf (recurse left, recurse right) expr acc
    | Or (left, right) -> orf (recurse left, recurse right) expr acc
    | Not x -> notf (recurse x) expr acc
    | Any x -> anyf (recurse x) expr acc
    | All x -> allf (recurse x) expr acc
    | Comparison t -> compf t expr acc

// Recursively navigate the AST in depth-first preorder, accumulating a final result as the traversal winds back up the three.
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
