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

let private copy matchExpr makeExpr args expr =
    match matchExpr expr with
    | Some args' -> if args = args' then expr else makeExpr args
    | None -> failwith "Invalid expression type."

let XAnd t expr = copy (fun e -> match e with And t -> Some t | _ -> None) And t expr

let XOr t expr = copy (fun e -> match e with Or t -> Some t | _ -> None) Or t expr

let XNot x expr = copy (fun e -> match e with Not x -> Some x | _ -> None) Not x expr

let XAny x expr = copy (fun e -> match e with Any x -> Some x | _ -> None) Any x expr

let XAll x expr = copy (fun e -> match e with All x -> Some x | _ -> None) All x expr

let XComparison t expr = copy (fun e -> match e with Comparison t -> Some t | _ -> None) Comparison t expr

let rec xfoldAcc andf orf notf anyf allf compf updateAcc acc expr : 'r =
    let newAcc = updateAcc expr acc
    let recurse = xfoldAcc andf orf notf anyf allf compf updateAcc newAcc
    match expr with
    | And (left, right) -> andf (recurse left, recurse right) expr acc
    | Or (left, right) -> orf (recurse left, recurse right) expr acc
    | Not x -> notf (recurse x) expr acc
    | Any x -> anyf (recurse x) expr acc
    | All x -> allf (recurse x) expr acc
    | Comparison t -> compf t expr acc

let xfold andf orf notf anyf allf compf expr : 'r =
    let ignoreAcc f t e _ = f t e
    let update _ acc = acc

    xfoldAcc (ignoreAcc andf) (ignoreAcc orf) (ignoreAcc notf) (ignoreAcc anyf) (ignoreAcc allf)
        (ignoreAcc compf) update 0 expr

let replaceAcc rewrite updateAcc acc expr =
    let andf t e acc = XAnd t e |> rewrite acc
    let orf t e acc = XOr t e |> rewrite acc
    let notf x e acc = XNot x e |> rewrite acc
    let anyf x e acc = XAny x e |> rewrite acc
    let allf x e acc = XAll x e |> rewrite acc
    let compf t e acc = XComparison t e |> rewrite acc

    xfoldAcc andf orf notf anyf allf compf updateAcc acc expr

let replace rewrite expr =
    let ignoreAcc _ e = rewrite e
    let update _ acc = acc
    replaceAcc ignoreAcc update 0 expr

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
