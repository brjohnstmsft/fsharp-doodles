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

let rec private xfold andf orf notf anyf allf compf updateAcc acc expr =
    let newAcc = updateAcc expr acc
    let recurse = xfold andf orf notf anyf allf compf updateAcc newAcc
    match expr with
    | And (left, right) -> andf (recurse left, recurse right) expr acc
    | Or (left, right) -> orf (recurse left, recurse right) expr acc
    | Not x -> notf (recurse x) expr acc
    | Any x -> anyf (recurse x) expr acc
    | All x -> allf (recurse x) expr acc
    | Comparison t -> compf t expr acc

let private foldAcc f updateAcc acc expr =
    let andf t e acc = XAnd t e |> f acc
    let orf t e acc = XOr t e |> f acc
    let notf x e acc = XNot x e |> f acc
    let anyf x e acc = XAny x e |> f acc
    let allf x e acc = XAll x e |> f acc
    let compf t e acc = XComparison t e |> f acc

    xfold andf orf notf anyf allf compf updateAcc acc expr

let private fold f expr =
    let ignoreAcc _ e = f e
    let update _ acc = acc
    foldAcc ignoreAcc update 0 expr

let canonicalize expr =
    let rewrite expr =
        match expr with
        | NotEquals (left, right) -> Not (Equals left right)
        | All x -> Not (Any (Not x))
        | _ -> expr

    fold rewrite expr    

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

    foldAcc rewrite notCount 0 expr

let nnfIterative expr =
    let nnfOneRound expr =
        let rewrite expr =
            match expr with
            | Not (Not x) -> x
            | Not (Or (left, right)) -> And (Not left, Not right)
            | Not (And (left, right)) -> Or (Not left, Not right)
            | Not (Comparison (left, op, right)) when op <> Equal -> Comparison (left, op.Negative, right)
            | _ -> expr

        fold rewrite expr

    let rec loop e =
        let nextExpr = nnfOneRound e
        if nextExpr = e then e
        else loop nextExpr

    loop expr

let testExpr =
    Not
        (And
            (All (Or (Equals "a" 7, GreaterThan "b" 10)),
            Not
                (Or
                    (LessThan "c" 7,
                    And (NotEquals "d" 3, Not (GreaterOrEqualTo "e" 6)))))) |> canonicalize

let nnfResult = nnf testExpr
let nnfResult' = nnfOnePass testExpr
let nnfResult'' = nnfIterative testExpr

let algo' = nnfResult = nnfResult'
let algo'' = nnfResult = nnfResult''
