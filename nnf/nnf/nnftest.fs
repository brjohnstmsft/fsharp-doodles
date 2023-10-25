module NNF.NNFTest

open FsCheck
open FsCheck.Gen
open FsCheck.Xunit
open Swensen.Unquote
open NNF
open NNF.NNFExpr
open NNF.DNF
open NNF.CNF

let tree =
    let rec tree' s =
        let arbComparison =
            gen {
                let! variableName = gen {
                    let alphabet = [|'a'..'z'|]
                    let! index = choose (0, alphabet.Length - 1)
                    return alphabet[index].ToString()
                }

                let! operator = Arb.generate<Operator>
                let! number = Gen.choose (-10, 10)
                return (variableName, operator, number)
            }

        match s with
        | 0 -> Gen.map Comparison arbComparison
        | n when n > 0 ->
            let subtree = tree' (n / 2)
            Gen.oneof [
                Gen.map Comparison arbComparison
                Gen.map Not subtree
                Gen.map Any subtree
                Gen.map All subtree
                Gen.map2 (fun x y -> And (x, y)) subtree subtree
                Gen.map2 (fun x y -> Or (x, y)) subtree subtree]
        | _ -> invalidArg "s" "Only non-negative sizes are allowed."

    Gen.sized tree'

type MyGenerators =
    static member Expr () = Arb.fromGen tree

let private hasAndAboveOr expr =
    let andf (left, right) e =
        match e with
        | And (Or _, _) | And (_, Or _) -> true
        | _ -> left || right
        
    let orf (left, right) = left || right
        
    let notf operand = operand
        
    let anyf body = body
    let allf body = body

    let compf _ = false

    let ignoreExpr f t _ = f t

    xfold andf (ignoreExpr orf) (ignoreExpr notf) (ignoreExpr anyf) (ignoreExpr allf) (ignoreExpr compf) expr

[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
let ``One-pass NNF algorithm gives same result as classical NNF algorithm`` ast =
    let normalizedAst = canonicalize ast
    let expected = nnf normalizedAst
    let actual = nnfOnePass normalizedAst
    expected =! actual

[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
let ``Iterative NNF algorithm gives same result as classical NNF algorithm`` ast =
    let normalizedAst = canonicalize ast
    let expected = nnf normalizedAst
    let actual = nnfIterative normalizedAst
    expected =! actual

[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
let ``Iterative DNF algorithm gives same result as classical DNF algorithm`` ast =
    let normalizedAst = ast |> nnf |> canonicalize
    let expected = classifyDnfRecursive normalizedAst
    let actual = classifyDnfIterative normalizedAst
    expected =! actual

[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
let ``Iterative CNF algorithm gives same result as classical CNF algorithm`` ast =
    let normalizedAst = ast |> nnf |> canonicalize
    let expected = classifyCnfRecursive normalizedAst
    let actual = classifyCnfIterative normalizedAst
    expected =! actual

[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
let ``If X is DNF then NNF of Not X is CNF`` ast =
    let normalizedAst = ast |> nnf |> canonicalize
    let cnfResult = Not normalizedAst |> nnf |> classifyCnfRecursive
    let dnfResult = normalizedAst |> classifyDnfRecursive
    let expected = cnfResult <> Unknown
    let actual = dnfResult <> Unknown
    expected =! actual

[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
let ``DNF implies no AND above OR`` ast =
    let normalizedAst = ast |> nnf |> canonicalize
    let expected = true
    let actual = (classifyDnfIterative normalizedAst) = Unknown || not (hasAndAboveOr normalizedAst)
    expected =! actual
