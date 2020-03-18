module FieldPrototypeTests

open System
open FsCheck.Gen
open FsCheck.Xunit
open FieldPrototype
open Microsoft.Azure.Search
open Microsoft.Azure.Search.Models
open Xunit

(*let tree =
    let rec tree' s =
        let arbComparison =
            gen {
                let! variableName = gen {
                    let alphabet = [|'a'..'z'|]
                    let! index = choose (0, alphabet.Length - 1)
                    return alphabet.[index].ToString ()
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
        | _ -> invalidArg "s" "Only positive sizes are allowed."

    Gen.sized tree'*)

(*type MyGenerators =
    static member Expr () = FsCheck.Arb.fromGen tree*)


let isRuntimeValidField (f : StrictField) =
    not (String.IsNullOrEmpty f.Name)

//[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
[<Property>]
let ``Cannot create invalid fields`` (field : StrictField) =
    if isRuntimeValidField field then
        let searchServiceName = "PASTE YOUR SERVICE NAME HERE"
        let apiKey = "PASTE YOUR API-KEY HERE" // DO NOT COMMIT THIS
        use client = new SearchServiceClient(searchServiceName, SearchCredentials apiKey)
        let needKey = not field.IsKey
        let fields =
            let extraField =
                let basicInfo = {
                    Name = if needKey then "id" else "somethingElse"
                    Type = Primitive String
                    IsKey = needKey
                    IsHidden = false
                    IsFilterable = false
                    IsSortable = false
                    IsFacetable = false
                }

                NonSearchableField basicInfo |> Simple

            [field; extraField] |> List.map (fun f -> f.Definition) |> ResizeArray<Field>

        let index = Index("myindex", fields)
        client.Indexes.Create(index) |> ignore
        Assert.True(true, "No exception thrown, as expected")
    else
        Assert.True(true, "Invalid field; skipping")

(*[<Property(Arbitrary = [|typeof<MyGenerators>|])>]
let ``One-pass NNF algorithm gives same result as classical NNF algorithm`` ast =
    let normalizedAst = canonicalize ast
    let expected = nnf normalizedAst
    let actual = nnfOnePass normalizedAst
    expected =! actual*)
