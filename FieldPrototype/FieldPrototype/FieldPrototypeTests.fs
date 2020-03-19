module FieldPrototypeTests

open FsCheck.Xunit
open FieldPrototype
open Microsoft.Azure.Search
open Microsoft.Azure.Search.Models
open Xunit

[<Property(EndSize = 2000, MaxTest = 3000)>]
let ``Cannot create invalid fields`` (field : StrictField) =
    if field.IsValid then
        let searchServiceName = "PASTE YOUR SERVICE NAME HERE"
        let apiKey = "PASTE YOUR API-KEY HERE" // DO NOT COMMIT THIS
        use client = new SearchServiceClient(searchServiceName, SearchCredentials apiKey)

        let fields =
            let extraField =
                let basicInfo =
                    let makeInfo name = { Name = name; IsFilterable = false; IsSortable = false; IsFacetable = false }
                    match field with
                    | KeyField -> NonKeyField { Common = makeInfo "somethingElse"; Type = Primitive String; IsHidden = false }
                    | _ -> makeInfo "id" |> KeyField

                NonSearchableField basicInfo |> Simple

            [field; extraField] |> List.map (fun f -> f.Definition) |> ResizeArray<Field>

        let index = Index("myindex", fields)

        try
            client.Indexes.Create(index) |> ignore
        finally
            client.Indexes.Delete(index.Name) |> ignore

        Assert.True(true, "No exception thrown, as expected")
    else
        Assert.True(true, "Invalid field; skipping")
