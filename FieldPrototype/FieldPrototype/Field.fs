module FieldPrototype

open System
open Microsoft.Azure.Search.Models

type PrimitiveType = String | Int32 | Int64 | Double | DateTimeOffset | Boolean | GeographyPoint
with
    member this.Definition =
        match this with
        | String -> DataType.String
        | Int32 -> DataType.Int32
        | Int64 -> DataType.Int64
        | Double -> DataType.Double
        | DateTimeOffset -> DataType.DateTimeOffset
        | Boolean -> DataType.Boolean
        | GeographyPoint -> DataType.GeographyPoint

type SimpleDataType =
    | Primitive of PrimitiveType
    | Collection of PrimitiveType
with
    member this.Definition =
        match this with
        | Primitive t -> t.Definition
        | Collection t -> DataType.Collection(t.Definition)

type ComplexDataType =
    | ComplexType
    | ComplexCollection
with
    member this.Definition =
        match this with
        | ComplexType -> DataType.Complex
        | ComplexCollection -> DataType.Collection(DataType.Complex)

// Use a subset of analyzer names for test purposes
type StrictAnalyzerName =
    | EnLucene
    | EnMicrosoft
    | FrLucene
    | FrMicrosoft
    | Standard
with
    member this.Definition =
        match this with
        | EnLucene -> AnalyzerName.EnLucene
        | EnMicrosoft -> AnalyzerName.EnMicrosoft
        | FrLucene -> AnalyzerName.FrLucene
        | FrMicrosoft -> AnalyzerName.FrMicrosoft
        | Standard -> AnalyzerName.StandardLucene

type DualAnalyzerInfo = {
    IndexAnalyzer : StrictAnalyzerName
    SearchAnalyzer : StrictAnalyzerName
}

type AnalyzerInfo =
    | Analyzer of StrictAnalyzerName
    | DualAnalyzers of DualAnalyzerInfo

type BasicFieldInfo = {
    Name : string
    // Searchability shouldn't depend on data type at compile time in case
    // we support it for new types in the future.
    Type : SimpleDataType 
    IsKey : bool
    IsHidden : bool
    IsFilterable : bool
    // Sortability depends on *where* the field is defined, not just how. For example,
    // scalar sub-fields held directly or indirectly by a complex collection have multiple
    // values per document, so until we support aggregation functions in sorting, we can't
    // sort on them. This may happen in the future though, so let's not enforce that using types.
    IsSortable : bool
    // We don't support faceting on geo-points for now, but geo-hash bucketing is a possibility
    // in the future, so let's not enforce that with types.
    IsFacetable : bool
}

type SearchableFieldInfo = {
    BasicInfo : BasicFieldInfo
    Analyzer : AnalyzerInfo option
    SynonymMaps : string list
}

type SimpleField =
    | NonSearchableField of BasicFieldInfo      // Implies searchable: false
    | SearchableField of SearchableFieldInfo    // Implies searchable: true
with
    member this.Definition =
        match this with
        | NonSearchableField nsf ->
            Field.New(
                nsf.Name, 
                nsf.Type.Definition, 
                nsf.IsKey, 
                not nsf.IsHidden, 
                false, // isSearchable
                nsf.IsFilterable, 
                nsf.IsSortable, 
                nsf.IsFacetable)
        | SearchableField sf ->
            match sf.Analyzer with
            | None ->
                Field.New(
                    sf.BasicInfo.Name,
                    sf.BasicInfo.Type.Definition,
                    sf.BasicInfo.IsKey,
                    not sf.BasicInfo.IsHidden,
                    true, // isSearchable
                    sf.BasicInfo.IsFilterable,
                    sf.BasicInfo.IsSortable,
                    sf.BasicInfo.IsFacetable,
                    Nullable<AnalyzerName> (), // analyzerName
                    Nullable<AnalyzerName> (), // searchAnalyzerName
                    Nullable<AnalyzerName> (), // indexAnalyzerName
                    sf.SynonymMaps |> ResizeArray<string>)
            | Some (Analyzer analyzerName) ->
                Field.New(
                    sf.BasicInfo.Name,
                    sf.BasicInfo.Type.Definition,
                    sf.BasicInfo.IsKey,
                    not sf.BasicInfo.IsHidden,
                    true, // isSearchable
                    sf.BasicInfo.IsFilterable,
                    sf.BasicInfo.IsSortable,
                    sf.BasicInfo.IsFacetable,
                    analyzerName.Definition |> Nullable<AnalyzerName>,
                    Nullable<AnalyzerName> (), // searchAnalyzerName
                    Nullable<AnalyzerName> (), // indexAnalyzerName
                    sf.SynonymMaps |> ResizeArray<string>)
            | Some (DualAnalyzers { IndexAnalyzer = indexAnalyzer; SearchAnalyzer = searchAnalyzer }) ->
                Field.New(
                    sf.BasicInfo.Name,
                    sf.BasicInfo.Type.Definition,
                    sf.BasicInfo.IsKey,
                    not sf.BasicInfo.IsHidden,
                    true, // isSearchable
                    sf.BasicInfo.IsFilterable,
                    sf.BasicInfo.IsSortable,
                    sf.BasicInfo.IsFacetable,
                    Nullable<AnalyzerName> (), // analyzerName
                    searchAnalyzer.Definition |> Nullable<AnalyzerName>,
                    indexAnalyzer.Definition |> Nullable<AnalyzerName>,
                    sf.SynonymMaps |> ResizeArray<string>)

type NonEmptyList<'a> = {
    Head : 'a
    Tail : 'a list
}
with
    member this.AsList = [this.Head] @ this.Tail

type StrictField =
    | Simple of SimpleField
    | Complex of ComplexField
with
    member this.Definition =
        let rec makeField = function
            | Simple sf -> sf.Definition
            | Complex cf ->
                let isCollection =
                    match cf.Type with
                    | ComplexType -> false
                    | ComplexCollection -> true

                Field.NewComplex(
                    cf.Name,
                    isCollection,
                    cf.Fields.AsList |> List.map makeField |> ResizeArray<Field>)
        makeField this

    member this.IsKey =
        match this with
        | Simple (NonSearchableField nsf) -> nsf.IsKey
        | Simple (SearchableField sf) -> sf.BasicInfo.IsKey
        | _ -> false

    member this.Name =
        match this with
        | Simple (NonSearchableField nsf) -> nsf.Name
        | Simple (SearchableField sf) -> sf.BasicInfo.Name
        | Complex cf -> cf.Name

    (*member this.IsValid =
        let rec isValid f =
            match f with
            | Simple (NonSearchableField nsf) -> nsf.Name
            | Simple (SearchableField sf) -> sf.BasicInfo.Name
            | Complex cf -> cf.Name

        not (String.IsNullOrEmpty this.Name) && (isValid this)*)

and ComplexField = {
    Name : string
    Type : ComplexDataType
    Fields : NonEmptyList<StrictField>
}
