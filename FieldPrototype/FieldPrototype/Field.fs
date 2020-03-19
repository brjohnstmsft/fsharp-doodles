module FieldPrototype

open System
open System.Text.RegularExpressions
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

let (|StringType|_|) t =
    match t with
    | Primitive String | Collection String -> Some ()
    | _ -> None

let (|GeoPointType|_|) t =
    match t with
    | Primitive GeographyPoint | Collection GeographyPoint -> Some ()
    | _ -> None

type ComplexDataType =
    | ComplexType
    | ComplexCollection
with
    member this.Definition =
        match this with
        | ComplexType -> DataType.Complex
        | ComplexCollection -> DataType.Collection(DataType.Complex)

// Use a subset of analyzer names for test purposes. Note the distinction between language and non-language analyzers.
type LanguageAnalyzerName =
    | EnLucene
    | EnMicrosoft
    | FrLucene
    | FrMicrosoft
    | Standard

type NonLanguageAnalyzerName =
    | Keyword
    | Simple
    | Stop
    | Whitespace

type StrictAnalyzerName =
    | Language of LanguageAnalyzerName
    | NonLanguage of NonLanguageAnalyzerName
with
    member this.Definition =
        match this with
        | Language EnLucene -> AnalyzerName.EnLucene
        | Language EnMicrosoft -> AnalyzerName.EnMicrosoft
        | Language FrLucene -> AnalyzerName.FrLucene
        | Language FrMicrosoft -> AnalyzerName.FrMicrosoft
        | Language Standard -> AnalyzerName.StandardLucene
        | NonLanguage Keyword -> AnalyzerName.Keyword
        | NonLanguage Simple -> AnalyzerName.Simple
        | NonLanguage Stop -> AnalyzerName.Stop
        | NonLanguage Whitespace -> AnalyzerName.Whitespace

type DualAnalyzerInfo = {
    IndexAnalyzer : NonLanguageAnalyzerName
    SearchAnalyzer : NonLanguageAnalyzerName
}

type AnalyzerInfo =
    | Analyzer of StrictAnalyzerName
    | DualAnalyzers of DualAnalyzerInfo

type CommonFieldInfo = {
    Name : string
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

type NonKeyFieldInfo = {
    Common : CommonFieldInfo
    // Searchability shouldn't depend on data type at compile time in case
    // we support it for new types in the future.
    Type : SimpleDataType 
    IsHidden : bool
}

type BasicFieldInfo =
    // This model makes it impossible to create a key that isn't a string or a hidden key.
    // This seems fine to enforce statically since we're unlikely to support compound keys or keys of other data types,
    // and the key field can't be hidden for obvious reasons.
    // However, some validation rules this doesn't enforce statically:
    // - Sub-fields of complex fields cannot be key fields.
    // - There must be exactly one key field per index.
    | KeyField of CommonFieldInfo
    | NonKeyField of NonKeyFieldInfo
with
    member this.Common =
        match this with
        | KeyField kf -> kf
        | NonKeyField nkf -> nkf.Common

    member this.DataType =
        match this with
        | KeyField kf -> (Primitive String)
        | NonKeyField nkf -> nkf.Type

type SearchableFieldInfo = {
    BasicInfo : BasicFieldInfo
    Analyzer : AnalyzerInfo option
    
    // Even though this is an array in the REST API, currently only one is allowed per field,
    // and must already exist in the service.  
    // Modeling this as just a string also prevents FsCheck from running too long. :P
    SynonymMap : string
}

type SimpleField =
    | NonSearchableField of BasicFieldInfo      // Implies searchable: false
    | SearchableField of SearchableFieldInfo    // Implies searchable: true
with
    member this.BasicInfo =
        match this with
        | NonSearchableField nsf -> nsf
        | SearchableField sf -> sf.BasicInfo

    member this.Definition =
        let makeField info isSearchable analyzerNameOpt searchAnalyzerNameOpt indexAnalyzerNameOpt =
            let convertAnalyzer (aOpt : StrictAnalyzerName option) =
                aOpt
                |> Option.map (fun a -> a.Definition |> Nullable<AnalyzerName>)
                |> (fun a -> defaultArg a (Nullable<AnalyzerName> ()))

            let convertNonLanguageAnalyzer = Option.map NonLanguage >> convertAnalyzer

            let makeField' common (dataType : SimpleDataType) isKey isHidden =
                Field.New(
                    common.Name,
                    dataType.Definition,
                    isKey,
                    not isHidden,
                    isSearchable,
                    common.IsFilterable, 
                    common.IsSortable, 
                    common.IsFacetable,
                    convertAnalyzer analyzerNameOpt,
                    convertNonLanguageAnalyzer searchAnalyzerNameOpt,
                    convertNonLanguageAnalyzer indexAnalyzerNameOpt,
                    ResizeArray<string> ()) // Ignore synonym maps. They're non-trivial to validate with FsCheck.

            match info with
            | KeyField kf -> makeField' kf (Primitive String) true false
            | NonKeyField nkf -> makeField' nkf.Common nkf.Type false nkf.IsHidden

        match this with
        | NonSearchableField nsf -> makeField nsf false None None None
        | SearchableField sf -> 
            match sf.Analyzer with
            | None -> makeField sf.BasicInfo true None None None
            | Some (Analyzer analyzerName) ->
                makeField sf.BasicInfo true (Some analyzerName) None None
            | Some (DualAnalyzers { IndexAnalyzer = indexAnalyzer; SearchAnalyzer = searchAnalyzer }) ->
                makeField sf.BasicInfo true None (Some searchAnalyzer) (Some indexAnalyzer)

type NonEmptyList<'a> = {
    Head : 'a
    Tail : 'a list
}
with
    member this.AsList = this.Head::this.Tail

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

    member this.Name =
        match this with
        | Simple (NonSearchableField nsf) -> nsf.Common.Name
        | Simple (SearchableField sf) -> sf.BasicInfo.Common.Name
        | Complex cf -> cf.Name

    member this.IsValid =
        let isNameValid name =
            if String.IsNullOrEmpty name then false
            else Regex.IsMatch (name, "^[a-zA-Z][a-zA-Z0-9_]*$")
 
        let rec isValid (f : StrictField) =
            if isNameValid f.Name then
                match f with
                | Simple sf ->
                    let info = sf.BasicInfo.Common
                    let dataType = sf.BasicInfo.DataType

                    let isFacetableValid =
                        match dataType with
                        | GeoPointType -> not info.IsFacetable
                        | _ -> true

                    let isSearchableValid =
                        match dataType, sf with
                        | _, NonSearchableField _ -> true
                        | StringType, SearchableField _ -> true
                        | _ -> false

                    let isSortableValid =
                        match dataType with
                        | Collection _ -> not info.IsSortable
                        | _ -> true

                    isFacetableValid && isSearchableValid && isSortableValid

                | Complex cf -> cf.Fields.AsList |> List.forall isValid
            else false

        isValid this

and ComplexField = {
    Name : string
    Type : ComplexDataType
    Fields : NonEmptyList<StrictField>
}

let (|KeyField|_|) f =
    match f with
    | Simple (SearchableField { BasicInfo = (KeyField _); Analyzer = _; SynonymMap = _ }) -> Some ()
    | Simple (NonSearchableField (KeyField _)) -> Some ()
    | _ -> None
