open CommandLineInvocationTypes

type t = GraphQlSelection.t list
type scopedFieldSelection = { relativeSelectionSet : t; rootSelectionSet : t }

let rootFieldPathPrefix = "root:"
let rootSelectionPathPrefix = "/"
let fragmentSpreadPrefix = "..."

let scopeAndFieldPathOfCliField fieldPath =
  if String.starts_with ~prefix:rootFieldPathPrefix fieldPath then
    ( `Root,
      StringPrefix.valueWithoutPrefix ~prefix:rootFieldPathPrefix fieldPath )
  else if String.starts_with ~prefix:rootSelectionPathPrefix fieldPath then
    ( `Root,
      StringPrefix.valueWithoutPrefix ~prefix:rootSelectionPathPrefix fieldPath
    )
  else (`Relative, fieldPath)

let segmentsOfScopedPath scopedPath =
  if String.starts_with ~prefix:"...on:" scopedPath then
    let inlineFragmentPath =
      StringPrefix.valueWithoutPrefix ~prefix:"...on:" scopedPath
    in

    match String.split_on_char '.' inlineFragmentPath with
    | typeCondition :: (_ :: _ as remainingSegments)
      when String.trim typeCondition <> "" ->
        CommandLineInvocationTypes.InlineFragmentSegment
          {
            inlineFragmentTypeCondition = String.trim typeCondition;
            inlineFragmentDirectiveTexts = [];
          }
        :: SelectionPath.fieldSegmentsOfPath
             (String.concat "." remainingSegments)
    | _ ->
        invalid_arg
          "Inline fragments require a type condition and selection set"
  else if String.starts_with ~prefix:fragmentSpreadPrefix scopedPath then
    [
      CommandLineInvocationTypes.FragmentSpreadSegment
        {
          fragmentSpreadName =
            StringPrefix.valueWithoutPrefix ~prefix:fragmentSpreadPrefix
              scopedPath;
          fragmentSpreadDirectiveTexts = [];
        };
    ]
  else SelectionPath.fieldSegmentsOfPath scopedPath

let selectionsOfScopedPaths ~capitalizeRootFieldNames scopedPaths =
  scopedPaths
  |> List.map (fun (_, scopedPath) ->
      SelectionPath.lowerSelectionPathSegments
        ~isTargetRoot:capitalizeRootFieldNames
        (segmentsOfScopedPath scopedPath)
        [])

let ofCliArguments ?(capitalizeRelativeRootFieldNames = false)
    ?(capitalizeRootFieldNames = false) fieldExpressions =
  let scopedPaths =
    fieldExpressions
    |> List.concat_map (fun fieldExpression ->
        fieldExpression |> String.split_on_char ',' |> List.map String.trim
        |> List.filter (fun fieldName -> fieldName <> ""))
    |> List.map scopeAndFieldPathOfCliField
  in
  let relativePaths, rootPaths =
    List.partition (fun (scope, _) -> scope = `Relative) scopedPaths
  in

  {
    relativeSelectionSet =
      selectionsOfScopedPaths
        ~capitalizeRootFieldNames:capitalizeRelativeRootFieldNames relativePaths;
    rootSelectionSet =
      selectionsOfScopedPaths ~capitalizeRootFieldNames rootPaths;
  }
