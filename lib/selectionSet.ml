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
        let fields =
          SelectionPath.fieldSegmentsOfPath
            (String.concat "." remainingSegments)
        in

        if fields = [] then
          invalid_arg "Inline fragments require a selection set.";

        CommandLineInvocationTypes.InlineFragmentSegment
          {
            inlineFragmentTypeCondition = String.trim typeCondition;
            inlineFragmentDirectiveTexts = [];
          }
        :: fields
    | _ ->
        invalid_arg
          "Inline fragments require a type condition and selection set"
  else if String.starts_with ~prefix:fragmentSpreadPrefix scopedPath then (
    let fragmentSpreadName =
      StringPrefix.valueWithoutPrefix ~prefix:fragmentSpreadPrefix scopedPath
      |> String.trim
    in

    if fragmentSpreadName = "" then
      invalid_arg "Fragment spreads require a name.";

    [
      CommandLineInvocationTypes.FragmentSpreadSegment
        { fragmentSpreadName; fragmentSpreadDirectiveTexts = [] };
    ])
  else SelectionPath.fieldSegmentsOfPath scopedPath

let parsedExpressions fieldExpressions =
  fieldExpressions
  |> List.concat_map (fun fieldExpression ->
      let paths =
        fieldExpression |> String.split_on_char ',' |> List.map String.trim
        |> List.filter (fun path -> path <> "")
      in

      if paths = [] then invalid_arg "Selection set cannot be empty.";

      List.map
        (fun path ->
          let scope, path = scopeAndFieldPathOfCliField path in
          let segments = segmentsOfScopedPath path in

          if segments = [] then invalid_arg "Selection path cannot be empty.";

          (scope, segments))
        paths)

let selectionsOfScopedPaths ~capitalizeRootFieldNames scopedPaths =
  scopedPaths
  |> List.map (fun (_, segments) ->
      SelectionPath.lowerSelectionPathSegments
        ~isTargetRoot:capitalizeRootFieldNames segments [])

let ofPaths ?(capitalizeRelativeRootFieldNames = false)
    ?(capitalizeRootFieldNames = false) scopedPaths =
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
