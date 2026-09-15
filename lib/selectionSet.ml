type pathSegment = { alias : string option; name : string }
type t = GraphQlSelection.t list
type scopedFieldSelection = { relativeSelectionSet : t; rootSelectionSet : t }

let rootFieldPathPrefix = "root:"
let rootSelectionPathPrefix = "/"
let fragmentSpreadPrefix = "..."

let pathSegmentOfCliSegment ~capitalizeFieldName segment =
  let aliasValue, fieldName =
    CommandLineInvocationShared.splitAliasAndFieldName segment
  in
  let normalizedAlias =
    match aliasValue with
    | Some alias when String.trim alias <> "" ->
        Some (GraphQlName.lowerCamelCaseOfCliToken (String.trim alias))
    | _ -> None
  in
  let normalizedFieldName =
    if capitalizeFieldName then
      GraphQlName.upperCamelCaseOfCliToken (String.trim fieldName)
    else GraphQlName.lowerCamelCaseOfCliToken (String.trim fieldName)
  in

  if String.trim fieldName = "" then
    invalid_arg
      (Printf.sprintf "Field path cannot be empty.\n\n%s"
         CommandLineInvocationShared.usageText)
  else { alias = normalizedAlias; name = normalizedFieldName }

let pathSegmentsOfCliField ~capitalizeRootFieldNames fieldPath =
  fieldPath |> String.split_on_char '.' |> List.map String.trim
  |> List.filter (fun fieldName -> fieldName <> "")
  |> List.mapi (fun index fieldName ->
      pathSegmentOfCliSegment
        ~capitalizeFieldName:(capitalizeRootFieldNames && index = 0)
        fieldName)

let rec fieldOfPathSegments = function
  | [] ->
      raise (Invalid_argument "Expected at least one selection path segment")
  | [ pathSegment ] ->
      GraphQlSelection.field
        (GraphQlSelection.makeField ?alias:pathSegment.alias
           ~name:pathSegment.name ~arguments:[] ~directives:[] ~selectionSet:[]
           ())
  | pathSegment :: remainingSegments ->
      let childField = fieldOfPathSegments remainingSegments in

      GraphQlSelection.field
        (GraphQlSelection.makeField ?alias:pathSegment.alias
           ~name:pathSegment.name ~arguments:[] ~directives:[]
           ~selectionSet:[ childField ] ())

let scopeAndFieldPathOfCliField fieldPath =
  if String.starts_with ~prefix:rootFieldPathPrefix fieldPath then
    ( `Root,
      StringPrefix.valueWithoutPrefix ~prefix:rootFieldPathPrefix fieldPath )
  else if String.starts_with ~prefix:rootSelectionPathPrefix fieldPath then
    ( `Root,
      StringPrefix.valueWithoutPrefix ~prefix:rootSelectionPathPrefix fieldPath
    )
  else (`Relative, fieldPath)

let selectionOfInlineFragmentPath ~capitalizeRootFieldNames inlineFragmentPath =
  match String.split_on_char '.' inlineFragmentPath with
  | typeCondition :: (_ :: _ as remainingSegments)
    when String.trim typeCondition <> "" ->
      let paths =
        String.concat "." remainingSegments
        |> pathSegmentsOfCliField ~capitalizeRootFieldNames
      in
      let selectionSet =
        match paths with [] -> [] | _ -> [ fieldOfPathSegments paths ]
      in

      GraphQlSelection.inlineFragment
        (GraphQlSelection.makeInlineFragment
           ~typeCondition:
             (GraphQlName.upperCamelCaseOfCliToken (String.trim typeCondition))
           ~selectionSet ())
  | _ ->
      invalid_arg "Inline fragments require a type condition and selection set"

let selectionsOfScopedPaths ~capitalizeRootFieldNames scopedPaths =
  scopedPaths |> List.map snd
  |> List.map (fun scopedPath ->
      if String.starts_with ~prefix:fragmentSpreadPrefix scopedPath then
        let spreadTarget =
          StringPrefix.valueWithoutPrefix ~prefix:fragmentSpreadPrefix
            scopedPath
        in

        if String.starts_with ~prefix:"on:" spreadTarget then
          let inlineFragmentPath =
            StringPrefix.valueWithoutPrefix ~prefix:"on:" spreadTarget
          in

          selectionOfInlineFragmentPath ~capitalizeRootFieldNames
            inlineFragmentPath
        else
          GraphQlSelection.fragmentSpread
            (GraphQlSelection.makeFragmentSpread ~name:spreadTarget ())
      else
        scopedPath
        |> pathSegmentsOfCliField ~capitalizeRootFieldNames
        |> fieldOfPathSegments)

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
