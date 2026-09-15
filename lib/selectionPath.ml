open CommandLineInvocationTypes

let makeFieldSegment ?fieldAlias fieldName =
  { fieldName; fieldAlias; fieldArgumentPairs = []; fieldDirectiveTexts = [] }

let fieldPathSegmentOfText pathSegmentText =
  let aliasValue, fieldName =
    CommandLineInvocationShared.splitAliasAndFieldName
      (String.trim pathSegmentText)
  in
  let trimmedFieldName = String.trim fieldName in

  if trimmedFieldName = "" then
    raise
      (Invalid_argument
         (Printf.sprintf "Field path cannot be empty.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    let fieldAlias =
      match aliasValue with
      | Some alias when String.trim alias <> "" ->
          Some (CommandLineInvocationShared.normalizedAlias (String.trim alias))
      | _ -> None
    in

    makeFieldSegment ?fieldAlias trimmedFieldName

let fieldSegmentsOfPath fieldPath =
  fieldPath |> String.split_on_char '.' |> List.map String.trim
  |> List.filter (fun segmentText -> segmentText <> "")
  |> List.map (fun segmentText ->
      FieldSegment (fieldPathSegmentOfText segmentText))

let graphQlArgumentsOfOptionPairs optionPairs =
  optionPairs
  |> List.map (fun (optionName, optionValue) ->
      CliArgument.make
        ~name:(GraphQlName.lowerCamelCaseOfCliToken optionName)
        ~rawValue:optionValue)

let graphQlFieldNameOfSegment ~isTargetRoot fieldName =
  if isTargetRoot then GraphQlName.upperCamelCaseOfCliToken fieldName
  else GraphQlName.lowerCamelCaseOfCliToken fieldName

let graphQlTypeCondition typeCondition =
  GraphQlName.upperCamelCaseOfCliToken typeCondition

let rec lowerSelectionPathSegments ~isTargetRoot selectionPathSegments
    selectionSet =
  let lowerTail ~isTargetRoot = function
    | [] -> selectionSet
    | remainingSelectionPathSegments ->
        [
          lowerSelectionPathSegments ~isTargetRoot
            remainingSelectionPathSegments selectionSet;
        ]
  in

  match selectionPathSegments with
  | [] ->
      raise (Invalid_argument "Expected at least one selection path segment")
  | FieldSegment
      ({ fieldName; fieldAlias; fieldArgumentPairs; fieldDirectiveTexts } :
        fieldSegment)
    :: remainingSelectionPathSegments ->
      GraphQlSelection.field
        (GraphQlSelection.makeField ?alias:fieldAlias
           ~name:(graphQlFieldNameOfSegment ~isTargetRoot fieldName)
           ~arguments:(graphQlArgumentsOfOptionPairs fieldArgumentPairs)
           ~directives:fieldDirectiveTexts
           ~selectionSet:
             (lowerTail ~isTargetRoot:false remainingSelectionPathSegments)
           ())
  | InlineFragmentSegment
      ({ inlineFragmentTypeCondition; inlineFragmentDirectiveTexts } :
        inlineFragmentSegment)
    :: remainingSelectionPathSegments ->
      GraphQlSelection.inlineFragment
        (GraphQlSelection.makeInlineFragment
           ~typeCondition:(graphQlTypeCondition inlineFragmentTypeCondition)
           ~directives:inlineFragmentDirectiveTexts
           ~selectionSet:
             (lowerTail ~isTargetRoot remainingSelectionPathSegments)
           ())
  | FragmentSpreadSegment
      ({ fragmentSpreadName; fragmentSpreadDirectiveTexts } :
        fragmentSpreadSegment)
    :: remainingSelectionPathSegments ->
      if remainingSelectionPathSegments <> [] then
        raise
          (Invalid_argument "Fragment spreads cannot contain child selections")
      else
        GraphQlSelection.fragmentSpread
          (GraphQlSelection.makeFragmentSpread ~name:fragmentSpreadName
             ~directives:fragmentSpreadDirectiveTexts ())
