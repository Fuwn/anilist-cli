open CommandLineInvocationTypes

let makeFieldSegment ?fieldAlias fieldName =
  { fieldName; fieldAlias; fieldArgumentPairs = []; fieldDirectiveTexts = [] }

let makeInlineFragmentSegment inlineFragmentTypeCondition =
  { inlineFragmentTypeCondition; inlineFragmentDirectiveTexts = [] }

let makeFragmentSpreadSegment fragmentSpreadName =
  { fragmentSpreadName; fragmentSpreadDirectiveTexts = [] }

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

let segmentsEndInFragmentSpread selectionPathSegments =
  match List.rev selectionPathSegments with
  | FragmentSpreadSegment _ :: _ -> true
  | (FieldSegment _ | InlineFragmentSegment _) :: _ | [] -> false

let branchEndsInFragmentSpread branch =
  segmentsEndInFragmentSpread branch.selectionPathSegments

let makeSelectionBranchFromSegments segments =
  if segments = [] then
    raise
      (Invalid_argument
         (Printf.sprintf "Selection path cannot be empty.\n\n%s"
            CommandLineInvocationShared.usageText))
  else { selectionPathSegments = segments; selectionExpressions = [] }

let makeSelectionBranchFromFieldPath fieldPath =
  fieldPath |> fieldSegmentsOfPath |> makeSelectionBranchFromSegments

let updateLastSegment branch mapSegment =
  match List.rev branch.selectionPathSegments with
  | [] -> raise (Invalid_argument "Empty selection branch")
  | last :: rest ->
      { branch with selectionPathSegments = List.rev (mapSegment last :: rest) }

let withAddedBranchSelectionExpression branch selectionExpression =
  if branchEndsInFragmentSpread branch then
    raise
      (Invalid_argument
         (Printf.sprintf "Fragment spreads cannot define a selection set.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    {
      branch with
      selectionExpressions =
        branch.selectionExpressions @ [ selectionExpression ];
    }

let withPushedFieldSelectionPathSegment branch fieldName =
  if branchEndsInFragmentSpread branch then
    raise
      (Invalid_argument
         (Printf.sprintf
            "Fragment spreads cannot contain child selections.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    let newSegments = fieldSegmentsOfPath fieldName in
    if newSegments = [] then
      raise
        (Invalid_argument
           (Printf.sprintf "Selection path cannot be empty.\n\n%s"
              CommandLineInvocationShared.usageText))
    else
      {
        branch with
        selectionPathSegments = branch.selectionPathSegments @ newSegments;
      }

let withAddedBranchFieldArgumentPair branch fieldArgumentPair =
  updateLastSegment branch (function
    | FieldSegment fieldSegment ->
        FieldSegment
          {
            fieldSegment with
            fieldArgumentPairs =
              fieldSegment.fieldArgumentPairs @ [ fieldArgumentPair ];
          }
    | InlineFragmentSegment _ | FragmentSpreadSegment _ ->
        raise
          (Invalid_argument
             (Printf.sprintf
                "Field arguments require the current selection branch to end \
                 in a field.\n\n\
                 %s"
                CommandLineInvocationShared.usageText)))

let withUpdatedBranchFieldAlias branch fieldAlias =
  updateLastSegment branch (function
    | FieldSegment fieldSegment ->
        FieldSegment
          {
            fieldSegment with
            fieldAlias =
              Some (CommandLineInvocationShared.normalizedAlias fieldAlias);
          }
    | InlineFragmentSegment _ | FragmentSpreadSegment _ ->
        raise
          (Invalid_argument
             (Printf.sprintf
                "--alias requires the current selection branch to end in a \
                 field.\n\n\
                 %s"
                CommandLineInvocationShared.usageText)))

let withAddedBranchDirective branch directiveText =
  updateLastSegment branch (function
    | FieldSegment fieldSegment ->
        FieldSegment
          {
            fieldSegment with
            fieldDirectiveTexts =
              fieldSegment.fieldDirectiveTexts @ [ directiveText ];
          }
    | InlineFragmentSegment inlineFragmentSegment ->
        InlineFragmentSegment
          {
            inlineFragmentSegment with
            inlineFragmentDirectiveTexts =
              inlineFragmentSegment.inlineFragmentDirectiveTexts
              @ [ directiveText ];
          }
    | FragmentSpreadSegment fragmentSpreadSegment ->
        FragmentSpreadSegment
          {
            fragmentSpreadSegment with
            fragmentSpreadDirectiveTexts =
              fragmentSpreadSegment.fragmentSpreadDirectiveTexts
              @ [ directiveText ];
          })

let currentSelectionBranchOfState parserState = parserState.currentBranch

let currentDefaultSelectionPathPrefix parserState =
  parserState.currentDefaultSelectionPathPrefix

let ensureSelectionPathCanAcceptChildren selectionPathSegments =
  if segmentsEndInFragmentSpread selectionPathSegments then
    raise
      (Invalid_argument
         (Printf.sprintf
            "Fragment spreads cannot contain child selections.\n\n%s"
            CommandLineInvocationShared.usageText))
  else selectionPathSegments

let classifiedBranchPath pathText =
  if
    StringPrefix.valueHasPrefix
      ~prefix:CommandLineInvocationShared.currentBranchPathPrefix pathText
  then
    ( `Relative,
      StringPrefix.valueWithoutPrefix
        ~prefix:CommandLineInvocationShared.currentBranchPathPrefix pathText )
  else if
    StringPrefix.valueHasPrefix
      ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix pathText
  then
    ( `Absolute,
      StringPrefix.valueWithoutPrefix
        ~prefix:CommandLineInvocationShared.absoluteBranchPathPrefix pathText )
  else (`Default, pathText)

let currentBranchSegmentsOrRaise parserState ~requiredMessage =
  match parserState.currentBranch with
  | Some branch ->
      ensureSelectionPathCanAcceptChildren branch.selectionPathSegments
  | None ->
      raise
        (Invalid_argument
           (Printf.sprintf "%s\n\n%s" requiredMessage
              CommandLineInvocationShared.usageText))

let parentSegmentsAndBareText parserState ~currentBranchRequiredMessage pathText
    =
  match classifiedBranchPath pathText with
  | `Relative, bareText ->
      ( currentBranchSegmentsOrRaise parserState
          ~requiredMessage:currentBranchRequiredMessage,
        bareText )
  | `Absolute, bareText -> ([], bareText)
  | `Default, bareText ->
      let parentSegments =
        match parserState.currentBranch with
        | Some branch ->
            ensureSelectionPathCanAcceptChildren branch.selectionPathSegments
        | None -> parserState.currentDefaultSelectionPathPrefix
      in
      (parentSegments, bareText)

let resolvedSelectionPathSegmentsOfFieldPath parserState fieldPath =
  let nonemptyFieldSegments path =
    let segments = fieldSegmentsOfPath path in
    if segments = [] then
      raise
        (Invalid_argument
           (Printf.sprintf "Selection path cannot be empty.\n\n%s"
              CommandLineInvocationShared.usageText))
    else segments
  in
  match classifiedBranchPath fieldPath with
  | `Relative, bareFieldPath ->
      currentBranchSegmentsOrRaise parserState
        ~requiredMessage:
          "Relative --field paths require an existing selection branch."
      @ nonemptyFieldSegments bareFieldPath
  | `Absolute, bareFieldPath -> nonemptyFieldSegments bareFieldPath
  | `Default, bareFieldPath ->
      parserState.currentDefaultSelectionPathPrefix
      @ nonemptyFieldSegments bareFieldPath

let resolvedSelectionPathSegmentsOfInlineFragment parserState
    inlineFragmentTypeConditionText =
  let parentSelectionPathSegments, typeConditionText =
    parentSegmentsAndBareText parserState
      ~currentBranchRequiredMessage:
        "Relative --inline-fragment paths require an existing selection branch."
      inlineFragmentTypeConditionText
  in
  let normalizedTypeCondition = String.trim typeConditionText in
  if normalizedTypeCondition = "" then
    raise
      (Invalid_argument
         (Printf.sprintf "Inline fragments require a type condition.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    parentSelectionPathSegments
    @ [
        InlineFragmentSegment
          (makeInlineFragmentSegment normalizedTypeCondition);
      ]

let resolvedSelectionPathSegmentsOfFragmentSpread parserState fragmentSpreadText
    =
  let parentSelectionPathSegments, fragmentSpreadName =
    parentSegmentsAndBareText parserState
      ~currentBranchRequiredMessage:
        "Relative --fragment-spread paths require an existing selection branch."
      fragmentSpreadText
  in
  let normalizedFragmentSpreadName = String.trim fragmentSpreadName in
  if normalizedFragmentSpreadName = "" then
    raise
      (Invalid_argument
         (Printf.sprintf "Fragment spreads require a fragment name.\n\n%s"
            CommandLineInvocationShared.usageText))
  else
    parentSelectionPathSegments
    @ [
        FragmentSpreadSegment
          (makeFragmentSpreadSegment normalizedFragmentSpreadName);
      ]
