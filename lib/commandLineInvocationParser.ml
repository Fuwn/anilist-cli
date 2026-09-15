open CommandLineInvocationTypes

exception HelpRequested

let bareOptionNameOfToken token =
  match CommandLineInvocationShared.optionNameAndValueOfEqualsSyntax token with
  | Some (optionName, _) -> optionName
  | None -> String.sub token 2 (String.length token - 2)

let replaceCurrentSelectionBranchWithSegments parserState resolvedSegments =
  let rec extendsPath parent child =
    match (parent, child) with
    | [], _ :: _ -> true
    | parentSegment :: parentTail, childSegment :: childTail
      when parentSegment = childSegment ->
        extendsPath parentTail childTail
    | _ -> false
  in
  let parserState =
    match parserState.currentBranch with
    | Some branch
      when branch.selectionPaths = []
           && extendsPath branch.selectionPathSegments resolvedSegments ->
        { parserState with currentBranch = None }
    | _ -> CommandLineInvocationState.finalizedCurrentBranch parserState
  in

  CommandLineInvocationState.withUpdatedCurrentSelectionBranch parserState
    (CommandLineInvocationBranch.makeSelectionBranchFromSegments
       resolvedSegments)

let withCurrentOperationOrRaise ~optionDescription parserState mapOperation =
  match parserState.currentTarget with
  | Some (OperationTarget operationDefinition) ->
      {
        parserState with
        currentTarget =
          Some (OperationTarget (mapOperation operationDefinition));
      }
  | None | Some (FragmentTarget _) ->
      raise
        (Invalid_argument
           (Printf.sprintf "%s requires a current operation.\n\n%s"
              optionDescription CommandLineInvocationShared.usageText))

let updateBranchOrFallback parserState ~onBranch ~onNoBranch =
  match
    CommandLineInvocationBranch.currentSelectionBranchOfState parserState
  with
  | Some branch ->
      CommandLineInvocationState.withUpdatedCurrentSelectionBranch parserState
        (onBranch branch)
  | None -> onNoBranch parserState

let handleOperation parserState operationHeaderText =
  let operationType, operationName =
    CommandLineInvocationShared.operationHeaderOfText operationHeaderText
  in

  CommandLineInvocationState.startedOperationDefinition parserState
    operationType operationName

let handleSelectionPath resolveSelectionPathSegments parserState pathText =
  replaceCurrentSelectionBranchWithSegments parserState
    (resolveSelectionPathSegments parserState pathText)

let handleFragment parserState fragmentText =
  let fragmentName, fragmentTypeCondition =
    CommandLineInvocationShared.structuredFragmentNameAndTypeCondition
      fragmentText
  in

  CommandLineInvocationState.startedStructuredFragmentDefinition parserState
    fragmentName fragmentTypeCondition

let handleSelectionSet parserState expressionText =
  let selectionPaths = SelectionSet.parsedExpressions [ expressionText ] in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      CommandLineInvocationBranch.withAddedBranchSelectionPaths branch
        selectionPaths)
    ~onNoBranch:(fun state ->
      CommandLineInvocationState.withAddedRootSelectionPaths state
        selectionPaths)

let handleOperationName parserState operationName =
  withCurrentOperationOrRaise ~optionDescription:"--operation-name" parserState
    (fun operationDefinition ->
      { operationDefinition with operationName = Some operationName })

let handleSelectedOperationName parserState selectedOperationName =
  { parserState with requestedOperationName = Some selectedOperationName }

let handleVariableDefinition parserState variableDefinition =
  withCurrentOperationOrRaise ~optionDescription:"--variable-definition"
    parserState (fun operationDefinition ->
      {
        operationDefinition with
        variableDefinitions =
          operationDefinition.variableDefinitions @ [ variableDefinition ];
      })

let handleVariable parserState variableAssignmentText =
  withCurrentOperationOrRaise ~optionDescription:"--variable" parserState
    (fun operationDefinition ->
      {
        operationDefinition with
        variableAssignments =
          operationDefinition.variableAssignments
          @ [
              CommandLineInvocationShared.variableAssignmentOfText
                variableAssignmentText;
            ];
      })

let handleDirective parserState directiveText =
  let directiveText = GraphQlSelection.renderDirectiveText directiveText in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      CommandLineInvocationBranch.withAddedBranchDirective branch directiveText)
    ~onNoBranch:(fun state ->
      CommandLineInvocationState.withAddedCurrentTargetDirective state
        directiveText)

let handleAlias parserState fieldAlias =
  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      CommandLineInvocationBranch.withUpdatedBranchFieldAlias branch fieldAlias)
    ~onNoBranch:(fun _ ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "--alias requires an existing selection branch.\n\n%s"
              CommandLineInvocationShared.usageText)))

let handleFragmentDefinition parserState fragmentDefinitionText =
  let fragmentDefinitionText = String.trim fragmentDefinitionText in

  if fragmentDefinitionText = "" then
    invalid_arg "Fragment definition text cannot be empty.";

  let finalizedParserState =
    CommandLineInvocationState.finalizedAllPending parserState
  in

  {
    finalizedParserState with
    pendingRawFragmentDefinitionTexts =
      finalizedParserState.pendingRawFragmentDefinitionTexts
      @ [ fragmentDefinitionText ];
  }

let handleFieldArgument parserState (name, rawValue) =
  let fieldArgumentPair = (name, CliArgument.valueOfRawValue rawValue) in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      CommandLineInvocationBranch.withAddedBranchFieldArgumentPair branch
        fieldArgumentPair)
    ~onNoBranch:(fun _ ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "Field argument %s requires an existing selection branch.\n\n%s"
              (fst fieldArgumentPair) CommandLineInvocationShared.usageText)))

let optionHandlers =
  [
    (CommandLineInvocationShared.operationOptionName, handleOperation);
    ( CommandLineInvocationShared.fieldOptionName,
      handleSelectionPath
        CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFieldPath );
    ( CommandLineInvocationShared.inlineFragmentOptionName,
      handleSelectionPath
        CommandLineInvocationBranch
        .resolvedSelectionPathSegmentsOfInlineFragment );
    ( CommandLineInvocationShared.fragmentSpreadOptionName,
      handleSelectionPath
        CommandLineInvocationBranch
        .resolvedSelectionPathSegmentsOfFragmentSpread );
    ( CommandLineInvocationShared.fragmentSpreadCompatibilityOptionName,
      handleSelectionPath
        CommandLineInvocationBranch
        .resolvedSelectionPathSegmentsOfFragmentSpread );
    (CommandLineInvocationShared.fragmentOptionName, handleFragment);
    (CommandLineInvocationShared.selectionSetOptionName, handleSelectionSet);
    ( CommandLineInvocationShared.compatibilitySelectionSetOptionName,
      handleSelectionSet );
    (CommandLineInvocationShared.operationNameOptionName, handleOperationName);
    ( CommandLineInvocationShared.selectedOperationNameOptionName,
      handleSelectedOperationName );
    ( CommandLineInvocationShared.variableDefinitionOptionName,
      handleVariableDefinition );
    (CommandLineInvocationShared.variableOptionName, handleVariable);
    (CommandLineInvocationShared.directiveOptionName, handleDirective);
    (CommandLineInvocationShared.aliasOptionName, handleAlias);
    ( CommandLineInvocationShared.fragmentDefinitionOptionName,
      handleFragmentDefinition );
  ]

let handlePositionalToken parserState token =
  match
    CommandLineInvocationBranch.currentSelectionBranchOfState parserState
  with
  | Some branch
    when CommandLineInvocationBranch.currentDefaultSelectionPathPrefix
           parserState
         = []
         && branch.selectionPaths <> [] ->
      let finalizedParserState =
        CommandLineInvocationState.finalizedCurrentBranch parserState
      in

      replaceCurrentSelectionBranchWithSegments finalizedParserState
        (CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFieldPath
           finalizedParserState token)
  | Some branch ->
      CommandLineInvocationState.withUpdatedCurrentSelectionBranch parserState
        (CommandLineInvocationBranch.withPushedFieldSelectionPathSegment branch
           token)
  | None -> (
      match parserState.currentTarget with
      | None | Some (FragmentTarget _) -> (
          match CommandLineInvocationShared.operationTypeOfToken token with
          | Some operationType ->
              CommandLineInvocationState.startedOperationDefinition parserState
                operationType None
          | None ->
              CommandLineInvocationState.startedShorthandQueryOperation
                parserState token)
      | Some (OperationTarget _) ->
          replaceCurrentSelectionBranchWithSegments parserState
            (CommandLineInvocationBranch
             .resolvedSelectionPathSegmentsOfFieldPath parserState token))

let rec parseRemainingArguments parserState = function
  | [] -> CommandLineInvocationState.currentInvocationOfState parserState
  | "--help" :: _ -> raise HelpRequested
  | token :: remainingTokens when CommandLineInvocationShared.isLongOption token
    ->
      let updatedParserState, remainingTokens =
        match List.assoc_opt (bareOptionNameOfToken token) optionHandlers with
        | Some handler ->
            let optionValue, remainingTokens =
              CommandLineInvocationShared.valueOfOptionToken token
                remainingTokens
            in

            (handler parserState optionValue, remainingTokens)
        | None ->
            let fieldArgumentPair, remainingTokens =
              CommandLineInvocationShared.optionPairOfToken token
                remainingTokens
            in

            (handleFieldArgument parserState fieldArgumentPair, remainingTokens)
      in

      parseRemainingArguments updatedParserState remainingTokens
  | token :: remainingTokens ->
      parseRemainingArguments
        (handlePositionalToken parserState token)
        remainingTokens

let invocationOfArguments arguments =
  match arguments with
  | [] -> Error CommandLineInvocationShared.usageText
  | "help" :: _ | "--help" :: _ -> Ok `Help
  | _ -> (
      try
        let invocation =
          parseRemainingArguments CommandLineInvocationState.initialParserState
            arguments
        in

        Result.map
          (fun invocation -> `Invocation invocation)
          (CommandLineInvocationValidation.validatedInvocation invocation)
      with
      | HelpRequested -> Ok `Help
      | Invalid_argument message -> Error message
      | Yojson.Json_error message -> Error ("Invalid JSON: " ^ message))
