open CommandLineInvocationTypes

let bareOptionNameOfToken token =
  match CommandLineInvocationShared.optionNameAndValueOfEqualsSyntax token with
  | Some (optionName, _) -> optionName
  | None -> String.sub token 2 (String.length token - 2)

let replaceCurrentSelectionBranchWithSegments parserState resolvedSegments =
  CommandLineInvocationState.withUpdatedCurrentSelectionBranch
    (CommandLineInvocationState.finalizedCurrentBranch parserState)
    (CommandLineInvocationBranch.makeSelectionBranchFromSegments
       resolvedSegments)

let withCurrentOperationOrRaise ~optionDescription parserState mapOperation =
  match parserState.currentOperationDefinition with
  | Some operationDefinition ->
      {
        parserState with
        currentOperationDefinition = Some (mapOperation operationDefinition);
      }
  | None ->
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

let handleSelectionSet parserState selectionExpression =
  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      CommandLineInvocationBranch.withAddedBranchSelectionExpression branch
        selectionExpression)
    ~onNoBranch:(fun state ->
      CommandLineInvocationState.withAddedRootSelectionExpression state
        selectionExpression)

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
  let finalizedParserState =
    CommandLineInvocationState.finalizedAllPending parserState
  in
  {
    finalizedParserState with
    pendingRawFragmentDefinitionTexts =
      finalizedParserState.pendingRawFragmentDefinitionTexts
      @ [ fragmentDefinitionText ];
  }

let handleFieldArgument parserState fieldArgumentPair =
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
         && branch.selectionExpressions <> [] ->
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
      match parserState.currentOperationDefinition with
      | None -> (
          match CommandLineInvocationShared.operationTypeOfToken token with
          | Some operationType ->
              CommandLineInvocationState.startedOperationDefinition parserState
                operationType None
          | None ->
              CommandLineInvocationState.startedShorthandQueryOperation
                parserState token)
      | Some _ ->
          replaceCurrentSelectionBranchWithSegments parserState
            (CommandLineInvocationBranch
             .resolvedSelectionPathSegmentsOfFieldPath parserState token))

let rec parseRemainingArguments parserState = function
  | [] -> CommandLineInvocationState.currentInvocationOfState parserState
  | "--help" :: _ ->
      raise (Invalid_argument CommandLineInvocationShared.usageText)
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
  | firstToken :: _ when firstToken = "help" || firstToken = "--help" ->
      Error CommandLineInvocationShared.usageText
  | _ -> (
      try
        let invocation =
          parseRemainingArguments CommandLineInvocationState.initialParserState
            arguments
        in
        CommandLineInvocationValidation.validatedInvocation invocation
      with Invalid_argument message -> Error message)
