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
      CommandLineInvocationState.withUpdatedCurrentOperationDefinition
        parserState
        (mapOperation operationDefinition)
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

let handleOperation parserState token remainingTokens =
  let operationHeaderText, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  let operationType, operationName =
    CommandLineInvocationShared.operationHeaderOfText operationHeaderText
  in
  ( CommandLineInvocationState.startedOperationDefinition parserState
      operationType operationName,
    remainingTokens )

let handleField parserState token remainingTokens =
  let fieldPath, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  let resolvedSegments =
    CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFieldPath
      parserState fieldPath
  in
  ( replaceCurrentSelectionBranchWithSegments parserState resolvedSegments,
    remainingTokens )

let handleInlineFragment parserState token remainingTokens =
  let inlineFragmentTypeCondition, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  let resolvedSegments =
    CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfInlineFragment
      parserState inlineFragmentTypeCondition
  in
  ( replaceCurrentSelectionBranchWithSegments parserState resolvedSegments,
    remainingTokens )

let handleFragmentSpread parserState token remainingTokens =
  let fragmentSpreadText, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  let resolvedSegments =
    CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFragmentSpread
      parserState fragmentSpreadText
  in
  ( replaceCurrentSelectionBranchWithSegments parserState resolvedSegments,
    remainingTokens )

let handleFragment parserState token remainingTokens =
  let fragmentText, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  let fragmentName, fragmentTypeCondition =
    CommandLineInvocationShared.structuredFragmentNameAndTypeCondition
      fragmentText
  in
  ( CommandLineInvocationState.startedStructuredFragmentDefinition parserState
      fragmentName fragmentTypeCondition,
    remainingTokens )

let handleSelectionSet parserState token remainingTokens =
  let selectionExpression, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  ( updateBranchOrFallback parserState
      ~onBranch:(fun branch ->
        CommandLineInvocationBranch.withAddedBranchSelectionExpression branch
          selectionExpression)
      ~onNoBranch:(fun state ->
        CommandLineInvocationState.withAddedRootSelectionExpression state
          selectionExpression),
    remainingTokens )

let handleOperationName parserState token remainingTokens =
  let operationName, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  ( withCurrentOperationOrRaise ~optionDescription:"--operation-name" parserState
      (fun operationDefinition ->
        { operationDefinition with operationName = Some operationName }),
    remainingTokens )

let handleSelectedOperationName parserState token remainingTokens =
  let selectedOperationName, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  ( { parserState with requestedOperationName = Some selectedOperationName },
    remainingTokens )

let handleVariableDefinition parserState token remainingTokens =
  let variableDefinition, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  ( withCurrentOperationOrRaise ~optionDescription:"--variable-definition"
      parserState (fun operationDefinition ->
        {
          operationDefinition with
          variableDefinitions =
            operationDefinition.variableDefinitions @ [ variableDefinition ];
        }),
    remainingTokens )

let handleVariable parserState token remainingTokens =
  let variableAssignmentText, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  ( withCurrentOperationOrRaise ~optionDescription:"--variable" parserState
      (fun operationDefinition ->
        {
          operationDefinition with
          variableAssignments =
            operationDefinition.variableAssignments
            @ [
                CommandLineInvocationShared.variableAssignmentOfText
                  variableAssignmentText;
              ];
        }),
    remainingTokens )

let handleDirective parserState token remainingTokens =
  let directiveText, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  ( updateBranchOrFallback parserState
      ~onBranch:(fun branch ->
        CommandLineInvocationBranch.withAddedBranchDirective branch
          directiveText)
      ~onNoBranch:(fun state ->
        CommandLineInvocationState.withAddedCurrentTargetDirective state
          directiveText),
    remainingTokens )

let handleAlias parserState token remainingTokens =
  let fieldAlias, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  ( updateBranchOrFallback parserState
      ~onBranch:(fun branch ->
        CommandLineInvocationBranch.withUpdatedBranchFieldAlias branch
          fieldAlias)
      ~onNoBranch:(fun _ ->
        raise
          (Invalid_argument
             (Printf.sprintf
                "--alias requires an existing selection branch.\n\n%s"
                CommandLineInvocationShared.usageText))),
    remainingTokens )

let handleFragmentDefinition parserState token remainingTokens =
  let fragmentDefinitionText, remainingTokens =
    CommandLineInvocationShared.valueOfOptionToken token remainingTokens
  in
  let finalizedParserState =
    CommandLineInvocationState.finalizedAllPending parserState
  in
  ( {
      finalizedParserState with
      pendingRawFragmentDefinitionTexts =
        finalizedParserState.pendingRawFragmentDefinitionTexts
        @ [ fragmentDefinitionText ];
    },
    remainingTokens )

let handleFieldArgument parserState token remainingTokens =
  let fieldArgumentPair, remainingTokens =
    CommandLineInvocationShared.optionPairOfToken token remainingTokens
  in
  ( updateBranchOrFallback parserState
      ~onBranch:(fun branch ->
        CommandLineInvocationBranch.withAddedBranchFieldArgumentPair branch
          fieldArgumentPair)
      ~onNoBranch:(fun _ ->
        raise
          (Invalid_argument
             (Printf.sprintf
                "Field argument %s requires an existing selection branch.\n\n%s"
                (fst fieldArgumentPair) CommandLineInvocationShared.usageText))),
    remainingTokens )

let optionHandlers =
  [
    (CommandLineInvocationShared.operationOptionName, handleOperation);
    (CommandLineInvocationShared.fieldOptionName, handleField);
    (CommandLineInvocationShared.inlineFragmentOptionName, handleInlineFragment);
    (CommandLineInvocationShared.fragmentSpreadOptionName, handleFragmentSpread);
    ( CommandLineInvocationShared.fragmentSpreadCompatibilityOptionName,
      handleFragmentSpread );
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

let handlePositionalToken parserState token remainingTokens =
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
      let resolvedSegments =
        CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFieldPath
          finalizedParserState token
      in
      ( CommandLineInvocationState.withUpdatedCurrentSelectionBranch
          finalizedParserState
          (CommandLineInvocationBranch.makeSelectionBranchFromSegments
             resolvedSegments),
        remainingTokens )
  | Some branch ->
      ( CommandLineInvocationState.withUpdatedCurrentSelectionBranch parserState
          (CommandLineInvocationBranch.withPushedFieldSelectionPathSegment
             branch token),
        remainingTokens )
  | None -> (
      match parserState.currentOperationDefinition with
      | None -> (
          match CommandLineInvocationShared.operationTypeOfToken token with
          | Some operationType ->
              ( CommandLineInvocationState.startedOperationDefinition
                  parserState operationType None,
                remainingTokens )
          | None ->
              ( CommandLineInvocationState.startedShorthandQueryOperation
                  parserState token,
                remainingTokens ))
      | Some _ ->
          let resolvedSegments =
            CommandLineInvocationBranch.resolvedSelectionPathSegmentsOfFieldPath
              parserState token
          in
          ( CommandLineInvocationState.withUpdatedCurrentSelectionBranch
              parserState
              (CommandLineInvocationBranch.makeSelectionBranchFromSegments
                 resolvedSegments),
            remainingTokens ))

let rec parseRemainingArguments parserState = function
  | [] -> CommandLineInvocationState.currentInvocationOfState parserState
  | "--help" :: _ ->
      raise (Invalid_argument CommandLineInvocationShared.usageText)
  | token :: remainingTokens when CommandLineInvocationShared.isLongOption token
    ->
      let handler =
        match List.assoc_opt (bareOptionNameOfToken token) optionHandlers with
        | Some handler -> handler
        | None -> handleFieldArgument
      in
      let updatedParserState, remainingTokens =
        handler parserState token remainingTokens
      in
      parseRemainingArguments updatedParserState remainingTokens
  | token :: remainingTokens ->
      let updatedParserState, remainingTokens =
        handlePositionalToken parserState token remainingTokens
      in
      parseRemainingArguments updatedParserState remainingTokens

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
