open CommandLineInvocationTypes

let raiseMissing message =
  raise
    (Invalid_argument
       (Printf.sprintf "%s\n\n%s" message CommandLineInvocationShared.usageText))

let withMappedCurrentTarget parserState ~onFragment ~onOperation ~onNeither =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragment ->
      {
        parserState with
        currentStructuredFragmentDefinition = Some (onFragment fragment);
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinition ->
          {
            parserState with
            currentOperationDefinition = Some (onOperation operationDefinition);
          }
      | None -> onNeither parserState)

let finalizedCurrentBranch parserState =
  match parserState.currentBranch with
  | None -> parserState
  | Some branch ->
      let parserState =
        withMappedCurrentTarget parserState
          ~onFragment:(fun fragment ->
            {
              fragment with
              fragmentSelectionBranches =
                fragment.fragmentSelectionBranches @ [ branch ];
            })
          ~onOperation:(fun operationDefinition ->
            {
              operationDefinition with
              selectionBranches =
                operationDefinition.selectionBranches @ [ branch ];
            })
          ~onNeither:(fun parserState -> parserState)
      in
      { parserState with currentBranch = None }

let finalizedCurrentStructuredFragmentDefinition parserState =
  let parserState = finalizedCurrentBranch parserState in
  match parserState.currentStructuredFragmentDefinition with
  | None -> parserState
  | Some fragment ->
      {
        parserState with
        currentStructuredFragmentDefinition = None;
        finalizedStructuredFragmentDefinitions =
          parserState.finalizedStructuredFragmentDefinitions @ [ fragment ];
      }

let finalizedCurrentOperationDefinition parserState =
  let parserState = finalizedCurrentBranch parserState in
  match parserState.currentOperationDefinition with
  | None -> parserState
  | Some operationDefinition ->
      {
        parserState with
        currentOperationDefinition = None;
        currentDefaultSelectionPathPrefix = [];
        finalizedOperationDefinitions =
          parserState.finalizedOperationDefinitions @ [ operationDefinition ];
      }

let finalizedAllPending parserState =
  parserState |> finalizedCurrentBranch
  |> finalizedCurrentStructuredFragmentDefinition
  |> finalizedCurrentOperationDefinition

let withUpdatedCurrentSelectionBranch parserState branch =
  if
    parserState.currentOperationDefinition = None
    && parserState.currentStructuredFragmentDefinition = None
  then raiseMissing "Selections require an operation or fragment context."
  else { parserState with currentBranch = Some branch }

let withAddedRootSelectionExpression parserState selectionExpression =
  withMappedCurrentTarget parserState
    ~onFragment:(fun fragment ->
      {
        fragment with
        fragmentRootSelectionExpressions =
          fragment.fragmentRootSelectionExpressions @ [ selectionExpression ];
      })
    ~onOperation:(fun operationDefinition ->
      {
        operationDefinition with
        rootSelectionExpressions =
          operationDefinition.rootSelectionExpressions @ [ selectionExpression ];
      })
    ~onNeither:(fun _ ->
      raiseMissing "Selections require an operation or fragment context.")

let withAddedCurrentTargetDirective parserState directiveText =
  withMappedCurrentTarget parserState
    ~onFragment:(fun fragment ->
      {
        fragment with
        fragmentDirectiveTexts =
          fragment.fragmentDirectiveTexts @ [ directiveText ];
      })
    ~onOperation:(fun operationDefinition ->
      {
        operationDefinition with
        operationDirectiveTexts =
          operationDefinition.operationDirectiveTexts @ [ directiveText ];
      })
    ~onNeither:(fun _ ->
      raiseMissing "Directives require an operation or fragment context.")

let startedOperationDefinition parserState operationType operationName =
  let parserState = finalizedAllPending parserState in
  {
    parserState with
    currentOperationDefinition =
      Some
        {
          operationType;
          operationName;
          variableDefinitions = [];
          variableAssignments = [];
          operationDirectiveTexts = [];
          rootSelectionExpressions = [];
          selectionBranches = [];
        };
    currentDefaultSelectionPathPrefix = [];
  }

let startedShorthandQueryOperation parserState firstFieldToken =
  let branch =
    CommandLineInvocationBranch.makeSelectionBranchFromFieldPath firstFieldToken
  in
  {
    (startedOperationDefinition parserState Query None) with
    currentBranch = Some branch;
    currentDefaultSelectionPathPrefix = branch.selectionPathSegments;
  }

let startedStructuredFragmentDefinition parserState fragmentName
    fragmentTypeCondition =
  let parserState = finalizedAllPending parserState in
  {
    parserState with
    currentStructuredFragmentDefinition =
      Some
        {
          fragmentName;
          fragmentTypeCondition;
          fragmentDirectiveTexts = [];
          fragmentRootSelectionExpressions = [];
          fragmentSelectionBranches = [];
        };
  }

let initialParserState =
  {
    finalizedOperationDefinitions = [];
    currentOperationDefinition = None;
    currentDefaultSelectionPathPrefix = [];
    currentBranch = None;
    requestedOperationName = None;
    finalizedStructuredFragmentDefinitions = [];
    currentStructuredFragmentDefinition = None;
    pendingRawFragmentDefinitionTexts = [];
  }

let currentInvocationOfState parserState =
  let parserState = finalizedAllPending parserState in
  {
    operationDefinitions = parserState.finalizedOperationDefinitions;
    selectedOperationName = parserState.requestedOperationName;
    structuredFragmentDefinitions =
      parserState.finalizedStructuredFragmentDefinitions;
    rawFragmentDefinitionTexts = parserState.pendingRawFragmentDefinitionTexts;
  }
