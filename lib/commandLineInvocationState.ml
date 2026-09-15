open CommandLineInvocationTypes

let raiseMissing message =
  raise
    (Invalid_argument
       (Printf.sprintf "%s\n\n%s" message CommandLineInvocationShared.usageText))

let withMappedCurrentTarget parserState ~onFragment ~onOperation ~onNeither =
  match parserState.currentTarget with
  | Some (FragmentTarget fragment) ->
      {
        parserState with
        currentTarget = Some (FragmentTarget (onFragment fragment));
      }
  | Some (OperationTarget operation) ->
      {
        parserState with
        currentTarget = Some (OperationTarget (onOperation operation));
      }
  | None -> onNeither parserState

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

let finalizedAllPending parserState =
  let parserState = finalizedCurrentBranch parserState in
  let finalizedState =
    match parserState.currentTarget with
    | None -> parserState
    | Some (FragmentTarget fragment) ->
        {
          parserState with
          finalizedStructuredFragmentDefinitions =
            fragment :: parserState.finalizedStructuredFragmentDefinitions;
        }
    | Some (OperationTarget operation) ->
        {
          parserState with
          finalizedOperationDefinitions =
            operation :: parserState.finalizedOperationDefinitions;
        }
  in

  {
    finalizedState with
    currentTarget = None;
    currentDefaultSelectionPathPrefix = [];
  }

let withUpdatedCurrentSelectionBranch parserState branch =
  if parserState.currentTarget = None then
    raiseMissing "Selections require an operation or fragment context."
  else { parserState with currentBranch = Some branch }

let withAddedRootSelectionPaths parserState selectionPaths =
  withMappedCurrentTarget parserState
    ~onFragment:(fun fragment ->
      {
        fragment with
        fragmentRootSelectionPaths =
          fragment.fragmentRootSelectionPaths @ selectionPaths;
      })
    ~onOperation:(fun operationDefinition ->
      {
        operationDefinition with
        rootSelectionPaths =
          operationDefinition.rootSelectionPaths @ selectionPaths;
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
    currentTarget =
      Some
        (OperationTarget
           {
             operationType;
             operationName;
             variableDefinitions = [];
             variableAssignments = [];
             operationDirectiveTexts = [];
             rootSelectionPaths = [];
             selectionBranches = [];
           });
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
    currentTarget =
      Some
        (FragmentTarget
           {
             fragmentName;
             fragmentTypeCondition;
             fragmentDirectiveTexts = [];
             fragmentRootSelectionPaths = [];
             fragmentSelectionBranches = [];
           });
  }

let initialParserState =
  {
    finalizedOperationDefinitions = [];
    currentTarget = None;
    currentDefaultSelectionPathPrefix = [];
    currentBranch = None;
    requestedOperationName = None;
    finalizedStructuredFragmentDefinitions = [];
    pendingRawFragmentDefinitionTexts = [];
  }

let currentInvocationOfState parserState =
  let parserState = finalizedAllPending parserState in

  {
    operationDefinitions = List.rev parserState.finalizedOperationDefinitions;
    selectedOperationName = parserState.requestedOperationName;
    structuredFragmentDefinitions =
      List.rev parserState.finalizedStructuredFragmentDefinitions;
    rawFragmentDefinitionTexts = parserState.pendingRawFragmentDefinitionTexts;
  }
