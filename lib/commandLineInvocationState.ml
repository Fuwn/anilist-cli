open CommandLineInvocationTypes

let finalizedCurrentBranch parserState =
  match parserState.currentBranch with
  | None -> parserState
  | Some branch -> (
      match parserState.currentStructuredFragmentDefinition with
      | Some fragment ->
          {
            parserState with
            currentBranch = None;
            currentStructuredFragmentDefinition =
              Some
                {
                  fragment with
                  fragmentSelectionBranches =
                    fragment.fragmentSelectionBranches @ [ branch ];
                };
          }
      | None -> (
          match parserState.currentOperationDefinition with
          | Some operationDefinition ->
              {
                parserState with
                currentBranch = None;
                currentOperationDefinition =
                  Some
                    {
                      operationDefinition with
                      selectionBranches =
                        operationDefinition.selectionBranches @ [ branch ];
                    };
              }
          | None -> { parserState with currentBranch = None }))

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

let raiseMissing message =
  raise
    (Invalid_argument
       (Printf.sprintf "%s\n\n%s" message CommandLineInvocationShared.usageText))

let withUpdatedCurrentSelectionBranch parserState branch =
  if
    parserState.currentOperationDefinition = None
    && parserState.currentStructuredFragmentDefinition = None
  then raiseMissing "Selections require an operation or fragment context."
  else { parserState with currentBranch = Some branch }

let withAddedRootSelectionExpression parserState selectionExpression =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragment ->
      {
        parserState with
        currentStructuredFragmentDefinition =
          Some
            {
              fragment with
              fragmentRootSelectionExpressions =
                fragment.fragmentRootSelectionExpressions
                @ [ selectionExpression ];
            };
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinition ->
          {
            parserState with
            currentOperationDefinition =
              Some
                {
                  operationDefinition with
                  rootSelectionExpressions =
                    operationDefinition.rootSelectionExpressions
                    @ [ selectionExpression ];
                };
          }
      | None ->
          raiseMissing "Selections require an operation or fragment context.")

let withAddedCurrentTargetDirective parserState directiveText =
  match parserState.currentStructuredFragmentDefinition with
  | Some fragment ->
      {
        parserState with
        currentStructuredFragmentDefinition =
          Some
            {
              fragment with
              fragmentDirectiveTexts =
                fragment.fragmentDirectiveTexts @ [ directiveText ];
            };
      }
  | None -> (
      match parserState.currentOperationDefinition with
      | Some operationDefinition ->
          {
            parserState with
            currentOperationDefinition =
              Some
                {
                  operationDefinition with
                  operationDirectiveTexts =
                    operationDefinition.operationDirectiveTexts
                    @ [ directiveText ];
                };
          }
      | None ->
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
  let parserState = finalizedAllPending parserState in
  {
    parserState with
    currentOperationDefinition =
      Some
        {
          operationType = Query;
          operationName = None;
          variableDefinitions = [];
          variableAssignments = [];
          operationDirectiveTexts = [];
          rootSelectionExpressions = [];
          selectionBranches = [];
        };
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

let withUpdatedCurrentOperationDefinition parserState operationDefinition =
  match parserState.currentOperationDefinition with
  | Some _ ->
      { parserState with currentOperationDefinition = Some operationDefinition }
  | None -> raiseMissing "Operation options require a current operation."

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
