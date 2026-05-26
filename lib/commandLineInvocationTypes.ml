type operationType = GraphQlOperation.operationType =
  | Query
  | Mutation
  | Subscription

type fieldSegment = {
  fieldName : string;
  fieldAlias : string option;
  fieldArgumentPairs : (string * string) list;
  fieldDirectiveTexts : string list;
}

type inlineFragmentSegment = {
  inlineFragmentTypeCondition : string;
  inlineFragmentDirectiveTexts : string list;
}

type fragmentSpreadSegment = {
  fragmentSpreadName : string;
  fragmentSpreadDirectiveTexts : string list;
}

type selectionPathSegment =
  | FieldSegment of fieldSegment
  | InlineFragmentSegment of inlineFragmentSegment
  | FragmentSpreadSegment of fragmentSpreadSegment

type selectionBranch = {
  selectionPathSegments : selectionPathSegment list;
  selectionExpressions : string list;
}

type operationDefinition = {
  operationType : operationType;
  operationName : string option;
  variableDefinitions : string list;
  variableAssignments : (string * string) list;
  operationDirectiveTexts : string list;
  rootSelectionExpressions : string list;
  selectionBranches : selectionBranch list;
}

type structuredFragmentDefinition = {
  fragmentName : string;
  fragmentTypeCondition : string;
  fragmentDirectiveTexts : string list;
  fragmentRootSelectionExpressions : string list;
  fragmentSelectionBranches : selectionBranch list;
}

type t = {
  operationDefinitions : operationDefinition list;
  selectedOperationName : string option;
  structuredFragmentDefinitions : structuredFragmentDefinition list;
  rawFragmentDefinitionTexts : string list;
}

type selectionBranchBuilder = {
  builderCurrentSelectionPathSegment : selectionPathSegment;
  builderPreviousSelectionPathSegments : selectionPathSegment list;
  builderSelectionExpressions : string list;
}

type selectionTargetBuilder = {
  builderRootSelectionExpressions : string list;
  builderCurrentSelectionBranch : selectionBranchBuilder option;
  builderSelectionBranches : selectionBranch list;
}

type operationDefinitionBuilder = {
  builderOperationType : operationType;
  builderOperationName : string option;
  builderVariableDefinitions : string list;
  builderVariableAssignments : (string * string) list;
  builderOperationDirectiveTexts : string list;
  builderOperationSelectionTarget : selectionTargetBuilder;
  builderDefaultSelectionPathPrefix : selectionPathSegment list;
}

type structuredFragmentDefinitionBuilder = {
  builderFragmentName : string;
  builderFragmentTypeCondition : string;
  builderFragmentDirectiveTexts : string list;
  builderFragmentSelectionTarget : selectionTargetBuilder;
}

type parserState = {
  finalizedOperationDefinitions : operationDefinition list;
  currentOperationDefinition : operationDefinitionBuilder option;
  requestedOperationName : string option;
  finalizedStructuredFragmentDefinitions : structuredFragmentDefinition list;
  currentStructuredFragmentDefinition :
    structuredFragmentDefinitionBuilder option;
  pendingRawFragmentDefinitionTexts : string list;
}
