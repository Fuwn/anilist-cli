type operationType = GraphQlOperation.operationType =
  | Query
  | Mutation
  | Subscription

type fieldSegment = {
  fieldName : string;
  fieldAlias : string option;
  fieldArgumentPairs : (string * CliArgument.value) list;
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

type scopedSelectionPath = [ `Relative | `Root ] * selectionPathSegment list

type selectionBranch = {
  selectionPathSegments : selectionPathSegment list;
  selectionPaths : scopedSelectionPath list;
}

type operationDefinition = {
  operationType : operationType;
  operationName : string option;
  variableDefinitions : string list;
  variableAssignments : (string * CliArgument.value) list;
  operationDirectiveTexts : string list;
  rootSelectionPaths : scopedSelectionPath list;
  selectionBranches : selectionBranch list;
}

type structuredFragmentDefinition = {
  fragmentName : string;
  fragmentTypeCondition : string;
  fragmentDirectiveTexts : string list;
  fragmentRootSelectionPaths : scopedSelectionPath list;
  fragmentSelectionBranches : selectionBranch list;
}

type t = {
  operationDefinitions : operationDefinition list;
  selectedOperationName : string option;
  structuredFragmentDefinitions : structuredFragmentDefinition list;
  rawFragmentDefinitionTexts : string list;
}

type parserState = {
  finalizedOperationDefinitions : operationDefinition list;
  currentOperationDefinition : operationDefinition option;
  currentDefaultSelectionPathPrefix : selectionPathSegment list;
  currentBranch : selectionBranch option;
  requestedOperationName : string option;
  finalizedStructuredFragmentDefinitions : structuredFragmentDefinition list;
  currentStructuredFragmentDefinition : structuredFragmentDefinition option;
  pendingRawFragmentDefinitionTexts : string list;
}
