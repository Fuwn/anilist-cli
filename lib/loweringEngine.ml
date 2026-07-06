open CommandLineInvocationTypes

type request = {
  operations : GraphQlOperation.t list;
  selectedOperationName : string option;
  loweredVariableAssignments : (string * CliArgument.value) list;
  fragmentDefinitions : GraphQlFragmentDefinition.t list;
}

let graphQlArgumentsOfOptionPairs optionPairs =
  optionPairs
  |> List.map (fun (optionName, optionValue) ->
      CliArgument.make
        ~name:(GraphQlName.lowerCamelCaseOfCliToken optionName)
        ~rawValue:optionValue)

let valuesOfVariableAssignments variableAssignments =
  variableAssignments
  |> List.map (fun (variableName, rawValue) ->
      ( variableName,
        (CliArgument.make ~name:variableName ~rawValue).CliArgument.value ))

let graphQlFieldNameOfSegment ~isTargetRoot fieldName =
  if isTargetRoot then GraphQlName.upperCamelCaseOfCliToken fieldName
  else GraphQlName.lowerCamelCaseOfCliToken fieldName

let graphQlTypeCondition typeCondition =
  GraphQlName.upperCamelCaseOfCliToken typeCondition

let rec lowerSelectionPathSegments ~isTargetRoot selectionPathSegments
    selectionSet =
  let lowerTail = function
    | [] -> selectionSet
    | remainingSelectionPathSegments ->
        [
          lowerSelectionPathSegments ~isTargetRoot:false
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
           ~selectionSet:(lowerTail remainingSelectionPathSegments)
           ())
  | InlineFragmentSegment
      ({ inlineFragmentTypeCondition; inlineFragmentDirectiveTexts } :
        inlineFragmentSegment)
    :: remainingSelectionPathSegments ->
      GraphQlSelection.inlineFragment
        (GraphQlSelection.makeInlineFragment
           ~typeCondition:(graphQlTypeCondition inlineFragmentTypeCondition)
           ~directives:inlineFragmentDirectiveTexts
           ~selectionSet:(lowerTail remainingSelectionPathSegments)
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

let lowerSelectionBranch ~capitalizeRootFieldNames ~isTargetRoot selectionBranch
    =
  let scopedSelection =
    SelectionSet.ofCliArguments ~capitalizeRelativeRootFieldNames:false
      ~capitalizeRootFieldNames selectionBranch.selectionExpressions
  in
  let loweredSelection =
    lowerSelectionPathSegments ~isTargetRoot
      selectionBranch.selectionPathSegments
      scopedSelection.SelectionSet.relativeSelectionSet
  in
  (loweredSelection, scopedSelection.SelectionSet.rootSelectionSet)

let selectionSetOfTarget ~capitalizeRootFieldNames ~rootSelectionExpressions
    ~selectionBranches =
  let rootScopedSelection =
    SelectionSet.ofCliArguments
      ~capitalizeRelativeRootFieldNames:capitalizeRootFieldNames
      ~capitalizeRootFieldNames rootSelectionExpressions
  in
  let loweredBranches =
    selectionBranches
    |> List.map
         (lowerSelectionBranch ~capitalizeRootFieldNames
            ~isTargetRoot:capitalizeRootFieldNames)
  in
  let branchSelectionSet =
    loweredBranches
    |> List.map (fun (selection, _) -> [ selection ])
    |> SelectionSet.merge SelectionSet.empty
  in
  let branchRootSelectionSet =
    loweredBranches |> List.map snd |> SelectionSet.merge SelectionSet.empty
  in
  SelectionSet.merge SelectionSet.empty
    [
      rootScopedSelection.SelectionSet.relativeSelectionSet;
      rootScopedSelection.SelectionSet.rootSelectionSet;
      branchSelectionSet;
      branchRootSelectionSet;
    ]

let lowerStructuredFragmentDefinition structuredFragmentDefinition =
  let selectionSet =
    selectionSetOfTarget ~capitalizeRootFieldNames:false
      ~rootSelectionExpressions:
        structuredFragmentDefinition.fragmentRootSelectionExpressions
      ~selectionBranches:structuredFragmentDefinition.fragmentSelectionBranches
  in
  GraphQlFragmentDefinition.make ~name:structuredFragmentDefinition.fragmentName
    ~typeCondition:
      (graphQlTypeCondition structuredFragmentDefinition.fragmentTypeCondition)
    ~directives:structuredFragmentDefinition.fragmentDirectiveTexts
    ~selectionSet ()

let lowerOperationDefinition
    ({
       operationType;
       operationName;
       variableDefinitions;
       variableAssignments;
       operationDirectiveTexts;
       rootSelectionExpressions;
       selectionBranches;
     } :
      operationDefinition) =
  let operationSelectionSet =
    selectionSetOfTarget ~capitalizeRootFieldNames:true
      ~rootSelectionExpressions ~selectionBranches
  in
  let operation =
    GraphQlOperation.make ?name:operationName ~variableDefinitions
      ~directives:operationDirectiveTexts ~operationType
      ~selectionSet:operationSelectionSet ()
  in
  (operation, valuesOfVariableAssignments variableAssignments)

let selectedOperationAndVariables operations selectedOperationName =
  match selectedOperationName with
  | None -> (None, snd (List.hd operations))
  | Some selectedOperationName ->
      let _, variableAssignments =
        operations
        |> List.find (fun (operation, _) ->
            operation.GraphQlOperation.name = Some selectedOperationName)
      in
      (Some selectedOperationName, variableAssignments)

(* Assumes a validated invocation (see CommandLineInvocationValidation):
   operation names are unique, and the selected operation name exists whenever
   the document requires one. *)
let lower (invocation : CommandLineInvocationTypes.t) =
  let loweredOperations =
    invocation.operationDefinitions |> List.map lowerOperationDefinition
  in
  let selectedOperationName, loweredVariableAssignments =
    selectedOperationAndVariables loweredOperations
      invocation.CommandLineInvocationTypes.selectedOperationName
  in
  let fragmentDefinitions =
    (invocation.structuredFragmentDefinitions
    |> List.map lowerStructuredFragmentDefinition)
    @ (invocation.rawFragmentDefinitionTexts
      |> List.map GraphQlFragmentDefinition.makeRaw)
  in
  {
    operations = loweredOperations |> List.map fst;
    selectedOperationName;
    loweredVariableAssignments;
    fragmentDefinitions;
  }

let graphQlQueryOfRequest request =
  GraphQlQuery.make ~operations:request.operations
    ~fragmentDefinitions:request.fragmentDefinitions ()
