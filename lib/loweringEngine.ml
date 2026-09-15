open CommandLineInvocationTypes

type request = {
  operations : GraphQlOperation.t list;
  selectedOperationName : string option;
  loweredVariableAssignments : (string * CliArgument.value) list;
  fragmentDefinitions : GraphQlFragmentDefinition.t list;
}

let lowerSelectionBranch ~capitalizeRootFieldNames ~isTargetRoot selectionBranch
    =
  let selectionsRemainAtRoot =
    isTargetRoot
    && List.for_all
         (function
           | InlineFragmentSegment _ -> true
           | FieldSegment _ | FragmentSpreadSegment _ -> false)
         selectionBranch.selectionPathSegments
  in
  let scopedSelection =
    SelectionSet.ofPaths
      ~capitalizeRelativeRootFieldNames:selectionsRemainAtRoot
      ~capitalizeRootFieldNames selectionBranch.selectionPaths
  in
  let loweredSelection =
    SelectionPath.lowerSelectionPathSegments ~isTargetRoot
      selectionBranch.selectionPathSegments
      scopedSelection.SelectionSet.relativeSelectionSet
  in

  (loweredSelection, scopedSelection.SelectionSet.rootSelectionSet)

let selectionSetOfTarget ~capitalizeRootFieldNames ~rootSelectionPaths
    ~selectionBranches =
  let rootScopedSelection =
    SelectionSet.ofPaths
      ~capitalizeRelativeRootFieldNames:capitalizeRootFieldNames
      ~capitalizeRootFieldNames rootSelectionPaths
  in
  let loweredBranches =
    selectionBranches
    |> List.map
         (lowerSelectionBranch ~capitalizeRootFieldNames
            ~isTargetRoot:capitalizeRootFieldNames)
  in

  rootScopedSelection.SelectionSet.relativeSelectionSet
  @ rootScopedSelection.SelectionSet.rootSelectionSet
  @ List.map fst loweredBranches
  @ List.concat_map snd loweredBranches

let lowerStructuredFragmentDefinition structuredFragmentDefinition =
  let selectionSet =
    selectionSetOfTarget ~capitalizeRootFieldNames:false
      ~rootSelectionPaths:
        structuredFragmentDefinition.fragmentRootSelectionPaths
      ~selectionBranches:structuredFragmentDefinition.fragmentSelectionBranches
  in

  GraphQlFragmentDefinition.make ~name:structuredFragmentDefinition.fragmentName
    ~typeCondition:
      (SelectionPath.graphQlTypeCondition
         structuredFragmentDefinition.fragmentTypeCondition)
    ~directives:structuredFragmentDefinition.fragmentDirectiveTexts
    ~selectionSet ()

let lowerOperationDefinition
    ({
       operationType;
       operationName;
       variableDefinitions;
       variableAssignments;
       operationDirectiveTexts;
       rootSelectionPaths;
       selectionBranches;
     } :
      operationDefinition) =
  let operationSelectionSet =
    selectionSetOfTarget ~capitalizeRootFieldNames:true ~rootSelectionPaths
      ~selectionBranches
  in
  let operation =
    GraphQlOperation.make ?name:operationName ~variableDefinitions
      ~directives:operationDirectiveTexts ~operationType
      ~selectionSet:operationSelectionSet ()
  in

  (operation, variableAssignments)

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

(* The lowerer assumes a validated invocation (see CommandLineInvocationValidation):
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
