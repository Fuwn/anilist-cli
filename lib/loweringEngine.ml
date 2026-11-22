open CommandLineInvocation

type scopedFieldSelection = {
  relativeSelectionSet : GraphQlDocument.selection list;
  rootSelectionSet : GraphQlDocument.selection list;
}

let graphQlArgumentsOfOptionPairs optionPairs =
  optionPairs
  |> List.map (fun (optionName, optionValue) ->
      {
        CliArgument.name = GraphQlName.lowerCamelCaseOfCliToken optionName;
        value = optionValue;
      })

let graphQlFieldNameOfSegment ~isTargetRoot fieldName =
  if isTargetRoot then GraphQlName.upperCamelCaseOfCliToken fieldName
  else GraphQlName.lowerCamelCaseOfCliToken fieldName

let graphQlTypeCondition typeCondition =
  GraphQlName.upperCamelCaseOfCliToken typeCondition

let rec lowerSelectionPathSegments ~isTargetRoot selectionPathSegments
    selectionSet =
  let lowerTail ~isTargetRoot = function
    | [] -> selectionSet
    | remainingSelectionPathSegments ->
        [
          lowerSelectionPathSegments ~isTargetRoot
            remainingSelectionPathSegments selectionSet;
        ]
  in

  match selectionPathSegments with
  | [] ->
      raise (Invalid_argument "Expected at least one selection path segment")
  | FieldSegment
      { fieldName; fieldAlias; fieldArgumentPairs; fieldDirectiveTexts }
    :: remainingSelectionPathSegments ->
      GraphQlDocument.Field
        {
          alias = fieldAlias;
          name = graphQlFieldNameOfSegment ~isTargetRoot fieldName;
          arguments = graphQlArgumentsOfOptionPairs fieldArgumentPairs;
          directives = fieldDirectiveTexts;
          selectionSet =
            lowerTail ~isTargetRoot:false remainingSelectionPathSegments;
        }
  | InlineFragmentSegment
      { inlineFragmentTypeCondition; inlineFragmentDirectiveTexts }
    :: remainingSelectionPathSegments ->
      GraphQlDocument.InlineFragment
        {
          typeCondition = graphQlTypeCondition inlineFragmentTypeCondition;
          directives = inlineFragmentDirectiveTexts;
          selectionSet = lowerTail ~isTargetRoot remainingSelectionPathSegments;
        }
  | FragmentSpreadSegment { fragmentSpreadName; fragmentSpreadDirectiveTexts }
    :: remainingSelectionPathSegments ->
      if remainingSelectionPathSegments <> [] then
        raise
          (Invalid_argument "Fragment spreads cannot contain child selections")
      else
        GraphQlDocument.FragmentSpread
          {
            name = fragmentSpreadName;
            directives = fragmentSpreadDirectiveTexts;
          }

let selectionsOfScopedPaths ~capitalizeRootFieldNames scopedPaths =
  scopedPaths
  |> List.map (fun (_, segments) ->
      lowerSelectionPathSegments ~isTargetRoot:capitalizeRootFieldNames segments
        [])

let scopedSelectionOfPaths ?(capitalizeRelativeRootFieldNames = false)
    ?(capitalizeRootFieldNames = false) scopedPaths =
  let relativePaths, rootPaths =
    List.partition (fun (scope, _) -> scope = `Relative) scopedPaths
  in

  {
    relativeSelectionSet =
      selectionsOfScopedPaths
        ~capitalizeRootFieldNames:capitalizeRelativeRootFieldNames relativePaths;
    rootSelectionSet =
      selectionsOfScopedPaths ~capitalizeRootFieldNames rootPaths;
  }

type request = {
  document : GraphQlDocument.t;
  selectedOperationName : string option;
  loweredVariableAssignments : (string * CliArgument.value) list;
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
    scopedSelectionOfPaths
      ~capitalizeRelativeRootFieldNames:selectionsRemainAtRoot
      ~capitalizeRootFieldNames selectionBranch.selectionPaths
  in
  let loweredSelection =
    lowerSelectionPathSegments ~isTargetRoot
      selectionBranch.selectionPathSegments scopedSelection.relativeSelectionSet
  in

  (loweredSelection, scopedSelection.rootSelectionSet)

let selectionSetOfBody ~capitalizeRootFieldNames body =
  let rootScopedSelection =
    scopedSelectionOfPaths
      ~capitalizeRelativeRootFieldNames:capitalizeRootFieldNames
      ~capitalizeRootFieldNames body.rootSelectionPaths
  in
  let loweredBranches =
    body.selectionBranches
    |> List.map
         (lowerSelectionBranch ~capitalizeRootFieldNames
            ~isTargetRoot:capitalizeRootFieldNames)
  in

  rootScopedSelection.relativeSelectionSet
  @ rootScopedSelection.rootSelectionSet
  @ List.map fst loweredBranches
  @ List.concat_map snd loweredBranches

let lowerStructuredFragmentDefinition
    { fragmentName; fragmentTypeCondition; fragmentBody } =
  let selectionSet =
    selectionSetOfBody ~capitalizeRootFieldNames:false fragmentBody
  in

  GraphQlDocument.Structured
    {
      name = fragmentName;
      typeCondition = graphQlTypeCondition fragmentTypeCondition;
      directives = fragmentBody.directiveTexts;
      selectionSet;
    }

let lowerOperationDefinition
    {
      operationType;
      operationName;
      variableDefinitions;
      variableAssignments;
      operationBody;
    } =
  let selectionSet =
    selectionSetOfBody ~capitalizeRootFieldNames:true operationBody
  in
  let operation =
    {
      GraphQlDocument.operationType;
      name = operationName;
      variableDefinitions;
      directives = operationBody.directiveTexts;
      selectionSet;
    }
  in

  (operation, variableAssignments)

let selectedOperationAndVariables operations selectedOperationName =
  match selectedOperationName with
  | None -> (None, snd (List.hd operations))
  | Some selectedOperationName ->
      let _, variableAssignments =
        operations
        |> List.find (fun (operation, _) ->
            operation.GraphQlDocument.name = Some selectedOperationName)
      in

      (Some selectedOperationName, variableAssignments)

(* The lowerer assumes a validated invocation (see
   CommandLineInvocation.validatedInvocation):
   operation names are unique, and the selected operation name exists whenever
   the document requires one. *)
let lower (invocation : CommandLineInvocation.t) =
  let loweredOperations =
    invocation.operationDefinitions |> List.map lowerOperationDefinition
  in
  let selectedOperationName, loweredVariableAssignments =
    selectedOperationAndVariables loweredOperations
      invocation.CommandLineInvocation.selectedOperationName
  in
  let fragmentDefinitions =
    (invocation.structuredFragmentDefinitions
    |> List.map lowerStructuredFragmentDefinition)
    @ (invocation.rawFragmentDefinitionTexts
      |> List.map GraphQlDocument.rawFragmentDefinition)
  in

  {
    document =
      {
        GraphQlDocument.operations = loweredOperations |> List.map fst;
        fragmentDefinitions;
      };
    selectedOperationName;
    loweredVariableAssignments;
  }
