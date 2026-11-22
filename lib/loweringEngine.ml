open CommandLineInvocation

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

let lowerScopedPaths ~relativeAtRoot ~rootAtRoot scopedPaths =
  let lowerPathsOfScope scope ~isTargetRoot =
    scopedPaths
    |> List.filter_map (fun (pathScope, segments) ->
        if pathScope = scope then
          Some (lowerSelectionPathSegments ~isTargetRoot segments [])
        else None)
  in

  ( lowerPathsOfScope `Relative ~isTargetRoot:relativeAtRoot,
    lowerPathsOfScope `Root ~isTargetRoot:rootAtRoot )

type request = {
  document : GraphQlDocument.t;
  selectedOperationName : string option;
  loweredVariableAssignments : (string * CliArgument.value) list;
}

let lowerSelectionBranch ~isTargetRoot selectionBranch =
  let selectionsRemainAtRoot =
    isTargetRoot
    && List.for_all
         (function
           | InlineFragmentSegment _ -> true
           | FieldSegment _ | FragmentSpreadSegment _ -> false)
         selectionBranch.selectionPathSegments
  in
  let relativeSelectionSet, rootSelectionSet =
    lowerScopedPaths ~relativeAtRoot:selectionsRemainAtRoot
      ~rootAtRoot:isTargetRoot selectionBranch.selectionPaths
  in

  ( lowerSelectionPathSegments ~isTargetRoot
      selectionBranch.selectionPathSegments relativeSelectionSet,
    rootSelectionSet )

let selectionSetOfBody ~isTargetRoot body =
  let relativeSelectionSet, rootSelectionSet =
    lowerScopedPaths ~relativeAtRoot:isTargetRoot ~rootAtRoot:isTargetRoot
      body.rootSelectionPaths
  in
  let loweredBranches =
    List.map (lowerSelectionBranch ~isTargetRoot) body.selectionBranches
  in

  relativeSelectionSet @ rootSelectionSet
  @ List.map fst loweredBranches
  @ List.concat_map snd loweredBranches

let lowerStructuredFragmentDefinition
    { fragmentName; fragmentTypeCondition; fragmentBody } =
  let selectionSet = selectionSetOfBody ~isTargetRoot:false fragmentBody in

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
  let selectionSet = selectionSetOfBody ~isTargetRoot:true operationBody in
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
