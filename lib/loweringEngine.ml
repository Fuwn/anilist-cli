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

let lowerSelectionBranch selectionBranch =
  let selectionsRemainAtRoot =
    List.for_all
      (function
        | InlineFragmentSegment _ -> true
        | FieldSegment _ | FragmentSpreadSegment _ -> false)
      selectionBranch.selectionPathSegments
  in
  let relativeSelectionSet, rootSelectionSet =
    lowerScopedPaths ~relativeAtRoot:selectionsRemainAtRoot ~rootAtRoot:true
      selectionBranch.selectionPaths
  in

  ( lowerSelectionPathSegments ~isTargetRoot:true
      selectionBranch.selectionPathSegments relativeSelectionSet,
    rootSelectionSet )

let lowerOperationDefinition
    {
      operationType;
      operationName;
      variableDefinitions;
      variableAssignments;
      operationDirectiveTexts;
      rootSelectionPaths;
      selectionBranches;
    } =
  let relativeSelectionSet, rootSelectionSet =
    lowerScopedPaths ~relativeAtRoot:true ~rootAtRoot:true rootSelectionPaths
  in
  let loweredBranches = List.map lowerSelectionBranch selectionBranches in
  let operation =
    {
      GraphQlDocument.operationType;
      name = operationName;
      variableDefinitions;
      directives = operationDirectiveTexts;
      selectionSet =
        relativeSelectionSet @ rootSelectionSet
        @ List.map fst loweredBranches
        @ List.concat_map snd loweredBranches;
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
    invocation.rawFragmentDefinitionTexts
    |> List.map GraphQlDocument.fragmentDefinitionOfText
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
