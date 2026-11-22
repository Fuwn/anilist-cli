open CommandLineInvocation

let graphQlArgumentsOfOptionPairs optionPairs =
  optionPairs
  |> List.map (fun (optionName, optionValue) ->
      {
        CliArgument.name = GraphQlName.lowerCamelCaseOfCliToken optionName;
        value = optionValue;
      })

let graphQlFieldNameOfSegment ~isAtOperationRoot fieldName =
  if isAtOperationRoot then GraphQlName.upperCamelCaseOfCliToken fieldName
  else GraphQlName.lowerCamelCaseOfCliToken fieldName

let rec lowerSelectionPathSegments ~isAtOperationRoot selectionPathSegments
    selectionSet =
  let lowerTail ~isAtOperationRoot = function
    | [] -> selectionSet
    | remainingSelectionPathSegments ->
        [
          lowerSelectionPathSegments ~isAtOperationRoot
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
          name = graphQlFieldNameOfSegment ~isAtOperationRoot fieldName;
          arguments = graphQlArgumentsOfOptionPairs fieldArgumentPairs;
          directives = fieldDirectiveTexts;
          selectionSet =
            lowerTail ~isAtOperationRoot:false remainingSelectionPathSegments;
        }
  | InlineFragmentSegment
      { inlineFragmentTypeCondition; inlineFragmentDirectiveTexts }
    :: remainingSelectionPathSegments ->
      GraphQlDocument.InlineFragment
        {
          typeCondition =
            GraphQlName.upperCamelCaseOfCliToken inlineFragmentTypeCondition;
          directives = inlineFragmentDirectiveTexts;
          selectionSet =
            lowerTail ~isAtOperationRoot remainingSelectionPathSegments;
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

let lowerSelectionPaths ~isAtOperationRoot selectionPaths =
  selectionPaths
  |> List.map (fun segments ->
      lowerSelectionPathSegments ~isAtOperationRoot segments [])

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

  lowerSelectionPathSegments ~isAtOperationRoot:true
    selectionBranch.selectionPathSegments
    (lowerSelectionPaths ~isAtOperationRoot:selectionsRemainAtRoot
       selectionBranch.selectionPaths)

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
  let operation =
    {
      GraphQlDocument.operationType;
      name = operationName;
      variableDefinitions;
      directives = operationDirectiveTexts;
      selectionSet =
        lowerSelectionPaths ~isAtOperationRoot:true rootSelectionPaths
        @ List.map lowerSelectionBranch selectionBranches;
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

  {
    document =
      {
        GraphQlDocument.operations = loweredOperations |> List.map fst;
        fragmentDefinitions = invocation.fragmentDefinitionTexts;
      };
    selectedOperationName;
    loweredVariableAssignments;
  }
