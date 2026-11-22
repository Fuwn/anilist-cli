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
  selectedVariableAssignments : (string * CliArgument.value) list;
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
      variableAssignments = _;
      operationDirectiveTexts;
      rootSelectionPaths;
      selectionBranches;
    } =
  {
    GraphQlDocument.operationType;
    name = operationName;
    variableDefinitions;
    directives = operationDirectiveTexts;
    selectionSet =
      lowerSelectionPaths ~isAtOperationRoot:true rootSelectionPaths
      @ List.map lowerSelectionBranch selectionBranches;
  }

let selectedOperationDefinition (invocation : CommandLineInvocation.t) =
  match invocation.selectedOperationName with
  | None -> List.hd invocation.operationDefinitions
  | Some selectedOperationName ->
      invocation.operationDefinitions
      |> List.find (fun operationDefinition ->
          operationDefinition.operationName = Some selectedOperationName)

(* The lowerer assumes a validated invocation (see
   CommandLineInvocation.validatedInvocation):
   operation names are unique, and the selected operation name exists whenever
   the document requires one. *)
let lower (invocation : CommandLineInvocation.t) =
  {
    document =
      {
        GraphQlDocument.operations =
          List.map lowerOperationDefinition invocation.operationDefinitions;
        fragmentDefinitions = invocation.fragmentDefinitionTexts;
      };
    selectedVariableAssignments =
      (selectedOperationDefinition invocation).variableAssignments;
  }
