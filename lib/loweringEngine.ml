open CommandLineInvocation

let rec lowerSelectionPath ~isAtOperationRoot selectionPath leafSelectionSet =
  let lowerRemaining ~isAtOperationRoot = function
    | [] -> leafSelectionSet
    | remainingSelectionPath ->
        [
          lowerSelectionPath ~isAtOperationRoot remainingSelectionPath
            leafSelectionSet;
        ]
  in

  match selectionPath with
  | [] -> invalid_arg "Expected at least one selection path segment"
  | GraphQlDocument.Field field :: remainingSelectionPath ->
      GraphQlDocument.Field
        {
          field with
          name =
            (if isAtOperationRoot then
               GraphQlName.upperCamelCaseOfCliToken field.name
             else GraphQlName.lowerCamelCaseOfCliToken field.name);
          arguments =
            field.arguments
            |> List.map (fun (name, value) ->
                (GraphQlName.lowerCamelCaseOfCliToken name, value));
          selectionSet =
            lowerRemaining ~isAtOperationRoot:false remainingSelectionPath;
        }
  | GraphQlDocument.InlineFragment inlineFragment :: remainingSelectionPath ->
      GraphQlDocument.InlineFragment
        {
          inlineFragment with
          typeCondition =
            GraphQlName.upperCamelCaseOfCliToken inlineFragment.typeCondition;
          selectionSet =
            lowerRemaining ~isAtOperationRoot remainingSelectionPath;
        }
  | (GraphQlDocument.FragmentSpread _ as fragmentSpread)
    :: remainingSelectionPath ->
      if remainingSelectionPath <> [] then
        invalid_arg "Fragment spreads cannot contain child selections"
      else fragmentSpread

let lowerSelectionPaths ~isAtOperationRoot selectionPaths =
  selectionPaths
  |> List.map (fun selectionPath ->
      lowerSelectionPath ~isAtOperationRoot selectionPath [])

type request = {
  document : GraphQlDocument.t;
  selectedVariableAssignments : (string * CliArgument.value) list;
}

let lowerSelectionBranch selectionBranch =
  let selectionsRemainAtRoot =
    List.for_all
      (function
        | GraphQlDocument.InlineFragment _ -> true
        | GraphQlDocument.Field _ | GraphQlDocument.FragmentSpread _ -> false)
      selectionBranch.selectionPathSegments
  in

  lowerSelectionPath ~isAtOperationRoot:true
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
