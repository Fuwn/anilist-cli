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
  | (GraphQlDocument.FragmentSpread _ as fragmentSpread) :: _ -> fragmentSpread

let lowerSelectionPaths ~isAtOperationRoot selectionPaths =
  selectionPaths
  |> List.map (fun selectionPath ->
      lowerSelectionPath ~isAtOperationRoot selectionPath [])

type request = {
  document : GraphQlDocument.t;
  selectedVariableAssignments : (string * CliArgument.value) list;
}

let lowerSelectionBranch
    { CommandLineInvocation.selectionPathSegments; selectionPaths } =
  let selectionsRemainAtRoot =
    List.for_all
      (function
        | GraphQlDocument.InlineFragment _ -> true
        | GraphQlDocument.Field _ | GraphQlDocument.FragmentSpread _ -> false)
      selectionPathSegments
  in

  lowerSelectionPath ~isAtOperationRoot:true selectionPathSegments
    (lowerSelectionPaths ~isAtOperationRoot:selectionsRemainAtRoot
       selectionPaths)

let lowerOperationDefinition
    {
      CommandLineInvocation.operationType;
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

(* The lowerer assumes a validated invocation (see
   CommandLineInvocation.validatedInvocation): operation names are unique, and
   the selected operation name exists whenever the document requires one. *)
let lower
    {
      CommandLineInvocation.operationDefinitions;
      selectedOperationName;
      fragmentDefinitionTexts;
    } =
  let selectedOperationDefinition =
    match selectedOperationName with
    | None -> List.hd operationDefinitions
    | Some _ ->
        operationDefinitions
        |> List.find (fun { CommandLineInvocation.operationName; _ } ->
            operationName = selectedOperationName)
  in

  {
    document =
      {
        GraphQlDocument.operations =
          List.map lowerOperationDefinition operationDefinitions;
        fragmentDefinitions = fragmentDefinitionTexts;
      };
    selectedVariableAssignments =
      selectedOperationDefinition.CommandLineInvocation.variableAssignments;
  }
