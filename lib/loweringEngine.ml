let rec lowerSelectionPath ~isAtOperationRoot ~leafSelectionPaths selectionPath
    =
  let lowerRemaining ~isAtOperationRoot = function
    | [] -> lowerSelectionPaths ~isAtOperationRoot leafSelectionPaths
    | remainingSelectionPath ->
        [
          lowerSelectionPath ~isAtOperationRoot ~leafSelectionPaths
            remainingSelectionPath;
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

and lowerSelectionPaths ~isAtOperationRoot selectionPaths =
  List.map
    (lowerSelectionPath ~isAtOperationRoot ~leafSelectionPaths:[])
    selectionPaths

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
      @ List.map
          (fun { CommandLineInvocation.selectionPathSegments; selectionPaths }
             ->
            lowerSelectionPath ~isAtOperationRoot:true
              ~leafSelectionPaths:selectionPaths selectionPathSegments)
          selectionBranches;
  }

let lower
    {
      CommandLineInvocation.operationDefinitions;
      selectedOperationName = _;
      fragmentDefinitionTexts;
    } =
  {
    GraphQlDocument.operations =
      List.map lowerOperationDefinition operationDefinitions;
    fragmentDefinitions = fragmentDefinitionTexts;
  }
