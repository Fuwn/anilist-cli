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

let lowerDefinitionBody ~isAtOperationRoot
    {
      CommandLineInvocation.directiveTexts;
      rootSelectionPaths;
      selectionBranches;
    } =
  ( directiveTexts,
    lowerSelectionPaths ~isAtOperationRoot rootSelectionPaths
    @ List.map
        (fun { CommandLineInvocation.parentPath; leafSelectionPaths } ->
          lowerSelectionPath ~isAtOperationRoot ~leafSelectionPaths parentPath)
        selectionBranches )

let lowerOperationDefinition
    {
      CommandLineInvocation.operationType;
      operationName;
      variableDefinitions;
      variableAssignments = _;
      operationBody;
    } =
  let directives, selectionSet =
    lowerDefinitionBody ~isAtOperationRoot:true operationBody
  in

  {
    GraphQlDocument.operationType;
    name = operationName;
    variableDefinitions;
    directives;
    selectionSet;
  }

let lowerFragmentDefinition = function
  | CommandLineInvocation.FragmentText text -> GraphQlDocument.FragmentText text
  | CommandLineInvocation.StructuredFragment
      {
        CommandLineInvocation.fragmentName;
        fragmentTypeCondition;
        fragmentBody;
      } ->
      let directives, selectionSet =
        lowerDefinitionBody ~isAtOperationRoot:false fragmentBody
      in

      GraphQlDocument.StructuredFragment
        {
          name = fragmentName;
          typeCondition =
            GraphQlName.upperCamelCaseOfCliToken fragmentTypeCondition;
          directives;
          selectionSet;
        }

let lower
    {
      CommandLineInvocation.operationDefinitions;
      selectedOperationName = _;
      fragmentDefinitions;
    } =
  {
    GraphQlDocument.operations =
      List.map lowerOperationDefinition operationDefinitions;
    fragmentDefinitions = List.map lowerFragmentDefinition fragmentDefinitions;
  }
