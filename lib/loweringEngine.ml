open CommandLineInvocation

type scopedFieldSelection = {
  relativeSelectionSet : GraphQlSelection.t list;
  rootSelectionSet : GraphQlSelection.t list;
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
      ({ fieldName; fieldAlias; fieldArgumentPairs; fieldDirectiveTexts } :
        fieldSegment)
    :: remainingSelectionPathSegments ->
      GraphQlSelection.field
        (GraphQlSelection.makeField ?alias:fieldAlias
           ~name:(graphQlFieldNameOfSegment ~isTargetRoot fieldName)
           ~arguments:(graphQlArgumentsOfOptionPairs fieldArgumentPairs)
           ~directives:fieldDirectiveTexts
           ~selectionSet:
             (lowerTail ~isTargetRoot:false remainingSelectionPathSegments)
           ())
  | InlineFragmentSegment
      ({ inlineFragmentTypeCondition; inlineFragmentDirectiveTexts } :
        inlineFragmentSegment)
    :: remainingSelectionPathSegments ->
      GraphQlSelection.inlineFragment
        (GraphQlSelection.makeInlineFragment
           ~typeCondition:(graphQlTypeCondition inlineFragmentTypeCondition)
           ~directives:inlineFragmentDirectiveTexts
           ~selectionSet:
             (lowerTail ~isTargetRoot remainingSelectionPathSegments)
           ())
  | FragmentSpreadSegment
      ({ fragmentSpreadName; fragmentSpreadDirectiveTexts } :
        fragmentSpreadSegment)
    :: remainingSelectionPathSegments ->
      if remainingSelectionPathSegments <> [] then
        raise
          (Invalid_argument "Fragment spreads cannot contain child selections")
      else
        GraphQlSelection.fragmentSpread
          (GraphQlSelection.makeFragmentSpread ~name:fragmentSpreadName
             ~directives:fragmentSpreadDirectiveTexts ())

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
    scopedSelectionOfPaths
      ~capitalizeRelativeRootFieldNames:selectionsRemainAtRoot
      ~capitalizeRootFieldNames selectionBranch.selectionPaths
  in
  let loweredSelection =
    lowerSelectionPathSegments ~isTargetRoot
      selectionBranch.selectionPathSegments scopedSelection.relativeSelectionSet
  in

  (loweredSelection, scopedSelection.rootSelectionSet)

let selectionSetOfTarget ~capitalizeRootFieldNames ~rootSelectionPaths
    ~selectionBranches =
  let rootScopedSelection =
    scopedSelectionOfPaths
      ~capitalizeRelativeRootFieldNames:capitalizeRootFieldNames
      ~capitalizeRootFieldNames rootSelectionPaths
  in
  let loweredBranches =
    selectionBranches
    |> List.map
         (lowerSelectionBranch ~capitalizeRootFieldNames
            ~isTargetRoot:capitalizeRootFieldNames)
  in

  rootScopedSelection.relativeSelectionSet
  @ rootScopedSelection.rootSelectionSet
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
      (graphQlTypeCondition structuredFragmentDefinition.fragmentTypeCondition)
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
