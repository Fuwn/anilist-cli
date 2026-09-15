open CommandLineInvocationTypes

let operationDefinitionsHaveSelections operationDefinitions =
  operationDefinitions
  |> List.for_all (fun operationDefinition ->
      operationDefinition.rootSelectionExpressions <> []
      || operationDefinition.selectionBranches <> [])

let structuredFragmentsHaveSelections structuredFragmentDefinitions =
  structuredFragmentDefinitions
  |> List.for_all (fun fragmentDefinition ->
      fragmentDefinition.fragmentRootSelectionExpressions <> []
      || fragmentDefinition.fragmentSelectionBranches <> [])

let branchHasCompleteInlineFragment branch =
  match List.rev branch.selectionPathSegments with
  | InlineFragmentSegment _ :: _ ->
      List.exists
        (fun (scope, _) -> scope = `Relative)
        (SelectionSet.parsedExpressions branch.selectionExpressions)
  | FieldSegment _ :: _ | FragmentSpreadSegment _ :: _ -> true
  | [] -> false

let duplicateNames names =
  let rec loop seenNames duplicates = function
    | [] -> duplicates
    | name :: remainingNames ->
        if List.mem name seenNames then
          loop seenNames (name :: duplicates) remainingNames
        else loop (name :: seenNames) duplicates remainingNames
  in

  names |> loop [] [] |> List.rev |> List.sort_uniq String.compare

let validatedInvocation invocation =
  if invocation.operationDefinitions = [] then
    Error
      (Printf.sprintf "Missing operation selection.\n\n%s"
         CommandLineInvocationShared.usageText)
  else if
    not (operationDefinitionsHaveSelections invocation.operationDefinitions)
  then
    Error
      (Printf.sprintf "Each operation requires a selection set.\n\n%s"
         CommandLineInvocationShared.usageText)
  else if
    not
      (structuredFragmentsHaveSelections
         invocation.structuredFragmentDefinitions)
  then
    Error
      (Printf.sprintf "Each structured fragment requires a selection set.\n\n%s"
         CommandLineInvocationShared.usageText)
  else if
    not
      (List.for_all
         (fun operation ->
           List.for_all branchHasCompleteInlineFragment
             operation.selectionBranches)
         invocation.operationDefinitions
      && List.for_all
           (fun fragment ->
             List.for_all branchHasCompleteInlineFragment
               fragment.fragmentSelectionBranches)
           invocation.structuredFragmentDefinitions)
  then Error "Inline fragments require a selection set."
  else
    let operationNames =
      invocation.operationDefinitions
      |> List.filter_map (fun operationDefinition ->
          operationDefinition.operationName)
    in
    let duplicateOperationNames = duplicateNames operationNames in

    if duplicateOperationNames <> [] then
      Error
        (Printf.sprintf "Operation names must be unique.\n\n%s"
           CommandLineInvocationShared.usageText)
    else if
      List.length invocation.operationDefinitions > 1
      && List.exists
           (fun operationDefinition -> operationDefinition.operationName = None)
           invocation.operationDefinitions
    then
      Error
        (Printf.sprintf
           "Multiple-operation documents require every operation to be named.\n\n\
            %s"
           CommandLineInvocationShared.usageText)
    else
      match invocation.selectedOperationName with
      | Some selectedOperationName ->
          if
            List.exists
              (fun operationDefinition ->
                operationDefinition.operationName = Some selectedOperationName)
              invocation.operationDefinitions
          then Ok invocation
          else
            Error
              (Printf.sprintf
                 "Selected operation %s does not exist in the document.\n\n%s"
                 selectedOperationName CommandLineInvocationShared.usageText)
      | None ->
          if List.length invocation.operationDefinitions > 1 then
            Error
              (Printf.sprintf
                 "Multiple-operation documents require \
                  --execute-operation-name.\n\n\
                  %s"
                 CommandLineInvocationShared.usageText)
          else Ok invocation
