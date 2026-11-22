type operationType = GraphQlDocument.operationType =
  | Query
  | Mutation
  | Subscription

type selectionPath = GraphQlDocument.selection list

type selectionBranch = {
  parentPath : selectionPath;
  leafSelectionPaths : selectionPath list;
}

type operationDefinition = {
  operationType : operationType;
  operationName : string option;
  variableDefinitions : string list;
  variableAssignments : (string * CliArgument.value) list;
  operationDirectiveTexts : string list;
  rootSelectionPaths : selectionPath list;
  selectionBranches : selectionBranch list;
}

type t = {
  operationDefinitions : operationDefinition list;
  selectedOperationName : string option;
  fragmentDefinitionTexts : string list;
}

type parserState = {
  finalizedOperationDefinitions : operationDefinition list;
  currentOperation : operationDefinition option;
  currentDefaultSelectionPathPrefix : selectionPath;
  currentBranch : selectionBranch option;
  requestedOperationName : string option;
  finalizedFragmentDefinitionTexts : string list;
}

let usageText =
  String.concat "\n"
    [
      "Usage:";
      "  anilist <field> [field-arguments...] [<child-field> \
       [child-arguments...]]... --selection-set <selection[,selection...]>";
      "  anilist schema";
      "  anilist schema --type <Type>";
      "  anilist schema --directive <Directive>";
      "  anilist ... --header 'Authorization: Bearer <token>'";
      "  anilist <operation> [--operation-name <name>] [--variable-definition \
       '$name: Type'] [--selection-set <selection[,selection...]>] [--field \
       <path> ...]";
      "  anilist --operation <query|mutation|subscription[:name]> [...] \
       [--operation <query|mutation|subscription[:name]> ...]";
      "  anilist ... --inline-fragment <Type|./Type|/Type> [--directive \
       <directive>] [--selection-set ...]";
      "  anilist ... --fragment-spread <name|./name|/name> [--directive \
       <directive>]";
      "  Operations: query, mutation, subscription";
      "  Canonical GraphQL term: --selection-set";
      "  Compatibility alias: --fields";
      "  Canonical GraphQL term: --fragment-spread";
      "  Compatibility alias: --spread";
      "  Prefix a value with string: to force a GraphQL string literal.";
      "  Prefix a value with enum: to force a GraphQL enum literal.";
      "  Prefix a value with var: to force a GraphQL variable reference.";
      "  Prefix a value with json: to force a JSON/GraphQL literal.";
      "  Use --header 'Name: Value' to send an HTTP header. Repeat it for \
       multiple headers.";
      "  Use --variable name=value to send GraphQL variable values.";
      "  Use --execute-operation-name <name> when the document contains \
       multiple operations.";
      "  Use --field ./path for a field relative to the current selection \
       branch.";
      "  Use --field /path for a field rooted at the current selection set.";
      "  Use --inline-fragment ./Type to nest under the current selection \
       branch.";
      "  Use --fragment-spread ./name to add a spread under the current \
       selection branch.";
      "  Use alias:field in --field paths or --selection-set paths for field \
       aliases.";
      "  Use ...fragmentName inside --selection-set for fragment spreads.";
      "  Use ...on:Type.path inside --selection-set for shorthand inline \
       fragments.";
      "  Use --directive with @directive or directive syntax.";
      "  Use --fragment-definition with a raw GraphQL fragment definition.";
      "";
      "Examples:";
      "  anilist user --name fuwn --selection-set id,name";
      "  anilist query media --id 1 --selection-set id user --name fuwn \
       --selection-set id";
      "  anilist --operation query:Viewer viewer --selection-set id \
       --operation query:UserLookup user --name fuwn --selection-set id \
       --execute-operation-name UserLookup";
      "  anilist query --field media --id 1 --fragment-spread mediaCore \
       --directive include(if: $withCore)";
    ]

let withUsageText message = message ^ "\n\n" ^ usageText
let raiseUsageError message = invalid_arg (withUsageText message)

let isLongOption token =
  String.length token > 2 && String.starts_with ~prefix:"--" token

let optionNameAndValueOfEqualsSyntax token =
  match String.index_opt token '=' with
  | Some equalsIndex ->
      let optionName = String.sub token 2 (equalsIndex - 2) in
      let optionValue =
        String.sub token (equalsIndex + 1)
          (String.length token - equalsIndex - 1)
      in

      Some (optionName, optionValue)
  | None -> None

let splitAliasAndFieldName selectionText =
  match String.split_on_char ':' selectionText with
  | [ fieldName ] -> (None, fieldName)
  | [ alias; fieldName ] -> (Some alias, fieldName)
  | _ ->
      raiseUsageError
        (Printf.sprintf "Invalid field path segment %s" selectionText)

let operationTypeOfToken = function
  | "query" -> Some Query
  | "mutation" -> Some Mutation
  | "subscription" -> Some Subscription
  | _ -> None

let optionPairOfToken token remainingTokens =
  match optionNameAndValueOfEqualsSyntax token with
  | Some optionPair -> (optionPair, remainingTokens)
  | None -> (
      match remainingTokens with
      | optionValue :: tail
        when not (String.starts_with ~prefix:"--" optionValue) ->
          let optionName = String.sub token 2 (String.length token - 2) in

          ((optionName, optionValue), tail)
      | [] | _ :: _ ->
          raiseUsageError (Printf.sprintf "Missing value for option %s" token))

let variableAssignmentOfText variableAssignmentText =
  match String.index_opt variableAssignmentText '=' with
  | Some equalsIndex when equalsIndex > 0 ->
      let variableName =
        String.sub variableAssignmentText 0 equalsIndex |> String.trim
      in
      let rawValue =
        String.sub variableAssignmentText (equalsIndex + 1)
          (String.length variableAssignmentText - equalsIndex - 1)
      in

      if variableName = "" then
        raiseUsageError "Variable assignment requires a name."
      else if String.contains variableName '-' then
        raiseUsageError "Variable names must use GraphQL variable syntax."
      else if String.starts_with ~prefix:"var:" rawValue then
        raiseUsageError
          "Variable assignments cannot contain variable references."
      else (variableName, CliArgument.valueOfRawValue rawValue)
  | _ -> raiseUsageError "Variable assignments must use name=value syntax."

let operationHeaderOfText operationText =
  let operationToken, operationName =
    match String.split_on_char ':' operationText with
    | [ operationToken ] -> (operationToken, None)
    | [ operationToken; operationName ] ->
        (operationToken, Some (String.trim operationName))
    | _ -> raiseUsageError "Operations must use type or type:name syntax."
  in

  match (operationTypeOfToken (String.trim operationToken), operationName) with
  | None, _ ->
      raiseUsageError
        (Printf.sprintf "Unknown operation type %s" operationToken)
  | Some _, Some "" -> raiseUsageError "Operation names cannot be empty."
  | Some operationType, operationName -> (operationType, operationName)

let fieldSelectionOfText selectionText =
  let aliasValue, fieldName = splitAliasAndFieldName selectionText in
  let trimmedFieldName = String.trim fieldName in

  if trimmedFieldName = "" then raiseUsageError "Field path cannot be empty."
  else
    let alias =
      match aliasValue with
      | Some alias when String.trim alias <> "" ->
          Some (GraphQlName.lowerCamelCaseOfCliToken (String.trim alias))
      | _ -> None
    in

    GraphQlDocument.Field
      {
        alias;
        name = trimmedFieldName;
        arguments = [];
        directives = [];
        selectionSet = [];
      }

let fieldPathOfText fieldPath =
  fieldPath |> String.split_on_char '.' |> List.map String.trim
  |> List.filter (fun selectionText -> selectionText <> "")
  |> List.map fieldSelectionOfText

let nonemptyFieldPathOfText fieldPath =
  match fieldPathOfText fieldPath with
  | [] -> raiseUsageError "Selection path cannot be empty."
  | fieldPath -> fieldPath

let selectionPathOfText pathText =
  match
    ( StringPrefix.valueWithoutPrefix ~prefix:"...on:" pathText,
      StringPrefix.valueWithoutPrefix ~prefix:"..." pathText )
  with
  | Some inlineFragmentPath, _ -> (
      match String.split_on_char '.' inlineFragmentPath with
      | typeCondition :: (_ :: _ as fieldPathParts)
        when String.trim typeCondition <> "" ->
          let fields = fieldPathOfText (String.concat "." fieldPathParts) in

          if fields = [] then
            invalid_arg "Inline fragments require a selection set.";

          GraphQlDocument.InlineFragment
            {
              typeCondition = String.trim typeCondition;
              directives = [];
              selectionSet = [];
            }
          :: fields
      | _ ->
          invalid_arg
            "Inline fragments require a type condition and selection set")
  | None, Some fragmentSpreadText ->
      let name = String.trim fragmentSpreadText in

      if name = "" then invalid_arg "Fragment spreads require a name.";

      [ GraphQlDocument.FragmentSpread { name; directives = [] } ]
  | None, None -> fieldPathOfText pathText

let selectionPathsOfExpression selectionExpression =
  let paths =
    selectionExpression |> String.split_on_char ',' |> List.map String.trim
    |> List.filter (fun path -> path <> "")
  in

  if paths = [] then invalid_arg "Selection set cannot be empty.";

  paths
  |> List.map (fun path ->
      if String.contains path '/' then
        invalid_arg
          "Selection paths cannot contain /; scope a path with --field instead.";

      let selectionPath = selectionPathOfText path in

      if selectionPath = [] then invalid_arg "Selection path cannot be empty.";

      selectionPath)

let pathEndsInFragmentSpread parentPath =
  match List.rev parentPath with
  | GraphQlDocument.FragmentSpread _ :: _ -> true
  | (GraphQlDocument.Field _ | GraphQlDocument.InlineFragment _) :: _ | [] ->
      false

let ensureSelectionPathCanAcceptChildren parentPath =
  if pathEndsInFragmentSpread parentPath then
    raiseUsageError "Fragment spreads cannot contain child selections."
  else parentPath

let withMappedLastSelection branch mapSelection =
  match List.rev branch.parentPath with
  | [] -> invalid_arg "Empty selection branch"
  | last :: rest ->
      { branch with parentPath = List.rev (mapSelection last :: rest) }

let withAddedBranchSelectionPaths branch selectionPaths =
  if pathEndsInFragmentSpread branch.parentPath then
    raiseUsageError "Fragment spreads cannot define a selection set."
  else
    {
      branch with
      leafSelectionPaths = branch.leafSelectionPaths @ selectionPaths;
    }

let withExtendedParentPath branch fieldPath =
  let parentPath = ensureSelectionPathCanAcceptChildren branch.parentPath in

  { branch with parentPath = parentPath @ nonemptyFieldPathOfText fieldPath }

let withAddedBranchFieldArgumentPair branch fieldArgumentPair =
  withMappedLastSelection branch (function
    | GraphQlDocument.Field field ->
        GraphQlDocument.Field
          { field with arguments = field.arguments @ [ fieldArgumentPair ] }
    | GraphQlDocument.InlineFragment _ | GraphQlDocument.FragmentSpread _ ->
        raiseUsageError
          "Field arguments require the current selection branch to end in a \
           field.")

let withUpdatedBranchFieldAlias branch fieldAlias =
  withMappedLastSelection branch (function
    | GraphQlDocument.Field field ->
        GraphQlDocument.Field
          {
            field with
            alias = Some (GraphQlName.lowerCamelCaseOfCliToken fieldAlias);
          }
    | GraphQlDocument.InlineFragment _ | GraphQlDocument.FragmentSpread _ ->
        raiseUsageError
          "--alias requires the current selection branch to end in a field.")

let withAddedBranchDirective branch directiveText =
  withMappedLastSelection branch (function
    | GraphQlDocument.Field field ->
        GraphQlDocument.Field
          { field with directives = field.directives @ [ directiveText ] }
    | GraphQlDocument.InlineFragment inlineFragment ->
        GraphQlDocument.InlineFragment
          {
            inlineFragment with
            directives = inlineFragment.directives @ [ directiveText ];
          }
    | GraphQlDocument.FragmentSpread fragmentSpread ->
        GraphQlDocument.FragmentSpread
          {
            fragmentSpread with
            directives = fragmentSpread.directives @ [ directiveText ];
          })

let classifiedBranchPath pathText =
  match
    ( StringPrefix.valueWithoutPrefix ~prefix:"./" pathText,
      StringPrefix.valueWithoutPrefix ~prefix:"/" pathText )
  with
  | Some relativePath, _ -> (`Relative, relativePath)
  | None, Some absolutePath -> (`Absolute, absolutePath)
  | None, None -> (`Default, pathText)

let currentBranchPathOrRaise parserState ~requiredMessage =
  match parserState.currentBranch with
  | Some branch -> ensureSelectionPathCanAcceptChildren branch.parentPath
  | None -> raiseUsageError requiredMessage

let resolvedFieldPath parserState fieldPath =
  match classifiedBranchPath fieldPath with
  | `Relative, bareFieldPath ->
      currentBranchPathOrRaise parserState
        ~requiredMessage:
          "Relative --field paths require an existing selection branch."
      @ nonemptyFieldPathOfText bareFieldPath
  | `Absolute, bareFieldPath -> nonemptyFieldPathOfText bareFieldPath
  | `Default, bareFieldPath ->
      parserState.currentDefaultSelectionPathPrefix
      @ nonemptyFieldPathOfText bareFieldPath

let resolvedFragmentPath ~requiredMessage ~emptyMessage selectionOfName
    parserState fragmentText =
  let parentPath, bareText =
    match classifiedBranchPath fragmentText with
    | `Relative, bareText ->
        (currentBranchPathOrRaise parserState ~requiredMessage, bareText)
    | `Absolute, bareText -> ([], bareText)
    | `Default, bareText -> (
        match parserState.currentBranch with
        | Some branch ->
            (ensureSelectionPathCanAcceptChildren branch.parentPath, bareText)
        | None -> ([], bareText))
  in
  let name = String.trim bareText in

  if name = "" then raiseUsageError emptyMessage
  else parentPath @ [ selectionOfName name ]

let resolvedInlineFragmentPath =
  resolvedFragmentPath
    ~requiredMessage:
      "Relative --inline-fragment paths require an existing selection branch."
    ~emptyMessage:"Inline fragments require a type condition."
    (fun typeCondition ->
      GraphQlDocument.InlineFragment
        { typeCondition; directives = []; selectionSet = [] })

let resolvedFragmentSpreadPath =
  resolvedFragmentPath
    ~requiredMessage:
      "Relative --fragment-spread paths require an existing selection branch."
    ~emptyMessage:"Fragment spreads require a fragment name." (fun name ->
      GraphQlDocument.FragmentSpread { name; directives = [] })

let withMappedCurrentOperation parserState mapOperation =
  {
    parserState with
    currentOperation = Option.map mapOperation parserState.currentOperation;
  }

let finalizedCurrentBranch parserState =
  match parserState.currentBranch with
  | None -> parserState
  | Some branch ->
      let parserState =
        withMappedCurrentOperation parserState (fun operation ->
            {
              operation with
              selectionBranches = operation.selectionBranches @ [ branch ];
            })
      in

      { parserState with currentBranch = None }

let finalizedAllPending parserState =
  let parserState = finalizedCurrentBranch parserState in

  {
    parserState with
    finalizedOperationDefinitions =
      Option.to_list parserState.currentOperation
      @ parserState.finalizedOperationDefinitions;
    currentOperation = None;
    currentDefaultSelectionPathPrefix = [];
  }

let withCurrentOperationOrRaise parserState ~missingMessage mapOperation =
  if parserState.currentOperation = None then raiseUsageError missingMessage
  else withMappedCurrentOperation parserState mapOperation

let withUpdatedCurrentSelectionBranch parserState branch =
  if parserState.currentOperation = None then
    raiseUsageError "Selections require a current operation."
  else { parserState with currentBranch = Some branch }

let withAddedRootSelectionPaths parserState selectionPaths =
  withCurrentOperationOrRaise parserState
    ~missingMessage:"Selections require a current operation." (fun operation ->
      {
        operation with
        rootSelectionPaths = operation.rootSelectionPaths @ selectionPaths;
      })

let withAddedOperationDirective parserState directiveText =
  withCurrentOperationOrRaise parserState
    ~missingMessage:"Directives require a current operation." (fun operation ->
      {
        operation with
        operationDirectiveTexts =
          operation.operationDirectiveTexts @ [ directiveText ];
      })

let startedOperationDefinition parserState operationType operationName =
  let parserState = finalizedAllPending parserState in

  {
    parserState with
    currentOperation =
      Some
        {
          operationType;
          operationName;
          variableDefinitions = [];
          variableAssignments = [];
          operationDirectiveTexts = [];
          rootSelectionPaths = [];
          selectionBranches = [];
        };
  }

let startedShorthandQueryOperation parserState firstFieldToken =
  let branch =
    {
      parentPath = nonemptyFieldPathOfText firstFieldToken;
      leafSelectionPaths = [];
    }
  in

  {
    (startedOperationDefinition parserState Query None) with
    currentBranch = Some branch;
    currentDefaultSelectionPathPrefix = branch.parentPath;
  }

let initialParserState =
  {
    finalizedOperationDefinitions = [];
    currentOperation = None;
    currentDefaultSelectionPathPrefix = [];
    currentBranch = None;
    requestedOperationName = None;
    finalizedFragmentDefinitionTexts = [];
  }

let currentInvocationOfState parserState =
  let parserState = finalizedAllPending parserState in

  {
    operationDefinitions = List.rev parserState.finalizedOperationDefinitions;
    selectedOperationName = parserState.requestedOperationName;
    fragmentDefinitionTexts = parserState.finalizedFragmentDefinitionTexts;
  }

let operationHasSelections operation =
  operation.rootSelectionPaths <> [] || operation.selectionBranches <> []

let branchHasCompleteInlineFragment branch =
  match List.rev branch.parentPath with
  | GraphQlDocument.InlineFragment _ :: _ -> branch.leafSelectionPaths <> []
  | (GraphQlDocument.Field _ | GraphQlDocument.FragmentSpread _) :: _ -> true
  | [] -> false

let rec hasDuplicateName = function
  | [] -> false
  | name :: remainingNames ->
      List.mem name remainingNames || hasDuplicateName remainingNames

let validatedInvocation invocation =
  let operations = invocation.operationDefinitions in
  let operationNames =
    List.filter_map (fun operation -> operation.operationName) operations
  in
  let isMultipleOperationDocument = List.length operations > 1 in
  let usageError message = Error (withUsageText message) in

  if operations = [] then usageError "Missing operation selection."
  else if not (List.for_all operationHasSelections operations) then
    usageError "Each operation requires a selection set."
  else if
    not
      (List.for_all
         (fun operation ->
           List.for_all branchHasCompleteInlineFragment
             operation.selectionBranches)
         operations)
  then Error "Inline fragments require a selection set."
  else if hasDuplicateName operationNames then
    usageError "Operation names must be unique."
  else if
    isMultipleOperationDocument
    && List.length operationNames < List.length operations
  then
    usageError
      "Multiple-operation documents require every operation to be named."
  else
    match invocation.selectedOperationName with
    | Some name when not (List.mem name operationNames) ->
        usageError
          (Printf.sprintf
             "Selected operation %s does not exist in the document." name)
    | None when isMultipleOperationDocument ->
        usageError
          "Multiple-operation documents require --execute-operation-name."
    | Some _ | None -> Ok invocation

let selectedVariables invocation =
  let isSelected operation =
    invocation.selectedOperationName = None
    || operation.operationName = invocation.selectedOperationName
  in

  match List.find_opt isSelected invocation.operationDefinitions with
  | None | Some { variableAssignments = []; _ } -> None
  | Some { variableAssignments; _ } ->
      Some
        (`Assoc
           (variableAssignments
           |> List.map (fun (name, value) ->
               (name, CliArgument.jsonLiteralOfValue value))))

let replaceCurrentBranchWithPath parserState resolvedPath =
  let rec extendsPath parent child =
    match (parent, child) with
    | [], _ :: _ -> true
    | parentSelection :: parentTail, childSelection :: childTail
      when parentSelection = childSelection ->
        extendsPath parentTail childTail
    | _ -> false
  in
  let parserState =
    match parserState.currentBranch with
    | Some branch
      when branch.leafSelectionPaths = []
           && extendsPath branch.parentPath resolvedPath ->
        { parserState with currentBranch = None }
    | _ -> finalizedCurrentBranch parserState
  in

  withUpdatedCurrentSelectionBranch parserState
    { parentPath = resolvedPath; leafSelectionPaths = [] }

let updateBranchOrFallback parserState ~onBranch ~onNoBranch =
  match parserState.currentBranch with
  | Some branch ->
      withUpdatedCurrentSelectionBranch parserState (onBranch branch)
  | None -> onNoBranch parserState

let handleOperation parserState operationHeaderText =
  let operationType, operationName =
    operationHeaderOfText operationHeaderText
  in

  startedOperationDefinition parserState operationType operationName

let handleSelectionPath resolvePath parserState pathText =
  replaceCurrentBranchWithPath parserState (resolvePath parserState pathText)

let handleSelectionSet parserState expressionText =
  let selectionPaths = selectionPathsOfExpression expressionText in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      withAddedBranchSelectionPaths branch selectionPaths)
    ~onNoBranch:(fun state -> withAddedRootSelectionPaths state selectionPaths)

let handleOperationName parserState operationName =
  withCurrentOperationOrRaise parserState
    ~missingMessage:"--operation-name requires a current operation."
    (fun operationDefinition ->
      { operationDefinition with operationName = Some operationName })

let handleSelectedOperationName parserState selectedOperationName =
  { parserState with requestedOperationName = Some selectedOperationName }

let handleVariableDefinition parserState variableDefinition =
  withCurrentOperationOrRaise parserState
    ~missingMessage:"--variable-definition requires a current operation."
    (fun operationDefinition ->
      {
        operationDefinition with
        variableDefinitions =
          operationDefinition.variableDefinitions @ [ variableDefinition ];
      })

let handleVariable parserState variableAssignmentText =
  withCurrentOperationOrRaise parserState
    ~missingMessage:"--variable requires a current operation."
    (fun operationDefinition ->
      {
        operationDefinition with
        variableAssignments =
          operationDefinition.variableAssignments
          @ [ variableAssignmentOfText variableAssignmentText ];
      })

let handleDirective parserState directiveText =
  let directiveText = GraphQlDocument.normalizedDirectiveText directiveText in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch -> withAddedBranchDirective branch directiveText)
    ~onNoBranch:(fun state -> withAddedOperationDirective state directiveText)

let handleAlias parserState fieldAlias =
  updateBranchOrFallback parserState
    ~onBranch:(fun branch -> withUpdatedBranchFieldAlias branch fieldAlias)
    ~onNoBranch:(fun _ ->
      raiseUsageError "--alias requires an existing selection branch.")

let handleFragmentDefinition parserState fragmentDefinitionText =
  let fragmentDefinitionText = String.trim fragmentDefinitionText in

  if fragmentDefinitionText = "" then
    invalid_arg "Fragment definition text cannot be empty.";

  let finalizedParserState = finalizedAllPending parserState in

  {
    finalizedParserState with
    finalizedFragmentDefinitionTexts =
      finalizedParserState.finalizedFragmentDefinitionTexts
      @ [ fragmentDefinitionText ];
  }

let handleFieldArgument parserState (name, rawValue) =
  let fieldArgumentPair = (name, CliArgument.valueOfRawValue rawValue) in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      withAddedBranchFieldArgumentPair branch fieldArgumentPair)
    ~onNoBranch:(fun _ ->
      raiseUsageError
        (Printf.sprintf
           "Field argument %s requires an existing selection branch." name))

let optionHandlers =
  [
    ("operation", handleOperation);
    ("field", handleSelectionPath resolvedFieldPath);
    ("inline-fragment", handleSelectionPath resolvedInlineFragmentPath);
    ("fragment-spread", handleSelectionPath resolvedFragmentSpreadPath);
    ("spread", handleSelectionPath resolvedFragmentSpreadPath);
    ("selection-set", handleSelectionSet);
    ("fields", handleSelectionSet);
    ("operation-name", handleOperationName);
    ("execute-operation-name", handleSelectedOperationName);
    ("variable-definition", handleVariableDefinition);
    ("variable", handleVariable);
    ("directive", handleDirective);
    ("alias", handleAlias);
    ("fragment-definition", handleFragmentDefinition);
  ]

let handlePositionalToken parserState token =
  match parserState.currentBranch with
  | Some branch
    when parserState.currentDefaultSelectionPathPrefix = []
         && branch.leafSelectionPaths <> [] ->
      let finalizedParserState = finalizedCurrentBranch parserState in

      replaceCurrentBranchWithPath finalizedParserState
        (resolvedFieldPath finalizedParserState token)
  | Some branch ->
      withUpdatedCurrentSelectionBranch parserState
        (withExtendedParentPath branch token)
  | None -> (
      match parserState.currentOperation with
      | None -> (
          match operationTypeOfToken token with
          | Some operationType ->
              startedOperationDefinition parserState operationType None
          | None -> startedShorthandQueryOperation parserState token)
      | Some _ ->
          replaceCurrentBranchWithPath parserState
            (resolvedFieldPath parserState token))

let rec parseRemainingArguments parserState = function
  | [] ->
      Result.map
        (fun invocation -> `Invocation invocation)
        (validatedInvocation (currentInvocationOfState parserState))
  | "--help" :: _ -> Ok `Help
  | token :: remainingTokens when isLongOption token ->
      let (optionName, optionValue), remainingTokens =
        optionPairOfToken token remainingTokens
      in
      let updatedParserState =
        match List.assoc_opt optionName optionHandlers with
        | Some handler -> handler parserState optionValue
        | None -> handleFieldArgument parserState (optionName, optionValue)
      in

      parseRemainingArguments updatedParserState remainingTokens
  | token :: remainingTokens ->
      parseRemainingArguments
        (handlePositionalToken parserState token)
        remainingTokens

let ofArguments arguments =
  match arguments with
  | [] -> Error usageText
  | "help" :: _ -> Ok `Help
  | _ -> (
      try parseRemainingArguments initialParserState arguments with
      | Invalid_argument message -> Error message
      | Yojson.Json_error message -> Error ("Invalid JSON: " ^ message))
