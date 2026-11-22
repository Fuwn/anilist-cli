type operationType = GraphQlOperation.operationType =
  | Query
  | Mutation
  | Subscription

type fieldSegment = {
  fieldName : string;
  fieldAlias : string option;
  fieldArgumentPairs : (string * CliArgument.value) list;
  fieldDirectiveTexts : string list;
}

type inlineFragmentSegment = {
  inlineFragmentTypeCondition : string;
  inlineFragmentDirectiveTexts : string list;
}

type fragmentSpreadSegment = {
  fragmentSpreadName : string;
  fragmentSpreadDirectiveTexts : string list;
}

type selectionPathSegment =
  | FieldSegment of fieldSegment
  | InlineFragmentSegment of inlineFragmentSegment
  | FragmentSpreadSegment of fragmentSpreadSegment

type scopedSelectionPath = [ `Relative | `Root ] * selectionPathSegment list

type selectionBranch = {
  selectionPathSegments : selectionPathSegment list;
  selectionPaths : scopedSelectionPath list;
}

type operationDefinition = {
  operationType : operationType;
  operationName : string option;
  variableDefinitions : string list;
  variableAssignments : (string * CliArgument.value) list;
  operationDirectiveTexts : string list;
  rootSelectionPaths : scopedSelectionPath list;
  selectionBranches : selectionBranch list;
}

type structuredFragmentDefinition = {
  fragmentName : string;
  fragmentTypeCondition : string;
  fragmentDirectiveTexts : string list;
  fragmentRootSelectionPaths : scopedSelectionPath list;
  fragmentSelectionBranches : selectionBranch list;
}

type t = {
  operationDefinitions : operationDefinition list;
  selectedOperationName : string option;
  structuredFragmentDefinitions : structuredFragmentDefinition list;
  rawFragmentDefinitionTexts : string list;
}

type selectionTarget =
  | OperationTarget of operationDefinition
  | FragmentTarget of structuredFragmentDefinition

type parserState = {
  finalizedOperationDefinitions : operationDefinition list;
  currentTarget : selectionTarget option;
  currentDefaultSelectionPathPrefix : selectionPathSegment list;
  currentBranch : selectionBranch option;
  requestedOperationName : string option;
  finalizedStructuredFragmentDefinitions : structuredFragmentDefinition list;
  pendingRawFragmentDefinitionTexts : string list;
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
      "  anilist ... --fragment <name:Type> [--directive <directive>] \
       [--selection-set ...] [--field ...]";
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
let currentBranchPathPrefix = "./"
let absoluteBranchPathPrefix = "/"
let isLongOption token = String.length token > 2 && String.sub token 0 2 = "--"

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

let normalizedAlias alias = GraphQlName.lowerCamelCaseOfCliToken alias

let splitAliasAndFieldName pathSegment =
  match String.split_on_char ':' pathSegment with
  | [ fieldName ] -> (None, fieldName)
  | [ alias; fieldName ] -> (Some alias, fieldName)
  | _ ->
      raiseUsageError
        (Printf.sprintf "Invalid field path segment %s" pathSegment)

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

let valueOfOptionToken token remainingTokens =
  let (_, optionValue), tail = optionPairOfToken token remainingTokens in

  (optionValue, tail)

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

let structuredFragmentNameAndTypeCondition fragmentText =
  match String.split_on_char ':' fragmentText with
  | [ fragmentName; typeCondition ] ->
      let normalizedFragmentName = String.trim fragmentName in
      let normalizedTypeCondition = String.trim typeCondition in

      if normalizedFragmentName = "" || normalizedTypeCondition = "" then
        raiseUsageError "Structured fragments require name:type syntax."
      else (normalizedFragmentName, normalizedTypeCondition)
  | _ -> raiseUsageError "Structured fragments require name:type syntax."

let operationHeaderOfText operationText =
  match String.split_on_char ':' operationText with
  | [ operationToken ] -> (
      match operationTypeOfToken (String.trim operationToken) with
      | Some operationType -> (operationType, None)
      | None ->
          raiseUsageError
            (Printf.sprintf "Unknown operation type %s" operationToken))
  | [ operationToken; operationName ] -> (
      match operationTypeOfToken (String.trim operationToken) with
      | Some operationType ->
          let normalizedOperationName = String.trim operationName in

          if normalizedOperationName = "" then
            raiseUsageError "Operation names cannot be empty."
          else (operationType, Some normalizedOperationName)
      | None ->
          raiseUsageError
            (Printf.sprintf "Unknown operation type %s" operationToken))
  | _ -> raiseUsageError "Operations must use type or type:name syntax."

let makeFieldSegment ?fieldAlias fieldName =
  { fieldName; fieldAlias; fieldArgumentPairs = []; fieldDirectiveTexts = [] }

let fieldPathSegmentOfText pathSegmentText =
  let aliasValue, fieldName =
    splitAliasAndFieldName (String.trim pathSegmentText)
  in
  let trimmedFieldName = String.trim fieldName in

  if trimmedFieldName = "" then raiseUsageError "Field path cannot be empty."
  else
    let fieldAlias =
      match aliasValue with
      | Some alias when String.trim alias <> "" ->
          Some (normalizedAlias (String.trim alias))
      | _ -> None
    in

    makeFieldSegment ?fieldAlias trimmedFieldName

let fieldSegmentsOfPath fieldPath =
  fieldPath |> String.split_on_char '.' |> List.map String.trim
  |> List.filter (fun segmentText -> segmentText <> "")
  |> List.map (fun segmentText ->
      FieldSegment (fieldPathSegmentOfText segmentText))

let rootFieldPathPrefix = "root:"
let rootSelectionPathPrefix = "/"
let fragmentSpreadPrefix = "..."

let scopeAndFieldPathOfCliField fieldPath =
  if String.starts_with ~prefix:rootFieldPathPrefix fieldPath then
    ( `Root,
      StringPrefix.valueWithoutPrefix ~prefix:rootFieldPathPrefix fieldPath )
  else if String.starts_with ~prefix:rootSelectionPathPrefix fieldPath then
    ( `Root,
      StringPrefix.valueWithoutPrefix ~prefix:rootSelectionPathPrefix fieldPath
    )
  else (`Relative, fieldPath)

let segmentsOfScopedPath scopedPath =
  if String.starts_with ~prefix:"...on:" scopedPath then
    let inlineFragmentPath =
      StringPrefix.valueWithoutPrefix ~prefix:"...on:" scopedPath
    in

    match String.split_on_char '.' inlineFragmentPath with
    | typeCondition :: (_ :: _ as remainingSegments)
      when String.trim typeCondition <> "" ->
        let fields =
          fieldSegmentsOfPath (String.concat "." remainingSegments)
        in

        if fields = [] then
          invalid_arg "Inline fragments require a selection set.";

        InlineFragmentSegment
          {
            inlineFragmentTypeCondition = String.trim typeCondition;
            inlineFragmentDirectiveTexts = [];
          }
        :: fields
    | _ ->
        invalid_arg
          "Inline fragments require a type condition and selection set"
  else if String.starts_with ~prefix:fragmentSpreadPrefix scopedPath then (
    let fragmentSpreadName =
      StringPrefix.valueWithoutPrefix ~prefix:fragmentSpreadPrefix scopedPath
      |> String.trim
    in

    if fragmentSpreadName = "" then
      invalid_arg "Fragment spreads require a name.";

    [
      FragmentSpreadSegment
        { fragmentSpreadName; fragmentSpreadDirectiveTexts = [] };
    ])
  else fieldSegmentsOfPath scopedPath

let selectionPathsOfExpressions fieldExpressions =
  fieldExpressions
  |> List.concat_map (fun fieldExpression ->
      let paths =
        fieldExpression |> String.split_on_char ',' |> List.map String.trim
        |> List.filter (fun path -> path <> "")
      in

      if paths = [] then invalid_arg "Selection set cannot be empty.";

      List.map
        (fun path ->
          let scope, path = scopeAndFieldPathOfCliField path in
          let segments = segmentsOfScopedPath path in

          if segments = [] then invalid_arg "Selection path cannot be empty.";

          (scope, segments))
        paths)

let makeInlineFragmentSegment inlineFragmentTypeCondition =
  { inlineFragmentTypeCondition; inlineFragmentDirectiveTexts = [] }

let makeFragmentSpreadSegment fragmentSpreadName =
  { fragmentSpreadName; fragmentSpreadDirectiveTexts = [] }

let segmentsEndInFragmentSpread selectionPathSegments =
  match List.rev selectionPathSegments with
  | FragmentSpreadSegment _ :: _ -> true
  | (FieldSegment _ | InlineFragmentSegment _) :: _ | [] -> false

let branchEndsInFragmentSpread branch =
  segmentsEndInFragmentSpread branch.selectionPathSegments

let makeSelectionBranchFromSegments segments =
  if segments = [] then raiseUsageError "Selection path cannot be empty."
  else { selectionPathSegments = segments; selectionPaths = [] }

let makeSelectionBranchFromFieldPath fieldPath =
  fieldPath |> fieldSegmentsOfPath |> makeSelectionBranchFromSegments

let updateLastSegment branch mapSegment =
  match List.rev branch.selectionPathSegments with
  | [] -> raise (Invalid_argument "Empty selection branch")
  | last :: rest ->
      { branch with selectionPathSegments = List.rev (mapSegment last :: rest) }

let withAddedBranchSelectionPaths branch selectionPaths =
  if branchEndsInFragmentSpread branch then
    raiseUsageError "Fragment spreads cannot define a selection set."
  else { branch with selectionPaths = branch.selectionPaths @ selectionPaths }

let withPushedFieldSelectionPathSegment branch fieldName =
  if branchEndsInFragmentSpread branch then
    raiseUsageError "Fragment spreads cannot contain child selections."
  else
    let newSegments = fieldSegmentsOfPath fieldName in

    if newSegments = [] then raiseUsageError "Selection path cannot be empty."
    else
      {
        branch with
        selectionPathSegments = branch.selectionPathSegments @ newSegments;
      }

let withAddedBranchFieldArgumentPair branch fieldArgumentPair =
  updateLastSegment branch (function
    | FieldSegment fieldSegment ->
        FieldSegment
          {
            fieldSegment with
            fieldArgumentPairs =
              fieldSegment.fieldArgumentPairs @ [ fieldArgumentPair ];
          }
    | InlineFragmentSegment _ | FragmentSpreadSegment _ ->
        raiseUsageError
          "Field arguments require the current selection branch to end in a \
           field.")

let withUpdatedBranchFieldAlias branch fieldAlias =
  updateLastSegment branch (function
    | FieldSegment fieldSegment ->
        FieldSegment
          { fieldSegment with fieldAlias = Some (normalizedAlias fieldAlias) }
    | InlineFragmentSegment _ | FragmentSpreadSegment _ ->
        raiseUsageError
          "--alias requires the current selection branch to end in a field.")

let withAddedBranchDirective branch directiveText =
  updateLastSegment branch (function
    | FieldSegment fieldSegment ->
        FieldSegment
          {
            fieldSegment with
            fieldDirectiveTexts =
              fieldSegment.fieldDirectiveTexts @ [ directiveText ];
          }
    | InlineFragmentSegment inlineFragmentSegment ->
        InlineFragmentSegment
          {
            inlineFragmentSegment with
            inlineFragmentDirectiveTexts =
              inlineFragmentSegment.inlineFragmentDirectiveTexts
              @ [ directiveText ];
          }
    | FragmentSpreadSegment fragmentSpreadSegment ->
        FragmentSpreadSegment
          {
            fragmentSpreadSegment with
            fragmentSpreadDirectiveTexts =
              fragmentSpreadSegment.fragmentSpreadDirectiveTexts
              @ [ directiveText ];
          })

let currentSelectionBranchOfState parserState = parserState.currentBranch

let currentDefaultSelectionPathPrefix parserState =
  parserState.currentDefaultSelectionPathPrefix

let ensureSelectionPathCanAcceptChildren selectionPathSegments =
  if segmentsEndInFragmentSpread selectionPathSegments then
    raiseUsageError "Fragment spreads cannot contain child selections."
  else selectionPathSegments

let classifiedBranchPath pathText =
  if String.starts_with ~prefix:currentBranchPathPrefix pathText then
    ( `Relative,
      StringPrefix.valueWithoutPrefix ~prefix:currentBranchPathPrefix pathText
    )
  else if String.starts_with ~prefix:absoluteBranchPathPrefix pathText then
    ( `Absolute,
      StringPrefix.valueWithoutPrefix ~prefix:absoluteBranchPathPrefix pathText
    )
  else (`Default, pathText)

let currentBranchSegmentsOrRaise parserState ~requiredMessage =
  match parserState.currentBranch with
  | Some branch ->
      ensureSelectionPathCanAcceptChildren branch.selectionPathSegments
  | None -> raiseUsageError requiredMessage

let parentSegmentsAndBareText parserState ~currentBranchRequiredMessage pathText
    =
  match classifiedBranchPath pathText with
  | `Relative, bareText ->
      ( currentBranchSegmentsOrRaise parserState
          ~requiredMessage:currentBranchRequiredMessage,
        bareText )
  | `Absolute, bareText -> ([], bareText)
  | `Default, bareText ->
      let parentSegments =
        match parserState.currentBranch with
        | Some branch ->
            ensureSelectionPathCanAcceptChildren branch.selectionPathSegments
        | None -> parserState.currentDefaultSelectionPathPrefix
      in

      (parentSegments, bareText)

let resolvedSelectionPathSegmentsOfFieldPath parserState fieldPath =
  let nonemptyFieldSegments path =
    let segments = fieldSegmentsOfPath path in

    if segments = [] then raiseUsageError "Selection path cannot be empty."
    else segments
  in

  match classifiedBranchPath fieldPath with
  | `Relative, bareFieldPath ->
      currentBranchSegmentsOrRaise parserState
        ~requiredMessage:
          "Relative --field paths require an existing selection branch."
      @ nonemptyFieldSegments bareFieldPath
  | `Absolute, bareFieldPath -> nonemptyFieldSegments bareFieldPath
  | `Default, bareFieldPath ->
      parserState.currentDefaultSelectionPathPrefix
      @ nonemptyFieldSegments bareFieldPath

let resolvedSelectionPathSegmentsOfInlineFragment parserState
    inlineFragmentTypeConditionText =
  let parentSelectionPathSegments, typeConditionText =
    parentSegmentsAndBareText parserState
      ~currentBranchRequiredMessage:
        "Relative --inline-fragment paths require an existing selection branch."
      inlineFragmentTypeConditionText
  in
  let normalizedTypeCondition = String.trim typeConditionText in

  if normalizedTypeCondition = "" then
    raiseUsageError "Inline fragments require a type condition."
  else
    parentSelectionPathSegments
    @ [
        InlineFragmentSegment
          (makeInlineFragmentSegment normalizedTypeCondition);
      ]

let resolvedSelectionPathSegmentsOfFragmentSpread parserState fragmentSpreadText
    =
  let parentSelectionPathSegments, fragmentSpreadName =
    parentSegmentsAndBareText parserState
      ~currentBranchRequiredMessage:
        "Relative --fragment-spread paths require an existing selection branch."
      fragmentSpreadText
  in
  let normalizedFragmentSpreadName = String.trim fragmentSpreadName in

  if normalizedFragmentSpreadName = "" then
    raiseUsageError "Fragment spreads require a fragment name."
  else
    parentSelectionPathSegments
    @ [
        FragmentSpreadSegment
          (makeFragmentSpreadSegment normalizedFragmentSpreadName);
      ]

let withMappedCurrentTarget parserState ~onFragment ~onOperation ~onNeither =
  match parserState.currentTarget with
  | Some (FragmentTarget fragment) ->
      {
        parserState with
        currentTarget = Some (FragmentTarget (onFragment fragment));
      }
  | Some (OperationTarget operation) ->
      {
        parserState with
        currentTarget = Some (OperationTarget (onOperation operation));
      }
  | None -> onNeither parserState

let finalizedCurrentBranch parserState =
  match parserState.currentBranch with
  | None -> parserState
  | Some branch ->
      let parserState =
        withMappedCurrentTarget parserState
          ~onFragment:(fun fragment ->
            {
              fragment with
              fragmentSelectionBranches =
                fragment.fragmentSelectionBranches @ [ branch ];
            })
          ~onOperation:(fun operationDefinition ->
            {
              operationDefinition with
              selectionBranches =
                operationDefinition.selectionBranches @ [ branch ];
            })
          ~onNeither:(fun parserState -> parserState)
      in

      { parserState with currentBranch = None }

let finalizedAllPending parserState =
  let parserState = finalizedCurrentBranch parserState in
  let finalizedState =
    match parserState.currentTarget with
    | None -> parserState
    | Some (FragmentTarget fragment) ->
        {
          parserState with
          finalizedStructuredFragmentDefinitions =
            fragment :: parserState.finalizedStructuredFragmentDefinitions;
        }
    | Some (OperationTarget operation) ->
        {
          parserState with
          finalizedOperationDefinitions =
            operation :: parserState.finalizedOperationDefinitions;
        }
  in

  {
    finalizedState with
    currentTarget = None;
    currentDefaultSelectionPathPrefix = [];
  }

let withUpdatedCurrentSelectionBranch parserState branch =
  if parserState.currentTarget = None then
    raiseUsageError "Selections require an operation or fragment context."
  else { parserState with currentBranch = Some branch }

let withAddedRootSelectionPaths parserState selectionPaths =
  withMappedCurrentTarget parserState
    ~onFragment:(fun fragment ->
      {
        fragment with
        fragmentRootSelectionPaths =
          fragment.fragmentRootSelectionPaths @ selectionPaths;
      })
    ~onOperation:(fun operationDefinition ->
      {
        operationDefinition with
        rootSelectionPaths =
          operationDefinition.rootSelectionPaths @ selectionPaths;
      })
    ~onNeither:(fun _ ->
      raiseUsageError "Selections require an operation or fragment context.")

let withAddedCurrentTargetDirective parserState directiveText =
  withMappedCurrentTarget parserState
    ~onFragment:(fun fragment ->
      {
        fragment with
        fragmentDirectiveTexts =
          fragment.fragmentDirectiveTexts @ [ directiveText ];
      })
    ~onOperation:(fun operationDefinition ->
      {
        operationDefinition with
        operationDirectiveTexts =
          operationDefinition.operationDirectiveTexts @ [ directiveText ];
      })
    ~onNeither:(fun _ ->
      raiseUsageError "Directives require an operation or fragment context.")

let startedOperationDefinition parserState operationType operationName =
  let parserState = finalizedAllPending parserState in

  {
    parserState with
    currentTarget =
      Some
        (OperationTarget
           {
             operationType;
             operationName;
             variableDefinitions = [];
             variableAssignments = [];
             operationDirectiveTexts = [];
             rootSelectionPaths = [];
             selectionBranches = [];
           });
    currentDefaultSelectionPathPrefix = [];
  }

let startedShorthandQueryOperation parserState firstFieldToken =
  let branch = makeSelectionBranchFromFieldPath firstFieldToken in

  {
    (startedOperationDefinition parserState Query None) with
    currentBranch = Some branch;
    currentDefaultSelectionPathPrefix = branch.selectionPathSegments;
  }

let startedStructuredFragmentDefinition parserState fragmentName
    fragmentTypeCondition =
  let parserState = finalizedAllPending parserState in

  {
    parserState with
    currentTarget =
      Some
        (FragmentTarget
           {
             fragmentName;
             fragmentTypeCondition;
             fragmentDirectiveTexts = [];
             fragmentRootSelectionPaths = [];
             fragmentSelectionBranches = [];
           });
  }

let initialParserState =
  {
    finalizedOperationDefinitions = [];
    currentTarget = None;
    currentDefaultSelectionPathPrefix = [];
    currentBranch = None;
    requestedOperationName = None;
    finalizedStructuredFragmentDefinitions = [];
    pendingRawFragmentDefinitionTexts = [];
  }

let currentInvocationOfState parserState =
  let parserState = finalizedAllPending parserState in

  {
    operationDefinitions = List.rev parserState.finalizedOperationDefinitions;
    selectedOperationName = parserState.requestedOperationName;
    structuredFragmentDefinitions =
      List.rev parserState.finalizedStructuredFragmentDefinitions;
    rawFragmentDefinitionTexts = parserState.pendingRawFragmentDefinitionTexts;
  }

let operationDefinitionsHaveSelections operationDefinitions =
  operationDefinitions
  |> List.for_all (fun operationDefinition ->
      operationDefinition.rootSelectionPaths <> []
      || operationDefinition.selectionBranches <> [])

let structuredFragmentsHaveSelections structuredFragmentDefinitions =
  structuredFragmentDefinitions
  |> List.for_all (fun fragmentDefinition ->
      fragmentDefinition.fragmentRootSelectionPaths <> []
      || fragmentDefinition.fragmentSelectionBranches <> [])

let branchHasCompleteInlineFragment branch =
  match List.rev branch.selectionPathSegments with
  | InlineFragmentSegment _ :: _ ->
      List.exists (fun (scope, _) -> scope = `Relative) branch.selectionPaths
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
    Error (withUsageText "Missing operation selection.")
  else if
    not (operationDefinitionsHaveSelections invocation.operationDefinitions)
  then Error (withUsageText "Each operation requires a selection set.")
  else if
    not
      (structuredFragmentsHaveSelections
         invocation.structuredFragmentDefinitions)
  then
    Error (withUsageText "Each structured fragment requires a selection set.")
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
      Error (withUsageText "Operation names must be unique.")
    else if
      List.length invocation.operationDefinitions > 1
      && List.exists
           (fun operationDefinition -> operationDefinition.operationName = None)
           invocation.operationDefinitions
    then
      Error
        (withUsageText
           "Multiple-operation documents require every operation to be named.")
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
              (withUsageText
                 (Printf.sprintf
                    "Selected operation %s does not exist in the document."
                    selectedOperationName))
      | None ->
          if List.length invocation.operationDefinitions > 1 then
            Error
              (withUsageText
                 "Multiple-operation documents require \
                  --execute-operation-name.")
          else Ok invocation

exception HelpRequested

let bareOptionNameOfToken token =
  match optionNameAndValueOfEqualsSyntax token with
  | Some (optionName, _) -> optionName
  | None -> String.sub token 2 (String.length token - 2)

let replaceCurrentSelectionBranchWithSegments parserState resolvedSegments =
  let rec extendsPath parent child =
    match (parent, child) with
    | [], _ :: _ -> true
    | parentSegment :: parentTail, childSegment :: childTail
      when parentSegment = childSegment ->
        extendsPath parentTail childTail
    | _ -> false
  in
  let parserState =
    match parserState.currentBranch with
    | Some branch
      when branch.selectionPaths = []
           && extendsPath branch.selectionPathSegments resolvedSegments ->
        { parserState with currentBranch = None }
    | _ -> finalizedCurrentBranch parserState
  in

  withUpdatedCurrentSelectionBranch parserState
    (makeSelectionBranchFromSegments resolvedSegments)

let withCurrentOperationOrRaise ~optionDescription parserState mapOperation =
  match parserState.currentTarget with
  | Some (OperationTarget operationDefinition) ->
      {
        parserState with
        currentTarget =
          Some (OperationTarget (mapOperation operationDefinition));
      }
  | None | Some (FragmentTarget _) ->
      raiseUsageError
        (Printf.sprintf "%s requires a current operation." optionDescription)

let updateBranchOrFallback parserState ~onBranch ~onNoBranch =
  match currentSelectionBranchOfState parserState with
  | Some branch ->
      withUpdatedCurrentSelectionBranch parserState (onBranch branch)
  | None -> onNoBranch parserState

let handleOperation parserState operationHeaderText =
  let operationType, operationName =
    operationHeaderOfText operationHeaderText
  in

  startedOperationDefinition parserState operationType operationName

let handleSelectionPath resolveSelectionPathSegments parserState pathText =
  replaceCurrentSelectionBranchWithSegments parserState
    (resolveSelectionPathSegments parserState pathText)

let handleFragment parserState fragmentText =
  let fragmentName, fragmentTypeCondition =
    structuredFragmentNameAndTypeCondition fragmentText
  in

  startedStructuredFragmentDefinition parserState fragmentName
    fragmentTypeCondition

let handleSelectionSet parserState expressionText =
  let selectionPaths = selectionPathsOfExpressions [ expressionText ] in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch ->
      withAddedBranchSelectionPaths branch selectionPaths)
    ~onNoBranch:(fun state -> withAddedRootSelectionPaths state selectionPaths)

let handleOperationName parserState operationName =
  withCurrentOperationOrRaise ~optionDescription:"--operation-name" parserState
    (fun operationDefinition ->
      { operationDefinition with operationName = Some operationName })

let handleSelectedOperationName parserState selectedOperationName =
  { parserState with requestedOperationName = Some selectedOperationName }

let handleVariableDefinition parserState variableDefinition =
  withCurrentOperationOrRaise ~optionDescription:"--variable-definition"
    parserState (fun operationDefinition ->
      {
        operationDefinition with
        variableDefinitions =
          operationDefinition.variableDefinitions @ [ variableDefinition ];
      })

let handleVariable parserState variableAssignmentText =
  withCurrentOperationOrRaise ~optionDescription:"--variable" parserState
    (fun operationDefinition ->
      {
        operationDefinition with
        variableAssignments =
          operationDefinition.variableAssignments
          @ [ variableAssignmentOfText variableAssignmentText ];
      })

let handleDirective parserState directiveText =
  let directiveText = GraphQlSelection.renderDirectiveText directiveText in

  updateBranchOrFallback parserState
    ~onBranch:(fun branch -> withAddedBranchDirective branch directiveText)
    ~onNoBranch:(fun state ->
      withAddedCurrentTargetDirective state directiveText)

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
    pendingRawFragmentDefinitionTexts =
      finalizedParserState.pendingRawFragmentDefinitionTexts
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
           "Field argument %s requires an existing selection branch."
           (fst fieldArgumentPair)))

let optionHandlers =
  [
    ("operation", handleOperation);
    ("field", handleSelectionPath resolvedSelectionPathSegmentsOfFieldPath);
    ( "inline-fragment",
      handleSelectionPath resolvedSelectionPathSegmentsOfInlineFragment );
    ( "fragment-spread",
      handleSelectionPath resolvedSelectionPathSegmentsOfFragmentSpread );
    ("spread", handleSelectionPath resolvedSelectionPathSegmentsOfFragmentSpread);
    ("fragment", handleFragment);
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
  match currentSelectionBranchOfState parserState with
  | Some branch
    when currentDefaultSelectionPathPrefix parserState = []
         && branch.selectionPaths <> [] ->
      let finalizedParserState = finalizedCurrentBranch parserState in

      replaceCurrentSelectionBranchWithSegments finalizedParserState
        (resolvedSelectionPathSegmentsOfFieldPath finalizedParserState token)
  | Some branch ->
      withUpdatedCurrentSelectionBranch parserState
        (withPushedFieldSelectionPathSegment branch token)
  | None -> (
      match parserState.currentTarget with
      | None | Some (FragmentTarget _) -> (
          match operationTypeOfToken token with
          | Some operationType ->
              startedOperationDefinition parserState operationType None
          | None -> startedShorthandQueryOperation parserState token)
      | Some (OperationTarget _) ->
          replaceCurrentSelectionBranchWithSegments parserState
            (resolvedSelectionPathSegmentsOfFieldPath parserState token))

let rec parseRemainingArguments parserState = function
  | [] -> currentInvocationOfState parserState
  | "--help" :: _ -> raise HelpRequested
  | token :: remainingTokens when isLongOption token ->
      let updatedParserState, remainingTokens =
        match List.assoc_opt (bareOptionNameOfToken token) optionHandlers with
        | Some handler ->
            let optionValue, remainingTokens =
              valueOfOptionToken token remainingTokens
            in

            (handler parserState optionValue, remainingTokens)
        | None ->
            let fieldArgumentPair, remainingTokens =
              optionPairOfToken token remainingTokens
            in

            (handleFieldArgument parserState fieldArgumentPair, remainingTokens)
      in

      parseRemainingArguments updatedParserState remainingTokens
  | token :: remainingTokens ->
      parseRemainingArguments
        (handlePositionalToken parserState token)
        remainingTokens

let ofArguments arguments =
  match arguments with
  | [] -> Error usageText
  | "help" :: _ | "--help" :: _ -> Ok `Help
  | _ -> (
      try
        let invocation = parseRemainingArguments initialParserState arguments in

        Result.map
          (fun invocation -> `Invocation invocation)
          (validatedInvocation invocation)
      with
      | HelpRequested -> Ok `Help
      | Invalid_argument message -> Error message
      | Yojson.Json_error message -> Error ("Invalid JSON: " ^ message))
