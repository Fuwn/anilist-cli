open CommandLineInvocationTypes

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

let helpRequestedOfArguments = function
  | [ "help" ] | [ "--help" ] | [ _; "--help" ] -> true
  | _ -> false

let selectionSetOptionName = "selection-set"
let compatibilitySelectionSetOptionName = "fields"
let fieldOptionName = "field"
let inlineFragmentOptionName = "inline-fragment"
let fragmentSpreadOptionName = "fragment-spread"
let fragmentSpreadCompatibilityOptionName = "spread"
let fragmentOptionName = "fragment"
let operationOptionName = "operation"
let operationNameOptionName = "operation-name"
let selectedOperationNameOptionName = "execute-operation-name"
let variableDefinitionOptionName = "variable-definition"
let variableOptionName = "variable"
let directiveOptionName = "directive"
let aliasOptionName = "alias"
let fragmentDefinitionOptionName = "fragment-definition"
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
      raise
        (Invalid_argument
           (Printf.sprintf "Invalid field path segment %s\n\n%s" pathSegment
              usageText))

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
      | optionValue :: tail ->
          let optionName = String.sub token 2 (String.length token - 2) in
          ((optionName, optionValue), tail)
      | [] ->
          raise
            (Invalid_argument
               (Printf.sprintf "Missing value for option %s\n\n%s" token
                  usageText)))

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
        raise
          (Invalid_argument
             (Printf.sprintf "Variable assignment requires a name.\n\n%s"
                usageText))
      else if String.contains variableName '-' then
        raise
          (Invalid_argument
             (Printf.sprintf
                "Variable names must use GraphQL variable syntax.\n\n%s"
                usageText))
      else if String.starts_with ~prefix:"var:" rawValue then
        raise
          (Invalid_argument
             (Printf.sprintf
                "Variable assignments cannot contain variable references.\n\n%s"
                usageText))
      else (variableName, rawValue)
  | _ ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "Variable assignments must use name=value syntax.\n\n%s" usageText))

let structuredFragmentNameAndTypeCondition fragmentText =
  match String.split_on_char ':' fragmentText with
  | [ fragmentName; typeCondition ] ->
      let normalizedFragmentName = String.trim fragmentName in
      let normalizedTypeCondition = String.trim typeCondition in
      if normalizedFragmentName = "" || normalizedTypeCondition = "" then
        raise
          (Invalid_argument
             (Printf.sprintf
                "Structured fragments require name:type syntax.\n\n%s" usageText))
      else (normalizedFragmentName, normalizedTypeCondition)
  | _ ->
      raise
        (Invalid_argument
           (Printf.sprintf
              "Structured fragments require name:type syntax.\n\n%s" usageText))

let operationHeaderOfText operationText =
  match String.split_on_char ':' operationText with
  | [ operationToken ] -> (
      match operationTypeOfToken (String.trim operationToken) with
      | Some operationType -> (operationType, None)
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf "Unknown operation type %s\n\n%s" operationToken
                  usageText)))
  | [ operationToken; operationName ] -> (
      match operationTypeOfToken (String.trim operationToken) with
      | Some operationType ->
          let normalizedOperationName = String.trim operationName in
          if normalizedOperationName = "" then
            raise
              (Invalid_argument
                 (Printf.sprintf "Operation names cannot be empty.\n\n%s"
                    usageText))
          else (operationType, Some normalizedOperationName)
      | None ->
          raise
            (Invalid_argument
               (Printf.sprintf "Unknown operation type %s\n\n%s" operationToken
                  usageText)))
  | _ ->
      raise
        (Invalid_argument
           (Printf.sprintf "Operations must use type or type:name syntax.\n\n%s"
              usageText))
