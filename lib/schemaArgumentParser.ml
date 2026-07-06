type parsedArguments = {
  typeName : string option;
  directiveName : string option;
}

let usageText =
  String.concat "\n"
    [
      "Usage:";
      "  anilist schema";
      "  anilist schema --type <Type>";
      "  anilist schema --directive <Directive>";
      "  anilist ... --header 'Authorization: Bearer <token>'";
      "";
      "Examples:";
      "  anilist schema";
      "  anilist schema --type Media";
      "  anilist schema --directive include";
    ]

let helpRequestedOfArguments = function
  | [ "schema"; "--help" ] | [ "schema"; "help" ] -> true
  | _ -> false

let missingValueError optionName =
  Error
    (Printf.sprintf "Schema --%s requires a value.\n\n%s" optionName usageText)

let setOption optionName value current =
  if String.starts_with ~prefix:"--" value then missingValueError optionName
  else
    match current with
    | None -> Ok (Some value)
    | Some _ ->
        Error
          (Printf.sprintf "Schema command accepts at most one --%s option."
             optionName)

let ( let* ) = Result.bind

let parseArguments arguments =
  let rec loop accumulated = function
    | [] -> Ok accumulated
    | "--type" :: [] -> missingValueError "type"
    | "--directive" :: [] -> missingValueError "directive"
    | "--type" :: value :: rest ->
        let* typeName = setOption "type" value accumulated.typeName in
        loop { accumulated with typeName } rest
    | "--directive" :: value :: rest ->
        let* directiveName =
          setOption "directive" value accumulated.directiveName
        in
        loop { accumulated with directiveName } rest
    | argument :: rest when String.starts_with ~prefix:"--type=" argument ->
        loop accumulated
          ("--type"
          :: StringPrefix.valueWithoutPrefix ~prefix:"--type=" argument
          :: rest)
    | argument :: rest when String.starts_with ~prefix:"--directive=" argument
      ->
        loop accumulated
          ("--directive"
          :: StringPrefix.valueWithoutPrefix ~prefix:"--directive=" argument
          :: rest)
    | argument :: _ ->
        Error
          (Printf.sprintf "Invalid schema argument: %s\n\n%s" argument usageText)
  in
  loop { typeName = None; directiveName = None } arguments

let commandOfParsedArguments = function
  | { typeName = Some _; directiveName = Some _ } ->
      Error
        (Printf.sprintf
           "Schema command accepts only one of --type or --directive.\n\n%s"
           usageText)
  | { typeName = Some ""; directiveName = None } ->
      Error
        (Printf.sprintf "Schema --type requires a non-empty type name.\n\n%s"
           usageText)
  | { typeName = Some name; directiveName = None } ->
      Ok (Some (SchemaCommandTypes.typeCommand name))
  | { typeName = None; directiveName = Some "" } ->
      Error
        (Printf.sprintf
           "Schema --directive requires a non-empty directive name.\n\n%s"
           usageText)
  | { typeName = None; directiveName = Some name } ->
      Ok (Some (SchemaCommandTypes.directiveCommand name))
  | { typeName = None; directiveName = None } ->
      Error (Printf.sprintf "Invalid schema command.\n\n%s" usageText)

let invocationOfArguments = function
  | [ "schema" ] -> Ok (Some SchemaCommandTypes.fullSchemaCommand)
  | "schema" :: [ "--help" ] | "schema" :: [ "help" ] -> Error usageText
  | "schema" :: schemaArguments ->
      Result.bind (parseArguments schemaArguments) commandOfParsedArguments
  | _ -> Ok None
