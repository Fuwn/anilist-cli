open Lwt.Infix

type t = {
  operationName : string option;
  queryText : string;
  variables : Yojson.Safe.t option;
  requestedDirectiveName : string option;
}

let fullSchemaCommand =
  {
    operationName = Some "IntrospectionQuery";
    queryText = SchemaQueries.fullSchemaQueryText;
    variables = None;
    requestedDirectiveName = None;
  }

let typeCommand typeName =
  {
    operationName = Some "IntrospectionTypeQuery";
    queryText = SchemaQueries.typeQueryText;
    variables = Some (`Assoc [ ("name", `String typeName) ]);
    requestedDirectiveName = None;
  }

let directiveCommand directiveName =
  {
    operationName = Some "IntrospectionDirectiveQuery";
    queryText = SchemaQueries.directiveQueryText;
    variables = None;
    requestedDirectiveName = Some directiveName;
  }

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
    | argument :: rest -> (
        match
          ( StringPrefix.valueWithoutPrefix ~prefix:"--type=" argument,
            StringPrefix.valueWithoutPrefix ~prefix:"--directive=" argument )
        with
        | Some value, _ -> loop accumulated ("--type" :: value :: rest)
        | None, Some value -> loop accumulated ("--directive" :: value :: rest)
        | None, None ->
            Error
              (Printf.sprintf "Invalid schema argument: %s\n\n%s" argument
                 usageText))
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
      Ok (`Command (typeCommand name))
  | { typeName = None; directiveName = Some "" } ->
      Error
        (Printf.sprintf
           "Schema --directive requires a non-empty directive name.\n\n%s"
           usageText)
  | { typeName = None; directiveName = Some name } ->
      Ok (`Command (directiveCommand name))
  | { typeName = None; directiveName = None } ->
      Error (Printf.sprintf "Invalid schema command.\n\n%s" usageText)

let ofArguments = function
  | [ "schema" ] -> Ok (`Command fullSchemaCommand)
  | [ "schema"; ("--help" | "help") ] -> Ok `Help
  | "schema" :: schemaArguments ->
      Result.bind (parseArguments schemaArguments) commandOfParsedArguments
  | _ -> Ok `NotSchemaCommand

let filterDirectiveResponse ~directiveName responseBody =
  let replaceField name value fields =
    List.map
      (fun (fieldName, fieldValue) ->
        (fieldName, if fieldName = name then value else fieldValue))
      fields
  in

  try
    let root = Yojson.Safe.from_string responseBody in
    let open Yojson.Safe.Util in
    let rootFields = to_assoc root in

    if List.mem_assoc "errors" rootFields then responseBody
    else
      let data = member "data" root in
      let schema = member "__schema" data in
      let directives = member "directives" schema |> to_list in
      let filtered =
        List.filter
          (fun directive -> member "name" directive = `String directiveName)
          directives
      in
      let schema =
        `Assoc (replaceField "directives" (`List filtered) (to_assoc schema))
      in
      let data = `Assoc (replaceField "__schema" schema (to_assoc data)) in

      `Assoc (replaceField "data" data rootFields)
      |> Yojson.Safe.pretty_to_string
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> responseBody

let execute ~headers schemaCommand =
  GraphQlTransport.executeQuery ~operationName:schemaCommand.operationName
    ~variables:schemaCommand.variables ~headers ~query:schemaCommand.queryText
  >|= fun (statusCode, responseBody) ->
  match schemaCommand.requestedDirectiveName with
  | Some directiveName when statusCode >= 200 && statusCode < 300 ->
      (statusCode, filterDirectiveResponse ~directiveName responseBody)
  | Some _ | None -> (statusCode, responseBody)
