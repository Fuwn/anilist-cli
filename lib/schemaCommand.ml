open Lwt.Infix

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

let execute ~headers ~endpoint (schemaCommand : SchemaCommandTypes.t) =
  GraphQlTransport.executeQuery
    ~operationName:schemaCommand.SchemaCommandTypes.operationName
    ~variables:schemaCommand.SchemaCommandTypes.variables ~headers ~endpoint
    ~query:schemaCommand.SchemaCommandTypes.queryText
  >|= fun (statusCode, responseBody) ->
  match schemaCommand.SchemaCommandTypes.requestedDirectiveName with
  | Some directiveName when statusCode >= 200 && statusCode < 300 ->
      (statusCode, filterDirectiveResponse ~directiveName responseBody)
  | Some _ | None -> (statusCode, responseBody)
