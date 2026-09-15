open Lwt.Infix

let filterDirectiveResponse ~directiveName responseBody =
  let filteredDirectives directives =
    List.filter
      (fun directive ->
        Yojson.Safe.Util.member "name" directive = `String directiveName)
      directives
  in

  try
    match Yojson.Safe.from_string responseBody with
    | `Assoc rootAssociation when List.mem_assoc "errors" rootAssociation ->
        Ok responseBody
    | `Assoc rootAssociation -> (
        match List.assoc_opt "data" rootAssociation with
        | Some (`Assoc dataAssociation) -> (
            match List.assoc_opt "__schema" dataAssociation with
            | Some (`Assoc schemaAssociation) -> (
                match List.assoc_opt "directives" schemaAssociation with
                | Some (`List directives) ->
                    let replaceField name value fields =
                      List.map
                        (fun (fieldName, fieldValue) ->
                          ( fieldName,
                            if fieldName = name then value else fieldValue ))
                        fields
                    in
                    let schema =
                      `Assoc
                        (replaceField "directives"
                           (`List (filteredDirectives directives))
                           schemaAssociation)
                    in
                    let data =
                      `Assoc (replaceField "__schema" schema dataAssociation)
                    in

                    Ok
                      (`Assoc (replaceField "data" data rootAssociation)
                      |> Yojson.Safe.pretty_to_string)
                | _ -> Ok responseBody)
            | _ -> Ok responseBody)
        | _ -> Ok responseBody)
    | _ -> Ok responseBody
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> Ok responseBody

let execute ~headers ~endpoint (schemaCommand : SchemaCommandTypes.t) =
  GraphQlTransport.executeQuery
    ~operationName:schemaCommand.SchemaCommandTypes.operationName
    ~variables:schemaCommand.SchemaCommandTypes.variables ~headers ~endpoint
    ~query:schemaCommand.SchemaCommandTypes.queryText
  >>= fun (statusCode, responseBody) ->
  match schemaCommand.SchemaCommandTypes.requestedDirectiveName with
  | None -> Lwt.return (statusCode, responseBody)
  | Some _ when statusCode < 200 || statusCode >= 300 ->
      Lwt.return (statusCode, responseBody)
  | Some directiveName -> (
      match filterDirectiveResponse ~directiveName responseBody with
      | Ok filteredResponse -> Lwt.return (statusCode, filteredResponse)
      | Error message -> Lwt.fail_with message)
