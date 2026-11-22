open Anilist
open Lwt.Infix

let printAndExit channel exitCode message =
  output_string channel message;
  output_char channel '\n';
  flush channel;
  exit exitCode

let exitCodeAfterPrintingResponse (statusCode, responseBody) =
  let printableResponse =
    GraphQlTransport.prettyPrintedJsonOrOriginal responseBody
  in

  if statusCode >= 200 && statusCode < 300 then (
    print_endline printableResponse;
    0)
  else (
    prerr_endline printableResponse;
    1)

let exitAfterRequest request =
  exit
    (Lwt_main.run
       (Lwt.catch
          (fun () ->
            request (GraphQlTransport.endpointOfEnvironment ())
            >|= exitCodeAfterPrintingResponse)
          (fun exceptionValue ->
            prerr_endline
              (Printf.sprintf "Request failed: %s"
                 (Printexc.to_string exceptionValue));
            Lwt.return 1)))

let executeInvocation ~headers ~endpoint invocation =
  let loweredRequest = LoweringEngine.lower invocation in
  let query = GraphQlDocument.render loweredRequest.LoweringEngine.document in
  let variables =
    match loweredRequest.LoweringEngine.selectedVariableAssignments with
    | [] -> None
    | variableAssignments ->
        Some
          (`Assoc
             (variableAssignments
             |> List.map (fun (variableName, variableValue) ->
                 (variableName, CliArgument.jsonLiteralOfValue variableValue))))
  in

  GraphQlTransport.executeQuery
    ~operationName:invocation.CommandLineInvocation.selectedOperationName
    ~variables ~headers ~endpoint ~query

let () =
  match
    RequestOptions.extractionOfArguments (List.tl (Array.to_list Sys.argv))
  with
  | Error message -> printAndExit stderr 1 message
  | Ok { RequestOptions.headerPairs = headers; remainingArguments } -> (
      match SchemaCommand.ofArguments remainingArguments with
      | Ok (Some schemaCommand) ->
          exitAfterRequest (fun endpoint ->
              SchemaCommand.execute ~headers ~endpoint schemaCommand)
      | Error message ->
          if SchemaCommand.helpRequestedOfArguments remainingArguments then
            printAndExit stdout 0 message
          else printAndExit stderr 1 message
      | Ok None -> (
          match CommandLineInvocation.ofArguments remainingArguments with
          | Ok `Help -> printAndExit stdout 0 CommandLineInvocation.usageText
          | Ok (`Invocation invocation) ->
              exitAfterRequest (fun endpoint ->
                  executeInvocation ~headers ~endpoint invocation)
          | Error message -> printAndExit stderr 1 message))
