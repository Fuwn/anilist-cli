open Anilist
open Lwt.Infix

let printAndExit channel exitCode message =
  output_string channel message;
  output_char channel '\n';
  flush channel;
  exit exitCode

let printResponse statusCode responseBody =
  let printableResponse =
    GraphQlTransport.prettyPrintedJsonOrOriginal responseBody
  in
  if statusCode >= 200 && statusCode < 300 then (
    print_endline printableResponse;
    Lwt.return 0)
  else (
    prerr_endline printableResponse;
    Lwt.return 1)

let executeQuery ~headers ~operationName ~variables ~query =
  let endpoint = GraphQlTransport.endpointOfEnvironment () in
  GraphQlTransport.executeQuery ~operationName ~variables ~headers ~endpoint
    ~query
  >>= fun (statusCode, responseBody) -> printResponse statusCode responseBody

let runProtected task =
  Lwt.catch task (fun exceptionValue ->
      prerr_endline
        (Printf.sprintf "Request failed: %s"
           (Printexc.to_string exceptionValue));
      Lwt.return 1)

let run ~headers invocation =
  runProtected (fun () ->
      let loweredRequest = LoweringEngine.lower invocation in
      let query =
        loweredRequest |> LoweringEngine.graphQlQueryOfRequest
        |> GraphQlQuery.render
      in
      let variables =
        match loweredRequest.LoweringEngine.loweredVariableAssignments with
        | [] -> None
        | variableAssignments ->
            Some
              (`Assoc
                 (variableAssignments
                 |> List.map (fun (variableName, variableValue) ->
                     (variableName, CliArgument.jsonLiteralOfValue variableValue))
                 ))
      in
      executeQuery ~headers
        ~operationName:loweredRequest.LoweringEngine.selectedOperationName
        ~variables ~query)

let () =
  let arguments = List.tl (Array.to_list Sys.argv) in
  match RequestOptions.extractionOfArguments arguments with
  | Error message -> printAndExit stderr 1 message
  | Ok requestOptions -> (
      match
        SchemaArgumentParser.invocationOfArguments
          requestOptions.RequestOptions.remainingArguments
      with
      | Ok (Some schemaCommand) ->
          exit
            (Lwt_main.run
               (runProtected (fun () ->
                    let endpoint = GraphQlTransport.endpointOfEnvironment () in
                    SchemaCommand.execute
                      ~headers:requestOptions.RequestOptions.headerPairs
                      ~endpoint schemaCommand
                    >>= fun (statusCode, responseBody) ->
                    printResponse statusCode responseBody)))
      | Error message ->
          if
            SchemaArgumentParser.helpRequestedOfArguments
              requestOptions.RequestOptions.remainingArguments
          then printAndExit stdout 0 message
          else printAndExit stderr 1 message
      | Ok None -> (
          match
            CommandLineInvocationParser.invocationOfArguments
              requestOptions.RequestOptions.remainingArguments
          with
          | Ok invocation ->
              exit
                (Lwt_main.run
                   (run ~headers:requestOptions.RequestOptions.headerPairs
                      invocation))
          | Error message ->
              if
                CommandLineInvocationShared.helpRequestedOfArguments
                  requestOptions.RequestOptions.remainingArguments
              then printAndExit stdout 0 message
              else printAndExit stderr 1 message))
