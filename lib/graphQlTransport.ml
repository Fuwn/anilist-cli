open Lwt.Infix

let defaultEndpoint = "https://graphql.anilist.co"

let endpointOfEnvironment () =
  match Sys.getenv_opt "ANILIST_GRAPHQL_ENDPOINT" with
  | Some endpoint when endpoint <> "" -> endpoint
  | _ -> defaultEndpoint

let requestTimeoutSeconds () =
  match Sys.getenv_opt "ANILIST_REQUEST_TIMEOUT_SECONDS" with
  | None -> 30.
  | Some text -> (
      match float_of_string_opt text with
      | Some seconds when Float.is_finite seconds && seconds > 0. -> seconds
      | Some _ | None ->
          invalid_arg
            "ANILIST_REQUEST_TIMEOUT_SECONDS must be a positive finite number")

let executeQuery ~operationName ~variables ~headers:additionalHeaders ~query =
  let requestBody =
    `Assoc
      ([ ("query", `String query) ]
      @ Option.to_list
          (Option.map
             (fun operationName -> ("operationName", `String operationName))
             operationName)
      @ Option.to_list
          (Option.map (fun variables -> ("variables", variables)) variables))
    |> Yojson.Safe.to_string
  in
  let headers =
    List.fold_left
      (fun requestHeaders (headerName, headerValue) ->
        Cohttp.Header.replace requestHeaders headerName headerValue)
      (Cohttp.Header.of_list
         [
           ("content-type", "application/json"); ("accept", "application/json");
         ])
      additionalHeaders
  in
  let uri = Uri.of_string (endpointOfEnvironment ()) in
  let body = Cohttp_lwt.Body.of_string requestBody in
  let timeoutSeconds = requestTimeoutSeconds () in

  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout timeoutSeconds (fun () ->
          Cohttp_lwt_unix.Client.post ~headers ~body uri
          >>= fun (response, responseBody) ->
          Cohttp_lwt.Body.to_string responseBody >|= fun responseBodyText ->
          ( Cohttp.Response.status response |> Cohttp.Code.code_of_status,
            responseBodyText )))
    (function
      | Lwt_unix.Timeout ->
          Lwt.fail_with
            (Printf.sprintf "Request timed out after %g seconds" timeoutSeconds)
      | exceptionValue -> Lwt.fail exceptionValue)

let prettyPrintedJsonOrOriginal responseBody =
  try responseBody |> Yojson.Safe.from_string |> Yojson.Safe.pretty_to_string
  with Yojson.Json_error _ -> responseBody
