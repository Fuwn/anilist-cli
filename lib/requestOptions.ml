type t = {
  headerPairs : (string * string) list;
  remainingArguments : string list;
}

let headerPairOfText headerText =
  match String.index_opt headerText ':' with
  | Some separatorIndex ->
      let headerName = String.sub headerText 0 separatorIndex |> String.trim in
      let headerValue =
        String.sub headerText (separatorIndex + 1)
          (String.length headerText - separatorIndex - 1)
        |> String.trim
      in
      if headerName = "" then Error "Header name cannot be empty."
      else Ok (headerName, headerValue)
  | None -> Error "Header value must use 'Name: Value' syntax."

let extractionOfArguments arguments =
  let rec extract accumulatedHeaderPairs accumulatedArguments = function
    | [] ->
        Ok
          {
            headerPairs = List.rev accumulatedHeaderPairs;
            remainingArguments = List.rev accumulatedArguments;
          }
    | "--header" :: [] -> Error "Missing value for option --header."
    | "--header" :: headerText :: remainingArguments -> (
        match headerPairOfText headerText with
        | Ok headerPair ->
            extract
              (headerPair :: accumulatedHeaderPairs)
              accumulatedArguments remainingArguments
        | Error message ->
            Error (Printf.sprintf "%s\n\nUse --header 'Name: Value'." message))
    | argument :: remainingArguments -> (
        match StringPrefix.valueWithoutPrefix ~prefix:"--header=" argument with
        | Some headerText ->
            extract accumulatedHeaderPairs accumulatedArguments
              ("--header" :: headerText :: remainingArguments)
        | None ->
            extract accumulatedHeaderPairs
              (argument :: accumulatedArguments)
              remainingArguments)
  in
  extract [] [] arguments
