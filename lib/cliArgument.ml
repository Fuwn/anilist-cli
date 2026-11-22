type value =
  | Null
  | Boolean of bool
  | Integer of int
  | Float of float
  | Enum of string
  | String of string
  | Variable of string
  | Json of Yojson.Safe.t

let graphQlStringLiteral value = Yojson.Safe.to_string (`String value)

let graphQlFloatLiteral value =
  let literal = Yojson.Safe.to_string (`Float value) in

  if
    String.contains literal '.'
    || String.contains literal 'e'
    || String.contains literal 'E'
  then literal
  else literal ^ ".0"

let rec graphQlLiteralOfJson = function
  | `Null -> "null"
  | `Bool value -> if value then "true" else "false"
  | `Int value -> string_of_int value
  | `Intlit value -> value
  | `Float value -> graphQlFloatLiteral value
  | `String value -> graphQlStringLiteral value
  | `Assoc pairs ->
      pairs
      |> List.map (fun (name, value) ->
          Printf.sprintf "%s: %s" name (graphQlLiteralOfJson value))
      |> String.concat ", " |> Printf.sprintf "{ %s }"
  | `List values | `Tuple values ->
      values
      |> List.map graphQlLiteralOfJson
      |> String.concat ", " |> Printf.sprintf "[%s]"
  | `Variant (name, None) -> name
  | `Variant (name, Some value) ->
      Printf.sprintf "{ %s: %s }" name (graphQlLiteralOfJson value)

let graphQlLiteralOfValue = function
  | Null -> "null"
  | Boolean value -> if value then "true" else "false"
  | Integer value -> string_of_int value
  | Float value -> graphQlFloatLiteral value
  | Enum value -> value
  | String value -> graphQlStringLiteral value
  | Variable value ->
      if String.length value > 0 && value.[0] = '$' then value else "$" ^ value
  | Json value -> graphQlLiteralOfJson value

let jsonLiteralOfValue = function
  | Null -> `Null
  | Boolean value -> `Bool value
  | Integer value -> `Int value
  | Float value -> `Float value
  | Enum value -> `String value
  | String value -> `String value
  | Variable _ ->
      invalid_arg "GraphQL variables cannot contain variable references"
  | Json value -> value

let rawValueLooksLikeFloat rawValue =
  String.contains rawValue '.'
  || String.contains rawValue 'e'
  || String.contains rawValue 'E'

let forcedValueOfRawValue rawValue =
  [
    ("string:", fun text -> String text);
    ("enum:", fun text -> Enum text);
    ("var:", fun text -> Variable text);
    ("json:", fun text -> Json (Yojson.Safe.from_string text));
  ]
  |> List.find_map (fun (prefix, valueOfText) ->
      StringPrefix.valueWithoutPrefix ~prefix rawValue |> Option.map valueOfText)

let valueOfRawValue rawValue =
  match forcedValueOfRawValue rawValue with
  | Some value -> value
  | None -> (
      match String.lowercase_ascii rawValue with
      | "null" -> Null
      | "true" -> Boolean true
      | "false" -> Boolean false
      | _ -> (
          match int_of_string_opt rawValue with
          | Some value when not (rawValueLooksLikeFloat rawValue) ->
              Integer value
          | _ -> (
              match float_of_string_opt rawValue with
              | Some value when rawValueLooksLikeFloat rawValue -> Float value
              | _ ->
                  let seemsJson =
                    String.length rawValue > 1
                    && match rawValue.[0] with '{' | '[' -> true | _ -> false
                  in

                  if seemsJson then
                    try Json (Yojson.Safe.from_string rawValue)
                    with Yojson.Json_error _ -> String rawValue
                  else String rawValue)))
