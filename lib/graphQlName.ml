let wordsOfCliToken token =
  token |> String.split_on_char '-' |> List.filter (fun word -> word <> "")

let upperCamelCaseOfCliToken token =
  token |> wordsOfCliToken
  |> List.map String.capitalize_ascii
  |> String.concat ""

let lowerCamelCaseOfCliToken token =
  match wordsOfCliToken token with
  | [] -> ""
  | firstWord :: remainingWords ->
      String.uncapitalize_ascii firstWord
      ^ String.concat "" (List.map String.capitalize_ascii remainingWords)
