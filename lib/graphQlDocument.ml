type operationType = Query | Mutation | Subscription

type selection =
  | Field of {
      alias : string option;
      name : string;
      arguments : CliArgument.t list;
      directives : string list;
      selectionSet : selection list;
    }
  | FragmentSpread of { name : string; directives : string list }
  | InlineFragment of {
      typeCondition : string;
      directives : string list;
      selectionSet : selection list;
    }

type operation = {
  operationType : operationType;
  name : string option;
  variableDefinitions : string list;
  directives : string list;
  selectionSet : selection list;
}

type t = { operations : operation list; fragmentDefinitions : string list }

let fragmentDefinitionOfText text =
  let trimmedText = String.trim text in

  if trimmedText = "" then
    invalid_arg "Fragment definition text cannot be empty"
  else trimmedText

let renderDirectiveText text =
  let trimmed = String.trim text in

  if trimmed = "" then invalid_arg "Directive text cannot be empty"
  else if trimmed.[0] = '@' then trimmed
  else "@" ^ trimmed

let renderDirectives directives =
  directives
  |> List.map (fun directive -> " " ^ renderDirectiveText directive)
  |> String.concat ""

let renderArguments = function
  | [] -> ""
  | arguments ->
      arguments
      |> List.map (fun { CliArgument.name; value } ->
          Printf.sprintf "%s: %s" name (CliArgument.graphQlLiteralOfValue value))
      |> String.concat ", " |> Printf.sprintf "(%s)"

let rec renderSelection ~indentationLevel selection =
  let indentation = String.make (indentationLevel * 2) ' ' in
  let header, selectionSet =
    match selection with
    | Field { alias; name; arguments; directives; selectionSet } ->
        let renderedAlias =
          match alias with Some alias -> alias ^ ": " | None -> ""
        in

        ( renderedAlias ^ name ^ renderArguments arguments
          ^ renderDirectives directives,
          selectionSet )
    | FragmentSpread { name; directives } ->
        ("..." ^ name ^ renderDirectives directives, [])
    | InlineFragment { typeCondition; directives; selectionSet } ->
        ("... on " ^ typeCondition ^ renderDirectives directives, selectionSet)
  in

  match selectionSet with
  | [] -> indentation ^ header
  | _ ->
      Printf.sprintf "%s%s {\n%s\n%s}" indentation header
        (renderSelectionSet ~indentationLevel:(indentationLevel + 1)
           selectionSet)
        indentation

and renderSelectionSet ~indentationLevel selectionSet =
  selectionSet
  |> List.map (renderSelection ~indentationLevel)
  |> String.concat "\n"

let renderOperation
    { operationType; name; variableDefinitions; directives; selectionSet } =
  let keyword =
    match operationType with
    | Query -> "query"
    | Mutation -> "mutation"
    | Subscription -> "subscription"
  in
  let renderedName = match name with Some name -> " " ^ name | None -> "" in
  let renderedVariableDefinitions =
    match variableDefinitions with
    | [] -> ""
    | _ -> Printf.sprintf "(%s)" (String.concat ", " variableDefinitions)
  in

  Printf.sprintf "%s%s%s%s {\n%s\n}" keyword renderedName
    renderedVariableDefinitions
    (renderDirectives directives)
    (renderSelectionSet ~indentationLevel:1 selectionSet)

let render { operations; fragmentDefinitions } =
  List.map renderOperation operations @ fragmentDefinitions
  |> String.concat "\n\n"
