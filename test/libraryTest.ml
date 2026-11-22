open Anilist

type expectation = Query of string | Variables of string | Failure of string

let argumentsOfCommandLine commandLine =
  String.split_on_char '\'' commandLine
  |> List.mapi (fun index chunk ->
      let isInsideSingleQuotes = index mod 2 = 1 in

      if isInsideSingleQuotes then [ chunk ]
      else
        String.split_on_char ' ' chunk |> List.filter (fun word -> word <> ""))
  |> List.concat

let assertEqual label expected actual =
  if actual <> expected then
    failwith
      (Printf.sprintf "%s\n\nExpected:\n%s\n\nActual:\n%s" label expected actual)

let assertContainsSubstring label substring text =
  let rec containsFrom startIndex =
    startIndex + String.length substring <= String.length text
    && (String.sub text startIndex (String.length substring) = substring
       || containsFrom (startIndex + 1))
  in

  if not (containsFrom 0) then
    failwith
      (Printf.sprintf "%s\n\nExpected substring %S in:\n%s" label substring text)

let invocationCases =
  [
    ( "user --name fuwn --selection-set id,name",
      Query "query {\n  User(name: \"fuwn\") {\n    id\n    name\n  }\n}" );
    ( "user --name fuwn --fields id",
      Query "query {\n  User(name: \"fuwn\") {\n    id\n  }\n}" );
    ( "--operation query:Viewer --variable-definition '$userName: String!' \
       --variable userName=string:fuwn --directive 'cacheControl(maxAge: 60)' \
       --selection-set viewer.id --field user --name var:userName \
       --selection-set id,name --field media --id 42 --selection-set \
       title.romaji",
      Query
        "query Viewer($userName: String!) @cacheControl(maxAge: 60) {\n\
        \  Viewer {\n\
        \    id\n\
        \  }\n\
        \  User(name: $userName) {\n\
        \    id\n\
        \    name\n\
        \  }\n\
        \  Media(id: 42) {\n\
        \    title {\n\
        \      romaji\n\
        \    }\n\
        \  }\n\
         }" );
    ( "user --alias account --directive 'include(if: $withUser)' --name fuwn \
       --selection-set id,name",
      Query
        "query {\n\
        \  account: User(name: \"fuwn\") @include(if: $withUser) {\n\
        \    id\n\
        \    name\n\
        \  }\n\
         }" );
    ( "query --operation-name Viewer --variable-definition '$userName: \
       String!' --variable userName=string:fuwn --field account:user \
       --directive 'include(if: $withUser)' --name var:userName \
       --selection-set id,name --fragment-definition 'fragment userCore on \
       User { id }'",
      Query
        "query Viewer($userName: String!) {\n\
        \  account: User(name: $userName) @include(if: $withUser) {\n\
        \    id\n\
        \    name\n\
        \  }\n\
         }\n\n\
         fragment userCore on User { id }" );
    ( "query --field media --type enum:ANIME --id 42 --selection-set id \
       --selection-set title.romaji --field ./cover-image --selection-set \
       extra-large",
      Query
        "query {\n\
        \  Media(type: ANIME, id: 42) {\n\
        \    id\n\
        \    title {\n\
        \      romaji\n\
        \    }\n\
        \  }\n\
        \  Media(type: ANIME, id: 42) {\n\
        \    coverImage {\n\
        \      extraLarge\n\
        \    }\n\
        \  }\n\
         }" );
    ( "query --field media --id 1 --field ./title --selection-set romaji",
      Query
        "query {\n  Media(id: 1) {\n    title {\n      romaji\n    }\n  }\n}" );
    ( "query --field __typename --field media --selection-set id --field \
       ./title --selection-set romaji",
      Query
        "query {\n\
        \  __typename\n\
        \  Media {\n\
        \    id\n\
        \  }\n\
        \  Media {\n\
        \    title {\n\
        \      romaji\n\
        \    }\n\
        \  }\n\
         }" );
    ( "query --field media --id 1 --selection-set id --inline-fragment ./Anime \
       --directive 'skip(if: $skipAnime)' --field ./title --selection-set \
       romaji --field /media --id 1 --inline-fragment ./Anime --field \
       ./relations --type enum:SEQUEL --selection-set edges.node.id",
      Query
        "query {\n\
        \  Media(id: 1) {\n\
        \    id\n\
        \  }\n\
        \  Media(id: 1) {\n\
        \    ... on Anime @skip(if: $skipAnime) {\n\
        \      title {\n\
        \        romaji\n\
        \      }\n\
        \    }\n\
        \  }\n\
        \  Media(id: 1) {\n\
        \    ... on Anime {\n\
        \      relations(type: SEQUEL) {\n\
        \        edges {\n\
        \          node {\n\
        \            id\n\
        \          }\n\
        \        }\n\
        \      }\n\
        \    }\n\
        \  }\n\
         }" );
    ( "query --field media --id 1 --inline-fragment Anime --directive \
       'skip(if: $skipAnime)' --selection-set episodes",
      Query
        "query {\n\
        \  Media(id: 1) {\n\
        \    ... on Anime @skip(if: $skipAnime) {\n\
        \      episodes\n\
        \    }\n\
        \  }\n\
         }" );
    ( "query --inline-fragment Query --selection-set viewer.id",
      Query "query {\n  ... on Query {\n    Viewer {\n      id\n    }\n  }\n}"
    );
    ( "query --selection-set ...on:Query.viewer.id",
      Query "query {\n  ... on Query {\n    Viewer {\n      id\n    }\n  }\n}"
    );
    ( "query --field media --id 1 --fragment-spread ./mediaCore --directive \
       'include(if: $withCore)' --fragment-definition 'fragment mediaCore on \
       Media { popularity }'",
      Query
        "query {\n\
        \  Media(id: 1) {\n\
        \    ...mediaCore @include(if: $withCore)\n\
        \  }\n\
         }\n\n\
         fragment mediaCore on Media { popularity }" );
    ( "query --field media --id 1 --selection-set \
       ...mediaCore,...legacyFragment --fragment-definition 'fragment \
       mediaCore on Media { popularity }' --fragment-definition 'fragment \
       legacyFragment on Media { favourites }'",
      Query
        "query {\n\
        \  Media(id: 1) {\n\
        \    ...mediaCore\n\
        \    ...legacyFragment\n\
        \  }\n\
         }\n\n\
         fragment mediaCore on Media { popularity }\n\n\
         fragment legacyFragment on Media { favourites }" );
    ( "--operation query:Viewer --directive 'cacheControl(maxAge: 60)' \
       --selection-set viewer.id --operation query:UserLookup user --name fuwn \
       --selection-set id,name --execute-operation-name UserLookup",
      Query
        "query Viewer @cacheControl(maxAge: 60) {\n\
        \  Viewer {\n\
        \    id\n\
        \  }\n\
         }\n\n\
         query UserLookup {\n\
        \  User(name: \"fuwn\") {\n\
        \    id\n\
        \    name\n\
        \  }\n\
         }" );
    ( "user --name fuwn --selection-set id --field ./statistics \
       --selection-set anime.count --field /media --id 42 --selection-set id",
      Query
        "query {\n\
        \  User(name: \"fuwn\") {\n\
        \    id\n\
        \  }\n\
        \  User(name: \"fuwn\") {\n\
        \    statistics {\n\
        \      anime {\n\
        \        count\n\
        \      }\n\
        \    }\n\
        \  }\n\
        \  Media(id: 42) {\n\
        \    id\n\
        \  }\n\
         }" );
    ( "query user --selection-set id media --selection-set id",
      Query "query {\n  User {\n    id\n  }\n  Media {\n    id\n  }\n}" );
    ( "media --fragment core:Media --selection-set id",
      Query "query {\n  Media(fragment: \"core:Media\") {\n    id\n  }\n}" );
    ( "media --selection-set root:id",
      Query "query {\n  Media {\n    root: id\n  }\n}" );
    ( "query --variable-definition '$user_name: String!' --variable \
       user_name=string:fuwn --field user --name var:user_name --selection-set \
       id",
      Variables {|{"user_name":"fuwn"}|} );
    ( "query --variable userName=var:otherUser --field user --selection-set id",
      Failure "Variable assignments cannot contain variable references." );
    ( "--operation query:Viewer viewer --selection-set id --operation \
       query:UserLookup user --name fuwn --selection-set id",
      Failure "Multiple-operation documents require --execute-operation-name."
    );
    ( "query --selection-set viewer.id --fragment-definition 'fragment core on \
       Media { id }' --variable id=1",
      Failure "--variable requires a current operation." );
    ( "--fragment-definition 'fragment core on Media { id }' --selection-set id",
      Failure "Selections require a current operation." );
    ("media --id --selection-set id", Failure "Missing value for option --id");
    ("query --selection-set ''", Failure "Selection set cannot be empty.");
    ("query --selection-set ', ,'", Failure "Selection set cannot be empty.");
    ("query --selection-set ...", Failure "Fragment spreads require a name.");
    ( "query --selection-set ...on:Media.",
      Failure "Inline fragments require a selection set." );
    ( "media --id 1 --selection-set ...on:Media",
      Failure "Inline fragments require a type condition and selection set" );
    ( "query --inline-fragment Query",
      Failure "Inline fragments require a selection set." );
    ( "media --selection-set /id",
      Failure "Selection paths cannot contain /; scope a path with --field" );
  ]

let testInvocations () =
  invocationCases
  |> List.iter (fun (commandLine, expectation) ->
      let outcome =
        match
          CommandLineInvocation.ofArguments (argumentsOfCommandLine commandLine)
        with
        | Ok (`Invocation invocation) -> Ok invocation
        | Ok `Help -> Error "Help was requested."
        | Error message -> Error message
      in

      match (expectation, outcome) with
      | Query expectedQuery, Ok invocation ->
          assertEqual commandLine expectedQuery
            (GraphQlDocument.render
               (LoweringEngine.lower invocation).LoweringEngine.document)
      | Variables expectedVariablesJson, Ok invocation ->
          let variablesJson =
            `Assoc
              ((LoweringEngine.lower invocation)
                 .LoweringEngine.selectedVariableAssignments
              |> List.map (fun (name, value) ->
                  (name, CliArgument.jsonLiteralOfValue value)))
            |> Yojson.Safe.to_string
          in

          assertEqual commandLine expectedVariablesJson variablesJson
      | Failure expectedMessage, Error message ->
          assertContainsSubstring commandLine expectedMessage message
      | Failure _, Ok _ -> failwith (commandLine ^ "\n\nExpected an error.")
      | (Query _ | Variables _), Error message ->
          failwith (commandLine ^ "\n\n" ^ message))

let testSchemaCommands () =
  [
    ("schema", Ok (`Command SchemaCommand.fullSchemaCommand));
    ("schema --type Media", Ok (`Command (SchemaCommand.typeCommand "Media")));
    ("schema --type=Media", Ok (`Command (SchemaCommand.typeCommand "Media")));
    ( "schema --directive include",
      Ok (`Command (SchemaCommand.directiveCommand "include")) );
    ( "schema --directive=include",
      Ok (`Command (SchemaCommand.directiveCommand "include")) );
    ("schema --help", Ok `Help);
    ("schema help", Ok `Help);
    ("media --selection-set id", Ok `NotSchemaCommand);
  ]
  |> List.iter (fun (commandLine, expectedResult) ->
      if
        SchemaCommand.ofArguments (argumentsOfCommandLine commandLine)
        <> expectedResult
      then failwith (commandLine ^ "\n\nUnexpected schema parse result."));
  [
    ("schema nope help", "Invalid schema argument: nope");
    ( "schema --type Media --directive include",
      "only one of --type or --directive" );
    ("schema --type --directive include", "Schema --type requires a value.");
  ]
  |> List.iter (fun (commandLine, expectedMessage) ->
      match SchemaCommand.ofArguments (argumentsOfCommandLine commandLine) with
      | Ok _ -> failwith (commandLine ^ "\n\nExpected an error.")
      | Error message ->
          assertContainsSubstring commandLine expectedMessage message)

let testRequestHeaders () =
  (match
     RequestOptions.extractionOfArguments
       (argumentsOfCommandLine
          "--header 'Authorization: Bearer token' user --name fuwn \
           '--header=X-Test: 1'")
   with
  | Ok { RequestOptions.headerPairs; remainingArguments } ->
      if
        headerPairs <> [ ("Authorization", "Bearer token"); ("X-Test", "1") ]
        || remainingArguments <> [ "user"; "--name"; "fuwn" ]
      then failwith "Unexpected header extraction."
  | Error message -> failwith message);

  [
    ( "--header 'Authorization Bearer token'",
      "Header value must use 'Name: Value' syntax." );
    ("--header", "Missing value for option --header.");
  ]
  |> List.iter (fun (commandLine, expectedMessage) ->
      match
        RequestOptions.extractionOfArguments
          (argumentsOfCommandLine commandLine)
      with
      | Ok _ -> failwith (commandLine ^ "\n\nExpected an error.")
      | Error message ->
          assertContainsSubstring commandLine expectedMessage message)

let () =
  testInvocations ();
  testSchemaCommands ();
  testRequestHeaders ()
