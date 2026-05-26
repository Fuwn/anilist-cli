open Anilist

let assertEqual expected actual =
  if actual <> expected then
    failwith (Printf.sprintf "Expected:\n%s\n\nActual:\n%s" expected actual)

let assertContainsSubstring substring text =
  let rec loop startIndex =
    if startIndex + String.length substring > String.length text then
      failwith (Printf.sprintf "Expected substring %S in:\n%s" substring text)
    else if String.sub text startIndex (String.length substring) = substring
    then ()
    else loop (startIndex + 1)
  in
  loop 0

let fieldSegment ?alias ?(directives = []) fieldName optionPairs =
  let loweredFieldSegment : CommandLineInvocationTypes.fieldSegment =
    {
      CommandLineInvocationTypes.fieldName;
      CommandLineInvocationTypes.fieldAlias = alias;
      CommandLineInvocationTypes.fieldArgumentPairs = optionPairs;
      CommandLineInvocationTypes.fieldDirectiveTexts = directives;
    }
  in
  CommandLineInvocationTypes.FieldSegment loweredFieldSegment

let inlineFragmentSegment ?(directives = []) typeCondition =
  CommandLineInvocationTypes.InlineFragmentSegment
    ({
       CommandLineInvocationTypes.inlineFragmentTypeCondition = typeCondition;
       inlineFragmentDirectiveTexts = directives;
     }
      : CommandLineInvocationTypes.inlineFragmentSegment)

let fragmentSpreadSegment ?(directives = []) name =
  CommandLineInvocationTypes.FragmentSpreadSegment
    ({
       CommandLineInvocationTypes.fragmentSpreadName = name;
       fragmentSpreadDirectiveTexts = directives;
     }
      : CommandLineInvocationTypes.fragmentSpreadSegment)

let selectionBranch selectionPathSegments selectionExpressions =
  { CommandLineInvocationTypes.selectionPathSegments; selectionExpressions }

let operationDefinition ?operationName ?(variableDefinitions = [])
    ?(variableAssignments = []) ?(directives = []) rootSelectionExpressions
    selectionBranches =
  ({
     CommandLineInvocationTypes.operationType = CommandLineInvocationTypes.Query;
     CommandLineInvocationTypes.operationName;
     CommandLineInvocationTypes.variableDefinitions;
     CommandLineInvocationTypes.variableAssignments;
     CommandLineInvocationTypes.operationDirectiveTexts = directives;
     CommandLineInvocationTypes.rootSelectionExpressions;
     CommandLineInvocationTypes.selectionBranches;
   }
    : CommandLineInvocationTypes.operationDefinition)

let structuredFragmentDefinition ?(directives = []) fragmentName
    fragmentTypeCondition fragmentRootSelectionExpressions
    fragmentSelectionBranches =
  {
    CommandLineInvocationTypes.fragmentName;
    fragmentTypeCondition;
    fragmentDirectiveTexts = directives;
    fragmentRootSelectionExpressions;
    fragmentSelectionBranches;
  }

let queryOfLoweredRequest loweredRequest =
  loweredRequest |> LoweringEngine.graphQlQueryOfRequest |> GraphQlQuery.render

let testSingleFieldSelectionShorthand () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [ fieldSegment "user" [ ("name", "fuwn") ] ]
                [ "id,name" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual "query {\n  User(name: \"fuwn\") {\n    id\n    name\n  }\n}"
    (queryOfLoweredRequest request)

let testExplicitOperationWithMultipleRootFields () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          ({
             CommandLineInvocationTypes.operationType =
               CommandLineInvocationTypes.Query;
             CommandLineInvocationTypes.operationName = Some "Viewer";
             CommandLineInvocationTypes.variableDefinitions =
               [ "$userName: String!" ];
             CommandLineInvocationTypes.variableAssignments =
               [ ("userName", "string:fuwn") ];
             CommandLineInvocationTypes.operationDirectiveTexts =
               [ "cacheControl(maxAge: 60)" ];
             CommandLineInvocationTypes.rootSelectionExpressions =
               [ "viewer.id" ];
             CommandLineInvocationTypes.selectionBranches =
               [
                 selectionBranch
                   [ fieldSegment "user" [ ("name", "var:userName") ] ]
                   [ "id,name" ];
                 selectionBranch
                   [ fieldSegment "media" [ ("id", "42") ] ]
                   [ "title.romaji" ];
               ];
           }
            : CommandLineInvocationTypes.operationDefinition);
        ]
      ~selectedOperationName:(Some "Viewer") ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
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
     }"
    (queryOfLoweredRequest request)

let testFieldAliasAndDirectiveLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [
                  fieldSegment ~alias:"account"
                    ~directives:[ "include(if: $withUser)" ]
                    "user"
                    [ ("name", "fuwn") ];
                ]
                [ "id,name" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  account: User(name: \"fuwn\") @include(if: $withUser) {\n\
    \    id\n\
    \    name\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testNestedFieldBranchLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [
                  fieldSegment "media" [ ("type", "enum:ANIME"); ("id", "42") ];
                ]
                [ "id"; "title.romaji" ];
              selectionBranch
                [
                  fieldSegment "media" [ ("type", "enum:ANIME"); ("id", "42") ];
                  fieldSegment "cover-image" [];
                ]
                [ "extra-large" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(type: ANIME, id: 42) {\n\
    \    id\n\
    \    title {\n\
    \      romaji\n\
    \    }\n\
    \    coverImage {\n\
    \      extraLarge\n\
    \    }\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testStructuredFragmentDefinitionLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [ fieldSegment "media" [ ("id", "1") ] ]
                [ "id"; "...mediaCore" ];
            ];
        ]
      ~selectedOperationName:None
      ~structuredFragmentDefinitions:
        [
          structuredFragmentDefinition "mediaCore" "Media" [ "popularity" ]
            [ selectionBranch [ fieldSegment "title" [] ] [ "romaji" ] ];
        ]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    id\n\
    \    ...mediaCore\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media {\n\
    \  popularity\n\
    \  title {\n\
    \    romaji\n\
    \  }\n\
     }"
    (queryOfLoweredRequest request)

let testInlineFragmentBranchLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch [ fieldSegment "media" [ ("id", "1") ] ] [ "id" ];
              selectionBranch
                [
                  fieldSegment "media" [ ("id", "1") ];
                  inlineFragmentSegment ~directives:[ "skip(if: $skipAnime)" ]
                    "Anime";
                  fieldSegment "title" [];
                ]
                [ "romaji" ];
              selectionBranch
                [
                  fieldSegment "media" [ ("id", "1") ];
                  inlineFragmentSegment "Anime";
                  fieldSegment "relations" [ ("type", "enum:SEQUEL") ];
                ]
                [ "edges.node.id" ];
            ];
        ]
      ~selectedOperationName:None ~structuredFragmentDefinitions:[]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    id\n\
    \    ... on Anime @skip(if: $skipAnime) {\n\
    \      title {\n\
    \        romaji\n\
    \      }\n\
    \    }\n\
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
     }"
    (queryOfLoweredRequest request)

let testFragmentSpreadDirectiveLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [
                  fieldSegment "media" [ ("id", "1") ];
                  fragmentSpreadSegment
                    ~directives:[ "include(if: $withCore)" ]
                    "mediaCore";
                ]
                [];
            ];
        ]
      ~selectedOperationName:None
      ~structuredFragmentDefinitions:
        [ structuredFragmentDefinition "mediaCore" "Media" [ "popularity" ] [] ]
      ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    ...mediaCore @include(if: $withCore)\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media {\n\
    \  popularity\n\
     }"
    (queryOfLoweredRequest request)

let testRawAndStructuredFragmentsCoexist () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          operationDefinition []
            [
              selectionBranch
                [ fieldSegment "media" [ ("id", "1") ] ]
                [ "...mediaCore"; "...legacyFragment" ];
            ];
        ]
      ~selectedOperationName:None
      ~structuredFragmentDefinitions:
        [ structuredFragmentDefinition "mediaCore" "Media" [ "popularity" ] [] ]
      ~rawFragmentDefinitionTexts:
        [ "fragment legacyFragment on Media { favourites }" ]
  in
  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    ...mediaCore\n\
    \    ...legacyFragment\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media {\n\
    \  popularity\n\
     }\n\n\
     fragment legacyFragment on Media { favourites }"
    (queryOfLoweredRequest request)

let testMultipleOperationDocumentLowering () =
  let request =
    LoweringEngine.lower
      ~operationDefinitions:
        [
          ({
             CommandLineInvocationTypes.operationType =
               CommandLineInvocationTypes.Query;
             CommandLineInvocationTypes.operationName = Some "Viewer";
             CommandLineInvocationTypes.variableDefinitions = [];
             CommandLineInvocationTypes.variableAssignments = [];
             CommandLineInvocationTypes.operationDirectiveTexts =
               [ "cacheControl(maxAge: 60)" ];
             CommandLineInvocationTypes.rootSelectionExpressions =
               [ "viewer.id" ];
             CommandLineInvocationTypes.selectionBranches = [];
           }
            : CommandLineInvocationTypes.operationDefinition);
          operationDefinition ~operationName:"UserLookup" []
            [
              selectionBranch
                [ fieldSegment "user" [ ("name", "fuwn") ] ]
                [ "id,name" ];
            ];
        ]
      ~selectedOperationName:(Some "UserLookup")
      ~structuredFragmentDefinitions:[] ~rawFragmentDefinitionTexts:[]
  in
  assertEqual
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
     }"
    (queryOfLoweredRequest request)

let testSelectionSetCompatibilityAliasParsing () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [ "user"; "--name"; "fuwn"; "--fields"; "id" ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocationTypes.operationDefinitions
      in
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocationTypes.selectionBranches))
  | Error message -> failwith message

let testOperationParsing () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "query";
        "--operation-name";
        "Viewer";
        "--variable-definition";
        "$userName: String!";
        "--variable";
        "userName=string:fuwn";
        "--field";
        "user";
        "--alias";
        "account";
        "--directive";
        "include(if: $withUser)";
        "--name";
        "var:userName";
        "--selection-set";
        "id,name";
        "--fragment";
        "userCore:User";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocationTypes.operationDefinitions
      in
      assertEqual "Viewer"
        (Option.get operationDefinition.CommandLineInvocationTypes.operationName);
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocationTypes.variableDefinitions));
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocationTypes.variableAssignments));
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocationTypes.selectionBranches));
      assertEqual "1"
        (string_of_int
           (List.length
              invocation
                .CommandLineInvocationTypes.structuredFragmentDefinitions))
  | Error message -> failwith message

let testVariableAssignmentsPreserveGraphQlVariableNames () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "query";
        "--variable-definition";
        "$user_name: String!";
        "--variable";
        "user_name=string:fuwn";
        "--field";
        "user";
        "--name";
        "var:user_name";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocationTypes.operationDefinitions
      in
      assertEqual "user_name"
        (fst
           (List.hd
              operationDefinition.CommandLineInvocationTypes.variableAssignments))
  | Error message -> failwith message

let testVariableAssignmentsRejectVariableReferences () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "query";
        "--variable";
        "userName=var:otherUser";
        "--field";
        "user";
        "--selection-set";
        "id";
      ]
  with
  | Ok _ -> failwith "Expected variable assignment rejection"
  | Error _ -> ()

let testInlineFragmentsRequireSelections () =
  try
    ignore
      (LoweringEngine.lower
         ~operationDefinitions:
           [
             operationDefinition []
               [
                 selectionBranch
                   [ fieldSegment "media" [ ("id", "1") ] ]
                   [ "...on:Media" ];
               ];
           ]
         ~selectedOperationName:None ~structuredFragmentDefinitions:[]
         ~rawFragmentDefinitionTexts:[]);
    failwith "Expected inline fragment validation failure"
  with Invalid_argument _ -> ()

let testRelativeAndAbsoluteFieldPathParsing () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "user";
        "--name";
        "fuwn";
        "--selection-set";
        "id";
        "--field";
        "./statistics";
        "--selection-set";
        "anime.count";
        "--field";
        "/media";
        "--id";
        "42";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation -> (
      let operationDefinition =
        List.hd invocation.CommandLineInvocationTypes.operationDefinitions
      in
      let secondSelectionBranch =
        operationDefinition.CommandLineInvocationTypes.selectionBranches
        |> List.tl |> List.hd
      in
      let thirdSelectionBranch =
        operationDefinition.CommandLineInvocationTypes.selectionBranches
        |> List.rev |> List.hd
      in
      assertEqual "2"
        (string_of_int
           (List.length
              secondSelectionBranch
                .CommandLineInvocationTypes.selectionPathSegments));
      match
        List.hd
          thirdSelectionBranch.CommandLineInvocationTypes.selectionPathSegments
      with
      | CommandLineInvocationTypes.FieldSegment fieldSegment ->
          assertEqual "media" fieldSegment.CommandLineInvocationTypes.fieldName
      | CommandLineInvocationTypes.InlineFragmentSegment _
      | CommandLineInvocationTypes.FragmentSpreadSegment _ ->
          failwith "Expected field segment")
  | Error message -> failwith message

let testExplicitOperationBarePositionalSiblingFields () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "query";
        "user";
        "--selection-set";
        "id";
        "media";
        "--selection-set";
        "id";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocationTypes.operationDefinitions
      in
      assertEqual "2"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocationTypes.selectionBranches))
  | Error message -> failwith message

let testInlineFragmentParsing () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "query";
        "--field";
        "media";
        "--id";
        "1";
        "--inline-fragment";
        "Anime";
        "--directive";
        "skip(if: $skipAnime)";
        "--selection-set";
        "episodes";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocationTypes.operationDefinitions
      in
      let inlineFragmentBranch =
        operationDefinition.CommandLineInvocationTypes.selectionBranches
        |> List.rev |> List.hd
      in
      assertEqual "2"
        (string_of_int
           (List.length
              inlineFragmentBranch
                .CommandLineInvocationTypes.selectionPathSegments))
  | Error message -> failwith message

let testFragmentSpreadParsing () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "query";
        "--field";
        "media";
        "--id";
        "1";
        "--fragment-spread";
        "./mediaCore";
        "--directive";
        "include(if: $withCore)";
      ]
  with
  | Ok invocation -> (
      let operationDefinition =
        List.hd invocation.CommandLineInvocationTypes.operationDefinitions
      in
      let fragmentSpreadBranch =
        operationDefinition.CommandLineInvocationTypes.selectionBranches
        |> List.rev |> List.hd
      in
      match
        List.rev
          fragmentSpreadBranch.CommandLineInvocationTypes.selectionPathSegments
        |> List.hd
      with
      | CommandLineInvocationTypes.FragmentSpreadSegment fragmentSpreadSegment
        ->
          assertEqual "mediaCore"
            fragmentSpreadSegment.CommandLineInvocationTypes.fragmentSpreadName;
          assertEqual "1"
            (string_of_int
               (List.length
                  fragmentSpreadSegment
                    .CommandLineInvocationTypes.fragmentSpreadDirectiveTexts))
      | CommandLineInvocationTypes.FieldSegment _
      | CommandLineInvocationTypes.InlineFragmentSegment _ ->
          failwith "Expected fragment spread segment")
  | Error message -> failwith message

let testMultipleOperationsRequireSelectedOperationName () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "--operation";
        "query:Viewer";
        "viewer";
        "--selection-set";
        "id";
        "--operation";
        "query:UserLookup";
        "user";
        "--name";
        "fuwn";
        "--selection-set";
        "id";
      ]
  with
  | Ok _ -> failwith "Expected selected operation validation failure"
  | Error _ -> ()

let testMultipleOperationParsing () =
  match
    CommandLineInvocationParser.invocationOfArguments
      [
        "--operation";
        "query:Viewer";
        "viewer";
        "--selection-set";
        "id";
        "--operation";
        "query:UserLookup";
        "user";
        "--name";
        "fuwn";
        "--selection-set";
        "id";
        "--execute-operation-name";
        "UserLookup";
      ]
  with
  | Ok invocation ->
      assertEqual "2"
        (string_of_int
           (List.length
              invocation.CommandLineInvocationTypes.operationDefinitions));
      assertEqual "UserLookup"
        (Option.get invocation.CommandLineInvocationTypes.selectedOperationName)
  | Error message -> failwith message

let testSchemaCommandParsing () =
  match SchemaArgumentParser.invocationOfArguments [ "schema" ] with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionQuery"
        (Option.get schemaCommand.SchemaCommandTypes.operationName);
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommandTypes.variables with
           | Some _ -> 1
           | None -> 0));
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommandTypes.requestedDirectiveName with
           | Some _ -> 1
           | None -> 0));
      assertContainsSubstring "__schema"
        schemaCommand.SchemaCommandTypes.queryText;
      assertContainsSubstring "fragment FullType"
        schemaCommand.SchemaCommandTypes.queryText
  | Ok None -> failwith "Expected schema command"
  | Error message -> failwith message

let testSchemaTypeCommandParsing () =
  match
    SchemaArgumentParser.invocationOfArguments [ "schema"; "--type"; "Media" ]
  with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionTypeQuery"
        (Option.get schemaCommand.SchemaCommandTypes.operationName);
      assertContainsSubstring "__type(name: $name)"
        schemaCommand.SchemaCommandTypes.queryText;
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommandTypes.requestedDirectiveName with
           | Some _ -> 1
           | None -> 0));
      assertEqual "{\"name\":\"Media\"}"
        (schemaCommand.SchemaCommandTypes.variables |> Option.get
       |> Yojson.Safe.to_string)
  | Ok None -> failwith "Expected schema type command"
  | Error message -> failwith message

let testSchemaDirectiveCommandParsing () =
  match
    SchemaArgumentParser.invocationOfArguments
      [ "schema"; "--directive"; "include" ]
  with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionDirectiveQuery"
        (Option.get schemaCommand.SchemaCommandTypes.operationName);
      assertContainsSubstring "directives"
        schemaCommand.SchemaCommandTypes.queryText;
      assertEqual "include"
        (Option.get schemaCommand.SchemaCommandTypes.requestedDirectiveName);
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommandTypes.variables with
           | Some _ -> 1
           | None -> 0))
  | Ok None -> failwith "Expected schema directive command"
  | Error message -> failwith message

let testSchemaEqualsOptionParsing () =
  (match
     SchemaArgumentParser.invocationOfArguments [ "schema"; "--type=Media" ]
   with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionTypeQuery"
        (Option.get schemaCommand.SchemaCommandTypes.operationName)
  | Ok None -> failwith "Expected schema type command"
  | Error message -> failwith message);
  match
    SchemaArgumentParser.invocationOfArguments
      [ "schema"; "--directive=include" ]
  with
  | Ok (Some schemaCommand) ->
      assertEqual "IntrospectionDirectiveQuery"
        (Option.get schemaCommand.SchemaCommandTypes.operationName)
  | Ok None -> failwith "Expected schema directive command"
  | Error message -> failwith message

let testSchemaCommandRejectsConflictingOptions () =
  match
    SchemaArgumentParser.invocationOfArguments
      [ "schema"; "--type"; "Media"; "--directive"; "include" ]
  with
  | Ok _ -> failwith "Expected schema command rejection"
  | Error _ -> ()

let testSchemaCommandRejectsMissingOptionValues () =
  match
    SchemaArgumentParser.invocationOfArguments
      [ "schema"; "--type"; "--directive"; "include" ]
  with
  | Ok _ -> failwith "Expected missing type value rejection"
  | Error message ->
      assertContainsSubstring "Schema --type requires a value." message

let testRequestHeaderExtraction () =
  match
    RequestOptions.extractionOfArguments
      [
        "--header";
        "Authorization: Bearer token";
        "user";
        "--name";
        "fuwn";
        "--header=X-Test: 1";
      ]
  with
  | Ok requestOptions ->
      assertEqual "2"
        (string_of_int (List.length requestOptions.RequestOptions.headerPairs));
      assertEqual "3"
        (string_of_int
           (List.length requestOptions.RequestOptions.remainingArguments));
      assertEqual "Authorization"
        (requestOptions.RequestOptions.headerPairs |> List.hd |> fst)
  | Error message -> failwith message

let testRequestHeaderExtractionRejectsMalformedHeader () =
  match
    RequestOptions.extractionOfArguments
      [ "--header"; "Authorization Bearer token" ]
  with
  | Ok _ -> failwith "Expected malformed header rejection"
  | Error message ->
      assertContainsSubstring "Header value must use 'Name: Value' syntax."
        message

let testRequestHeaderExtractionRejectsMissingHeaderValue () =
  match RequestOptions.extractionOfArguments [ "--header" ] with
  | Ok _ -> failwith "Expected missing header rejection"
  | Error message ->
      assertContainsSubstring "Missing value for option --header." message

let testCommandLineHelpDetection () =
  if not (CommandLineInvocationShared.helpRequestedOfArguments [ "--help" ])
  then failwith "Expected top-level help detection";
  if
    not
      (CommandLineInvocationShared.helpRequestedOfArguments
         [ "query"; "--help" ])
  then failwith "Expected trailing flag help detection";
  if CommandLineInvocationShared.helpRequestedOfArguments [ "--bogus"; "help" ]
  then failwith "Unexpected bare help token detection";
  if
    CommandLineInvocationShared.helpRequestedOfArguments
      [ "query"; "--help"; "extra" ]
  then failwith "Unexpected non-trailing help detection"

let testSchemaHelpDetection () =
  if not (SchemaArgumentParser.helpRequestedOfArguments [ "schema"; "--help" ])
  then failwith "Expected schema help detection";
  if not (SchemaArgumentParser.helpRequestedOfArguments [ "schema"; "help" ])
  then failwith "Expected schema bare help detection";
  if SchemaArgumentParser.helpRequestedOfArguments [ "schema"; "nope"; "help" ]
  then failwith "Unexpected schema malformed help detection"

let () =
  testSingleFieldSelectionShorthand ();
  testExplicitOperationWithMultipleRootFields ();
  testFieldAliasAndDirectiveLowering ();
  testNestedFieldBranchLowering ();
  testStructuredFragmentDefinitionLowering ();
  testInlineFragmentBranchLowering ();
  testFragmentSpreadDirectiveLowering ();
  testRawAndStructuredFragmentsCoexist ();
  testMultipleOperationDocumentLowering ();
  testSelectionSetCompatibilityAliasParsing ();
  testOperationParsing ();
  testVariableAssignmentsPreserveGraphQlVariableNames ();
  testVariableAssignmentsRejectVariableReferences ();
  testInlineFragmentsRequireSelections ();
  testRelativeAndAbsoluteFieldPathParsing ();
  testExplicitOperationBarePositionalSiblingFields ();
  testInlineFragmentParsing ();
  testFragmentSpreadParsing ();
  testMultipleOperationsRequireSelectedOperationName ();
  testMultipleOperationParsing ();
  testSchemaCommandParsing ();
  testSchemaTypeCommandParsing ();
  testSchemaDirectiveCommandParsing ();
  testSchemaEqualsOptionParsing ();
  testSchemaCommandRejectsConflictingOptions ();
  testSchemaCommandRejectsMissingOptionValues ();
  testRequestHeaderExtraction ();
  testRequestHeaderExtractionRejectsMalformedHeader ();
  testRequestHeaderExtractionRejectsMissingHeaderValue ();
  testCommandLineHelpDetection ();
  testSchemaHelpDetection ()
