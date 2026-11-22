open Anilist

let invocationOfArguments arguments =
  match CommandLineInvocation.ofArguments arguments with
  | Ok (`Invocation invocation) -> Ok invocation
  | Ok `Help -> Error CommandLineInvocation.usageText
  | Error message -> Error message

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
  let loweredFieldSegment : CommandLineInvocation.fieldSegment =
    {
      CommandLineInvocation.fieldName;
      CommandLineInvocation.fieldAlias = alias;
      CommandLineInvocation.fieldArgumentPairs =
        List.map
          (fun (name, value) -> (name, CliArgument.valueOfRawValue value))
          optionPairs;
      CommandLineInvocation.fieldDirectiveTexts = directives;
    }
  in

  CommandLineInvocation.FieldSegment loweredFieldSegment

let inlineFragmentSegment ?(directives = []) typeCondition =
  CommandLineInvocation.InlineFragmentSegment
    ({
       CommandLineInvocation.inlineFragmentTypeCondition = typeCondition;
       inlineFragmentDirectiveTexts = directives;
     }
      : CommandLineInvocation.inlineFragmentSegment)

let fragmentSpreadSegment ?(directives = []) name =
  CommandLineInvocation.FragmentSpreadSegment
    ({
       CommandLineInvocation.fragmentSpreadName = name;
       fragmentSpreadDirectiveTexts = directives;
     }
      : CommandLineInvocation.fragmentSpreadSegment)

let selectionBranch selectionPathSegments selectionPaths =
  {
    CommandLineInvocation.selectionPathSegments;
    selectionPaths =
      CommandLineInvocation.selectionPathsOfExpressions selectionPaths;
  }

let operationDefinition ?operationName ?(variableDefinitions = [])
    ?(variableAssignments = []) ?(directives = []) rootSelectionPaths
    selectionBranches =
  {
    CommandLineInvocation.operationType = CommandLineInvocation.Query;
    operationName;
    variableDefinitions;
    variableAssignments =
      List.map
        (fun (name, value) -> (name, CliArgument.valueOfRawValue value))
        variableAssignments;
    operationDirectiveTexts = directives;
    rootSelectionPaths =
      CommandLineInvocation.selectionPathsOfExpressions rootSelectionPaths;
    selectionBranches;
  }

let selectionBranchesOfOperation operationDefinition =
  operationDefinition.CommandLineInvocation.selectionBranches

let invocationOfOperations ?selectedOperationName
    ?(fragmentDefinitionTexts = []) operationDefinitions =
  {
    CommandLineInvocation.operationDefinitions;
    selectedOperationName;
    fragmentDefinitionTexts;
  }

let queryOfLoweredRequest loweredRequest =
  GraphQlDocument.render loweredRequest.LoweringEngine.document

let testSingleFieldSelectionShorthand () =
  let request =
    LoweringEngine.lower
      (invocationOfOperations
         [
           operationDefinition []
             [
               selectionBranch
                 [ fieldSegment "user" [ ("name", "fuwn") ] ]
                 [ "id,name" ];
             ];
         ])
  in

  assertEqual "query {\n  User(name: \"fuwn\") {\n    id\n    name\n  }\n}"
    (queryOfLoweredRequest request)

let testExplicitOperationWithMultipleRootFields () =
  let request =
    LoweringEngine.lower
      (invocationOfOperations ~selectedOperationName:"Viewer"
         [
           operationDefinition ~operationName:"Viewer"
             ~variableDefinitions:[ "$userName: String!" ]
             ~variableAssignments:[ ("userName", "string:fuwn") ]
             ~directives:[ "cacheControl(maxAge: 60)" ]
             [ "viewer.id" ]
             [
               selectionBranch
                 [ fieldSegment "user" [ ("name", "var:userName") ] ]
                 [ "id,name" ];
               selectionBranch
                 [ fieldSegment "media" [ ("id", "42") ] ]
                 [ "title.romaji" ];
             ];
         ])
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
      (invocationOfOperations
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
         ])
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
      (invocationOfOperations
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
         ])
  in

  assertEqual
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
     }"
    (queryOfLoweredRequest request)

let testInlineFragmentBranchLowering () =
  let request =
    LoweringEngine.lower
      (invocationOfOperations
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
         ])
  in

  assertEqual
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
     }"
    (queryOfLoweredRequest request)

let testFragmentSpreadDirectiveLowering () =
  let request =
    LoweringEngine.lower
      (invocationOfOperations
         ~fragmentDefinitionTexts:
           [ "fragment mediaCore on Media { popularity }" ]
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
         ])
  in

  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    ...mediaCore @include(if: $withCore)\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media { popularity }"
    (queryOfLoweredRequest request)

let testRawFragmentDefinitionsKeepTheirOrder () =
  let request =
    LoweringEngine.lower
      (invocationOfOperations
         ~fragmentDefinitionTexts:
           [
             "fragment mediaCore on Media { popularity }";
             "fragment legacyFragment on Media { favourites }";
           ]
         [
           operationDefinition []
             [
               selectionBranch
                 [ fieldSegment "media" [ ("id", "1") ] ]
                 [ "...mediaCore"; "...legacyFragment" ];
             ];
         ])
  in

  assertEqual
    "query {\n\
    \  Media(id: 1) {\n\
    \    ...mediaCore\n\
    \    ...legacyFragment\n\
    \  }\n\
     }\n\n\
     fragment mediaCore on Media { popularity }\n\n\
     fragment legacyFragment on Media { favourites }"
    (queryOfLoweredRequest request)

let testMultipleOperationDocumentLowering () =
  let request =
    LoweringEngine.lower
      (invocationOfOperations ~selectedOperationName:"UserLookup"
         [
           operationDefinition ~operationName:"Viewer"
             ~directives:[ "cacheControl(maxAge: 60)" ]
             [ "viewer.id" ] [];
           operationDefinition ~operationName:"UserLookup" []
             [
               selectionBranch
                 [ fieldSegment "user" [ ("name", "fuwn") ] ]
                 [ "id,name" ];
             ];
         ])
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
    invocationOfArguments [ "user"; "--name"; "fuwn"; "--fields"; "id" ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in

      let selectionBranch =
        List.hd (selectionBranchesOfOperation operationDefinition)
      in

      assertEqual "1"
        (string_of_int
           (List.length selectionBranch.CommandLineInvocation.selectionPaths))
  | Error message -> failwith message

let testOperationParsing () =
  match
    invocationOfArguments
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
        "--fragment-definition";
        "fragment userCore on User { id }";
      ]
  with
  | Ok invocation ->
      let operationDefinition =
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in

      assertEqual "Viewer"
        (Option.get operationDefinition.CommandLineInvocation.operationName);
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocation.variableDefinitions));
      assertEqual "1"
        (string_of_int
           (List.length
              operationDefinition.CommandLineInvocation.variableAssignments));
      assertEqual "1"
        (string_of_int
           (List.length (selectionBranchesOfOperation operationDefinition)));
      assertEqual "1"
        (string_of_int
           (List.length invocation.CommandLineInvocation.fragmentDefinitionTexts))
  | Error message -> failwith message

let testVariableAssignmentsPreserveGraphQlVariableNames () =
  match
    invocationOfArguments
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
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in

      assertEqual "user_name"
        (fst
           (List.hd
              operationDefinition.CommandLineInvocation.variableAssignments))
  | Error message -> failwith message

let testVariableAssignmentsRejectVariableReferences () =
  match
    invocationOfArguments
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

let testInlineFragmentShorthandRequiresSelections () =
  match
    invocationOfArguments
      [ "media"; "--id"; "1"; "--selection-set"; "...on:Media" ]
  with
  | Ok _ -> failwith "Expected inline fragment shorthand rejection"
  | Error message ->
      assertContainsSubstring
        "Inline fragments require a type condition and selection set" message

let testRelativeAndAbsoluteFieldPathParsing () =
  match
    invocationOfArguments
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
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      let secondSelectionBranch =
        selectionBranchesOfOperation operationDefinition |> List.tl |> List.hd
      in
      let thirdSelectionBranch =
        selectionBranchesOfOperation operationDefinition |> List.rev |> List.hd
      in

      assertEqual "2"
        (string_of_int
           (List.length
              secondSelectionBranch.CommandLineInvocation.selectionPathSegments));
      match
        List.hd thirdSelectionBranch.CommandLineInvocation.selectionPathSegments
      with
      | CommandLineInvocation.FieldSegment fieldSegment ->
          assertEqual "media" fieldSegment.CommandLineInvocation.fieldName
      | CommandLineInvocation.InlineFragmentSegment _
      | CommandLineInvocation.FragmentSpreadSegment _ ->
          failwith "Expected field segment")
  | Error message -> failwith message

let testExplicitOperationBarePositionalSiblingFields () =
  match
    invocationOfArguments
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
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in

      assertEqual "2"
        (string_of_int
           (List.length (selectionBranchesOfOperation operationDefinition)))
  | Error message -> failwith message

let testInlineFragmentParsing () =
  match
    invocationOfArguments
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
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      let inlineFragmentBranch =
        selectionBranchesOfOperation operationDefinition |> List.rev |> List.hd
      in

      assertEqual "2"
        (string_of_int
           (List.length
              inlineFragmentBranch.CommandLineInvocation.selectionPathSegments))
  | Error message -> failwith message

let testFragmentSpreadParsing () =
  match
    invocationOfArguments
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
        List.hd invocation.CommandLineInvocation.operationDefinitions
      in
      let fragmentSpreadBranch =
        selectionBranchesOfOperation operationDefinition |> List.rev |> List.hd
      in

      match
        List.rev
          fragmentSpreadBranch.CommandLineInvocation.selectionPathSegments
        |> List.hd
      with
      | CommandLineInvocation.FragmentSpreadSegment fragmentSpreadSegment ->
          assertEqual "mediaCore"
            fragmentSpreadSegment.CommandLineInvocation.fragmentSpreadName;
          assertEqual "1"
            (string_of_int
               (List.length
                  fragmentSpreadSegment
                    .CommandLineInvocation.fragmentSpreadDirectiveTexts))
      | CommandLineInvocation.FieldSegment _
      | CommandLineInvocation.InlineFragmentSegment _ ->
          failwith "Expected fragment spread segment")
  | Error message -> failwith message

let testMultipleOperationsRequireSelectedOperationName () =
  match
    invocationOfArguments
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
    invocationOfArguments
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
           (List.length invocation.CommandLineInvocation.operationDefinitions));
      assertEqual "UserLookup"
        (Option.get invocation.CommandLineInvocation.selectedOperationName)
  | Error message -> failwith message

let testSchemaCommandParsing () =
  match SchemaCommand.ofArguments [ "schema" ] with
  | Ok (`Command schemaCommand) ->
      assertEqual "IntrospectionQuery"
        (Option.get schemaCommand.SchemaCommand.operationName);
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.variables with
           | Some _ -> 1
           | None -> 0));
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.requestedDirectiveName with
           | Some _ -> 1
           | None -> 0));
      assertContainsSubstring "__schema" schemaCommand.SchemaCommand.queryText;
      assertContainsSubstring "fragment FullType"
        schemaCommand.SchemaCommand.queryText
  | Ok (`Help | `NotSchemaCommand) -> failwith "Expected schema command"
  | Error message -> failwith message

let testSchemaTypeCommandParsing () =
  match SchemaCommand.ofArguments [ "schema"; "--type"; "Media" ] with
  | Ok (`Command schemaCommand) ->
      assertEqual "IntrospectionTypeQuery"
        (Option.get schemaCommand.SchemaCommand.operationName);
      assertContainsSubstring "__type(name: $name)"
        schemaCommand.SchemaCommand.queryText;
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.requestedDirectiveName with
           | Some _ -> 1
           | None -> 0));
      assertEqual "{\"name\":\"Media\"}"
        (schemaCommand.SchemaCommand.variables |> Option.get
       |> Yojson.Safe.to_string)
  | Ok (`Help | `NotSchemaCommand) -> failwith "Expected schema type command"
  | Error message -> failwith message

let testSchemaDirectiveCommandParsing () =
  match SchemaCommand.ofArguments [ "schema"; "--directive"; "include" ] with
  | Ok (`Command schemaCommand) ->
      assertEqual "IntrospectionDirectiveQuery"
        (Option.get schemaCommand.SchemaCommand.operationName);
      assertContainsSubstring "directives" schemaCommand.SchemaCommand.queryText;
      assertEqual "include"
        (Option.get schemaCommand.SchemaCommand.requestedDirectiveName);
      assertEqual "0"
        (string_of_int
           (match schemaCommand.SchemaCommand.variables with
           | Some _ -> 1
           | None -> 0))
  | Ok (`Help | `NotSchemaCommand) ->
      failwith "Expected schema directive command"
  | Error message -> failwith message

let testSchemaEqualsOptionParsing () =
  (match SchemaCommand.ofArguments [ "schema"; "--type=Media" ] with
  | Ok (`Command schemaCommand) ->
      assertEqual "IntrospectionTypeQuery"
        (Option.get schemaCommand.SchemaCommand.operationName)
  | Ok (`Help | `NotSchemaCommand) -> failwith "Expected schema type command"
  | Error message -> failwith message);
  match SchemaCommand.ofArguments [ "schema"; "--directive=include" ] with
  | Ok (`Command schemaCommand) ->
      assertEqual "IntrospectionDirectiveQuery"
        (Option.get schemaCommand.SchemaCommand.operationName)
  | Ok (`Help | `NotSchemaCommand) ->
      failwith "Expected schema directive command"
  | Error message -> failwith message

let testSchemaCommandRejectsConflictingOptions () =
  match
    SchemaCommand.ofArguments
      [ "schema"; "--type"; "Media"; "--directive"; "include" ]
  with
  | Ok _ -> failwith "Expected schema command rejection"
  | Error _ -> ()

let testSchemaCommandRejectsMissingOptionValues () =
  match
    SchemaCommand.ofArguments [ "schema"; "--type"; "--directive"; "include" ]
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

let testSchemaHelpDetection () =
  if SchemaCommand.ofArguments [ "schema"; "--help" ] <> Ok `Help then
    failwith "Expected schema help detection";

  if SchemaCommand.ofArguments [ "schema"; "help" ] <> Ok `Help then
    failwith "Expected schema bare help detection";

  match SchemaCommand.ofArguments [ "schema"; "nope"; "help" ] with
  | Ok _ -> failwith "Unexpected schema malformed help detection"
  | Error _ -> ()

let () =
  testSingleFieldSelectionShorthand ();
  testExplicitOperationWithMultipleRootFields ();
  testFieldAliasAndDirectiveLowering ();
  testNestedFieldBranchLowering ();
  testInlineFragmentBranchLowering ();
  testFragmentSpreadDirectiveLowering ();
  testRawFragmentDefinitionsKeepTheirOrder ();
  testMultipleOperationDocumentLowering ();
  testSelectionSetCompatibilityAliasParsing ();
  testOperationParsing ();
  testVariableAssignmentsPreserveGraphQlVariableNames ();
  testVariableAssignmentsRejectVariableReferences ();
  testInlineFragmentShorthandRequiresSelections ();
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
  testSchemaHelpDetection ()
