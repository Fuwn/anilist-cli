(* GraphQL fragments cannot recurse, so wrapper types are unwrapped to a fixed
   depth. *)
let ofTypeNestingDepth = 7

let rec typeReferenceFields remainingOfTypeDepth =
  if remainingOfTypeDepth = 0 then "kind name"
  else
    Printf.sprintf "kind name ofType { %s }"
      (typeReferenceFields (remainingOfTypeDepth - 1))

let typeRefFragment =
  Printf.sprintf "fragment TypeRef on __Type { %s }"
    (typeReferenceFields ofTypeNestingDepth)

let inputValueFragment =
  "fragment InputValue on __InputValue { name description type { ...TypeRef } \
   defaultValue }"

let fullTypeFragment =
  "fragment FullType on __Type { kind name description \
   fields(includeDeprecated: true) { name description args { ...InputValue } \
   type { ...TypeRef } isDeprecated deprecationReason } inputFields { \
   ...InputValue } interfaces { ...TypeRef } enumValues(includeDeprecated: \
   true) { name description isDeprecated deprecationReason } possibleTypes { \
   ...TypeRef } }"

let directivesSelection =
  "directives { name description locations args { ...InputValue } }"

let fullSchemaQueryText =
  String.concat " "
    [
      Printf.sprintf
        "query IntrospectionQuery { __schema { queryType { name } mutationType \
         { name } subscriptionType { name } types { ...FullType } %s } }"
        directivesSelection;
      fullTypeFragment;
      inputValueFragment;
      typeRefFragment;
    ]

let typeQueryText =
  String.concat " "
    [
      "query IntrospectionTypeQuery($name: String!) { __type(name: $name) { \
       ...FullType } }";
      fullTypeFragment;
      inputValueFragment;
      typeRefFragment;
    ]

let directiveQueryText =
  String.concat " "
    [
      Printf.sprintf "query IntrospectionDirectiveQuery { __schema { %s } }"
        directivesSelection;
      inputValueFragment;
      typeRefFragment;
    ]
