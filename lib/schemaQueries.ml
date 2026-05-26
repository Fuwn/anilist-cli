let typeRefFragment =
  {|fragment TypeRef on __Type {
  kind
  name
  ofType {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    }
  }
}|}

let inputValueFragment =
  {|fragment InputValue on __InputValue {
  name
  description
  type {
    ...TypeRef
  }
  defaultValue
}|}

let fullTypeFragment =
  {|fragment FullType on __Type {
  kind
  name
  description
  fields(includeDeprecated: true) {
    name
    description
    args {
      ...InputValue
    }
    type {
      ...TypeRef
    }
    isDeprecated
    deprecationReason
  }
  inputFields {
    ...InputValue
  }
  interfaces {
    ...TypeRef
  }
  enumValues(includeDeprecated: true) {
    name
    description
    isDeprecated
    deprecationReason
  }
  possibleTypes {
    ...TypeRef
  }
}|}

let fullSchemaQueryText =
  Printf.sprintf
    {|query IntrospectionQuery {
  __schema {
    queryType {
      name
    }
    mutationType {
      name
    }
    subscriptionType {
      name
    }
    types {
      ...FullType
    }
    directives {
      name
      description
      locations
      args {
        ...InputValue
      }
    }
  }
}

%s

%s

%s|}
    fullTypeFragment inputValueFragment typeRefFragment

let typeQueryText =
  Printf.sprintf
    {|query IntrospectionTypeQuery($name: String!) {
  __type(name: $name) {
    ...FullType
  }
}

%s

%s

%s|}
    fullTypeFragment inputValueFragment typeRefFragment

let directiveQueryText =
  Printf.sprintf
    {|query IntrospectionDirectiveQuery {
  __schema {
    directives {
      name
      description
      locations
      args {
        ...InputValue
      }
    }
  }
}

%s

%s|}
    inputValueFragment typeRefFragment
