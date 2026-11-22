let valueWithoutPrefix ~prefix value =
  if String.starts_with ~prefix value then
    Some
      (String.sub value (String.length prefix)
         (String.length value - String.length prefix))
  else None
