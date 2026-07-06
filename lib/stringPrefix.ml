let valueWithoutPrefix ~prefix value =
  String.sub value (String.length prefix)
    (String.length value - String.length prefix)
