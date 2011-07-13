-record(xmlAttribute, {name :: binary(),
                       value :: binary()}).

-record(xmlCData, {content = <<>> :: binary()}).

-record(xmlElement, {name :: binary(),
                     attrs = [] :: list(#xmlAttribute{}),
                     body = [] :: list(#xmlElement{} | #xmlCData{})}).
