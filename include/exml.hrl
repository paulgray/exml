-type xmlattr() :: {binary(), binary()}.

-record(xmlcdata, {content = <<>> :: binary()}).

-record(xmlelement, {name :: binary(),
                     attrs = [] :: [xmlattr()],
                     body =  [] :: [#xmlelement{} | #xmlcdata{}]}).

-type xmlterm() :: #xmlelement{} | xmlattr() | #xmlcdata{}.
