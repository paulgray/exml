-type xmlattr() :: {binary(), binary()}.

-record(xmlcdata, {content = [] :: iodata()}).

-record(xmlelement, {name :: binary(),
                     attrs = [] :: [xmlattr()],
                     body =  [] :: [#xmlelement{} | #xmlcdata{}]}).

-type xmlterm() :: #xmlelement{} | xmlattr() | #xmlcdata{}.
