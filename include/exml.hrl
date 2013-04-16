-type xmlattr() :: {binary(), binary()}.

-record(xmlcdata, {content = [] :: iodata()}).

-record(xmlel, {name :: binary(),
                attrs = [] :: [xmlattr()],
                children =  [] :: [#xmlel{} | #xmlcdata{}]}).

-type xmlterm() :: #xmlel{} | xmlattr() | #xmlcdata{}.
