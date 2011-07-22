-include("exml.hrl").

-record(xmlStreamStart, {name :: binary(),
                         attrs = [] :: list(#xmlAttribute{})}).
-record(xmlStreamEnd, {name :: binary()}).

-type xmpp_term() :: xml_term() | #xmlStreamStart{} | #xmlStreamEnd{}.
