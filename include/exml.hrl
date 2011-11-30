-type xmlattr() :: {binary(), binary()}.

-record(xmlcdata, {content = [] :: iodata()}).

-record(xmlelement, {name :: binary(),
                     attrs = [] :: [xmlattr()],
                     body =  [] :: [#xmlelement{} | #xmlcdata{}]}).

-type xmlterm() :: #xmlelement{} | xmlattr() | #xmlcdata{}.
-type xmlevent() :: {xml_element_start, binary(), [{binary(), binary()}]} |
                    {xml_cdata, binary()} |
                    {xml_element_end, binary()}.
