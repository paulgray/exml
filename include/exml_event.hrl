-type xmlevent() :: {xml_element_start, binary(), [{binary(), binary()}]} |
                    {xml_cdata, binary()} |
                    {xml_element_end, binary()}.
