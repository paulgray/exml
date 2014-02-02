-module(exml_escape_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

escape_attr_test() ->
    ?assertError(badarg, exml:escape_attr("")),
    ?assertError(badarg, exml:escape_attr([<<"a">>,<<"b">>])),
    assert_escape_attr(<<"">>, <<"">>),
    assert_escape_attr(<<"&amp;">>, <<"&">>),
    assert_escape_attr(<<"&lt;">>, <<"<">>),
    assert_escape_attr(<<"&gt;">>, <<">">>),
    assert_escape_attr(<<"&quot;">>, <<"\"">>),
    assert_escape_attr(<<"&apos;">>, <<"'">>),
    assert_escape_attr(<<"&#x9;">>, <<"\t">>),
    assert_escape_attr(<<"&#xA;">>, <<"\n">>),
    assert_escape_attr(<<"&#xD;">>, <<"\r">>),
    assert_escape_attr(<<"&amp;&lt;&gt;&quot;&apos;&#xA;&#x9;&#xD;">>, <<"&<>\"'\n\t\r">>),
    assert_escape_attr(
        <<"0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789">>,
        <<"0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789">>).

unescape_attr_test() ->
    ?assertError(badarg, exml:unescape_attr("")),
    ?assertError(badarg, exml:unescape_attr([<<"a">>,<<"b">>])),
    assert_unescape_attr(<<"">>, <<"">>),
    assert_unescape_attr(<<"&">>, <<"&amp;">>),
    assert_unescape_attr(<<"&<>">>, <<"&amp;&lt;&gt;">>),
    assert_unescape_attr(<<"&amp">>, <<"&amp">>),
    assert_unescape_attr(<<"&amm;">>, <<"&amm;">>).

escape_cdata_test() ->
    assert_escape_cdata(<<"">>, ""),
    assert_escape_cdata(<<"abc">>, ["a","b","c"]),
    assert_escape_cdata(<<"abc">>, [<<"a">>,<<"b">>,<<"c">>]),
    assert_escape_cdata(<<"">>, <<"">>),
    assert_escape_cdata(<<"&amp;">>, <<"&">>),
    assert_escape_cdata(<<"&lt;">>, <<"<">>),
    assert_escape_cdata(<<"&gt;">>, <<">">>),
    assert_escape_cdata(<<"\"">>, <<"\"">>),
    assert_escape_cdata(<<"'">>, <<"'">>),
    assert_escape_cdata(<<"\t">>, <<"\t">>),
    assert_escape_cdata(<<"\n">>, <<"\n">>),
    assert_escape_cdata(<<"\r">>, <<"\r">>),
    assert_escape_cdata(<<"&amp;&lt;&gt;\"'\n\t\r">>, <<"&<>\"'\n\t\r">>),
    assert_escape_cdata(
        <<"0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789">>,
        <<"0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789">>).

unescape_cdata_test() ->
    assert_unescape_cdata(<<"">>, ""),
    assert_unescape_cdata(<<"abc">>, ["a","b","c"]),
    assert_unescape_cdata(<<"abc">>, [<<"a">>,<<"b">>,<<"c">>]),
    assert_unescape_cdata(<<"">>, <<"">>),
    assert_unescape_cdata(<<"&">>, <<"&amp;">>),
    assert_unescape_cdata(<<"&<>">>, <<"&amp;&lt;&gt;">>),
    assert_unescape_cdata(<<"&amp">>, <<"&amp">>),
    assert_unescape_cdata(<<"&amm;">>, <<"&amm;">>).


assert_escape_attr(EscapedText, Text) ->
    ?assertEqual(EscapedText, exml:escape_attr(Text)),
    ?assertEqual(to_binary(Text), exml:unescape_attr(EscapedText)).

assert_unescape_attr(UnescapedText, Text) ->
    ?assertEqual(UnescapedText, exml:unescape_attr(Text)).

assert_escape_cdata(EscapedText, Text) ->
    ?assertEqual({xmlcdata, EscapedText}, exml:escape_cdata(Text)),
    ?assertEqual(to_binary(Text), exml:unescape_cdata({xmlcdata, EscapedText})).

assert_unescape_cdata(UnescapedText, Text) ->
    ?assertEqual(UnescapedText, exml:unescape_cdata({xmlcdata, Text})).

to_binary(Bin) when is_binary(Bin) ->
    Bin;
to_binary(List) when is_list(List) ->
    list_to_binary(List).

