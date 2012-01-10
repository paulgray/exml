-module(exml_stream_tests).

-include("exml_stream.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_parse_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Empty0} =
            exml_stream:parse(Parser0, <<"<stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='1.0'">>),
    ?assertEqual([], Empty0),
    {ok, Parser2, StreamStart} =
            exml_stream:parse(Parser1, <<" to='i.am.banana.com' xml:lang='en'><auth">>),
    ?assertEqual(
       [#xmlstreamstart{name = <<"stream:stream">>,
                        attrs = [{<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                                 {<<"version">>, <<"1.0">>},
                                 {<<"to">>, <<"i.am.banana.com">>},
                                 {<<"xml:lang">>, <<"en">>}]}],
       StreamStart),
    {ok, Parser3, Auth} = exml_stream:parse(Parser2, <<" mechanism='DIGEST-MD5'/>">>),
    ?assertEqual(
       [#xmlelement{name = <<"auth">>, attrs = [{<<"mechanism">>, <<"DIGEST-MD5">>}]}],
       Auth),
    {ok, Parser4, Empty1} = exml_stream:parse(Parser3, <<"<stream:features><bind xmlns='some_ns'">>),
    ?assertEqual([], Empty1),
    {ok, Parser5, Empty2} = exml_stream:parse(Parser4, <<"/><session xmlns='some_other'/>This is ">>),
    ?assertEqual([], Empty2),
    {ok, Parser6, Features} = exml_stream:parse(Parser5, <<"some CData</stream:features>">>),
    ?assertMatch(
       [#xmlelement{name = <<"stream:features">>,
                    body = [#xmlelement{name = <<"bind">>,
                                        attrs = [{<<"xmlns">>, <<"some_ns">>}]},
                            #xmlelement{name = <<"session">>,
                                        attrs = [{<<"xmlns">>, <<"some_other">>}]},
                            _CData]}],
       Features),
    [#xmlelement{body=[_, _, CData]}] = Features,
    ?assertEqual(<<"This is some CData">>, exml:unescape_cdata(CData)),
    ?assertEqual(ok, exml_stream:free_parser(Parser6)).

-define(BANANA_STREAM, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo></stream:stream>">>).
-define(assertIsBanana(Elements), (fun() -> % fun instead of begin/end because we bind CData in unhygenic macro
        ?assertMatch([#xmlstreamstart{name = <<"stream:stream">>,
                                      attrs = [{<<"xmlns:stream">>, <<"something">>}]},
                      #xmlelement{name = <<"foo">>,
                                  attrs = [{<<"attr">>, <<"bar">>}],
                                  body = [_CData, #xmlelement{name = <<"baz">>}]},
                      #xmlstreamend{name = <<"stream:stream">>}],
                     Elements),
        [_, #xmlelement{body=[CData|_]}|_] = Elements,
        ?assertEqual(<<"I am a banana!">>, exml:unescape_cdata(CData)),
        Elements
end)()).

conv_test() ->
    AssertParses = fun(Input) ->
        {ok, Parser0} = exml_stream:new_parser(),
        {ok, Parser1, Elements} = exml_stream:parse(Parser0, Input),
        ok = exml_stream:free_parser(Parser1),
        ?assertIsBanana(Elements)
    end,
    Elements = AssertParses(?BANANA_STREAM),
    AssertParses(exml:to_binary(Elements)),
    AssertParses(list_to_binary(exml:to_list(Elements))),
    AssertParses(list_to_binary(exml:to_iolist(Elements))).

stream_reopen_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Elements1} = exml_stream:parse(Parser0, ?BANANA_STREAM),
    ?assertIsBanana(Elements1),
    {ok, Parser2} = exml_stream:reset_parser(Parser1),
    {ok, Parser3, Elements2} = exml_stream:parse(Parser2, ?BANANA_STREAM),
    ?assertIsBanana(Elements2),
    ok = exml_stream:free_parser(Parser3).

parse_error_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    Input = <<"top-level non-tag">>,
    ?assertEqual({error, {"syntax error", Input}}, exml_stream:parse(Parser0, Input)),
    ok = exml_stream:free_parser(Parser0).

assert_parses_escape_cdata(Text) ->
    Escaped = exml:escape_cdata(Text),
    Tag = #xmlelement{name = <<"tag">>, body=[Escaped]},
    Stream = [#xmlstreamstart{name = <<"s">>}, Tag, #xmlstreamend{name = <<"s">>}],
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Elements} = exml_stream:parse(Parser0, exml:to_binary(Stream)),
    ?assertMatch([#xmlstreamstart{name = <<"s">>},
                  #xmlelement{name = <<"tag">>, body=[_CData]},
                  #xmlstreamend{name = <<"s">>}],
                 Elements),
    [_, #xmlelement{body=[CData]}, _] = Elements,
    ?assertEqual(Text, exml:unescape_cdata(CData)),
    ok = exml_stream:free_parser(Parser1).

cdata_test() ->
    assert_parses_escape_cdata(<<"I am a banana!">>),
    assert_parses_escape_cdata(<<"]:-> ]]> >">>),
    assert_parses_escape_cdata(<<"><tag">>),
    assert_parses_escape_cdata(<<"<!--">>),
    assert_parses_escape_cdata(<<"<![CDATA[ test">>).
