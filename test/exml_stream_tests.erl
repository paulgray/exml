-module(exml_stream_tests).

-include("exml_stream.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_parse_test() ->
    {ok, Pid} = exml_stream:start_link(),
    ?assert(is_pid(Pid)),

    ?assertEqual(
       [#xmlstreamstart{name = <<"stream:stream">>,
                        attrs = [{<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                                 {<<"version">>, <<"1.0">>},
                                 {<<"to">>, <<"i.am.banana.com">>},
                                 {<<"xml:lang">>, <<"en">>}]}],
       exml_stream:parse(Pid, <<"<stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='1.0' to='i.am.banana.com' xml:lang='en'><auth">>)),
    ?assertEqual(
       [#xmlelement{name = <<"auth">>,
                    attrs = [{<<"mechanism">>, <<"DIGEST-MD5">>}]}],
       exml_stream:parse(Pid, <<" mechanism='DIGEST-MD5'/>">>)),
    ?assertEqual(
       [],
       exml_stream:parse(Pid, <<"<stream:features><bind xmlns='some_ns'">>)),
    ?assertEqual(
       [],
       exml_stream:parse(Pid, <<"/><session xmlns='some_other'/>This is ">>)),
    ?assertEqual(
       [#xmlelement{name = <<"stream:features">>,
                    body = [#xmlelement{name = <<"bind">>,
                                        attrs = [{<<"xmlns">>, <<"some_ns">>}]},
                            #xmlelement{name = <<"session">>,
                                        attrs = [{<<"xmlns">>, <<"some_other">>}]},
                            #xmlcdata{content = <<"This is some CData">>}]}],
       exml_stream:parse(Pid, <<"some CData</stream:features>">>)),
    ?assertEqual(
       [#xmlstreamend{name = <<"stream:stream">>}],
       exml_stream:parse(Pid, <<" </stream:stream>">>)),

    ?assertEqual(ok, exml_stream:stop(Pid)),
    ?assertNot(is_process_alive(Pid)).

conv_test() ->
    {ok, Pid} = exml_stream:start_link(),

    Elements = exml_stream:parse(Pid, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo></stream:stream>">>),
    ?assertEqual([#xmlstreamstart{name = <<"stream:stream">>,
                                  attrs = [{<<"xmlns:stream">>, <<"something">>}]},
                  #xmlelement{name = <<"foo">>,
                              attrs = [{<<"attr">>, <<"bar">>}],
                              body = [#xmlcdata{content = <<"I am a banana!">>},
                                      #xmlelement{name = <<"baz">>}]},
                  #xmlstreamend{name = <<"stream:stream">>}],
                 Elements),
    ?assertEqual(ok, exml_stream:stop(Pid)),


    {ok, Pid2} = exml_stream:start_link(),

    ?assertEqual(Elements,
                 exml_stream:parse(Pid2, exml:to_binary(Elements))),
    ?assertEqual(ok, exml_stream:stop(Pid2)),


    {ok, Pid3} = exml_stream:start_link(),

    ?assertEqual(Elements,
                 exml_stream:parse(Pid3, list_to_binary(exml:to_string(Elements)))),
    ?assertEqual(ok, exml_stream:stop(Pid3)).

stream_reopen_test() ->
    {ok, Pid} = exml_stream:start_link(),

    Elements = exml_stream:parse(Pid, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo>">>),
    ?assertEqual([#xmlstreamstart{name = <<"stream:stream">>,
                                  attrs = [{<<"xmlns:stream">>, <<"something">>}]},
                  #xmlelement{name = <<"foo">>,
                              attrs = [{<<"attr">>, <<"bar">>}],
                              body = [#xmlcdata{content = <<"I am a banana!">>},
                                      #xmlelement{name = <<"baz">>}]}],
                 Elements),

    ?assertEqual(ok, exml_stream:reset_stream(Pid)),

    Elements2 = exml_stream:parse(Pid, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo>">>),
    ?assertEqual([#xmlstreamstart{name = <<"stream:stream">>,
                                  attrs = [{<<"xmlns:stream">>, <<"something">>}]},
                  #xmlelement{name = <<"foo">>,
                              attrs = [{<<"attr">>, <<"bar">>}],
                              body = [#xmlcdata{content = <<"I am a banana!">>},
                                      #xmlelement{name = <<"baz">>}]}],
                 Elements2),

    ?assertEqual(ok, exml_stream:stop(Pid)).
