-module(exml_xmpp_tests).

-include("exml_xmpp.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

basic_parse_test() ->
    {ok, Pid} = exml_xmpp:start_link(),
    ?assert(is_pid(Pid)),

    ?assertEqual(
       [#xmlStreamStart{name = <<"stream:stream">>,
                        attrs = [#xmlAttribute{name = <<"xmlns:stream">>,
                                               value = <<"http://etherx.jabber.org/streams">>},
                                 #xmlAttribute{name = <<"version">>,
                                               value = <<"1.0">>},
                                 #xmlAttribute{name = <<"to">>,
                                               value = <<"i.am.banana.com">>},
                                 #xmlAttribute{name = <<"xml:lang">>,
                                               value = <<"en">>}]}],
       exml_xmpp:parse(Pid, <<"<stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='1.0' to='i.am.banana.com' xml:lang='en'><auth">>)),
    ?assertEqual(
       [#xmlElement{name = <<"auth">>,
                    attrs = [#xmlAttribute{name = <<"mechanism">>,
                                           value = <<"DIGEST-MD5">>}]}],
       exml_xmpp:parse(Pid, <<" mechanism='DIGEST-MD5'/>">>)),
    ?assertEqual(
       [],
       exml_xmpp:parse(Pid, <<"<stream:features><bind xmlns='some_ns'">>)),
    ?assertEqual(
       [],
       exml_xmpp:parse(Pid, <<"/><session xmlns='some_other'/>This is ">>)),
    ?assertEqual(
       [#xmlElement{name = <<"stream:features">>,
                    body = [#xmlElement{name = <<"bind">>,
                                        attrs = [#xmlAttribute{name = <<"xmlns">>,
                                                               value = <<"some_ns">>}]},
                            #xmlElement{name = <<"session">>,
                                        attrs = [#xmlAttribute{name = <<"xmlns">>,
                                                               value = <<"some_other">>}]},
                            #xmlCData{content = <<"This is some CData">>}]}],
       exml_xmpp:parse(Pid, <<"some CData</stream:features>">>)),
    ?assertEqual(
       [#xmlStreamEnd{name = <<"stream:stream">>}],
       exml_xmpp:parse(Pid, <<" </stream:stream>">>)),

    ?assertEqual(ok, exml_xmpp:stop(Pid)),
    ?assertNot(is_process_alive(Pid)).

conv_test() ->
    {ok, Pid} = exml_xmpp:start_link(),

    Elements = exml_xmpp:parse(Pid, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo></stream:stream>">>),
    ?assertEqual([#xmlStreamStart{name = <<"stream:stream">>,
                                  attrs = [#xmlAttribute{name = <<"xmlns:stream">>,
                                                         value = <<"something">>}]},
                  #xmlElement{name = <<"foo">>,
                              attrs = [#xmlAttribute{name = <<"attr">>,
                                                     value = <<"bar">>}],
                              body = [#xmlCData{content = <<"I am a banana!">>},
                                      #xmlElement{name = <<"baz">>}]},
                  #xmlStreamEnd{name = <<"stream:stream">>}],
                 Elements),
    ?assertEqual(ok, exml_xmpp:stop(Pid)),


    {ok, Pid2} = exml_xmpp:start_link(),

    ?assertEqual(Elements,
                 exml_xmpp:parse(Pid2, exml:to_binary(Elements))),
    ?assertEqual(ok, exml_xmpp:stop(Pid2)),


    {ok, Pid3} = exml_xmpp:start_link(),

    ?assertEqual(Elements,
                 exml_xmpp:parse(Pid3, list_to_binary(exml:to_string(Elements)))),
    ?assertEqual(ok, exml_xmpp:stop(Pid3)).

stream_reopen_test() ->
    {ok, Pid} = exml_xmpp:start_link(),

    Elements = exml_xmpp:parse(Pid, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo>">>),
    ?assertEqual([#xmlStreamStart{name = <<"stream:stream">>,
                                  attrs = [#xmlAttribute{name = <<"xmlns:stream">>,
                                                         value = <<"something">>}]},
                  #xmlElement{name = <<"foo">>,
                              attrs = [#xmlAttribute{name = <<"attr">>,
                                                     value = <<"bar">>}],
                              body = [#xmlCData{content = <<"I am a banana!">>},
                                      #xmlElement{name = <<"baz">>}]}],
                 Elements),

    ?assertEqual(ok, exml_xmpp:reset_stream(Pid)),

    Elements2 = exml_xmpp:parse(Pid, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo>">>),
    ?assertEqual([#xmlStreamStart{name = <<"stream:stream">>,
                                  attrs = [#xmlAttribute{name = <<"xmlns:stream">>,
                                                         value = <<"something">>}]},
                  #xmlElement{name = <<"foo">>,
                              attrs = [#xmlAttribute{name = <<"attr">>,
                                                     value = <<"bar">>}],
                              body = [#xmlCData{content = <<"I am a banana!">>},
                                      #xmlElement{name = <<"baz">>}]}],
                 Elements2),

    ?assertEqual(ok, exml_xmpp:stop(Pid)).
