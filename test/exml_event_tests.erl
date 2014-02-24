%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Unit tests for exml_event event-based parser
%%% @end
%%%-------------------------------------------------------------------

%% TODO: write Proper properties!

-module(exml_event_tests).

-include_lib("eunit/include/eunit.hrl").
-include("exml_event.hrl").

-compile(export_all).

basic_parse_test() ->
    {ok, Parser} = exml_event:new_parser(),
    ?assertEqual({ok, [{xml_element_start, <<"test">>, [], []},
                       {xml_element_end, <<"test">>}]},
                 exml_event:parse_final(Parser, <<"<test/>">>)),
    ?assertEqual(ok, exml_event:free_parser(Parser)).

attrs_parsing_test() ->
    {ok, Parser} = exml_event:new_parser(),
    ?assertEqual({ok, [{xml_element_start, <<"test">>, [], [{<<"attr">>, <<"val">>},
                                                            {<<"second_attr">>, <<"val2">>}]},
                       {xml_element_end, <<"test">>}]},
                 exml_event:parse_final(Parser, <<"<test attr='val' second_attr='val2'/>">>)),
    ?assertEqual(ok, exml_event:free_parser(Parser)).

open_tag_test() ->
    {ok, Parser} = exml_event:new_parser(),
    ?assertEqual({ok, [{xml_element_start, <<"test">>, [], []}]},
                 exml_event:parse(Parser, <<"<test>">>)),
    ?assertEqual(ok, exml_event:free_parser(Parser)).

cdata_test() ->
    {ok, Parser} = exml_event:new_parser(),
    ?assertEqual({ok, [{xml_element_start, <<"test">>, [], []},
                       {xml_cdata, <<"some_cdata stuff">>},
                       {xml_element_end, <<"test">>}]},
                 exml_event:parse_final(Parser, <<"<test>some_cdata stuff</test>">>)),
    ?assertEqual(ok, exml_event:free_parser(Parser)).

xmlns_declaration_test() ->
    {ok, Parser} = exml_event:new_parser(),
    ?assertEqual({ok, [{xml_element_start, <<"str:stream">>,
                        %% note the reverse order of namespaces
                        [{<<"naked-ns">>, none},
                         {<<"stream-ns">>, <<"str">>}],
                        []}]},
                 exml_event:parse(Parser, <<"<str:stream"
                                            " xmlns:str='stream-ns'"
                                            " xmlns='naked-ns'
                                            >">>)),
    ?assertEqual(ok, exml_event:free_parser(Parser)).

large_element_test() ->
    {ok, Parser} = exml_event:new_parser(),
    LargeElement = list_to_binary([<<"<test>">> |
                                   lists:duplicate(20000, <<"<element/>">>)]),
    ExpectedEvents = [{xml_element_start, <<"test">>, [], []} |
                      lists:foldl(fun(_, Acc) ->
                                          [{xml_element_start, <<"element">>, [], []},
                                           {xml_element_end, <<"element">>} | Acc]
                                  end, [], lists:seq(1, 20000))],
    ?assertEqual({ok, ExpectedEvents},
                 exml_event:parse(Parser, LargeElement)),
    ?assertEqual(ok, exml_event:free_parser(Parser)).

large_element_fail_test() ->
    {ok, Parser} = exml_event:new_parser(),
    LargeElement = list_to_binary([<<"<test>">> |
                                   lists:duplicate(20000, <<"<element/>">>)]),
    ?assertMatch({error, _},
                 exml_event:parse(Parser, <<LargeElement/binary, <<"<a></b>">>/binary>>)),
    ?assertEqual(ok, exml_event:free_parser(Parser)).
