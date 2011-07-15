%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Unit tests for exml application
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------

%% TODO: write Proper properties!

-module(exml_tests).

-include_lib("eunit/include/eunit.hrl").
-include("exml.hrl").

-compile(export_all).

application_test() ->
    ?assertEqual(ok, application:start(exml)),
    ?assertEqual(ok, application:stop(exml)).

basic_parse_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual([{xml_element_start, <<"test">>, []},
                  {xml_element_end, <<"test">>}],
                 exml:parse(Parser, <<"<test/>">>, true)),
    ?assertEqual(ok, exml:free_parser(Parser)).

attrs_parsing_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual([{xml_element_start, <<"test">>, [{<<"attr">>, <<"val">>},
                                                   {<<"second_attr">>, <<"val2">>}]},
                  {xml_element_end, <<"test">>}],
                 exml:parse(Parser, <<"<test attr='val' second_attr='val2'/>">>, true)),
    ?assertEqual(ok, exml:free_parser(Parser)).

open_tag_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual([{xml_element_start, <<"test">>, []}],
                 exml:parse(Parser, <<"<test>">>, false)),
    ?assertEqual(ok, exml:free_parser(Parser)).

cdata_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual([{xml_element_start, <<"test">>, []},
                  {xml_cdata, <<"some_cdata stuff">>},
                  {xml_element_end, <<"test">>}],
                 exml:parse(Parser, <<"<test>some_cdata stuff</test>">>, true)),
    ?assertEqual(ok, exml:free_parser(Parser)).

collect_msgs(Acc) ->
    receive
        Msg ->
            collect_msgs([Msg | Acc])
    after 0 ->
            lists:reverse(Acc)
    end.
