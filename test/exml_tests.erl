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

    ?assertEqual({ok, [{xml_element_start, <<"test">>, []},
                       {xml_element_end, <<"test">>}]},
                 exml:parse_final(Parser, <<"<test/>">>)),
    ?assertEqual(ok, exml:free_parser(Parser)).

attrs_parsing_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual({ok, [{xml_element_start, <<"test">>, [{<<"attr">>, <<"val">>},
                                                        {<<"second_attr">>, <<"val2">>}]},
                       {xml_element_end, <<"test">>}]},
                 exml:parse_final(Parser, <<"<test attr='val' second_attr='val2'/>">>)),
    ?assertEqual(ok, exml:free_parser(Parser)).

open_tag_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual({ok, [{xml_element_start, <<"test">>, []}]},
                 exml:parse(Parser, <<"<test>">>)),
    ?assertEqual(ok, exml:free_parser(Parser)).

cdata_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual({ok, [{xml_element_start, <<"test">>, []},
                       {xml_cdata, <<"some_cdata stuff">>},
                       {xml_element_end, <<"test">>}]},
                 exml:parse_final(Parser, <<"<test>some_cdata stuff</test>">>)),
    ?assertEqual(ok, exml:free_parser(Parser)).
