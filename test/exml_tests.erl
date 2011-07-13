%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Unit tests for exml application
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------

%% TODO: write proper properties!
-module(exml_tests).

-include_lib("eunit/include/eunit.hrl").
-include("exml.hrl").

-compile(export_all).

application_test() ->
    ?assertEqual(ok, application:start(exml)),
    ?assertEqual(ok, application:stop(exml)).

basic_parse_test() ->
    {ok, Parser} = exml:new_parser(),

    ?assertEqual(ok, exml:parse(Parser, <<"<test/>">>, 1)),
    Elements = collect_msgs([]),
    ?assertEqual([{xml_element_start, <<"test">>, []},
                  {xml_element_end, <<"test">>}],
                 Elements).

collect_msgs(Acc) ->
    receive
        Msg ->
            collect_msgs([Msg | Acc])
    after 0 ->
            lists:reverse(Acc)
    end.

