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
    io:format(user, "Parser: ~p~n", [Parser]),

    timer:sleep(100),

    ?assertEqual(#xmlElement{name = <<"test">>},
                 exml:parse(Parser, <<"<test/>">>, 1)).
