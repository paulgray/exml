%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Unit tests for exml application
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

application_test() ->
    ?assertEqual(ok, application:start(exml)),
    ?assertEqual(ok, application:stop(exml)).
