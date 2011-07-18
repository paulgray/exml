%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc A set of benchmarks comparing expat_sax_parser, erlsom and exml.
%%%
%%% @end
%%% Created : 15 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(benchmarks).

-export([run/2,
         run_exml/3,
         run_erlsom/3,
         run_xmerl/3]).

run(Path, N) ->
    application:start(exml),
    [apply(?MODULE, F, [Path, N, []]) || F <- [run_exml, run_erlsom, run_xmerl]],
    application:stop(exml).

run_exml(_, 0, Acc) ->
    Avg = avg(Acc),
    io:format("Total: ~w~n"
              "Avg: ~w~n"
              "Std dev.: ~w~n",
              [total(Acc), Avg, std_dev(Acc, Avg)]);
run_exml(Path, N, Acc) ->
    {ok, Fd} = file:open(Path, [read, binary, read_ahead]),
    T = now(),

    {ok, Parser} = exml:new_parser(),
    run_exml(Fd, Parser),
    exml:free_parser(Parser),

    Diff = timer:now_diff(now(), T),
    run_exml(Path, N-1, [Diff | Acc]).

run_exml(Fd, Parser) ->
    case file:read_line(Fd) of
        eof ->
            ok;
        {ok, Data} ->
            exml:parse(Parser, Data, true),
            run_exml(Fd, Parser)
    end.

total(Times) ->
    lists:sum(Times).

avg(Times) ->
    total(Times)/length(Times).

std_dev(Times, Avg) ->
    Sums = lists:foldl(
             fun(V, Acc) -> D = V - Avg, Acc + (D * D) end,
             0, Times),
    math:sqrt(Sums / (length(Times) - 1)).

run_xmerl(_, _, _) ->
    ok.

run_erlsom(_, _, _) ->
    ok.
