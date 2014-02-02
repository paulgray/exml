%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Event-based XML parser
%%% @end
%%%-------------------------------------------------------------------
-module(exml_event).

-include("exml_event.hrl").

-export([load/0]).
-export([new_parser/0, reset_parser/1, free_parser/1, parse/2, parse_final/2]).

-on_load(load/0).

-spec load() -> any().
load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "exml_event"), none).

-spec new_parser() -> term().
new_parser() ->
    throw({?MODULE, nif_not_loaded}).

-spec reset_parser(term()) -> ok.
reset_parser(_Parser) ->
    throw({?MODULE, nif_not_loaded}).

-spec free_parser(term()) -> ok.
free_parser(_Parser) ->
    throw({?MODULE, nif_not_loaded}).

-spec parse(term(), binary()) -> {ok, list()} | {error, string()}.
parse(Parser, Data) ->
    do_parse(Parser, Data, 0).

-spec parse_final(term(), binary()) -> {ok, list()} | {error, string()}.
parse_final(Parser, Data) ->
    do_parse(Parser, Data, 1).

-spec do_parse(term(), binary(), 0 | 1) -> {ok, list()} | {error, string()}.
do_parse(Parser, Data, Final) ->
    case parse_nif(Parser, Data, Final) of
        {ok, Res} ->
            {ok, lists:reverse(Res)};
        Error ->
            Error
    end.

-spec parse_nif(term(), binary(), integer()) -> list().
parse_nif(_Parser, _Data, _Final) ->
    throw({?MODULE, nif_not_loaded}).
