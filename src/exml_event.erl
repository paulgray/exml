%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal@ptaszek.net>
%%% @doc Event-based XML parser
%%% @end
%%%-------------------------------------------------------------------
-module(exml_event).

-include("exml_event.hrl").

-export([load/0]).
-export([new_parser/0, reset_parser/1, free_parser/1, parse/2, parse_final/2]).

-on_load(load/0).

%% Maximum bytes passed to the NIF handler at once
%% Current value is erlang:system_info(context_reductions) * 10
-define(MAX_BYTES_TO_NIF, 20000).

-define(NOT_FINAL, 0).
-define(FINAL, 1).

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

-spec parse(term(), binary()) -> {ok, list()} | {error, string()}.
parse(Parser, Data) ->
    do_parse(Parser, Data, ?NOT_FINAL, byte_size(Data), []).

-spec parse_final(term(), binary()) -> {ok, list()} | {error, string()}.
parse_final(Parser, Data) ->
    do_parse(Parser, Data, ?FINAL, byte_size(Data), []).

-spec do_parse(term(), binary(), 0 | 1, integer(), list()) ->
                      {ok, list()} | {error, string()}.
do_parse(Parser, Data, Final, Size, Acc) when Size > ?MAX_BYTES_TO_NIF ->
    <<DataToPass:?MAX_BYTES_TO_NIF/binary, Rest/binary>> = Data,
    case parse_nif(Parser, DataToPass, ?NOT_FINAL) of
        {ok, Res} ->
            do_parse(Parser, Rest, Final, Size - ?MAX_BYTES_TO_NIF,
                     [lists:reverse(Res) | Acc]);
        Error ->
            Error
    end;
do_parse(Parser, Data, Final, _Size, Acc) ->
    case parse_nif(Parser, Data, Final) of
        {ok, Res} ->
            {ok, lists:append(lists:reverse([lists:reverse(Res) | Acc]))};
        Error ->
            Error
    end.

-spec new_parser() -> term().
new_parser() ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec reset_parser(term()) -> ok.
reset_parser(_Parser) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec free_parser(term()) -> ok.
free_parser(_Parser) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec parse_nif(term(), binary(), integer()) -> list().
parse_nif(_Parser, _Data, _Final) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

