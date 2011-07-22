%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml).

-include("exml_xmpp.hrl").

-export([load/0]).
-export([new_parser/0, free_parser/1, parse/3]).
-export([to_string/1, to_binary/1]).

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
    erlang:load_nif(filename:join(PrivDir, "exml"), none).

-spec new_parser() -> term().
new_parser() ->
    throw({?MODULE, nif_not_loaded}).

-spec free_parser(term()) -> ok.
free_parser(_Parser) ->
    throw({?MODULE, nif_not_loaded}).

-spec parse(term(), binary(), boolean()) -> {ok, list()} | {error, string()}.
parse(Parser, Data, Final) ->
    case parse_nif(Parser, Data, bool(Final)) of
        {ok, Res} ->
            {ok, lists:reverse(Res)};
        Error ->
            Error
    end.

-spec parse_nif(term(), binary(), integer()) -> list().
parse_nif(_Parser, _Data, _Final) ->
    throw({?MODULE, nif_not_loaded}).

-spec to_binary(xml_term() | list(xml_term())) -> binary().
to_binary(Element) when is_binary(Element) ->
    list_to_binary(to_binary_int(Element));
to_binary(Elements) when is_list(Elements) ->
    list_to_binary(lists:map(fun to_binary_int/1, Elements)).

-spec to_string(xml_term() | list(xml_term())) -> string().
to_string(Element) ->
    binary_to_list(to_binary(Element)).

-spec to_binary_int(xml_term()) -> iolist().
to_binary_int(#xmlElement{name = Name, attrs = Attrs, body = []}) ->
    ["<", Name, attrs_to_binary(Attrs, []), "/>"];
to_binary_int(#xmlElement{name = Name, attrs = Attrs, body = Body}) ->
    ["<", Name, attrs_to_binary(Attrs, []), ">",
     lists:map(fun to_binary_int/1, Body), "</", Name, ">"];
to_binary_int(#xmlStreamStart{name = Name, attrs = Attrs}) ->
    ["<", Name, attrs_to_binary(Attrs, []), ">"];
to_binary_int(#xmlStreamEnd{name = Name}) ->
    ["</", Name, ">"];
to_binary_int(#xmlCData{content = Content}) ->
    Content.

-spec attrs_to_binary(list(#xmlAttribute{}), iolist()) -> iolist().
attrs_to_binary([], Acc) ->
    Acc;
attrs_to_binary([#xmlAttribute{name = Name, value = Value} | Rest], Acc) ->
    attrs_to_binary(Rest, [" ", Name, "='", Value, "' " | Acc]).

-spec bool(boolean()) -> 1 | 0.
bool(true) -> 1;
bool(false) -> 0.
