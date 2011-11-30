%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 12 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml).

-include("exml_stream.hrl").

-export([to_string/1, to_binary/1, to_iolist/1]).
-export([cdata_to_binary/1]).

-spec to_string(xmlterm() | [xmlterm()]) -> string().
to_string(Element) ->
    binary_to_list(to_binary(Element)).

-spec to_binary(xmlterm() | [xmlterm()]) -> binary().
to_binary(Element) ->
    list_to_binary(to_iolist(Element)).

-spec to_iolist(xmlterm() | [xmlterm()]) -> iolist().
to_iolist(Elements) when is_list(Elements) ->
    lists:map(fun to_iolist/1, Elements);
to_iolist(#xmlelement{name = Name, attrs = Attrs, body = []}) ->
    ["<", Name, attrs_to_iolist(Attrs, []), "/>"];
to_iolist(#xmlelement{name = Name, attrs = Attrs, body = Body}) ->
    ["<", Name, attrs_to_iolist(Attrs, []), ">",
     to_iolist(Body),
     "</", Name, ">"];
to_iolist(#xmlstreamstart{name = Name, attrs = Attrs}) ->
    ["<", Name, attrs_to_iolist(Attrs, []), ">"];
to_iolist(#xmlstreamend{name = Name}) ->
    ["</", Name, ">"];
to_iolist(#xmlcdata{content = Content}) ->
    Content.

-spec cdata_to_binary(#xmlcdata{}) -> binary().
cdata_to_binary(#xmlcdata{content = Content}) ->
    list_to_binary([Content]).

-spec attrs_to_iolist([{binary(), binary()}], iolist()) -> iolist().
attrs_to_iolist([], Acc) ->
    Acc;
attrs_to_iolist([{Name, Value} | Rest], Acc) ->
    attrs_to_iolist(Rest, [" ", Name, "='", Value, "' " | Acc]).
