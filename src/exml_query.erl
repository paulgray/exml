%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Easy navigation in XML trees
%%% @end
%%%-------------------------------------------------------------------
-module(exml_query).

-include("exml.hrl").

-export([path/2, path/3]).
-export([subelement/2, subelement/3]).
-export([attr/2, attr/3]).
-export([cdata/1]).

%% gets the element contained in leftmost path
path(Element, Path) ->
    path(Element, Path, undefined).

path(Element, [], _) ->
    Element;
path(#xmlelement{} = Element, [{element, Name} | Rest], Default) ->
    Child = subelement(Element, Name),
    path(Child, Rest, Default);
path(#xmlelement{} = Element, [cdata], _) ->
    cdata(Element);
path(#xmlelement{} = Element, [{attr, Name}], Default) ->
    attr(Element, Name, Default);
path(_, _, Default) ->
    Default.

subelement(Element, Name) ->
    subelement(Element, Name, undefined).

subelement(#xmlelement{body = Body}, Name, Default) ->
    case lists:keyfind(Name, #xmlelement.name, Body) of
        false ->
            Default;
        Result ->
            Result
    end.

cdata(#xmlelement{body = Body}) ->
    list_to_binary([exml:unescape_cdata(C) || #xmlcdata{}=C <- Body]).

attr(Element, Name) ->
    attr(Element, Name, undefined).

attr(#xmlelement{attrs = Attrs}, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            Default
    end.
