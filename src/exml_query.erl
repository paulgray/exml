%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Easy navigation in XML trees
%%% @end
%%%-------------------------------------------------------------------
-module(exml_query).

-include("exml.hrl").

-export([path/2, path/3]).
-export([subelement/2, subelement/3]).
-export([subelements/2]).
-export([attr/2, attr/3]).
-export([cdata/1]).

-type path() :: [cdata | {element, binary()} | {attr, binary()}].

%% @doc gets the element/attr/cdata contained in the leftmost path
-spec path(#xmlelement{}, path()) -> #xmlelement{} | binary() | undefined.
path(Element, Path) ->
    path(Element, Path, undefined).

%% @doc gets the element/attr/cdata contained in the leftmost path
-spec path(#xmlelement{}, path(), Other) -> #xmlelement{} | binary() | Other.
path(#xmlelement{} = Element, [], _) ->
    Element;
path(#xmlelement{} = Element, [{element, Name} | Rest], Default) ->
    Child = subelement(Element, Name), % may return undefined
    path(Child, Rest, Default);
path(#xmlelement{} = Element, [{elements, Name} | Rest], Default) ->
    Children = subelements(Element, Name),
    lists:concat([[path(Child, Rest, Default) || Child <- Children]]);
path(#xmlelement{} = Element, [cdata], _) ->
    cdata(Element);
path(#xmlelement{} = Element, [{attr, Name}], Default) ->
    attr(Element, Name, Default);
path(_, _, Default) ->
    Default.

-spec subelement(#xmlelement{}, binary()) -> #xmlelement{} | undefined.
subelement(Element, Name) ->
    subelement(Element, Name, undefined).

-spec subelement(#xmlelement{}, binary(), Other) -> #xmlelement{} | Other.
subelement(#xmlelement{body = Body}, Name, Default) ->
    case lists:keyfind(Name, #xmlelement.name, Body) of
        false ->
            Default;
        Result ->
            Result
    end.

-spec subelements(#xmlelement{}, binary()) -> [#xmlelement{}].
subelements(#xmlelement{body = Body}, Name) ->
    lists:filter(fun(#xmlelement{name = N}) when N =:= Name ->
                        true;
                    (_) ->
                        false
                 end,
                 Body).

-spec cdata(#xmlelement{}) -> binary().
cdata(#xmlelement{body = Body}) ->
    list_to_binary([exml:unescape_cdata(C) || #xmlcdata{}=C <- Body]).

-spec attr(#xmlelement{}, binary()) -> binary() | undefined.
attr(Element, Name) ->
    attr(Element, Name, undefined).

-spec attr(#xmlelement{}, binary(), Other) -> binary() | Other.
attr(#xmlelement{attrs = Attrs}, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            Default
    end.
