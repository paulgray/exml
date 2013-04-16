%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Easy navigation in XML trees
%%% @end
%%%-------------------------------------------------------------------
-module(exml_query).

-include("exml.hrl").

-export([path/2, path/3]).
-export([paths/2]).
-export([subelement/2, subelement/3]).
-export([subelements/2]).
-export([attr/2, attr/3]).
-export([cdata/1]).

-type path() :: [cdata | {element, binary()} | {attr, binary()}].

%% @doc gets the element/attr/cdata contained in the leftmost path
-spec path(#xmlel{}, path()) -> #xmlel{} | binary() | undefined.
path(Element, Path) ->
    path(Element, Path, undefined).

%% @doc gets the element/attr/cdata in the leftmost possible described path
-spec path(#xmlel{}, path(), Other) -> #xmlel{} | binary() | Other.
path(#xmlel{} = Element, [], _) ->
    Element;
path(#xmlel{} = Element, [{element, Name} | Rest], Default) ->
    Child = subelement(Element, Name), % may return undefined
    path(Child, Rest, Default);
path(#xmlel{} = Element, [cdata], _) ->
    cdata(Element);
path(#xmlel{} = Element, [{attr, Name}], Default) ->
    attr(Element, Name, Default);
path(_, _, Default) ->
    Default.

%% @doc gets the elements/attrs/cdatas reachable by the described path
-spec paths(#xmlel{}, path()) -> [#xmlel{} | binary()].
paths(#xmlel{} = Element, []) ->
    [Element];
paths(#xmlel{} = Element, [{element, Name} | Rest]) ->
    Children = subelements(Element, Name),
    lists:append([paths(Child, Rest) || Child <- Children]);
paths(#xmlel{} = Element, [cdata]) ->
    [cdata(Element)];
paths(#xmlel{attrs = Attrs}, [{attr, Name}]) ->
    lists:sublist([V || {N, V} <- Attrs, N =:= Name], 1);
paths(#xmlel{}, Path) when is_list(Path) ->
    [].

-spec subelement(#xmlel{}, binary()) -> #xmlel{} | undefined.
subelement(Element, Name) ->
    subelement(Element, Name, undefined).

-spec subelement(#xmlel{}, binary(), Other) -> #xmlel{} | Other.
subelement(#xmlel{children = Children}, Name, Default) ->
    case lists:keyfind(Name, #xmlel.name, Children) of
        false ->
            Default;
        Result ->
            Result
    end.

-spec subelements(#xmlel{}, binary()) -> [#xmlel{}].
subelements(#xmlel{children = Children}, Name) ->
    lists:filter(fun(#xmlel{name = N}) when N =:= Name ->
                        true;
                    (_) ->
                        false
                 end, Children).

-spec cdata(#xmlel{}) -> binary().
cdata(#xmlel{children = Children}) ->
    list_to_binary([exml:unescape_cdata(C) || #xmlcdata{} = C <- Children]).

-spec attr(#xmlel{}, binary()) -> binary() | undefined.
attr(Element, Name) ->
    attr(Element, Name, undefined).

-spec attr(#xmlel{}, binary(), Other) -> binary() | Other.
attr(#xmlel{attrs = Attrs}, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            Default
    end.
