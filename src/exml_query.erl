%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Easy navigation in XML trees
%%% @end
%%%-------------------------------------------------------------------
-module(exml_query).

-include("exml_stream.hrl").

-export([path/2, path/3]).
-export([paths/2]).
-export([subelement/2, subelement/3]).
-export([subelements/2]).
-export([attr/2, attr/3]).
-export([cdata/1]).

-type path() :: [cdata | {element, binary()} | {attr, binary()}].

%% @doc gets the element/attr/cdata contained in the leftmost path
-spec path(#xmlelement{} | #xmlstreamstart{}, path()) -> #xmlelement{} | binary() | undefined.
path(Element, Path) ->
    path(Element, Path, undefined).

%% @doc gets the element/attr/cdata in the leftmost possible described path
-spec path(#xmlelement{} | #xmlstreamstart{}, path(), Other) -> #xmlelement{} | binary() | Other.
path(#xmlelement{} = Element, [], _) ->
    Element;
path(#xmlelement{} = Element, [{element, Name} | Rest], Default) ->
    Child = subelement(Element, Name), % may return undefined
    path(Child, Rest, Default);
path(#xmlelement{} = Element, [cdata], _) ->
    cdata(Element);
path(#xmlelement{} = Element, [{attr, Name}], Default) ->
    attr(Element, Name, Default);
path(#xmlstreamstart{} = Element, [{attr, Name}], Default) ->
    attr(Element, Name, Default);
path(_, _, Default) ->
    Default.

%% @doc gets the elements/attrs/cdatas reachable by the described path
-spec paths(#xmlelement{}, path()) -> [#xmlelement{} | binary()].
paths(#xmlelement{} = Element, []) ->
    [Element];
paths(#xmlelement{} = Element, [{element, Name} | Rest]) ->
    Children = subelements(Element, Name),
    lists:append([paths(Child, Rest) || Child <- Children]);
paths(#xmlelement{} = Element, [cdata]) ->
    [cdata(Element)];
paths(#xmlelement{attrs = Attrs}, [{attr, Name}]) ->
    lists:sublist([V || {N, V} <- Attrs, N =:= Name], 1);
paths(#xmlelement{}, Path) when is_list(Path) ->
    [].

-spec subelement(#xmlelement{}, binary()) -> #xmlelement{} | undefined.
subelement(Element, Name) ->
    subelement(Element, Name, undefined).

-spec subelement(#xmlelement{}, binary(), Other) -> #xmlelement{} | Other.
subelement(#xmlelement{children = Children}, Name, Default) ->
    case lists:keyfind(Name, #xmlelement.name, Children) of
        false ->
            Default;
        Result ->
            Result
    end.

-spec subelements(#xmlelement{}, binary()) -> [#xmlelement{}].
subelements(#xmlelement{children = Children}, Name) ->
    lists:filter(fun(#xmlelement{name = N}) when N =:= Name ->
                        true;
                    (_) ->
                        false
                 end, Children).

-spec cdata(#xmlelement{}) -> binary().
cdata(#xmlelement{children = Children}) ->
    list_to_binary([exml:unescape_cdata(C) || #xmlcdata{} = C <- Children]).

-spec attr(#xmlelement{} | #xmlstreamstart{}, binary()) -> binary() | undefined.
attr(Element, Name) ->
    attr(Element, Name, undefined).

-spec attr(#xmlelement{} | #xmlstreamstart{} | list(), binary(), Other) -> binary() | Other.
attr(#xmlelement{attrs = Attrs}, Name, Default) ->
    attr(Attrs, Name, Default);
attr(#xmlstreamstart{attrs = Attrs}, Name, Default) ->
    attr(Attrs, Name, Default);
attr(Attrs, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            Default
    end.