%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc QuickCheck properties for exml
%%%
%%% @end
%%% Created :  1 Mar 2012 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_eqc_tests).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml.hrl").

-compile(export_all).

ascii_text() ->
    non_empty(list(choose($a, $z))).

xml_attr() ->
    ?LET({Key, Val}, {ascii_text(), ascii_text()},
         {list_to_binary(Key), list_to_binary(Val)}).

xml_element() ->
    ?SIZED(Size, ?LET({Name, Attrs}, {ascii_text(), list(xml_attr())},
                      #xmlelement{name = list_to_binary(Name),
                                  attrs = lists:ukeysort(1, Attrs),
                                  children = xml_children(Size)})).

xml_children(0) ->
    [];
xml_children(Size) ->
    oneof([xml_children(0),
           list(xml_child(Size div 3))]).

xml_child(Size) ->
    ?LET({Bin, Attrs}, {ascii_text(), list(xml_attr())},
         oneof([#xmlcdata{content = list_to_binary(Bin)},
                #xmlelement{name = list_to_binary(Bin),
                            attrs = lists:ukeysort(1, Attrs),
                            children = xml_children(Size)}])).

prop_encode_decode() ->
    ?FORALL(Doc, xml_element(),
            begin
                {ok, Result} = exml:parse(exml:to_binary(Doc)),
                unify(merge_cdata(Result)) == unify(merge_cdata(Doc))
            end).

encode_decode_test_() ->
    {timeout, 30000, fun encode_decode/0}.

encode_decode() ->
    ?assert(eqc:quickcheck(eqc:numtests(500, prop_encode_decode()))).

unify(#xmlelement{attrs = Attrs, children = Children} = Element) ->
    Element#xmlelement{attrs = lists:sort(Attrs),
                       children = lists:map(fun unify/1, Children)};
unify(#xmlcdata{} = CData) ->
    CData.

merge_cdata(#xmlelement{children = Children} = Element) ->
    Element#xmlelement{children = merge_cdata(Children, [])}.

merge_cdata([#xmlcdata{content = C1}, #xmlcdata{content = C2} | Rest], Acc) ->
    merge_cdata([#xmlcdata{content = <<C1/binary, C2/binary>>} | Rest], Acc);
merge_cdata([#xmlelement{children = Children} = Element | Rest], Acc) ->
    merge_cdata(Rest, [Element#xmlelement{children = merge_cdata(Children, [])} | Acc]);
merge_cdata([Else | Rest], Acc) ->
    merge_cdata(Rest, [Else | Acc]);
merge_cdata([], Acc) ->
    lists:reverse(Acc).

-endif.
