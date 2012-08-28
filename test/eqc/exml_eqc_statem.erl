%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc QuickCheck properties for exml
%%%
%%% @end
%%% Created :  1 Mar 2012 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_eqc_statem).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("../../include/exml.hrl").

-compile(export_all).

-record(state, {chops :: list(binary()),
                events :: list(),
                events_history = [] :: list(),
                parser = {var, parser}}).

command(S) ->
    oneof([{call, ?MODULE, parse, [S#state.parser, hd(S#state.chops)]} ||
              S#state.chops /= []] ++
              [{call, ?MODULE, dummy, [S#state.parser, <<>>]} ||
                  S#state.chops == []]).

next_state(S, V, {call, ?MODULE, dummy, _}) ->
    S;
next_state(S, V, {call, ?MODULE, parse, _}) ->
    S#state{events_history = {call, lists, append,
                              [S#state.events_history, V]},
            chops = tl(S#state.chops)}.

precondition(S, {call, _, _, _}) ->
    true.

postcondition(S, {call, _, _, _}, R) ->
    true.

binary_range(Gen) ->
    ?LET(Bytes, list(Gen), list_to_binary(Bytes)).

chop(Bin) ->
    ?LET(L, [{N, frequency([{1, false}, {1, bool()}])} ||
                N <- lists:seq(0, size(Bin))],
         chop(Bin, 0, [N || {N, true} <- L])).

chop(Bin,_,[]) ->
    [Bin];
chop(Bin,Len,[To|Rest]) ->
    Bits = 8*(To-Len),
    <<Take:Bits,BinRest/binary>>  = Bin,
    [ <<Take:Bits>> | chop(BinRest,To,Rest)].

ascii_text() ->
    non_empty(list(choose($a, $z))).

xml_attr() ->
    ?LET({Key, Val}, {ascii_text(), ascii_text()},
         {list_to_binary(Key), list_to_binary(Val)}).

xml_element_event() ->
    ?SIZED(Size, ?LET({Name, Attrs, {Body, BodyEvents}},
                      {ascii_text(), list(xml_attr()), xml_body(Size)},
                      {#xmlelement{name = list_to_binary(Name),
                                   attrs = lists:ukeysort(1, Attrs),
                                   body = Body},
                       lists:flatten([{xml_element_start, list_to_binary(Name), Attrs}, BodyEvents,
                                      {xml_element_end, list_to_binary(Name)}])})).

xml_body(0) ->
    {[], []};
xml_body(Size) ->
    oneof([xml_body(0),
           ?LAZY(?LET(Body, list(xml_child(Size div 3)),
                      begin
                          {El, Ev} = lists:foldl(fun({Els, Evs}, {AEls, AEvs}) ->
                                                         {[Els | AEls], [Evs | AEvs]}
                                                 end, {[], []}, Body),
                          {lists:reverse(El), lists:reverse(Ev)}
                      end))]).

xml_child(Size) ->
    ?LET({Bin, Attrs, {Body, BodyEvents}},
         {ascii_text(), list(xml_attr()), xml_body(Size)},
         oneof([{#xmlcdata{content = list_to_binary(Bin)},
                 [{xml_cdata, list_to_binary(Bin)}]},
                {#xmlelement{name = list_to_binary(Bin),
                             attrs = lists:ukeysort(1, Attrs),
                             body = Body},
                 [{xml_element_start, list_to_binary(Bin), lists:ukeysort(1, Attrs)},
                  BodyEvents,
                  {xml_element_end, list_to_binary(Bin)}]}])).


prop_parse_chopified() ->
    ?FORALL({Doc, Events}, xml_element_event(),
            ?FORALL(Choppings, chop(exml:to_binary(Doc)),
                    ?FORALL(Cmds, commands(?MODULE, #state{chops = Choppings,
                                                           events = Events}),
                            collect(Cmds, begin
                                              {ok, Parser} = exml_event:new_parser(),
                                              {H, S, Res} = run_commands(?MODULE, Cmds, [{parser, Parser}]),
                                              exml_event:free_parser(S#state.parser),

                                              ?WHENFAIL(io:format("H: ~p~nS: ~p~nRes: ~p~n",
                                                                  [H, S, Res]),
                                                        Res == ok andalso
                                                        prefix(
                                                          unify(S#state.events_history),
                                                          unify(S#state.events)))
                                          end)))).

encode_decode_test_() ->
    {timeout, 30000, fun parse_chopified/0}.

parse_chopified() ->
    ?assert(eqc:quickcheck(eqc:numtests(500, prop_parse_chopified()))).

parse(Parser, Chop) ->
    {ok, Events} = exml_event:parse(Parser, Chop),
    Events.

dummy(_, _) ->
    ok.

unify([{xml_element_start, Name, Attrs} | Rest]) ->
    [{xml_element_start, Name, lists:sort(Attrs)} | unify(Rest)];
unify([{xml_cdata, CData1}, {xml_cdata, CData2} | Rest]) ->
    unify([{xml_cdata, <<CData1/binary, CData2/binary>>} | Rest]);
unify([Element | Rest]) ->
    [Element | unify(Rest)];
unify([]) ->
    [].

prefix([Element | Rest1], [Element | Rest2]) ->
    prefix(Rest1, Rest2);
prefix([], _) ->
    true;
prefix([{xml_cdata, CData1}], [{xml_cdata, CData2} | _]) ->
    Size = size(CData1),
    Size == binary:longest_common_prefix([CData1, CData2]);
prefix(_, _) ->
    false.

-endif.
