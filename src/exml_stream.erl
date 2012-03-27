%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc XML stream parser
%%%
%%% @end
%%% Created : 21 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_stream).

-include("exml_stream.hrl").
-export([new_parser/0, parse/2, reset_parser/1, free_parser/1]).

-record(parser, {
        event_parser,
        stack = []
}).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec new_parser() -> {ok, #parser{}} | {error, any()}.
new_parser() ->
    case exml_event:new_parser() of
        {ok, EventParser} ->
            {ok, #parser{event_parser=EventParser}};
        {error, Error} ->
            {error, Error}
    end.

-spec parse(#parser{}, binary()) ->
        {ok, #parser{}, [xmlstreamelement()]} | {error, {string(), binary()}}.
parse(#parser{event_parser = EventParser, stack = OldStack} = Parser, Input) ->
    case exml_event:parse(EventParser, Input) of
        {ok, Events} ->
            {Elements, NewStack} = parse_events(Events, OldStack, []),
            {ok, Parser#parser{stack=NewStack}, Elements};
        {error, Msg} ->
            {error, {Msg, Input}}
    end.

-spec reset_parser(#parser{}) -> {ok, #parser{}} | {error, any()}.
reset_parser(#parser{event_parser=EventParser}) ->
    case exml_event:reset_parser(EventParser) of
        ok ->
            %% drop all the state except event_parser
            {ok, #parser{event_parser=EventParser}};
        Error ->
            {error, Error}
    end.

-spec free_parser(#parser{}) -> ok | {error, any()}.
free_parser(#parser{event_parser = EventParser}) ->
    exml_event:free_parser(EventParser).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec parse_events(list(), list(), list()) -> {list(xmlstreamelement()), list()}.
parse_events([], Stack, Acc) ->
    {lists:reverse(Acc), Stack};
parse_events([{xml_element_start, Name, Attrs} | Rest], [], Acc) ->
    NewAttrs = Attrs,
    parse_events(Rest, [#xmlelement{name = Name, attrs = NewAttrs}],
                 [#xmlstreamstart{name = Name, attrs = NewAttrs} | Acc]);
parse_events([{xml_element_start, Name, Attrs} | Rest], Stack, Acc) ->
    NewAttrs = Attrs,
    parse_events(Rest, [#xmlelement{name = Name, attrs = NewAttrs} | Stack], Acc);
parse_events([{xml_element_end, Name} | Rest], [#xmlelement{name = Name}], Acc) ->
    parse_events(Rest, [], [#xmlstreamend{name = Name} | Acc]);
parse_events([{xml_element_end, Name} | Rest], [#xmlelement{name = Name} = Element, Top], Acc) ->
    parse_events(Rest, [Top], [xml_element(Element) | Acc]);
parse_events([{xml_element_end, _Name} | Rest], [Element, Parent | Stack], Acc) ->
    NewElement = Element#xmlelement{body = lists:reverse(Element#xmlelement.body)},
    NewParent = Parent#xmlelement{body = [NewElement | Parent#xmlelement.body]},
    parse_events(Rest, [NewParent | Stack], Acc);
parse_events([{xml_cdata, _CData} | Rest], [Top], Acc) ->
    parse_events(Rest, [Top], Acc);
parse_events([{xml_cdata, CData} | Rest], [#xmlelement{body = [#xmlcdata{content = Content} | RestBody]} = XML | Stack], Acc) ->
    NewBody = [#xmlcdata{content = list_to_binary([Content, CData])} | RestBody],
    parse_events(Rest, [XML#xmlelement{body = NewBody} | Stack], Acc);
parse_events([{xml_cdata, CData} | Rest], [Element | Stack], Acc) ->
    NewBody = [#xmlcdata{content = CData} | Element#xmlelement.body],
    parse_events(Rest, [Element#xmlelement{body = NewBody} | Stack], Acc).

-spec xml_element(#xmlelement{}) -> #xmlelement{}.
xml_element(#xmlelement{body = Body} = Element) ->
    Element#xmlelement{body = xml_body(Body, [])}.

-spec xml_body(list(xmlterm()), list(xmlterm())) -> list(xmlterm()).
xml_body([], Body) ->
    Body;
xml_body([#xmlcdata{content = Content1}, #xmlcdata{content = Content2} | Rest], Body) ->
    xml_body([#xmlcdata{content = list_to_binary([Content2, Content1])} | Rest], Body);
xml_body([Element | Rest], Body) ->
    xml_body(Rest, [Element | Body]).
