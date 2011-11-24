%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Example of XMPP parser using exml library
%%%
%%% @end
%%% Created : 21 Jul 2011 by Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%%-------------------------------------------------------------------
-module(exml_xmpp).

-behaviour(gen_server).

-include("exml_xmpp.hrl").

%% API
-export([start_link/0, stop/1]).
-export([parse/2, reset_stream/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {parser :: term(),
                stack = [] :: list()}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec parse(pid(), binary()) -> {ok, list(xmlterm())} | {error, string()}.
parse(Parser, Bin) ->
    gen_server:call(Parser, {parse, Bin}).

-spec reset_stream(pid()) -> ok.
reset_stream(Parser) ->
    gen_server:cast(Parser, reset_stream).


init([]) ->
    {ok, Parser} = exml:new_parser(),

    {ok, #state{parser = Parser}}.


handle_call({parse, Bin}, _From, State) ->
    {NewState, Reply} = case exml:parse(State#state.parser, Bin, false) of
                            {ok, Events} ->
                                {Elements, Stack} = parse_events(Events, State#state.stack, []),
                                {State#state{stack = Stack}, Elements};

                            {error, Reason} ->
                                {State, {error, Reason}}
                        end,

    {reply, Reply, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.



handle_cast(reset_stream, State) ->
    exml:reset_parser(State#state.parser),
    {noreply, State#state{stack = []}}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    catch exml:free_parser(State#state.parser).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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
    NewParent = Parent#xmlelement{body = [Element | Parent#xmlelement.body]},
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
