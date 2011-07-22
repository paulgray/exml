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
-export([parse/2]).

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

-spec parse(pid(), binary()) -> {ok, list(xml_term())} | {error, string()}.
parse(Parser, Bin) ->
    gen_server:call(Parser, {parse, Bin}).


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



handle_cast(_Cast, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


-spec parse_events(list(), list(), list()) -> {list(xmpp_term()), list()}.
parse_events([], Stack, Acc) ->
    {lists:reverse(Acc), Stack};
parse_events([{xml_element_start, Name, Attrs} | Rest], [], Acc) ->
    NewAttrs = lists:map(fun xml_attribute/1, Attrs),
    parse_events(Rest, [#xmlElement{name = Name, attrs = NewAttrs}],
                 [#xmlStreamStart{name = Name, attrs = NewAttrs} | Acc]);
parse_events([{xml_element_start, Name, Attrs} | Rest], Stack, Acc) ->
    NewAttrs = lists:map(fun xml_attribute/1, Attrs),
    parse_events(Rest, [#xmlElement{name = Name, attrs = NewAttrs} | Stack], Acc);
parse_events([{xml_element_end, Name} | Rest], [#xmlElement{name = Name}], Acc) ->
    parse_events(Rest, [], [#xmlStreamEnd{name = Name} | Acc]);
parse_events([{xml_element_end, Name} | Rest], [#xmlElement{name = Name} = Element, Top], Acc) ->
    parse_events(Rest, [Top], [xml_element(Element) | Acc]);
parse_events([{xml_element_end, _Name} | Rest], [Element, Parent | Stack], Acc) ->
    NewParent = Parent#xmlElement{body = [Element | Parent#xmlElement.body]},
    parse_events(Rest, [NewParent | Stack], Acc);
parse_events([{xml_cdata, _CData} | Rest], [Top], Acc) ->
    parse_events(Rest, [Top], Acc);
parse_events([{xml_cdata, CData} | Rest], [#xmlElement{body = [#xmlCData{content = Content} | RestBody]} = XML | Stack], Acc) ->
    NewBody = [#xmlCData{content = list_to_binary([Content, CData])} | RestBody],
    parse_events(Rest, [XML#xmlElement{body = NewBody} | Stack], Acc);
parse_events([{xml_cdata, CData} | Rest], [Element | Stack], Acc) ->
    NewBody = [#xmlCData{content = CData} | Element#xmlElement.body],
    parse_events(Rest, [Element#xmlElement{body = NewBody} | Stack], Acc).


-spec xml_attribute({binary(), binary()}) -> #xmlAttribute{}.
xml_attribute({Name, Value}) ->
    #xmlAttribute{name = Name,
                  value = Value}.

-spec xml_element(#xmlElement{}) -> #xmlElement{}.
xml_element(#xmlElement{body = Body} = Element) ->
    Element#xmlElement{body = xml_body(Body, [])}.

-spec xml_body(list(xml_term()), list(xml_term())) -> list(xml_term()).
xml_body([], Body) ->
    Body;
xml_body([#xmlCData{content = Content1}, #xmlCData{content = Content2} | Rest], Body) ->
    xml_body([#xmlCData{content = list_to_binary([Content2, Content1])} | Rest], Body);
xml_body([Element | Rest], Body) ->
    xml_body(Rest, [Element | Body]).
