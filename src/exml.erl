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

-export([parse/1]).
-export([to_list/1, to_binary/1, to_iolist/1,
         to_pretty_iolist/1, to_pretty_iolist/3]).
-export([to_list/2, to_binary/2, to_iolist/2,
         to_pretty_iolist/2, to_pretty_iolist/4]).
-export([escape_attr/1, unescape_attr/1,
         escape_cdata/1, unescape_cdata/1, unescape_cdata_as/2]).
-on_load(load/0).

%% Maximum bytes passed to the NIF handler at once
%% Current value is erlang:system_info(context_reductions) * 10
-define(MAX_BYTES_TO_NIF, 20000).

-spec load() -> any().
load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "exml_escape"), none).

-spec to_list(#xmlstreamstart{} | #xmlstreamend{}
              | xmlterm()) -> string().
to_list(Element) ->
    binary_to_list(to_binary(Element)).

-spec to_list(#xmlstreamstart{} | #xmlstreamend{}
              | xmlterm(), [escape]) -> string().
to_list(Element, Opts) ->
    binary_to_list(to_binary(Element, Opts)).

-spec to_binary(#xmlstreamstart{} | #xmlstreamend{}
                | xmlterm() | [xmlterm()]) -> binary().
to_binary(Element) ->
    list_to_binary(to_iolist(Element)).

-spec to_binary(#xmlstreamstart{} | #xmlstreamend{}
                | xmlterm() | [xmlterm()], [escape]) -> binary().
to_binary(Element, Opts) ->
    list_to_binary(to_iolist(Element, Opts)).

-spec to_iolist(#xmlstreamstart{} | #xmlstreamend{}
                | xmlterm() | [xmlterm()]) -> iolist().
to_iolist(Elements) ->
    to_iolist(Elements, []).

-spec to_iolist(#xmlstreamstart{} | #xmlstreamend{}
                | xmlterm() | [xmlterm()], [escape]) -> iolist().
to_iolist(Data, Opts) ->
    case lists:member(escape, Opts) of
        true ->
            to_iolist_escape(Data);
        false ->
            to_iolist_clean(Data)
    end.

-spec to_iolist_escape(#xmlstreamstart{} | #xmlstreamend{}
                       | xmlterm() | [xmlterm()]) -> iolist().
to_iolist_escape(Elements) when is_list(Elements) ->
    lists:map(fun to_iolist_escape/1, Elements);
to_iolist_escape(#xmlel{name = Name, attrs = Attrs, children = []}) ->
    EAttrs = [{AttrName, escape_attr(AttrVal)} ||
                 {AttrName, AttrVal} <- Attrs],
    ["<", Name, attrs_to_iolist(EAttrs), "/>"];
to_iolist_escape(#xmlel{name = Name, attrs = Attrs, children = Children}) ->
    EAttrs = [{AttrName, escape_attr(AttrVal)} ||
                 {AttrName, AttrVal} <- Attrs],
    ["<", Name, attrs_to_iolist(EAttrs), ">",
     to_iolist_escape(Children),
     "</", Name, ">"];
to_iolist_escape(#xmlstreamstart{name = Name, attrs = Attrs}) ->
    EAttrs = [{AttrName, escape_attr(AttrVal)} ||
                 {AttrName, AttrVal} <- Attrs],
    ["<", Name, attrs_to_iolist(EAttrs), ">"];
to_iolist_escape(#xmlstreamend{name = Name}) ->
    ["</", Name, ">"];
to_iolist_escape(#xmlcdata{content = Content}) ->
    [escape_cdata(Content)]. %% ensure we return io*list*

-spec to_iolist_clean(#xmlstreamstart{} | #xmlstreamend{}
                      | xmlterm() | [xmlterm()]) -> iolist().
to_iolist_clean(Elements) when is_list(Elements) ->
    lists:map(fun to_iolist_clean/1, Elements);
to_iolist_clean(#xmlel{name = Name, attrs = Attrs, children = []}) ->
    ["<", Name, attrs_to_iolist(Attrs), "/>"];
to_iolist_clean(#xmlel{name = Name, attrs = Attrs, children = Children}) ->
    ["<", Name, attrs_to_iolist(Attrs), ">",
     to_iolist_clean(Children),
     "</", Name, ">"];
to_iolist_clean(#xmlstreamstart{name = Name, attrs = Attrs}) ->
    ["<", Name, attrs_to_iolist(Attrs), ">"];
to_iolist_clean(#xmlstreamend{name = Name}) ->
    ["</", Name, ">"];
to_iolist_clean(#xmlcdata{content = Content}) ->
    [Content]. %% ensure we return io*list*

-spec to_pretty_iolist(#xmlstreamstart{} | #xmlstreamend{}
                       | xmlterm()) -> iolist().
to_pretty_iolist(Term) ->
    to_pretty_iolist(Term, 0, "  ", []).

-spec to_pretty_iolist(#xmlstreamstart{} | #xmlstreamend{} | xmlterm(),
                       non_neg_integer(), string()) -> iolist().
to_pretty_iolist(Term, Level, Indent) ->
    to_pretty_iolist(Term, Level, Indent, []).

-spec to_pretty_iolist(#xmlstreamstart{} | #xmlstreamend{}
                       | xmlterm(), [escape]) -> iolist().
to_pretty_iolist(Term, Opts) ->
    to_pretty_iolist(Term, 0, "  ", Opts).

%% `to_pretty_iolist/3' is generic enough to express `to_iolist/1'
%% by passing an empty string as `Indent', but that would be less efficient,
%% so let's leave the implementations separate.
-spec to_pretty_iolist(#xmlstreamstart{} | #xmlstreamend{} | xmlterm(),
                       non_neg_integer(), string(), [escape]) -> iolist().
to_pretty_iolist(Data, Level, Indent, Opts) ->
    case lists:member(escape, Opts) of
        true ->
            to_pretty_iolist_escape(Data, Level, Indent);
        false ->
            to_pretty_iolist_clean(Data, Level, Indent)
    end.

to_pretty_iolist_escape(#xmlel{name = Name, attrs = Attrs, children = []},
                        Level, Indent) ->
    EAttrs = [{AttrName, escape_attr(AttrVal)} ||
                 {AttrName, AttrVal} <- Attrs],
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(EAttrs), "/>\n"];
to_pretty_iolist_escape(#xmlel{name = Name, attrs = Attrs,
                               children = [#xmlcdata{content = Content}]},
                        Level, Indent) ->
    EAttrs = [{AttrName, escape_attr(AttrVal)} ||
                 {AttrName, AttrVal} <- Attrs],
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(EAttrs), ">",
     escape_cdata(Content), "</", Name, ">\n"];
to_pretty_iolist_escape(#xmlel{name = Name, attrs = Attrs, children = Children},
                        Level, Indent) ->
    EAttrs = [{AttrName, escape_attr(AttrVal)} ||
                 {AttrName, AttrVal} <- Attrs],
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(EAttrs), ">\n",
     [to_pretty_iolist_escape(C, Level+1, Indent) || C <- Children],
     Shift, "</", Name, ">\n"];
to_pretty_iolist_escape(#xmlstreamstart{name = Name, attrs = Attrs},
                        Level, Indent) ->
    EAttrs = [{AttrName, escape_attr(AttrVal)} ||
                 {AttrName, AttrVal} <- Attrs],
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(EAttrs), ">\n"];
to_pretty_iolist_escape(#xmlstreamend{name = Name}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "</", Name, ">\n"];
to_pretty_iolist_escape(#xmlcdata{content = Content}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, escape_cdata(Content), "\n"].

to_pretty_iolist_clean(#xmlel{name = Name, attrs = Attrs, children = []},
                       Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), "/>\n"];
to_pretty_iolist_clean(#xmlel{name = Name, attrs = Attrs,
                              children = [#xmlcdata{content = Content}]},
                       Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), ">",
     Content, "</", Name, ">\n"];
to_pretty_iolist_clean(#xmlel{name = Name, attrs = Attrs, children = Children},
                       Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), ">\n",
     [to_pretty_iolist_clean(C, Level+1, Indent) || C <- Children],
     Shift, "</", Name, ">\n"];
to_pretty_iolist_clean(#xmlstreamstart{name = Name, attrs = Attrs},
                       Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs), ">\n"];
to_pretty_iolist_clean(#xmlstreamend{name = Name}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "</", Name, ">\n"];
to_pretty_iolist_clean(#xmlcdata{content = Content}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, Content, "\n"].

-spec attrs_to_iolist([{binary(), binary()}]) -> iolist().
attrs_to_iolist(Attrs) ->
    [ [" ", Name, "='", Value, "'"] || {Name, Value} <- Attrs].

-spec parse(binary()) -> {ok, #xmlel{}} | {error, any()}.
parse(XML) ->
    {ok, Parser} = exml_stream:new_parser(),
    Stream = <<"<stream>", XML/binary, "</stream>">>,
    Result = case exml_stream:parse(Parser, Stream) of
                 {ok, _, [#xmlstreamstart{}, Tree, #xmlstreamend{}]} ->
                     {ok, Tree};
                 {ok, _, Other} ->
                     {error, {bad_parse, Other}};
                 {error, Error} ->
                     {error, Error}
             end,
    ok = exml_stream:free_parser(Parser),
    Result.

-spec escape_cdata(iodata()) -> #xmlcdata{}.
escape_cdata(Content) ->
    BContent = list_to_binary([Content]),
    NewContent = feed_nif(fun escape_cdata_nif/1, BContent,
                          byte_size(BContent), []),
    #xmlcdata{content = NewContent}.

-spec unescape_cdata(#xmlcdata{}) -> binary().
unescape_cdata(#xmlcdata{content = Content}) ->
    BContent = list_to_binary([Content]),
    feed_nif(fun unescape_cdata_nif/1, BContent, byte_size(BContent), []).

-spec unescape_cdata_as(binary|list|iodata, #xmlcdata{}) -> binary().
unescape_cdata_as(What, CData) ->
    unescape_cdata_as_erl(What, CData).

-spec escape_cdata_nif(iodata()) -> binary().
escape_cdata_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_cdata_nif(iodata()) -> binary().
unescape_cdata_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_cdata_as_erl(binary|list|iodata, #xmlcdata{}) -> binary().
unescape_cdata_as_erl(What, #xmlcdata{content=GtEsc}) ->
    LtEsc  = re:replace(GtEsc,  "&gt;",  ">",   [global]),
    AmpEsc = re:replace(LtEsc,  "&lt;",  "<",   [global]),
    Text   = re:replace(AmpEsc, "&amp;", "\\&", [global, {return, What}]),
    Text.

-spec escape_attr(binary()) -> binary().
escape_attr(Text) ->
    feed_nif(fun escape_attr_nif/1, Text, byte_size(Text), []).

-spec unescape_attr(binary()) -> binary().
unescape_attr(Text) ->
    feed_nif(fun unescape_attr_nif/1, Text, byte_size(Text), []).

-spec feed_nif(function(), binary(), integer(), list()) -> binary().
feed_nif(Fun, Text, Size, Acc) when Size > ?MAX_BYTES_TO_NIF ->
    <<Chunk:?MAX_BYTES_TO_NIF/binary, Rest/binary>> = Text,
    Resp = Fun(Chunk),
    feed_nif(Fun, Rest, Size - ?MAX_BYTES_TO_NIF, [Resp | Acc]);
feed_nif(Fun, Text, _Size, Acc) ->
    Resp = Fun(Text),
    list_to_binary(lists:reverse([Resp | Acc])).

-spec escape_attr_nif(binary()) -> binary().
escape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_attr_nif(binary()) -> binary().
unescape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).
