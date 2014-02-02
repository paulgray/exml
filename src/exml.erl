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
-export([escape_attr/1, unescape_attr/1,
         escape_cdata/1, unescape_cdata/1, unescape_cdata_as/2]).
-on_load(load/0).

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

-spec to_binary(#xmlstreamstart{} | #xmlstreamend{}
                | xmlterm() | [xmlterm()]) -> binary().
to_binary(Element) ->
    list_to_binary(to_iolist(Element)).

-spec to_iolist(#xmlstreamstart{} | #xmlstreamend{}
                | xmlterm() | [xmlterm()]) -> iolist().
to_iolist(Elements) when is_list(Elements) ->
    lists:map(fun to_iolist/1, Elements);
to_iolist(#xmlel{name = Name, attrs = Attrs, children = []}) ->
    ["<", Name, attrs_to_iolist(Attrs, []), "/>"];
to_iolist(#xmlel{name = Name, attrs = Attrs, children = Children}) ->
    ["<", Name, attrs_to_iolist(Attrs, []), ">",
     to_iolist(Children),
     "</", Name, ">"];
to_iolist(#xmlstreamstart{name = Name, attrs = Attrs}) ->
    ["<", Name, attrs_to_iolist(Attrs, []), ">"];
to_iolist(#xmlstreamend{name = Name}) ->
    ["</", Name, ">"];
to_iolist(#xmlcdata{content = Content}) ->
    %% it's caller's responsibility to make sure that
    %% #xmlcdata's content is escaped properly!
    [Content]. %% ensure we return io*list*

-spec to_pretty_iolist(#xmlstreamstart{} | #xmlstreamend{}
                       | xmlterm()) -> iolist().
to_pretty_iolist(Term) ->
    to_pretty_iolist(Term, 0, "  ").

%% `to_pretty_iolist/3' is generic enough to express `to_iolist/1'
%% by passing an empty string as `Indent', but that would be less efficient,
%% so let's leave the implementations separate.
-spec to_pretty_iolist(#xmlstreamstart{} | #xmlstreamend{} | xmlterm(),
                       non_neg_integer(), string()) -> iolist().
to_pretty_iolist(#xmlel{name = Name, attrs = Attrs, children = []},
                 Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs, []), "/>\n"];
to_pretty_iolist(#xmlel{name = Name, attrs = Attrs,
                        children = [#xmlcdata{content = Content}]},
                 Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs, []), ">",
     Content, "</", Name, ">\n"];
to_pretty_iolist(#xmlel{name = Name, attrs = Attrs, children = Children},
                 Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs, []), ">\n",
     [to_pretty_iolist(C, Level+1, Indent) || C <- Children],
     Shift, "</", Name, ">\n"];
to_pretty_iolist(#xmlstreamstart{name = Name, attrs = Attrs}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "<", Name, attrs_to_iolist(Attrs, []), ">\n"];
to_pretty_iolist(#xmlstreamend{name = Name}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, "</", Name, ">\n"];
to_pretty_iolist(#xmlcdata{content = Content}, Level, Indent) ->
    Shift = lists:duplicate(Level, Indent),
    [Shift, Content, "\n"].

-spec attrs_to_iolist([{binary(), binary()}], iolist()) -> iolist().
attrs_to_iolist([], Acc) ->
    Acc;
attrs_to_iolist([{Name, Value} | Rest], Acc) ->
    attrs_to_iolist(Rest, [" ", Name, "='", Value, "'" | Acc]).

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
    #xmlcdata{content = escape_cdata_nif(Content)}.

-spec unescape_cdata(#xmlcdata{}) -> binary().
unescape_cdata(#xmlcdata{content = Content}) ->
    unescape_cdata_nif(Content).

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
    escape_attr_nif(Text).

-spec unescape_attr(binary()) -> binary().
unescape_attr(Text) ->
    unescape_attr_nif(Text).

-spec escape_attr_nif(binary()) -> binary().
escape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_attr_nif(binary()) -> binary().
unescape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

