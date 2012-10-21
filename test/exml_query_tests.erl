%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Unit tests for exml_query module
%%% @end
%%%-------------------------------------------------------------------

-module(exml_query_tests).

-include_lib("eunit/include/eunit.hrl").
-include("exml_stream.hrl").

-compile(export_all).

-define(MY_SPOON, xml(<<"<spoon whose='my'>",
                          "<problem no='1'>is too big</problem>",
                          "<problem no='2'>is too big</problem>",
                          "<problem no='3'>is too big</problem>",
                        "</spoon>">>)).
-define (HTML, xml(<<"<html>
                          <li>
                              <ul><i>My</i> spoon is too
                                  <span class=\"size\">big</span></ul>
                              <ul>My <i>spoon</i> is too
                                  <span class=\"size\">big</span></ul>
                              <ul>My spoon <i>is</i> too
                                  <span class=\"size\">big</span></ul>
                          </li>
                      </html>">>)).

%%--------------------------------------------------------------------
%% tests
%%--------------------------------------------------------------------

element_query_test() ->
    %% we return only the first (leftmost) match
    ?assertEqual(xml(<<"<problem no='1'>is too big</problem>">>),
                 exml_query:subelement(?MY_SPOON, <<"problem">>)),
    ?assertEqual(xml(<<"<problem no='1'>is too big</problem>">>),
                 exml_query:path(?MY_SPOON, [{element, <<"problem">>}])).

elements_query_test() ->
    Exemplar = [xml(<<"<problem no='1'>is too big</problem>">>),
                xml(<<"<problem no='2'>is too big</problem>">>),
                xml(<<"<problem no='3'>is too big</problem>">>)],
    ?assertEqual(Exemplar, exml_query:subelements(?MY_SPOON, <<"problem">>)).

xmlelement_attribute_query_test() ->
    ?assertEqual(<<"my">>, exml_query:attr(?MY_SPOON, <<"whose">>)),
    ?assertEqual(<<"my">>, exml_query:path(?MY_SPOON, [{attr, <<"whose">>}])),
    ?assertEqual(undefined, exml_query:attr(?MY_SPOON, <<"banana">>)),
    ?assertEqual('IAmA', exml_query:attr(?MY_SPOON, <<"banana">>, 'IAmA')).

xmlstreamstart_attribute_query_test() ->
    XmlStreamStart = #xmlstreamstart{name = <<"stream:stream">>, attrs = [{<<"whose">>, <<"my">>}]},
    ?assertEqual(<<"my">>, exml_query:attr(XmlStreamStart, <<"whose">>)),
    ?assertEqual(<<"my">>, exml_query:path(XmlStreamStart, [{attr, <<"whose">>}])),
    ?assertEqual(undefined, exml_query:attr(XmlStreamStart, <<"banana">>)),
    ?assertEqual('IAmA', exml_query:attr(XmlStreamStart, <<"banana">>, 'IAmA')).

cdata_query_test() ->
    ?assertEqual(<<"">>, exml_query:cdata(?MY_SPOON)),
    ?assertEqual(<<"">>, exml_query:path(?MY_SPOON, [cdata])),
    IAmA = xml(<<"<i-am>a banana</i-am>">>),
    ?assertEqual(<<"a banana">>, exml_query:cdata(IAmA)),
    ?assertEqual(<<"a banana">>, exml_query:path(IAmA, [cdata])).

path_query_test() ->
    ?assertEqual(?MY_SPOON, exml_query:path(?MY_SPOON, [])),
    ?assertEqual(<<"is too big">>,
                 exml_query:path(?MY_SPOON, [{element, <<"problem">>}, cdata])),
    ?assertEqual(<<"1">>,
                 exml_query:path(?MY_SPOON, [{element, <<"problem">>},
                                             {attr, <<"no">>}])),

    %% I couldn't find anything complex enough in that silly cartoon :[
    Qux = xml(<<"<foo><bar><baz a='b'>qux</baz></bar></foo>">>),
    ?assertEqual(<<"qux">>, exml_query:path(Qux, [{element, <<"bar">>},
                                                  {element, <<"baz">>},
                                                  cdata])),
    ?assertEqual(<<"b">>, exml_query:path(Qux, [{element, <<"bar">>},
                                                {element, <<"baz">>},
                                                {attr, <<"a">>}])).

failed_path_query_test() ->
    ?assertEqual(undefined, exml_query:path(?MY_SPOON,
                                            [{element, <<"banana">>}])),
    ?assertEqual('IAmA', exml_query:path(?MY_SPOON,
                                         [{element, <<"banana">>}],
                                         'IAmA')).

paths_query_test() ->
    ?assertEqual([?MY_SPOON], exml_query:paths(?MY_SPOON, [])),
    ?assertEqual([<<"is too big">>, <<"is too big">>, <<"is too big">>],
                  exml_query:paths(?MY_SPOON, [{element, <<"problem">>},
                                               cdata])),
    ?assertEqual([<<"1">>, <<"2">>, <<"3">>],
                 exml_query:paths(?MY_SPOON, [{element, <<"problem">>},
                                              {attr, <<"no">>}])),
    ?assertEqual([], exml_query:paths(?MY_SPOON, [{element, <<"banana">>}])),
    ?assertEqual([<<"My">>, <<"spoon">>, <<"is">>],
                 exml_query:paths(?HTML, [{element, <<"li">>},
                                          {element, <<"ul">>},
                                          {element, <<"i">>},
                                          cdata])),
    ?assertEqual([<<"size">>, <<"size">>, <<"size">>],
                 exml_query:paths(?HTML, [{element, <<"li">>},
                                          {element, <<"ul">>},
                                          {element, <<"span">>},
                                          {attr, <<"class">>}])).

%%--------------------------------------------------------------------
%% helpers
%%--------------------------------------------------------------------

xml(Raw) ->
    {ok, Tree} = exml:parse(Raw),
    Tree.
