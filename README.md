exml
====

[![Build Status](https://secure.travis-ci.org/paulgray/exml.png)](http://travis-ci.org/paulgray/exml)

**exml** is an Erlang library helpful with parsing XML streams
and doing some basic XML structures manipulation.

Building
========

**exml** is a rebar-compatible OTP application, run `make` or
`./rebar compile` in order to build it.

As a requirement, development headers for expat library are
required.

Using
=====

**exml** can parse both XML streams as well as single XML
documents at once.

To parse a whole XML Document:

```erlang
{ok, Parser} = exml:parse(<<"<my_xml_doc/>">>).
```

To generate an XML document from Erlang terms:

```erlang
Xml = {xmlel,<<"foo">>,
        [{<<"attr1">>,<<"bar">>}], % Attributes
        [{xmlcdata,"Some Value"}] %Elements
      }.
exml:to_list(Xml).
```

Which results in
```
<foo attr1='bar'>Some Value</foo>
```

```exml:to_binary/1``` works similarly.

For an example of using the streaming API, so test/exml_stream_tests.erl