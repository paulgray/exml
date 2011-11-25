exml
====

[![Build Status](https://secure.travis-ci.org/paulgray/exml.png)](http://travis-ci.org/paulgray/exml)

**exml** is an Erlang library helpful with parsing XML streams
and doing some basic XML structures manipulation.

Building
========

**exml** is rebar-compatible OTP application, run *make* or
*./rebar compile* in order to build it.

As a requirement, development headers for expat library are
required.

Using
=====

**exml** can parse both XML streams as well as single XML
documents at once.

At first, new parser instance must be created:
<pre>
    {ok, Parser} = exml:new_parser().
</pre>

Then, one must feed parser with XML document:
<pre>
    ok = exml:parse(Parser, <<"<my_xml_doc/>">>, 1).
</pre>

