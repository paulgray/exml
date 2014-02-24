#ifndef EXML_H
#define EXML_H

#include <erl_nif.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <expat.h>

#define EXML_CDATA_BUF_SIZE 1024
#define EXML_ATTR_BUF_SIZE 64

// functions 'exported' by exml_escape.c module
ERL_NIF_TERM exml_escape_attr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM exml_unescape_attr(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM exml_escape_cdata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM exml_unescape_cdata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// structure used as a private data by expat parser
typedef struct
{
    ErlNifEnv *env;
    ERL_NIF_TERM result;
    ERL_NIF_TERM xmlns;
} expat_parser;

// functions 'exported' by exml_event.c module
ERL_NIF_TERM exml_new_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM exml_reset_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM exml_free_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM exml_parse(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// functions 'exported' by exml_utils.c module
void consume_timeslice(ErlNifEnv *env, ErlNifBinary bin);

#endif
