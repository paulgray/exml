#ifndef EXML_EVENT_H
#define EXML_EVENT_H

#include <erl_nif.h>
#include <expat.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

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
