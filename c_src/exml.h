#ifndef EXML_H
#define EXML_H

#include <erl_nif.h>
#include <expat.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

static XML_Memory_Handling_Suite ms =
    {enif_alloc, enif_realloc, enif_free};

typedef struct
{
    ErlNifEnv *env;
    ERL_NIF_TERM result;
} expat_parser;

ERL_NIF_TERM parse(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif
