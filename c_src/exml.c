#include "exml.h"

static ErlNifResourceType *PARSER_POINTER = NULL;

void* start_element_handler(XML_Parser parser, const XML_Char *name, const XML_Char **atts)
{
    fprintf(stderr, "parsing start_element_handler %s\n", name);
    return NULL;
};

void* end_element_handler(XML_Parser parser, const XML_Char *name)
{
    fprintf(stderr, "parsing end_element_handler %s\n", name);
    return NULL;
};

void* character_data_handler(XML_Parser parser, const XML_Char *s, int len)
{
    fprintf(stderr, "parsing character_data_handler %s\n", s);
    return NULL;
};

void* start_ns_decl_handler(XML_Parser parser, const XML_Char *prefix, const XML_Char *uri)
{
    fprintf(stderr, "parsing start_ns_decl_handler %s %s\n", prefix, uri);
    return NULL;
};

ERL_NIF_TERM new_parser(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser parser = XML_ParserCreate_MM("UTF-8", &ms, "\n");
    XML_SetUserData(parser, parser);

    XML_SetStartElementHandler(parser, (XML_StartElementHandler)start_element_handler);
    XML_SetEndElementHandler(parser, (XML_EndElementHandler)end_element_handler);
    XML_SetCharacterDataHandler(parser, (XML_CharacterDataHandler)character_data_handler);

    XML_SetStartNamespaceDeclHandler(parser, (XML_StartNamespaceDeclHandler)start_ns_decl_handler);
    XML_SetReturnNSTriplet(parser, 1);

    XML_SetDefaultHandler(parser, NULL);

    XML_Parser *xml_parser = (XML_Parser *)enif_alloc_resource(PARSER_POINTER, sizeof(XML_Parser));
    *xml_parser = parser;

    return enif_make_tuple(env, 2, enif_make_atom(env, "ok"),
                           enif_make_resource(env, (void *)xml_parser));
};

ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser **parser;
    int is_final, res;
    ErlNifBinary stream;

    assert(argc == 3);

    if (!enif_get_resource(env, argv[0], PARSER_POINTER, (void **)&parser))
        return enif_make_badarg(env);

    if (!enif_is_binary(env, argv[1]))
        return enif_make_badarg(env);

    enif_get_int(env, argv[2], &is_final);
    enif_inspect_binary(env, argv[1], &stream);

    fprintf(stderr, "Parser: %p, stream: %s\n", *parser, stream.data);
    res = XML_Parse(*parser, (const char *)stream.data, stream.size, is_final);

    if(!res)
        {
            //            errcode = XML_GetErrorCode(d->parser);
            //            errstring = (char *)XML_ErrorString(errcode);
        }

    return enif_make_atom(env, "ok");
};

static int load(ErlNifEnv* env, void **priv, ERL_NIF_TERM info)
{
    PARSER_POINTER = enif_open_resource_type(env, "exml", "parser_pointer", NULL,
                                             ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                                             NULL);
    return 0;
};

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    *priv = *old_priv;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv)
{
}

static ErlNifFunc funcs[] =
    {
        {"new_parser", 0, new_parser},
        {"parse", 3, parse}
    };

ERL_NIF_INIT(exml, funcs, &load, &reload, &upgrade, &unload);
