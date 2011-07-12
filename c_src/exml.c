#include "exml.h"

void* start_element_handler(XML_Parser parser, const XML_Char *name, const XML_Char **atts)
{
    return NULL;
};

void* end_element_handler(XML_Parser parser, const XML_Char *name)
{
    return NULL;
};

void* character_data_handler(XML_Parser parser, const XML_Char *s, int len)
{
    return NULL;
};

void* start_ns_decl_handler(XML_Parser parser, const XML_Char *prefix, const XML_Char *uri)
{
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

    return enif_make_resource(env, parser);
};

ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 4);

    XML_Parser parser = (XML_Parser)argv[0];
    int size = (int)argv[1];
    char *stream = (char *)enif_alloc(size);
    int final = (int)argv[3];

    enif_get_string(env, argv[2], stream, size, ERL_NIF_LATIN1);

    XML_Parse(parser, stream, size, final);
    return enif_make_atom(env, "ok");
};

static int load(ErlNifEnv* env, void **priv, ERL_NIF_TERM info)
{
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
        {"parse", 4, parse}
    };

ERL_NIF_INIT(exml, funcs, &load, &reload, &upgrade, &unload);
