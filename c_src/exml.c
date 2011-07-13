#include "exml.h"

static ErlNifResourceType *PARSER_POINTER = NULL;

void *start_element_handler(expat_parser *parser_data, const XML_Char *name, const XML_Char **atts)
{
    ErlNifPid pid;
    ErlNifBinary element_name;
    ErlNifEnv* msg_env = enif_alloc_env();
    ERL_NIF_TERM attrs_list = enif_make_list(parser_data->env, 0);
    int i;

    enif_self(parser_data->env, &pid);

    enif_alloc_binary(strlen(name), &element_name);
    strcpy((char *) element_name.data, (const char *)name);

    for(i = 0; atts[i]; i += 2);
    while(i)
        {
            ErlNifBinary attr_name, attr_value;

            enif_alloc_binary(strlen(atts[i-1]), &attr_value);
            enif_alloc_binary(strlen(atts[i-2]), &attr_name);
            strcpy((char *) attr_value.data, (const char *)atts[i-1]);
            strcpy((char *) attr_name.data, (const char *)atts[i-2]);

            ERL_NIF_TERM attr = enif_make_tuple(parser_data->env, 2,
                                                enif_make_binary(parser_data->env, &attr_name),
                                                enif_make_binary(parser_data->env, &attr_value));
            attrs_list = enif_make_list_cell(parser_data->env, attr, attrs_list);

            i -= 2;
        };

    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 3,
                                         enif_make_atom(parser_data->env, "xml_element_start"),
                                         enif_make_binary(parser_data->env, &element_name),
                                         attrs_list);
    enif_send(parser_data->env, &pid, parser_data->env, event);
    enif_free_env(msg_env);

    return NULL;
};

void *end_element_handler(expat_parser *parser_data, const XML_Char *name)
{
    ErlNifPid pid;
    ErlNifBinary element_name;
    ErlNifEnv* msg_env = enif_alloc_env();

    enif_self(parser_data->env, &pid);

    enif_alloc_binary(strlen(name), &element_name);
    strcpy((char *) element_name.data, (const char *)name);

    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 2,
                                         enif_make_atom(parser_data->env, "xml_element_end"),
                                         enif_make_binary(parser_data->env, &element_name));
    enif_send(parser_data->env, &pid, parser_data->env, event);
    enif_free_env(msg_env);

    return NULL;
};

void *character_data_handler(expat_parser *parser_data, const XML_Char *s, int len)
{
    ErlNifPid pid;
    ErlNifBinary cdata;
    ErlNifEnv* msg_env = enif_alloc_env();

    enif_self(parser_data->env, &pid);

    enif_alloc_binary(len, &cdata);
    strcpy((char *) cdata.data, (const char *)s);

    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 2,
                                         enif_make_atom(parser_data->env, "xml_cdata"),
                                         enif_make_binary(parser_data->env, &cdata));
    enif_send(parser_data->env, &pid, parser_data->env, event);
    enif_free_env(msg_env);

    return NULL;
};

void* start_ns_decl_handler(XML_Parser parser, const XML_Char *prefix, const XML_Char *uri)
{
    fprintf(stderr, "parsing start_ns_decl_handler %s %s\n", prefix, uri);
    return NULL;
};

ERL_NIF_TERM new_parser(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser parser;
    expat_parser *parser_data = (expat_parser *)enif_alloc(sizeof(expat_parser));

    parser = XML_ParserCreate_MM("UTF-8", &ms, "\n");
    parser_data->env = env;

    XML_SetUserData(parser, parser_data);

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
    int is_final, res, errcode;
    ErlNifBinary stream;
    char *errstring;

    assert(argc == 3);

    if (!enif_get_resource(env, argv[0], PARSER_POINTER, (void **)&parser))
        return enif_make_badarg(env);

    if (!enif_is_binary(env, argv[1]))
        return enif_make_badarg(env);

    enif_get_int(env, argv[2], &is_final);
    enif_inspect_binary(env, argv[1], &stream);

    res = XML_Parse((XML_Parser)(*parser), (const char *)stream.data, stream.size, is_final);
    if(!res)
        {
            errcode = XML_GetErrorCode((XML_Parser)(*parser));
            errstring = (char *)XML_ErrorString(errcode);

            return enif_make_tuple(env, 2, enif_make_atom(env, "error"),
                                   enif_make_string(env, errstring, ERL_NIF_LATIN1));
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
