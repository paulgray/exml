#include "exml.h"

static ErlNifResourceType *PARSER_POINTER = NULL;

void *start_element_handler(expat_parser *parser_data, const XML_Char *name, const XML_Char **atts)
{
    ErlNifBinary element_name;
    ERL_NIF_TERM attrs_list = enif_make_list(parser_data->env, 0);
    int i;

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
    parser_data->result = enif_make_list_cell(parser_data->env, event, parser_data->result);

    return NULL;
};

void *end_element_handler(expat_parser *parser_data, const XML_Char *name)
{
    ErlNifBinary element_name;

    enif_alloc_binary(strlen(name), &element_name);
    strcpy((char *) element_name.data, (const char *)name);

    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 2,
                                         enif_make_atom(parser_data->env, "xml_element_end"),
                                         enif_make_binary(parser_data->env, &element_name));
    parser_data->result = enif_make_list_cell(parser_data->env, event, parser_data->result);

    return NULL;
};

void *character_data_handler(expat_parser *parser_data, const XML_Char *s, int len)
{
    ErlNifBinary cdata;

    enif_alloc_binary(len, &cdata);
    strcpy((char *) cdata.data, (const char *)s);

    ERL_NIF_TERM event = enif_make_tuple(parser_data->env, 2,
                                         enif_make_atom(parser_data->env, "xml_cdata"),
                                         enif_make_binary(parser_data->env, &cdata));
    parser_data->result = enif_make_list_cell(parser_data->env, event, parser_data->result);

    return NULL;
};

ERL_NIF_TERM new_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser parser;
    expat_parser *parser_data = (expat_parser *)enif_alloc(sizeof(expat_parser));
    ERL_NIF_TERM parser_resource;

    parser = XML_ParserCreate_MM("UTF-8", &ms, "\n");
    parser_data->env = env;

    XML_SetUserData(parser, parser_data);

    XML_SetStartElementHandler(parser, (XML_StartElementHandler)start_element_handler);
    XML_SetEndElementHandler(parser, (XML_EndElementHandler)end_element_handler);
    XML_SetCharacterDataHandler(parser, (XML_CharacterDataHandler)character_data_handler);

    XML_SetReturnNSTriplet(parser, 1);
    XML_SetDefaultHandler(parser, NULL);

    XML_Parser *xml_parser = (XML_Parser *)enif_alloc_resource(PARSER_POINTER, sizeof(XML_Parser));
    *xml_parser = parser;
    parser_resource = enif_make_resource(env, (void *)xml_parser);
    enif_release_resource(xml_parser);

    return enif_make_tuple(env, 2, enif_make_atom(env, "ok"), parser_resource);
};

ERL_NIF_TERM free_parser(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    XML_Parser **parser;

    assert(argc == 1);

    if (!enif_get_resource(env, argv[0], PARSER_POINTER, (void **)&parser))
        return enif_make_badarg(env);

    expat_parser *parser_data = XML_GetUserData((XML_Parser)(*parser));
    enif_free(parser_data);

    return enif_make_atom(env, "ok");
};

ERL_NIF_TERM parse(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
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

    expat_parser *parser_data = XML_GetUserData((XML_Parser)(*parser));
    parser_data->result = enif_make_list(env, 0);
    XML_SetUserData((XML_Parser)(*parser), parser_data);

    res = XML_Parse((XML_Parser)(*parser), (const char *)stream.data, stream.size, is_final);
    if(!res)
        {
            errcode = XML_GetErrorCode((XML_Parser)(*parser));
            errstring = (char *)XML_ErrorString(errcode);

            return enif_make_tuple(env, 2, enif_make_atom(env, "error"),
                                   enif_make_string(env, errstring, ERL_NIF_LATIN1));
        }

    return enif_make_tuple(env, 2,
                           enif_make_atom(env, "ok"),
                           parser_data->result);
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
        {"free_parser", 1, free_parser},
        {"parse_nif", 3, parse}
    };

ERL_NIF_INIT(exml, funcs, &load, &reload, &upgrade, &unload);
