#include "exml.h"

ERL_NIF_TERM parse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
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
        {"parse", 1, parse}
    };

ERL_NIF_INIT(exml, funcs, &load, &reload, &upgrade, &unload);
