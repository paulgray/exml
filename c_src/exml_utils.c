#include "exml_event.h"

/* This should correspond to the similar define in exml_event.erl */
/* Current value is: erlang:system_info(context_reductions) * 10 */
#define MAX_BYTES_TO_NIF 20000

void consume_timeslice(ErlNifEnv *env, ErlNifBinary bin) {
    int cost = (bin.size * 100) / MAX_BYTES_TO_NIF;
    if(cost)
        enif_consume_timeslice(env, cost > 100 ? 100 : cost);
};
