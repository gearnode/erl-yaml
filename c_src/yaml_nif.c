// Copyright (c) 2021 Nicolas Martyanoff <khaelin@gmail.com>.
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
// SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
// IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#include "yaml_nif.h"

void
eyaml_nif_data_dtor(ErlNifEnv *env, void *ptr) {
        struct eyaml_nif_data *data;

        data = ptr;
        enif_free(data);
}

static ErlNifFunc eyaml_nif_functions[] = {
        {"get_version", 0, eyaml_get_version, 0},
        {"get_version_string", 0, eyaml_get_version_string, 0},

        {"parse", 1, eyaml_parse, 0},
};

static int
eyaml_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM info) {
        struct eyaml_nif_data *data;

        data = enif_alloc(sizeof(*data));
        if(data == NULL) {
                enif_fprintf(stderr, "cannot allocate nif data: %s\n",
                             strerror(errno));
                return 1;
        }

        data->parser_resource_type =
                eyaml_create_resource_type(env, "parser", eyaml_nif_data_dtor);

        *priv = (void *)data;
        return 0;
}

ERL_NIF_INIT(yaml_nif, eyaml_nif_functions, eyaml_load, NULL, NULL, NULL);
