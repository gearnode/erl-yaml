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

        data->atom_alias = enif_make_atom(env, "alias");
        data->atom_anchor = enif_make_atom(env, "anchor");
        data->atom_any = enif_make_atom(env, "any");
        data->atom_block = enif_make_atom(env, "block");
        data->atom_data = enif_make_atom(env, "data");
        data->atom_document_end = enif_make_atom(env, "document_end");
        data->atom_document_start = enif_make_atom(env, "document_start");
        data->atom_double_quoted = enif_make_atom(env, "double_quoted");
        data->atom_end = enif_make_atom(env, "end");
        data->atom_flow = enif_make_atom(env, "flow");
        data->atom_folded = enif_make_atom(env, "folded");
        data->atom_implicit = enif_make_atom(env, "implicit");
        data->atom_length = enif_make_atom(env, "length");
        data->atom_literal = enif_make_atom(env, "literal");
        data->atom_mapping_end = enif_make_atom(env, "mapping_end");
        data->atom_mapping_start = enif_make_atom(env, "mapping_start");
        data->atom_plain = enif_make_atom(env, "plain");
        data->atom_plain_implicit = enif_make_atom(env, "plain_implicit");
        data->atom_quoted_implicit = enif_make_atom(env, "quoted_implicit");
        data->atom_scalar = enif_make_atom(env, "scalar");
        data->atom_sequence_end = enif_make_atom(env, "sequence_end");
        data->atom_sequence_start = enif_make_atom(env, "sequence_start");
        data->atom_single_quoted = enif_make_atom(env, "single_quoted");
        data->atom_start = enif_make_atom(env, "start");
        data->atom_stream_end = enif_make_atom(env, "stream_end");
        data->atom_stream_start = enif_make_atom(env, "stream_start");
        data->atom_style = enif_make_atom(env, "style");
        data->atom_tag = enif_make_atom(env, "tag");
        data->atom_type = enif_make_atom(env, "type");
        data->atom_unknown = enif_make_atom(env, "unknown");
        data->atom_value = enif_make_atom(env, "value");

        data->parser_resource_type =
                eyaml_create_resource_type(env, "parser", eyaml_parser_delete);

        *priv = (void *)data;
        return 0;
}

static void
eyaml_unload(ErlNifEnv *env, void *priv) {
        struct eyaml_nif_data *data;

        data = priv;
        enif_free(data);
}


ERL_NIF_INIT(yaml_nif, eyaml_nif_functions,
             eyaml_load, NULL, NULL, eyaml_unload);
