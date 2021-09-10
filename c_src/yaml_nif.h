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

#ifndef YAML_NIF_H
#define YAML_NIF_H

#include <stdbool.h>

#include <ei.h>
#include <erl_nif.h>

#include <yaml.h>

struct eyaml_nif_data {
        ErlNifResourceType *parser_resource_type;
        ErlNifResourceType *emitter_resource_type;

        ERL_NIF_TERM atom_alias;
        ERL_NIF_TERM atom_anchor;
        ERL_NIF_TERM atom_any;
        ERL_NIF_TERM atom_block;
        ERL_NIF_TERM atom_data;
        ERL_NIF_TERM atom_document_end;
        ERL_NIF_TERM atom_document_start;
        ERL_NIF_TERM atom_double_quoted;
        ERL_NIF_TERM atom_encoding;
        ERL_NIF_TERM atom_end;
        ERL_NIF_TERM atom_flow;
        ERL_NIF_TERM atom_folded;
        ERL_NIF_TERM atom_implicit;
        ERL_NIF_TERM atom_length;
        ERL_NIF_TERM atom_literal;
        ERL_NIF_TERM atom_mapping_end;
        ERL_NIF_TERM atom_mapping_start;
        ERL_NIF_TERM atom_plain;
        ERL_NIF_TERM atom_plain_implicit;
        ERL_NIF_TERM atom_quoted_implicit;
        ERL_NIF_TERM atom_scalar;
        ERL_NIF_TERM atom_sequence_end;
        ERL_NIF_TERM atom_sequence_start;
        ERL_NIF_TERM atom_single_quoted;
        ERL_NIF_TERM atom_start;
        ERL_NIF_TERM atom_stream_end;
        ERL_NIF_TERM atom_stream_start;
        ERL_NIF_TERM atom_style;
        ERL_NIF_TERM atom_tag;
        ERL_NIF_TERM atom_type;
        ERL_NIF_TERM atom_unknown;
        ERL_NIF_TERM atom_value;
};

void eyaml_nif_data_dtor(ErlNifEnv *, void *);

#define YAML_EXPORT(name_) \
        ERL_NIF_TERM name_(ErlNifEnv *, int, const ERL_NIF_TERM [])

YAML_EXPORT(eyaml_get_version);
YAML_EXPORT(eyaml_get_version_string);

YAML_EXPORT(eyaml_parse);

YAML_EXPORT(eyaml_new_emitter);
YAML_EXPORT(eyaml_emitter_data);
YAML_EXPORT(eyaml_emit);

void eyaml_parser_delete(ErlNifEnv *, void *);
void eyaml_emitter_delete(ErlNifEnv *, void *);

ERL_NIF_TERM eyaml_ok_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM eyaml_error_tuple(ErlNifEnv *, ERL_NIF_TERM);
ERL_NIF_TERM eyaml_binary_string(ErlNifEnv *, const char *);
ERL_NIF_TERM eyaml_binary_ustring(ErlNifEnv *, const unsigned char *);
ERL_NIF_TERM eyaml_boolean(ErlNifEnv *, bool);
void eyaml_reverse_list(ErlNifEnv *, ERL_NIF_TERM *);
void eyaml_map_put(ErlNifEnv *, ERL_NIF_TERM, ERL_NIF_TERM, ERL_NIF_TERM *);
bool eyaml_is_atom(ErlNifEnv *, ERL_NIF_TERM, const char *);
int eyaml_inspect_bool_int(ErlNifEnv *, ERL_NIF_TERM, int *);
ErlNifResourceType *eyaml_create_resource_type(ErlNifEnv *, const char *,
                                               ErlNifResourceDtor *);

#endif
