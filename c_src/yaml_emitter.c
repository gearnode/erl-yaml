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

struct eyaml_emitter {
        yaml_emitter_t emitter;

        ErlNifBinary buf;
};

static int
eyaml_inspect_emitter(ErlNifEnv *, ERL_NIF_TERM, struct eyaml_emitter **);
static int
eyaml_write_handler(void *, unsigned char *, size_t);
static int
eyaml_inspect_event(ErlNifEnv *, struct eyaml_emitter *, ERL_NIF_TERM,
                    ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_stream_start(ErlNifEnv *, struct eyaml_emitter *,
                                 ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_stream_end(ErlNifEnv *, struct eyaml_emitter *,
                               ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_document_start(ErlNifEnv *, struct eyaml_emitter *,
                                   ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_document_end(ErlNifEnv *, struct eyaml_emitter *,
                                 ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_alias(ErlNifEnv *, struct eyaml_emitter *,
                          ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_scalar(ErlNifEnv *, struct eyaml_emitter *,
                           ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_sequence_start(ErlNifEnv *, struct eyaml_emitter *,
                                   ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_sequence_end(ErlNifEnv *, struct eyaml_emitter *,
                                 ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_mapping_start(ErlNifEnv *, struct eyaml_emitter *,
                                  ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_event_mapping_end(ErlNifEnv *, struct eyaml_emitter *,
                                ERL_NIF_TERM, yaml_event_t *);
static int
eyaml_inspect_encoding(ErlNifEnv *, ERL_NIF_TERM, yaml_encoding_t *);
static int
eyaml_inspect_sequence_style(ErlNifEnv *, ERL_NIF_TERM,
                             yaml_sequence_style_t *);
static int
eyaml_inspect_mapping_style(ErlNifEnv *, ERL_NIF_TERM, yaml_mapping_style_t *);
static int
eyaml_inspect_scalar_style(ErlNifEnv *, ERL_NIF_TERM, yaml_scalar_style_t *);

struct eyaml_emitter *
eyaml_emitter_new(ErlNifEnv *env) {
        struct eyaml_nif_data *nif_data;
        struct eyaml_emitter *emitter;

        nif_data = enif_priv_data(env);

        emitter = enif_alloc_resource(nif_data->emitter_resource_type,
                                      sizeof(*emitter));

        if (yaml_emitter_initialize(&emitter->emitter) == 0)
                return NULL;

        yaml_emitter_set_output(&emitter->emitter, eyaml_write_handler,
                                emitter);

        yaml_emitter_set_canonical(&emitter->emitter, 0);
        yaml_emitter_set_unicode(&emitter->emitter, 1);
        yaml_emitter_set_indent(&emitter->emitter, 2);
        yaml_emitter_set_width(&emitter->emitter, 80);
        yaml_emitter_set_break(&emitter->emitter, YAML_LN_BREAK);

        memset(&emitter->buf, 0, sizeof(ErlNifBinary));

        return emitter;
}

void
eyaml_emitter_delete(ErlNifEnv *env, void *ptr) {
        struct eyaml_emitter *emitter;

        emitter = ptr;

        yaml_emitter_delete(&emitter->emitter);

        enif_release_binary(&emitter->buf);
}

ERL_NIF_TERM
eyaml_new_emitter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct eyaml_emitter *emitter;
        ERL_NIF_TERM term;

        if (argc != 0)
                return enif_make_badarg(env);

        emitter = eyaml_emitter_new(env);
        if (!emitter) {
                ERL_NIF_TERM reason_term;

                reason_term = enif_make_atom(env, "memory_error");
                return eyaml_error_tuple(env, reason_term);
        }

        term = enif_make_resource(env, emitter);

        enif_release_resource(emitter);

        return term;
}

ERL_NIF_TERM
eyaml_emitter_data(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct eyaml_emitter *emitter;

        if (argc != 1)
                return enif_make_badarg(env);

        if (eyaml_inspect_emitter(env, argv[0], &emitter) == 0)
                return enif_make_badarg(env);

        if (emitter->buf.data == NULL)
                return eyaml_binary_string(env, "");

        return enif_make_binary(env, &emitter->buf);
}

ERL_NIF_TERM
eyaml_emit(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct eyaml_emitter *emitter;
        ERL_NIF_TERM emitter_term;
        yaml_event_t event;
        int ret;

        if (argc != 3)
                return enif_make_badarg(env);

        emitter_term = argv[0];
        if (eyaml_inspect_emitter(env, emitter_term, &emitter) == 0)
                return enif_make_badarg(env);

        ret = eyaml_inspect_event(env, emitter, argv[1], argv[2], &event);
        if (ret == 0)
                return enif_make_badarg(env);

        if (yaml_emitter_emit(&emitter->emitter, &event) == 0) {
                ERL_NIF_TERM reason_term;

                reason_term = enif_make_atom(env, "serialization_error");
                return eyaml_error_tuple(env, reason_term);
        }

        return eyaml_ok_tuple(env, emitter_term);
}

int
eyaml_inspect_emitter(ErlNifEnv *env, ERL_NIF_TERM term,
                      struct eyaml_emitter **pemitter) {
        struct eyaml_nif_data *nif_data;
        ErlNifResourceType *resource_type;
        int ret;

        nif_data = enif_priv_data(env);
        resource_type = nif_data->emitter_resource_type;

        ret = enif_get_resource(env, term, resource_type, (void **)pemitter);
        if (ret == 0)
                return 0;

        return 1;
}

int
eyaml_write_handler(void *param, unsigned char *data, size_t sz) {
        struct eyaml_emitter *emitter;
        size_t old_len, new_len;

        emitter = param;

        old_len = emitter->buf.size;
        new_len = old_len + sz;

        if (enif_realloc_binary(&emitter->buf, new_len) == 0)
                return 0;

        memcpy(emitter->buf.data + old_len, data, sz);

        return 1;
}

int
eyaml_inspect_event(ErlNifEnv *env, struct eyaml_emitter *emitter,
                    ERL_NIF_TERM type_term, ERL_NIF_TERM data_term,
                    yaml_event_t *event) {
        int (*inspect_fun)(ErlNifEnv *, struct eyaml_emitter *, ERL_NIF_TERM,
                           yaml_event_t *);

        // enif_fprintf(stderr, "XXX inspect %T %T\n", type_term, data_term);

        memset(event, 0, sizeof(yaml_event_t));

        if (eyaml_is_atom(env, type_term, "stream_start")) {
                inspect_fun = eyaml_inspect_event_stream_start;
        } else if (eyaml_is_atom(env, type_term, "stream_end")) {
                inspect_fun = eyaml_inspect_event_stream_end;
        } else if (eyaml_is_atom(env, type_term, "document_start")) {
                inspect_fun = eyaml_inspect_event_document_start;
        } else if (eyaml_is_atom(env, type_term, "document_end")) {
                inspect_fun = eyaml_inspect_event_document_end;
        } else if (eyaml_is_atom(env, type_term, "alias")) {
                inspect_fun = eyaml_inspect_event_alias;
        } else if (eyaml_is_atom(env, type_term, "scalar")) {
                inspect_fun = eyaml_inspect_event_scalar;
        } else if (eyaml_is_atom(env, type_term, "sequence_start")) {
                inspect_fun = eyaml_inspect_event_sequence_start;
        } else if (eyaml_is_atom(env, type_term, "sequence_end")) {
                inspect_fun = eyaml_inspect_event_sequence_end;
        } else if (eyaml_is_atom(env, type_term, "mapping_start")) {
                inspect_fun = eyaml_inspect_event_mapping_start;
        } else if (eyaml_is_atom(env, type_term, "mapping_end")) {
                inspect_fun = eyaml_inspect_event_mapping_end;
        } else {
                return 0;
        }

        return inspect_fun(env, emitter, data_term, event);
}

int
eyaml_inspect_event_stream_start(ErlNifEnv *env, struct eyaml_emitter *emitter,
                                 ERL_NIF_TERM data_term, yaml_event_t *event) {
        struct eyaml_nif_data *nif_data;
        yaml_encoding_t encoding;
        ERL_NIF_TERM term;
        int ret;

        nif_data = enif_priv_data(env);

        if (!enif_is_map(env, data_term))
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_encoding,
                                 &term);
        if (ret == 0) {
                encoding = YAML_ANY_ENCODING;
        } else if (eyaml_inspect_encoding(env, term, &encoding) == 0) {
                return 0;
        }

        yaml_stream_start_event_initialize(event, encoding);

        return 1;
}

int
eyaml_inspect_event_stream_end(ErlNifEnv *env, struct eyaml_emitter *emitter,
                               ERL_NIF_TERM data_term, yaml_event_t *event) {
        yaml_stream_end_event_initialize(event);

        return 1;
}

int
eyaml_inspect_event_document_start(ErlNifEnv *env, struct eyaml_emitter *emitter,
                                   ERL_NIF_TERM data_term, yaml_event_t *event) {
        struct eyaml_nif_data *nif_data;
        ERL_NIF_TERM term;
        int implicit;
        int ret;

        // TODO version_directives, tag_directives

        nif_data = enif_priv_data(env);

        if (!enif_is_map(env, data_term))
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_implicit,
                                 &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_bool_int(env, term, &implicit) == 0)
                return 0;

        yaml_document_start_event_initialize(event, NULL, NULL, NULL,
                                             implicit);

        return 1;
}

int
eyaml_inspect_event_document_end(ErlNifEnv *env, struct eyaml_emitter *emitter,
                                 ERL_NIF_TERM data_term, yaml_event_t *event) {
        struct eyaml_nif_data *nif_data;
        ERL_NIF_TERM term;
        int implicit;
        int ret;

        nif_data = enif_priv_data(env);

        if (!enif_is_map(env, data_term))
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_implicit,
                                 &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_bool_int(env, term, &implicit) == 0)
                return 0;

        yaml_document_end_event_initialize(event, implicit);

        return 1;
}

int
eyaml_inspect_event_alias(ErlNifEnv *env, struct eyaml_emitter *emitter,
                          ERL_NIF_TERM data_term, yaml_event_t *event) {
        // TODO

        return 1;
}

int
eyaml_inspect_event_scalar(ErlNifEnv *env, struct eyaml_emitter *emitter,
                           ERL_NIF_TERM data_term, yaml_event_t *event) {
        struct eyaml_nif_data *nif_data;
        ERL_NIF_TERM term;
        ErlNifBinary value;
        int length, plain_implicit, quoted_implicit;
        yaml_scalar_style_t style;
        int ret;

        // TODO anchor, tag

        nif_data = enif_priv_data(env);

        if (!enif_is_map(env, data_term))
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_value, &term);
        if (ret == 0)
                return 0;
        if (enif_inspect_binary(env, term, &value) == 0)
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_length, &term);
        if (ret == 0)
                return 0;
        if (enif_get_int(env, term, &length) == 0)
                return 0;
        if (length < 0 || (size_t)length > value.size)
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_plain_implicit,
                                 &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_bool_int(env, term, &plain_implicit) == 0)
                return 0;

        ret = enif_get_map_value(env, data_term,
                                 nif_data->atom_quoted_implicit, &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_bool_int(env, term, &quoted_implicit) == 0)
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_style, &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_scalar_style(env, term, &style) == 0)
                return 0;

        yaml_scalar_event_initialize(event, NULL, NULL, value.data, length,
                                     plain_implicit, quoted_implicit, style);

        return 1;
}

int
eyaml_inspect_event_sequence_start(ErlNifEnv *env, struct eyaml_emitter *emitter,
                                   ERL_NIF_TERM data_term, yaml_event_t *event) {
        struct eyaml_nif_data *nif_data;
        ERL_NIF_TERM term;
        int implicit;
        yaml_sequence_style_t style;
        int ret;

        // TODO anchor, tag

        nif_data = enif_priv_data(env);

        if (!enif_is_map(env, data_term))
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_implicit,
                                 &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_bool_int(env, term, &implicit) == 0)
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_style, &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_sequence_style(env, term, &style) == 0)
                return 0;

        yaml_sequence_start_event_initialize(event, NULL, NULL, implicit,
                                             style);

        return 1;
}

int
eyaml_inspect_event_sequence_end(ErlNifEnv *env, struct eyaml_emitter *emitter,
                                 ERL_NIF_TERM data_term, yaml_event_t *event) {
        yaml_sequence_end_event_initialize(event);

        return 1;
}

int
eyaml_inspect_event_mapping_start(ErlNifEnv *env, struct eyaml_emitter *emitter,
                                  ERL_NIF_TERM data_term, yaml_event_t *event) {
        struct eyaml_nif_data *nif_data;
        ERL_NIF_TERM term;
        int implicit;
        yaml_mapping_style_t style;
        int ret;

        // TODO anchor, tag

        nif_data = enif_priv_data(env);

        if (!enif_is_map(env, data_term))
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_implicit,
                                 &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_bool_int(env, term, &implicit) == 0)
                return 0;

        ret = enif_get_map_value(env, data_term, nif_data->atom_style, &term);
        if (ret == 0)
                return 0;
        if (eyaml_inspect_mapping_style(env, term, &style) == 0)
                return 0;

        yaml_mapping_start_event_initialize(event, NULL, NULL, implicit,
                                            style);

        return 1;
}

int
eyaml_inspect_event_mapping_end(ErlNifEnv *env, struct eyaml_emitter *emitter,
                                ERL_NIF_TERM data_term, yaml_event_t *event) {
        yaml_mapping_end_event_initialize(event);

        return 1;
}

int
eyaml_inspect_encoding(ErlNifEnv *env, ERL_NIF_TERM term,
                       yaml_encoding_t *encoding) {
        if (eyaml_is_atom(env, term, "any")) {
                *encoding = YAML_ANY_ENCODING;
        } else if (eyaml_is_atom(env, term, "utf8")) {
                *encoding = YAML_UTF8_ENCODING;
        } else if (eyaml_is_atom(env, term, "utf16le")) {
                *encoding = YAML_UTF16LE_ENCODING;
        } else if (eyaml_is_atom(env, term, "utf16be")) {
                *encoding = YAML_UTF16BE_ENCODING;
        } else {
                return 0;
        }

        return 1;
}

int
eyaml_inspect_sequence_style(ErlNifEnv *env, ERL_NIF_TERM term,
                             yaml_sequence_style_t *style) {
        if (eyaml_is_atom(env, term, "any")) {
                *style = YAML_ANY_SEQUENCE_STYLE;
        } else if (eyaml_is_atom(env, term, "block")) {
                *style = YAML_BLOCK_SEQUENCE_STYLE;
        } else if (eyaml_is_atom(env, term, "flow")) {
                *style = YAML_FLOW_SEQUENCE_STYLE;
        } else {
                return 0;
        }

        return 1;
}

int
eyaml_inspect_mapping_style(ErlNifEnv *env, ERL_NIF_TERM term,
                            yaml_mapping_style_t *style) {
        if (eyaml_is_atom(env, term, "any")) {
                *style = YAML_ANY_MAPPING_STYLE;
        } else if (eyaml_is_atom(env, term, "block")) {
                *style = YAML_BLOCK_MAPPING_STYLE;
        } else if (eyaml_is_atom(env, term, "flow")) {
                *style = YAML_FLOW_MAPPING_STYLE;
        } else {
                return 0;
        }

        return 1;
}

int
eyaml_inspect_scalar_style(ErlNifEnv *env, ERL_NIF_TERM term,
                           yaml_scalar_style_t *style) {
        if (eyaml_is_atom(env, term, "any")) {
                *style = YAML_ANY_SCALAR_STYLE;
        } else if (eyaml_is_atom(env, term, "plain")) {
                *style = YAML_PLAIN_SCALAR_STYLE;
        } else if (eyaml_is_atom(env, term, "single_quoted")) {
                *style = YAML_SINGLE_QUOTED_SCALAR_STYLE;
        } else if (eyaml_is_atom(env, term, "double_quoted")) {
                *style = YAML_DOUBLE_QUOTED_SCALAR_STYLE;
        } else if (eyaml_is_atom(env, term, "literal")) {
                *style = YAML_LITERAL_SCALAR_STYLE;
        } else if (eyaml_is_atom(env, term, "folded")) {
                *style = YAML_FOLDED_SCALAR_STYLE;
        } else {
                return 0;
        }

        return 1;
}
