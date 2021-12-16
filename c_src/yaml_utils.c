// Copyright (c) 2021 Exograd SAS.
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

ERL_NIF_TERM
eyaml_ok_tuple(ErlNifEnv *env, ERL_NIF_TERM term) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), term);
}

ERL_NIF_TERM
eyaml_error_tuple(ErlNifEnv *env, ERL_NIF_TERM term) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), term);
}

ERL_NIF_TERM
eyaml_binary_string(ErlNifEnv *env, const char *s) {
        ERL_NIF_TERM term;
        unsigned char *buf;
        size_t len;

        len = strlen(s);
        buf = enif_make_new_binary(env, len, &term);
        memcpy(buf, s, len);

        return term;
}

ERL_NIF_TERM
eyaml_binary_ustring(ErlNifEnv *env, const unsigned char *s) {
        return eyaml_binary_string(env, (const char *)s);
}

ERL_NIF_TERM
eyaml_boolean(ErlNifEnv *env, bool b) {
        return enif_make_atom(env, b ? "true" : "false");
}

void
eyaml_reverse_list(ErlNifEnv *env, ERL_NIF_TERM *term) {
        ERL_NIF_TERM term_2;

        enif_make_reverse_list(env, *term, &term_2);
        *term = term_2;
}

void
eyaml_map_put(ErlNifEnv *env, ERL_NIF_TERM key_term, ERL_NIF_TERM value_term,
              ERL_NIF_TERM *map_term) {
        ERL_NIF_TERM map_term_2;

        enif_make_map_put(env, *map_term, key_term, value_term, &map_term_2);
        *map_term = map_term_2;
}

bool
eyaml_is_atom(ErlNifEnv *env, ERL_NIF_TERM term, const char *name) {
        char atom_name[256]; // totally arbitrary max length
        int ret;

        ret = enif_get_atom(env, term, atom_name, sizeof(atom_name),
                            ERL_NIF_LATIN1);
        if (ret == -1)
                return false;

        return strcmp(atom_name, name) == 0;
}

int
eyaml_inspect_bool_int(ErlNifEnv *env, ERL_NIF_TERM term, int *value) {
        if (eyaml_is_atom(env, term, "true")) {
                *value = 1;
        } else if (eyaml_is_atom(env, term, "false")) {
                *value = 0;
        } else {
                return 0;
        }

        return 1;
}

ErlNifResourceType *
eyaml_create_resource_type(ErlNifEnv *env, const char *name,
                           ErlNifResourceDtor *dtor) {
        ErlNifResourceFlags flags;
        ErlNifResourceType *type;

        flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

        type = enif_open_resource_type(env, NULL, name, NULL, flags, NULL);
        if (type == NULL) {
                enif_fprintf(stderr, "cannot open resource type '%s'\n");
                return NULL;
        }

        return type;
}
