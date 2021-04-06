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

ERL_NIF_TERM
eyaml_get_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        int major, minor, patch;
        ERL_NIF_TERM major_term, minor_term, patch_term;

        yaml_get_version(&major, &minor, &patch);

        major_term = enif_make_int(env, major);
        minor_term = enif_make_int(env, minor);
        patch_term = enif_make_int(env, patch);

        return enif_make_tuple3(env, major_term, minor_term, patch_term);
}

ERL_NIF_TERM
eyaml_get_version_string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        return eyaml_binary_string(env, yaml_get_version_string());
}
