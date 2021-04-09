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

static ERL_NIF_TERM
eyaml_parse_1(ErlNifEnv *, int, const ERL_NIF_TERM []);

static ERL_NIF_TERM
eyaml_syntax_error(ErlNifEnv *, const yaml_parser_t *);
static ERL_NIF_TERM
eyaml_mark_to_term(ErlNifEnv *, const yaml_mark_t *);
static ERL_NIF_TERM
eyaml_event_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_event_type_to_term(ErlNifEnv *, yaml_event_type_t);
static bool
eyaml_event_data_to_term(ErlNifEnv *, const yaml_event_t *, ERL_NIF_TERM *);
static ERL_NIF_TERM
eyaml_stream_start_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_document_start_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_document_end_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_alias_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_scalar_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_sequence_start_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_mapping_start_to_term(ErlNifEnv *, const yaml_event_t *);
static ERL_NIF_TERM
eyaml_encoding_to_term(ErlNifEnv *, yaml_encoding_t);
static ERL_NIF_TERM
eyaml_version_directive_to_term(ErlNifEnv *, const yaml_version_directive_t *);
static ERL_NIF_TERM
eyaml_tag_directive_to_term(ErlNifEnv *, const yaml_tag_directive_t *);
static ERL_NIF_TERM
eyaml_sequence_style_to_term(ErlNifEnv *, yaml_sequence_style_t);
static ERL_NIF_TERM
eyaml_mapping_style_to_term(ErlNifEnv *, yaml_mapping_style_t);
static ERL_NIF_TERM
eyaml_scalar_style_to_term(ErlNifEnv *, yaml_scalar_style_t);

struct eyaml_parser {
        yaml_parser_t parser;

        size_t max_block_size;
        size_t block_size;
};

struct eyaml_parser *
eyaml_parser_new(ErlNifEnv *env, ErlNifBinary bin) {
        struct eyaml_nif_data *nif_data;
        struct eyaml_parser *parser;

        nif_data = enif_priv_data(env);

        parser = enif_alloc_resource(nif_data->parser_resource_type,
                                     sizeof(*parser));

        if (yaml_parser_initialize(&parser->parser) == 0) {
                return NULL;
        }
        yaml_parser_set_input_string(&parser->parser, bin.data, bin.size);

        parser->max_block_size = (1 << 16); // 64KiB
        parser->block_size = 0;

        return parser;
}

void
eyaml_parser_delete(ErlNifEnv *env, void *ptr) {
        struct eyaml_parser *parser;

        parser = ptr;

        yaml_parser_delete(&parser->parser);
}

ERL_NIF_TERM
eyaml_parse(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct eyaml_nif_data *nif_data;
        struct eyaml_parser *parser;
        ErlNifBinary data;
        ERL_NIF_TERM argv2[2];

        nif_data = enif_priv_data(env);

        if (argc != 1) {
                return enif_make_badarg(env);
        }

        if (enif_inspect_binary(env, argv[0], &data) == 0) {
                return enif_make_badarg(env);
        }

        parser = eyaml_parser_new(env, data);
        if (!parser) {
                ERL_NIF_TERM reason_term;

                reason_term = enif_make_atom(env, "memory_error");
                return eyaml_error_tuple(env, reason_term);
        }

        argv2[0] = enif_make_resource(env, parser);
        argv2[1] = enif_make_list(env, 0);

        enif_release_resource(parser);

        return eyaml_parse_1(env, 2, argv2);
}

ERL_NIF_TERM
eyaml_parse_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
        struct eyaml_nif_data *nif_data;
        struct eyaml_parser *parser;
        ERL_NIF_TERM events_term;
        int ret;

        nif_data = enif_priv_data(env);

        if (argc != 2) {
                return enif_make_badarg(env);
        }

        ret = enif_get_resource(env, argv[0], nif_data->parser_resource_type,
                                (void **)&parser);
        if (ret == 0) {
                return enif_make_badarg(env);
        }

        events_term = argv[1];

        for (;;) {
                ERL_NIF_TERM event_term;
                yaml_event_t event;
                size_t prev_offset, offset, nb_consumed_bytes;
                bool end;

                prev_offset = parser->parser.mark.index;

                if (yaml_parser_parse(&parser->parser, &event) == 0) {
                        ERL_NIF_TERM error;

                        error = eyaml_syntax_error(env, &parser->parser);

                        return eyaml_error_tuple(env, error);
                }

                offset = parser->parser.mark.index;

                event_term = eyaml_event_to_term(env, &event);
                events_term = enif_make_list_cell(env, event_term,
                                                  events_term);

                end = (event.type == YAML_STREAM_END_EVENT);

                yaml_event_delete(&event);

                if (end) {
                        break;
                }

                nb_consumed_bytes = offset - prev_offset;
                parser->block_size += nb_consumed_bytes;

                if (parser->block_size > parser->max_block_size) {
                        int percent;

                        percent = parser->block_size * 100;
                        percent /= parser->max_block_size;

                        if (percent > 100) percent = 100;
                        if (percent == 0) percent = 1;

                        if (enif_consume_timeslice(env, percent) == 1) {
                                ERL_NIF_TERM argv2[2];

                                parser->block_size = 0;

                                argv2[0] = argv[0];
                                argv2[1] = events_term;

                                return enif_schedule_nif(env, "parse_1", 0,
                                                         eyaml_parse_1,
                                                         argc, argv2);
                        }
                }
        }

        eyaml_reverse_list(env, &events_term);

        return eyaml_ok_tuple(env, events_term);
}

ERL_NIF_TERM
eyaml_syntax_error(ErlNifEnv *env, const yaml_parser_t *parser) {
        ERL_NIF_TERM reason_term, message_term, mark_tuple;
        const char *message;

        if (parser->problem == NULL) {
                message = "unknown error";
        } else {
                message = parser->problem;
        }

        reason_term = enif_make_atom(env, "syntax_error");
        message_term = eyaml_binary_string(env, message);
        mark_tuple = eyaml_mark_to_term(env, &parser->problem_mark);

        return enif_make_tuple3(env, reason_term, message_term, mark_tuple);
}

ERL_NIF_TERM
eyaml_mark_to_term(ErlNifEnv *env, const yaml_mark_t *mark) {
        ERL_NIF_TERM line_term, column_term, offset_term;

        line_term = enif_make_ulong(env, mark->line + 1);
        column_term = enif_make_ulong(env, mark->column + 1);
        offset_term = enif_make_ulong(env, mark->index);

        return enif_make_tuple3(env, line_term, column_term, offset_term);
}

ERL_NIF_TERM
eyaml_event_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM event_term, data_term, key_terms[3], value_terms[3];

        key_terms[0] = enif_make_atom(env, "type");
        value_terms[0] = eyaml_event_type_to_term(env, event->type);

        key_terms[1] = enif_make_atom(env, "start");
        value_terms[1] = eyaml_mark_to_term(env, &event->start_mark);

        key_terms[2] = enif_make_atom(env, "end");
        value_terms[2] = eyaml_mark_to_term(env, &event->end_mark);

        enif_make_map_from_arrays(env, key_terms, value_terms, 3, &event_term);

        if (eyaml_event_data_to_term(env, event, &data_term) == true) {
                eyaml_map_put(env, enif_make_atom(env, "data"), data_term,
                              &event_term);
        }

        return event_term;
}

ERL_NIF_TERM
eyaml_event_type_to_term(ErlNifEnv *env, yaml_event_type_t type) {
        const char *name;

        switch (type) {
        case YAML_STREAM_START_EVENT:
                name = "stream_start";
                break;
        case YAML_STREAM_END_EVENT:
                name = "stream_end";
                break;
        case YAML_DOCUMENT_START_EVENT:
                name = "document_start";
                break;
        case YAML_DOCUMENT_END_EVENT:
                name = "document_end";
                break;
        case YAML_ALIAS_EVENT:
                name = "alias";
                break;
        case YAML_SCALAR_EVENT:
                name = "scalar";
                break;
        case YAML_SEQUENCE_START_EVENT:
                name = "sequence_start";
                break;
        case YAML_SEQUENCE_END_EVENT:
                name = "sequence_end";
                break;
        case YAML_MAPPING_START_EVENT:
                name = "mapping_start";
                break;
        case YAML_MAPPING_END_EVENT:
                name = "mapping_end";
                break;

        default:
                return enif_make_tuple2(env, enif_make_atom(env, "unknown"),
                                        enif_make_uint(env, type));
        }

        return enif_make_atom(env, name);
}

bool
eyaml_event_data_to_term(ErlNifEnv *env, const yaml_event_t *event,
                         ERL_NIF_TERM *term) {
        switch (event->type) {
        case YAML_STREAM_START_EVENT:
                *term = eyaml_stream_start_to_term(env, event);
                break;

        case YAML_DOCUMENT_START_EVENT:
                *term = eyaml_document_start_to_term(env, event);
                break;

        case YAML_DOCUMENT_END_EVENT:
                *term = eyaml_document_end_to_term(env, event);
                break;

        case YAML_ALIAS_EVENT:
                *term = eyaml_alias_to_term(env, event);
                break;

        case YAML_SCALAR_EVENT:
                *term = eyaml_scalar_to_term(env, event);
                break;

        case YAML_SEQUENCE_START_EVENT:
                *term = eyaml_sequence_start_to_term(env, event);
                break;

        case YAML_MAPPING_START_EVENT:
                *term = eyaml_mapping_start_to_term(env, event);
                break;

        default:
                return false;
        }

        return true;
}

ERL_NIF_TERM
eyaml_stream_start_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM key_term, encoding_term, map_term;
        yaml_encoding_t encoding;

        encoding = event->data.stream_start.encoding;

        key_term = enif_make_atom(env, "encoding");
        encoding_term = eyaml_encoding_to_term(env, encoding);

        enif_make_map_from_arrays(env, &key_term, &encoding_term, 1,
                                  &map_term);

        return map_term;
}

ERL_NIF_TERM
eyaml_document_start_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM map_term, key_term, value_term;
        yaml_version_directive_t *version;
        yaml_tag_directive_t *tag_start, *tag_end;
        bool implicit;

        implicit = (event->data.document_start.implicit != 0);

        key_term = enif_make_atom(env, "implicit");
        value_term = eyaml_boolean(env, implicit);

        enif_make_map_from_arrays(env, &key_term, &value_term, 1, &map_term);

        version = event->data.document_start.version_directive;
        if (version) {
                key_term = enif_make_atom(env, "version_directive");
                value_term = eyaml_version_directive_to_term(env, version);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        tag_start = event->data.document_start.tag_directives.start;
        tag_end = event->data.document_start.tag_directives.end;
        if (tag_start) {
                yaml_tag_directive_t *tag;

                key_term = enif_make_atom(env, "tag_directives");

                value_term = enif_make_list(env, 0);

                for (tag = tag_start; tag != tag_end; tag++) {
                        ERL_NIF_TERM tag_term;

                        tag_term = eyaml_tag_directive_to_term(env, tag);

                        value_term = enif_make_list_cell(env, tag_term,
                                                         value_term);
                }

                eyaml_reverse_list(env, &value_term);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        return map_term;
}

ERL_NIF_TERM
eyaml_document_end_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM map_term, key_term, value_term;
        bool implicit;

        implicit = (event->data.document_end.implicit != 0);

        key_term = enif_make_atom(env, "implicit");
        value_term = eyaml_boolean(env, implicit);

        enif_make_map_from_arrays(env, &key_term, &value_term, 1, &map_term);

        return map_term;
}

ERL_NIF_TERM
eyaml_alias_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM map_term, key_term, value_term;

        key_term = enif_make_atom(env, "anchor");
        value_term = eyaml_binary_ustring(env, event->data.alias.anchor);

        enif_make_map_from_arrays(env, &key_term, &value_term, 1, &map_term);

        return map_term;
}

ERL_NIF_TERM
eyaml_scalar_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM map_term, key_terms[5], value_terms[5];
        ERL_NIF_TERM key_term, value_term;
        const unsigned char *value, *anchor, *tag;
        size_t length;
        bool plain_implicit, quoted_implicit;
        yaml_scalar_style_t style;

        value = event->data.scalar.value;
        length = event->data.scalar.length;
        plain_implicit = event->data.scalar.plain_implicit;
        quoted_implicit = event->data.scalar.quoted_implicit;
        style = event->data.scalar.style;

        key_terms[0] = enif_make_atom(env, "value");
        value_terms[0] = eyaml_binary_ustring(env, value);

        key_terms[1] = enif_make_atom(env, "length");
        value_terms[1] = enif_make_ulong(env, length);

        key_terms[2] = enif_make_atom(env, "plain_implicit");
        value_terms[2] = eyaml_boolean(env, plain_implicit);

        key_terms[3] = enif_make_atom(env, "quoted_implicit");
        value_terms[3] = eyaml_boolean(env, quoted_implicit);

        key_terms[4] = enif_make_atom(env, "style");
        value_terms[4] = eyaml_scalar_style_to_term(env, style);

        enif_make_map_from_arrays(env, key_terms, value_terms, 5, &map_term);

        anchor = event->data.scalar.anchor;
        if (anchor) {
                key_term = enif_make_atom(env, "anchor");
                value_term = eyaml_binary_ustring(env, anchor);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        tag = event->data.scalar.tag;
        if (tag) {
                key_term = enif_make_atom(env, "tag");
                value_term = eyaml_binary_ustring(env, tag);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        return map_term;
}

ERL_NIF_TERM
eyaml_sequence_start_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM map_term, key_terms[2], value_terms[2];
        ERL_NIF_TERM key_term, value_term;
        bool implicit;
        const unsigned char *anchor, *tag;
        yaml_sequence_style_t style;

        implicit = (event->data.document_start.implicit != 0);
        style = event->data.sequence_start.style;

        key_terms[0] = enif_make_atom(env, "implicit");
        value_terms[0] = eyaml_boolean(env, implicit);

        key_terms[1] = enif_make_atom(env, "style");
        value_terms[1] = eyaml_sequence_style_to_term(env, style);

        enif_make_map_from_arrays(env, key_terms, value_terms, 2, &map_term);

        anchor = event->data.sequence_start.anchor;
        if (anchor) {
                key_term = enif_make_atom(env, "anchor");
                value_term = eyaml_binary_ustring(env, anchor);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        tag = event->data.sequence_start.tag;
        if (tag) {
                key_term = enif_make_atom(env, "tag");
                value_term = eyaml_binary_ustring(env, tag);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        return map_term;
}

ERL_NIF_TERM
eyaml_mapping_start_to_term(ErlNifEnv *env, const yaml_event_t *event) {
        ERL_NIF_TERM map_term, key_terms[2], value_terms[2];
        ERL_NIF_TERM key_term, value_term;
        bool implicit;
        const unsigned char *anchor, *tag;
        yaml_mapping_style_t style;

        implicit = (event->data.document_start.implicit != 0);
        style = event->data.mapping_start.style;

        key_terms[0] = enif_make_atom(env, "implicit");
        value_terms[0] = eyaml_boolean(env, implicit);

        key_terms[1] = enif_make_atom(env, "style");
        value_terms[1] = eyaml_mapping_style_to_term(env, style);

        enif_make_map_from_arrays(env, key_terms, value_terms, 2, &map_term);

        anchor = event->data.mapping_start.anchor;
        if (anchor) {
                key_term = enif_make_atom(env, "anchor");
                value_term = eyaml_binary_ustring(env, anchor);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        tag = event->data.mapping_start.tag;
        if (tag) {
                key_term = enif_make_atom(env, "tag");
                value_term = eyaml_binary_ustring(env, tag);

                eyaml_map_put(env, key_term, value_term, &map_term);
        }

        return map_term;
}

ERL_NIF_TERM
eyaml_encoding_to_term(ErlNifEnv *env, yaml_encoding_t encoding) {
        const char *name;

        switch (encoding) {
        case YAML_ANY_ENCODING:
                name = "any";
                break;
        case YAML_UTF8_ENCODING:
                name = "utf8";
                break;
        case YAML_UTF16LE_ENCODING:
                name = "utf16le";
                break;
        case YAML_UTF16BE_ENCODING:
                name = "utf16be";
                break;

        default:
                return enif_make_tuple2(env, enif_make_atom(env, "unknown"),
                                        enif_make_uint(env, encoding));
        }

        return enif_make_atom(env, name);
}

ERL_NIF_TERM
eyaml_version_directive_to_term(ErlNifEnv *env,
                                const yaml_version_directive_t *version) {
        ERL_NIF_TERM key_terms[2], value_terms[2], version_term;

        key_terms[0] = enif_make_atom(env, "major");
        value_terms[0] = enif_make_int(env, version->major);

        key_terms[1] = enif_make_atom(env, "minor");
        value_terms[1] = enif_make_int(env, version->minor);

        enif_make_map_from_arrays(env, key_terms, value_terms, 2,
                                  &version_term);

        return version_term;
}

ERL_NIF_TERM
eyaml_tag_directive_to_term(ErlNifEnv *env, const yaml_tag_directive_t *tag) {
        ERL_NIF_TERM key_terms[2], value_terms[2], tag_term;

        key_terms[0] = enif_make_atom(env, "handle");
        value_terms[0] = eyaml_binary_ustring(env, tag->handle);

        key_terms[1] = enif_make_atom(env, "prefix");
        value_terms[1] = eyaml_binary_ustring(env, tag->prefix);

        enif_make_map_from_arrays(env, key_terms, value_terms, 2, &tag_term);

        return tag_term;
}

ERL_NIF_TERM
eyaml_sequence_style_to_term(ErlNifEnv *env, yaml_sequence_style_t style) {
        const char *name;

        switch (style) {
        case YAML_ANY_SEQUENCE_STYLE:
                name = "any";
                break;
        case YAML_BLOCK_SEQUENCE_STYLE:
                name = "block";
                break;
        case YAML_FLOW_SEQUENCE_STYLE:
                name = "flow";
                break;

        default:
                return enif_make_tuple2(env, enif_make_atom(env, "unknown"),
                                        enif_make_uint(env, style));
        }

        return enif_make_atom(env, name);
}

ERL_NIF_TERM
eyaml_mapping_style_to_term(ErlNifEnv *env, yaml_mapping_style_t style) {
        const char *name;

        switch (style) {
        case YAML_ANY_MAPPING_STYLE:
                name = "any";
                break;
        case YAML_BLOCK_MAPPING_STYLE:
                name = "block";
                break;
        case YAML_FLOW_MAPPING_STYLE:
                name = "flow";
                break;

        default:
                return enif_make_tuple2(env, enif_make_atom(env, "unknown"),
                                        enif_make_uint(env, style));
        }

        return enif_make_atom(env, name);
}

ERL_NIF_TERM
eyaml_scalar_style_to_term(ErlNifEnv *env, yaml_scalar_style_t style) {
        const char *name;

        switch (style) {
        case YAML_ANY_SCALAR_STYLE:
                name = "any";
                break;
        case YAML_PLAIN_SCALAR_STYLE:
                name = "plain";
                break;
        case YAML_SINGLE_QUOTED_SCALAR_STYLE:
                name = "single_quoted";
                break;
        case YAML_DOUBLE_QUOTED_SCALAR_STYLE:
                name = "double_quoted";
                break;
        case YAML_LITERAL_SCALAR_STYLE:
                name = "literal";
                break;
        case YAML_FOLDED_SCALAR_STYLE:
                name = "folded";
                break;

        default:
                return enif_make_tuple2(env, enif_make_atom(env, "unknown"),
                                        enif_make_uint(env, style));
        }

        return enif_make_atom(env, name);
}
