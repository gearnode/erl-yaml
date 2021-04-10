% erl-yaml

# Introduction
The erl-yaml project is an implementation of the [YAML](https://yaml.org) data
format.

# Conformance
The erl-yaml project tries to follow [the YAML 1.2
specification](https://yaml.org/spec/1.2/spec.html) as much as possible.

# Interface
## Parsing
**TODO**

### Options
**TODO**

## JSON compatibility
**TODO**

# Types
YAML values and Erlang terms are mapped according to the following table:

| YAML value | Erlang term                                                |
| ---------- | -----------                                                |
| null       | `null`                                                     |
| boolean    | `true \| false`                                            |
| integer    | `integer()`                                                |
| float      | `float() \| nan \| positive_infinity \| negative_infinity` |
| string     | `binary()`                                                 |
| sequence   | `[yaml:value()]`                                           |
| mapping    | `#{yaml:value() := yaml:value()}`                          |

# Caveats
## Character encodings
While YAML supports UTF-8, UTF-16 and UTF-32, we currently only support UTF-8
for convenience. This may change in the future.

## Floating point values
YAML can represent all IEEE.754 floating point values, but Erlang cannot
represent `NaN` or infinite values. Additionally, it cannot differenciate
`0.0` and `-0.0`. As a result:
- `0.0` and `-0.0` are decoded to the same `0.0` Erlang term.
- `.NaN` and its variants are decoded to the `nan` atom.
- `.Inf` and its variants are decoded to the `positive_infinity` atom.
- `-.Inf` and its variants are decoded to the `negative_infinity` atom.

## Alias support
A YAML document is the representation of a directed graph, where nodes are
connected using a mechanism of aliases and anchors.

While YAML documents could be represented using a graph data structure, e.g.
with the `digraph` module, it would be impractical for real world use cases.
Instead of having to walk the graph every time a value has to be extracted, it
makes sense to resolve aliases and return documents as trees.

YAML does not forbid self-referencing nodes, e.g. `[&a [*a]]`, which is a
problem because Erlang cannot represent circular data structures. As a result,
we do not support these constructions. In this very example, the `*a` alias
will result in an error.
