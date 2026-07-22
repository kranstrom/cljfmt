# Line Breaks

## Overview

cljfmt provides rules for enforcing line breaks within specific forms
according to the [Clojure Style Guide](https://guide.clojure.style/).
Any function or macro that requires specific line breaking behavior can
be defined using the `:line-breaks` option.

To enable line break formatting, the `:line-breaking?`
option must be set to `true`. One can subsequently override/extend
these by supplying a map to the `:line-breaks` option.

The `:line-breaks` option will **replace** all default line break rules,
while the `:extra-line-breaks` option will **add** to them.

For example:

```clojure
{:line-breaks {if [[:consistent {:max-children 4}]]}}
```

The key can be a symbol or a regular expression.

## Rule Types

### `:inner` Modifier

Any rule can have `:inner` appended to it (before the options map) to
apply the line break rule to a specific child node rather than the form
itself. The `index` of the rule determines which child node is targeted.

For example, `let` applies the `:pairs` rule to its binding vector
(index `0`), not to the `let` form itself:

```clojure
{let [[:pairs 0 :inner]]}
```

### `:always-body`

The `:always-body` rule forces a line break before every child element
starting from a specified index. This is useful for forms where the
body should always be placed on new lines, such as `do` or `catch`.
The rule takes an `index` which specifies where the body begins (e.g.
`1` for `do`, `3` for `catch`).

### `:consistent`

The `:consistent` rule forces all child elements of a form to either be
on a single line or each on their own line. This ensures consistent
layout for short versus long forms.

Options:
* `:max-children` (default `3`): The maximum number of child nodes
  allowed on a single line before forcing line breaks.

### `:pairs`

The `:pairs` rule formats pairs of elements (such as `let` bindings,
`cond` clauses, or map literals). The rule takes an `index` which
specifies where the pairs begin in the form (e.g. `1` for `let`).

Options:
* `:blank-lines?` (default `false`): Inserts an empty blank line between
  each pair. Useful for visually separating complex `cond` clauses.
* `:split-pairs?` (default `false`): Forces the pairs themselves to be
  split across lines (each element of the pair gets its own line).
* `:pair-prefix` (optional): A string to prefix the second element of
  the pair if it is moved to a new line (e.g., `,,` for visual
  alignment).

### `:defn`

The `:defn` rule formats `defn`, `defmacro`, `fn` and similar
multi-arity constructs. It preserves single-line definitions for simple
bodies, while breaking forms with multi-body definitions, docstrings,
attribute maps, and multi-arity lists according to the style guide.

Options:
* `:max-body-forms` (default `1`): The maximum number of forms allowed
  in the body before forcing a line break.
* `:force-args-newline?` (default `false`): Forces the argument vector
  to a new line, even if no docstring or attribute map is present.

### `:ns`

The `:ns` rule formats `ns` declarations, handling its children like
`:require`, `:import`, and `:refer`. It enforces breaking dependencies
onto multiple lines for readability and cleaner diffs for dependency
changes.

Options:
* `:max-dependencies` (default `1`): The maximum number of dependencies
  allowed on a single line.
* `:single-dependency-newline?` (default `false`): Forces a newline
  before the dependency list even if there is only a single dependency.
* `:first-entry-same-line?` (default `false`): Keeps the first required
  namespace on the same line as the `:require` keyword, formatting
  subsequent ones below it.
* `:ns-keywords` (default `#{:import :require :require-macros :use}`):
  The set of keywords recognized as ns reference forms. Override this
  to support custom ns-like macros.

## Defaults

The default line breaks for cljfmt are stored in the following
resources:

* [cljfmt/line_breaks/clojure.clj](../cljfmt/resources/cljfmt/line_breaks/clojure.clj)
