# jolt-markdown

A minimal, renderer-agnostic markdown parser for jolt: block structure plus
inline spans, and span-aware wrapping. No I/O, no terminal dependency, so any
surface (terminal, web, console) can render the same blocks its own way.

## Use

```clojure
;; deps.edn — :local/root today; pin a :git/sha once pushed
{:deps {racklr-dev/jolt-markdown {:local/root "../jolt-markdown"}}}
```

```clojure
(require '[jolt.markdown.core :as md])

(md/parse-inline "**bold** and `code`")   ; -> spans with :style #{:strong} / #{:code}
(md/parse-blocks  "# Title\n- item\n```\nx\n```")
(md/wrap-spans (md/parse-inline "some prose") 80)
(md/spans-text spans)                      ; plain text of spans
```

## API

- `parse-inline` — inline markdown → spans (`:strong :em :strike :code :link`;
  link spans carry `:href`). Word-boundary guards keep `snake_case` and
  `a * b * c` literal.
- `parse-blocks` — markdown → blocks: `:heading :quote :bullet :numbered :text
  :code` (code carries verbatim `:lines` and the fence `:lang`).
- `wrap-spans` — reflow spans to a column width, keeping styles across breaks.
- `spans-text` — plain text of spans.

## License

EPL-2.0.
