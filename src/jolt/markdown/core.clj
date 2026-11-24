;; jolt.markdown.core: a minimal, renderer-agnostic markdown parser: block
;; structure (headings, quotes, bullets, numbered items, fenced code) plus
;; inline spans (**strong**, *em*, _em_, ~~strike~~, `code`, [label](url)), and
;; span-aware wrapping. No I/O, no terminal dependency — faraday's chat surface
;; renders these blocks today; a future web/console renderer can reuse them.
(ns jolt.markdown.core
  "Minimal markdown -> blocks and inline spans.

  A span is {:text \"..\" :style #{...}} with style ⊂ #{:strong :em :strike :code
  :link}; a link span also carries :href. A block is one of
    {:type :heading  :level n :spans [span ...]}
    {:type :quote    :spans [span ...]}
    {:type :bullet   :spans [span ...]}
    {:type :numbered :n k :spans [span ...]}
    {:type :text     :spans [span ...]}
    {:type :code     :lines [\"..\" ..] :lang \"..\"}
  Parse is line-oriented: consecutive prose lines stay separate blocks, and
  fenced code is one block of verbatim lines. wrap-spans reflows a span
  sequence to a column width without losing span styles across breaks."
  (:require [clojure.string :as str]))

(defn spans-text
  "The plain text of a span sequence, for previews and line counts."
  [spans]
  (apply str (map :text spans)))

;; -- inline -----------------------------------------------------------------

(defn- word-char? [c]
  (boolean (re-find #"\w" (str c))))

(def ^:private inline-rules
  "Ordered [regex style] pairs tried at each position; strong before em so
  ** wins over the * rules, and the em rules run last."
  [[#"^\*\*(.+?)\*\*" :strong]
   [#"^~~(.+?)~~" :strike]
   [#"^`([^`]+?)`" :code]
   [#"^\[([^\]]+?)\]\(([^)]*)\)" :link]
   [#"^\*(?!\s)([^*]+?)(?<!\s)\*" :em]
   [#"^_(?!\s)([^_]+?)(?<!\s)_(?![\w])" :em]])

(defn- try-inline [s pos]
  (let [before-ok (or (zero? pos) (not (word-char? (nth s (dec pos)))))]
    (some (fn [[re style]]
            ;; em needs a word boundary before the marker so snake_case and
            ;; a*b*c stay literal; the other rules are unambiguous.
            (when (or (not= :em style) before-ok)
              (let [m (re-matcher re s)]
                (.region m pos (count s))
                (when (.lookingAt m)
                  (let [text (.group m 1)
                        href (when (= :link style) (.group m 2))]
                    {:end (.end m) :text text :style style :href href}))))) 
          inline-rules)))

(defn parse-inline
  "Parse inline markdown into spans. Plain runs carry :style nil; each styled
  span carries a #{...} style set and, for links, the :href."
  [text]
  (let [s (str text)
        n (count s)]
    (loop [pos 0 out [] plain ""]
      (if (>= pos n)
        (cond-> out (seq plain) (conj {:text plain :style nil}))
        (if-let [{:keys [end text style href]} (try-inline s pos)]
          (let [out (cond-> out (seq plain) (conj {:text plain :style nil}))]
            (recur end
                   (conj out (cond-> {:text text :style #{style}}
                               href (assoc :href href)))
                   ""))
          (recur (inc pos) out (str plain (subs s pos (inc pos)))))))))

;; -- blocks -----------------------------------------------------------------

(defn parse-blocks
  "Parse markdown into blocks, one per significant line. Fenced code is one
  block with verbatim lines and the fence's :lang; blank prose lines are
  dropped. Inline syntax inside prose is parsed to :spans."
  [text]
  (let [in-code (volatile! false)
        lang (volatile! nil)
        buf (volatile! [])
        flush! (fn []
                 (let [ls (vec @buf)
                       l @lang]
                   (vreset! buf [])
                   (vreset! lang nil)
                   (when (seq ls) [{:type :code :lines ls :lang l}])))]
    (vec
     (concat
      (mapcat
       (fn [line]
         (let [t (str/trim line)]
           (cond
             (str/starts-with? t "```")
             (if @in-code
               (do (vreset! in-code false) (flush!))
               (do (vreset! in-code true)
                   (vreset! lang (not-empty (str/trim (subs t 3))))
                   []))

             @in-code
             (do (vswap! buf conj line) [])

             (str/starts-with? t "#")
             (let [hashes (re-find #"^#+" t)]
               [{:type :heading :level (count hashes)
                 :spans (parse-inline (str/trim (subs t (count hashes))))}])

             (str/starts-with? t "> ")
             [{:type :quote :spans (parse-inline (str/trim (subs t 2)))}]

             (or (str/starts-with? t "- ")
                 (str/starts-with? t "* ")
                 (str/starts-with? t "+ "))
             [{:type :bullet :spans (parse-inline (str/trim (subs t 2)))}]

             (re-find #"^\d+\.\s" t)
             (let [nstr (re-find #"^\d+" t)]
               [{:type :numbered :n (Long/parseLong nstr)
                 :spans (parse-inline (str/trim (subs t (inc (str/index-of t ".")))))}])

             :else
             [{:type :text :spans (parse-inline line)}])))
       (str/split-lines (str text)))
      (when @in-code (flush!))))))

;; -- wrapping ---------------------------------------------------------------

(defn- wrap-seg
  "Ranges [start end] into `s` for one line segment at offset `base`, mirroring
  a greedy word-wrap: leading spaces dropped, one space dropped at each soft
  break, over-long words hard-broken."
  [seg base w]
  (if (<= (count seg) w)
    [[base (+ base (count seg))]]
    (let [t (str/triml seg)
          lead (- (count seg) (count t))]
      (loop [rem t off (+ base lead) acc []]
        (let [n (count rem)]
          (if (<= n w)
            (conj acc [off (+ off n)])
            (let [chunk (subs rem 0 w)
                  cut (str/last-index-of chunk " ")]
              (if (and cut (pos? cut))
                (recur (subs rem (inc cut)) (+ off (inc cut))
                       (conj acc [off (+ off cut)]))
                (recur (subs rem w) (+ off w)
                       (conj acc [off (+ off w)]))))))))))

(defn- wrap-ranges
  "Line ranges [start end] of `s` wrapped to width `w`."
  [s w]
  (let [segs (str/split s #"\n" -1)]
    (loop [segs (seq segs) base 0 acc []]
      (if-not segs
        acc
        (let [seg (first segs)
              n (count seg)]
          (recur (next segs)
                 (+ base n 1)
                 (into acc (wrap-seg seg base w))))))))

(defn- slice-spans
  "The span fragments covering [start end) of the flat span text."
  [spans offs start end]
  (loop [i 0 out []]
    (if (>= i (count spans))
      out
      (let [soff (offs i)
            slen (count (:text (spans i)))
            a (max start soff)
            b (min end (+ soff slen))]
        (if (< a b)
          (recur (inc i)
                 (conj out (assoc (spans i)
                                  :text (subs (:text (spans i)) (- a soff) (- b soff)))))
          (recur (inc i) out))))))

(defn wrap-spans
  "Wrap `spans` (a flat sequence of {:text ..} maps, optionally :style/:href)
  into lines no wider than `width` cells, reflowing at spaces like a word-wrap
  and hard-breaking words longer than the width. A split span keeps its style
  on both fragments; leading spaces and the space at each soft break are
  dropped. Returns a vector of lines, each a vector of span maps."
  [spans width]
  (let [spans (vec (remove #(empty? (:text %)) spans))]
    (if (empty? spans)
      []
      (let [s (apply str (map :text spans))
            offs (vec (reductions + 0 (map #(count (:text %)) spans)))]
        (mapv (fn [[a b]] (slice-spans spans offs a b))
              (wrap-ranges s (max 1 (long width))))))))
