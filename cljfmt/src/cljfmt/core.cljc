(ns cljfmt.core
  #?(:clj (:refer-clojure :exclude [reader-conditional?]))
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.string :as str]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]
            [rewrite-clj.zip :as z])
  #?(:clj (:import java.util.regex.Pattern)
     :cljs (:require-macros [cljfmt.core :refer [read-resource]])))

#?(:clj (def read-resource* (comp read-string slurp io/resource)))
#?(:clj (defmacro read-resource [path] `'~(read-resource* path)))

(def includes?
  #?(:clj  (fn [^String a ^String b] (.contains a b))
     :cljs str/includes?))

#?(:clj
   (defn- find-all [zloc p?]
     (loop [matches []
            zloc zloc]
       (if-let [zloc (z/find-next zloc z/next* p?)]
         (recur (conj matches zloc)
                (z/next* zloc))
         matches))))

(defn- edit-all
  ([zloc p? f]
   (edit-all zloc p? f z/next*))
  ([zloc p? f nextf]
   (loop [zloc (if (p? zloc) (f zloc) zloc)]
     (if-let [zloc (z/find-next zloc nextf p?)]
       (recur (f zloc))
       zloc))))

(defn- transform [form zf & args]
  (z/root (apply zf (z/of-node form) args)))

(defn root? [zloc]
  (nil? (z/up* zloc)))

(defn- top? [zloc]
  (some-> zloc z/up root?))

(defn- root [zloc]
  (if (root? zloc) zloc (recur (z/up zloc))))

(defn- clojure-whitespace? [zloc]
  (z/whitespace? zloc))

(defn- unquote? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :unquote)))

(defn- deref? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :deref)))

(defn- unquote-deref? [zloc]
  (and (deref? zloc)
       (unquote? (z/up* zloc))))

(defn- comment? [zloc]
  (some-> zloc z/node n/comment?))

(defn- surrounding-whitespace? [zloc]
  (and (not (top? zloc))
       (clojure-whitespace? zloc)
       (or (and (nil? (z/left* zloc))
                ;; don't convert ~ @ to ~@
                (not (unquote-deref? (z/right* zloc)))
                ;; ignore space before comments
                (not (comment? (z/right* zloc))))
           (nil? (z/skip z/right* clojure-whitespace? zloc)))))

(defn remove-surrounding-whitespace [form]
  (transform form edit-all surrounding-whitespace? z/remove*))

(defn- element? [zloc]
  (and zloc (not (z/whitespace-or-comment? zloc))))

(defn- reader-macro? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :reader-macro)))

(defn- namespaced-map? [zloc]
  (and zloc (= (n/tag (z/node zloc)) :namespaced-map)))

(defn- missing-whitespace? [zloc]
  (and (element? zloc)
       (not (reader-macro? (z/up* zloc)))
       (not (namespaced-map? (z/up* zloc)))
       (element? (z/right* zloc))))

(defn insert-missing-whitespace [form]
  (transform form edit-all missing-whitespace? z/insert-space-right))

(defn- space? [zloc]
  (= (z/tag zloc) :whitespace))

(defn- line-comment? [zloc]
  (and (comment? zloc) (re-matches #"(?s);;([^;].*)?" (z/string zloc))))

(defn- comma? [zloc]
  (some-> zloc z/node n/comma?))

(defn- line-break? [zloc]
  (or (z/linebreak? zloc) (comment? zloc)))

(defn- skip-whitespace [zloc]
  (z/skip z/next* space? zloc))

(defn- skip-whitespace-and-commas
  ([zloc] (skip-whitespace-and-commas zloc z/next*))
  ([zloc f] (z/skip f (some-fn space? comma?) zloc)))

(defn- skip-clojure-whitespace
  ([zloc] (skip-clojure-whitespace zloc z/next*))
  ([zloc f] (z/skip f clojure-whitespace? zloc)))

(defn- count-newlines [zloc]
  (loop [zloc' zloc, newlines 0]
    (if (z/linebreak? zloc')
      (recur (-> zloc' z/right* skip-whitespace-and-commas)
             (-> zloc' z/string count (+ newlines)))
      (if (comment? (skip-clojure-whitespace zloc z/left*))
        (inc newlines)
        newlines))))

(defn- final-transform-element? [zloc]
  (nil? (skip-clojure-whitespace (z/next* zloc))))

(defn- consecutive-blank-line? [zloc]
  (and (> (count-newlines zloc) 2)
       (not (final-transform-element? zloc))))

(defn- remove-clojure-whitespace [zloc]
  (if (clojure-whitespace? zloc)
    (recur (z/remove* zloc))
    zloc))

(defn- replace-consecutive-blank-lines [zloc]
  (let [zloc-elem-before (-> zloc
                             skip-clojure-whitespace
                             z/prev*
                             remove-clojure-whitespace)]
    (-> zloc-elem-before
        z/next*
        (z/insert-left* (n/newlines (if (comment? zloc-elem-before) 1 2))))))

(defn remove-consecutive-blank-lines [form]
  (transform form edit-all consecutive-blank-line? replace-consecutive-blank-lines))

(defn- indentation? [zloc]
  (and (line-break? (z/left* zloc)) (space? zloc)))

(defn- comment-next? [zloc]
  (-> zloc z/next* skip-whitespace comment?))

(defn- comment-next-other-than-line-comment? [zloc]
  (when-let [znext (-> zloc z/next* skip-whitespace)]
    (and (comment? znext) (not (line-comment? znext)))))

(defn- should-indent? [zloc opts]
  (and (line-break? zloc)
       (if (:indent-line-comments? opts)
         (not (comment-next-other-than-line-comment? zloc))
         (not (comment-next? zloc)))))

(defn- should-unindent? [zloc opts]
  (and (indentation? zloc)
       (if (:indent-line-comments? opts)
         (not (comment-next-other-than-line-comment? zloc))
         (not (comment-next? zloc)))))

(defn unindent
  ([form]
   (unindent form {}))
  ([form opts]
   (transform form edit-all #(should-unindent? % opts) z/remove*)))

(def ^:private start-element
  {:meta "^", :meta* "#^", :vector "[",       :map "{"
   :list "(", :eval "#=",  :uneval "#_",      :fn "#("
   :set "#{", :deref "@",  :reader-macro "#", :unquote "~"
   :var "#'", :quote "'",  :syntax-quote "`", :unquote-splicing "~@"
   :namespaced-map "#"})

(defn- prior-line-string [zloc]
  (loop [zloc     zloc
         worklist '()]
    (if-let [p (z/left* zloc)]
      (let [s            (str (n/string (z/node p)))
            new-worklist (cons s worklist)]
        (if-not (includes? s "\n")
          (recur p new-worklist)
          (apply str new-worklist)))
      (if-let [p (z/up* zloc)]
        ;; newline cannot be introduced by start-element
        (recur p (cons (start-element (n/tag (z/node p))) worklist))
        (apply str worklist)))))

(defn- last-line-in-string ^String [^String s]
  (subs s (inc (.lastIndexOf s "\n"))))

(defn- margin [zloc]
  (-> zloc prior-line-string last-line-in-string count))

(defn- whitespace [width]
  (n/whitespace-node (apply str (repeat width " "))))

(defn- coll-indent [zloc]
  (-> zloc z/leftmost* margin))

(defn- uneval? [zloc]
  (= (z/tag zloc) :uneval))

(defn- index-of [zloc]
  (->> (iterate z/left zloc)
       (remove uneval?)
       (take-while identity)
       (count)
       (dec)))

(defn- meta? [zloc]
  (#{:meta :meta*} (z/tag zloc)))

(defn- skip-meta [zloc]
  (if (meta? zloc)
    (-> zloc z/down z/right recur)
    zloc))

(defn- cursive-two-space-list-indent? [zloc]
  (-> zloc z/leftmost* skip-meta z/tag #{:vector :map :list :set} not))

(defn- zprint-two-space-list-indent? [zloc]
  (-> zloc z/leftmost* z/tag #{:token :list}))

(defn two-space-list-indent? [zloc context]
  (case (:function-arguments-indentation context)
    :community false
    :cursive (cursive-two-space-list-indent? zloc)
    :zprint (zprint-two-space-list-indent? zloc)))

(defn- list-indent [zloc context]
  (if (> (index-of zloc) 1)
    (-> zloc z/leftmost* z/right margin)
    (cond-> (coll-indent zloc)
      (two-space-list-indent? zloc context) inc)))

(def indent-size 2)

(defn- indent-width [zloc]
  (case (z/tag zloc)
    :list indent-size
    :fn   (inc indent-size)))

(defn pattern? [v]
  (instance? #?(:clj Pattern :cljs js/RegExp) v))

#?(:clj
   (defn- top-level-form [zloc]
     (->> zloc
          (iterate z/up)
          (take-while (complement root?))
          last)))

(defn- token? [zloc]
  (= (z/tag zloc) :token))

(defn- ns-token? [zloc]
  (and (token? zloc)
       (= 'ns (z/sexpr zloc))))

(defn- ns-form? [zloc]
  (and (top? zloc)
       (= (z/tag zloc) :list)
       (some-> zloc z/down ns-token?)))

(defn- token-value [zloc]
  (let [zloc (skip-meta zloc)]
    (when (token? zloc) (z/sexpr zloc))))

(defn- reader-conditional? [zloc]
  (and (reader-macro? zloc) (#{"?" "?@"} (-> zloc z/down token-value str))))

(defn- find-next-keyword [zloc]
  (z/find zloc z/right #(n/keyword-node? (z/node %))))

(defn- first-symbol-in-reader-conditional [zloc]
  (when (reader-conditional? zloc)
    (when-let [key-loc (-> zloc z/down z/right z/down find-next-keyword)]
      (when-let [value-loc (-> key-loc z/next skip-meta)]
        (when (token? value-loc)
          (z/sexpr value-loc))))))

(defn- form-symbol [zloc]
  (let [zloc (z/leftmost zloc)
        sym  (or (token-value zloc)
                 (first-symbol-in-reader-conditional zloc))]
    (when (symbol? sym) sym)))

(defn- index-matches-top-argument? [zloc depth idx]
  (and (> depth 0)
       (= (inc idx) (index-of (nth (iterate z/up zloc) depth)))))

(defn- qualify-symbol-by-alias-map [possible-sym alias-map]
  (when-let [ns-str (namespace possible-sym)]
    (symbol (get alias-map ns-str ns-str) (name possible-sym))))

(defn- qualify-symbol-by-refer-map [possible-sym refer-map]
  (when-not (namespace possible-sym)
    (when-let [ns-str (get refer-map (str possible-sym))]
      (symbol ns-str (name possible-sym)))))

(defn- qualify-symbol-by-ns-name [sym ns-name]
  (when ns-name (symbol (name ns-name) (name sym))))

(defn- fully-qualified-symbol [sym context]
  (or (qualify-symbol-by-alias-map sym (:alias-map context))
      (qualify-symbol-by-refer-map sym (:refer-map context))
      (qualify-symbol-by-ns-name sym (:ns-name context))))

(defn- string-matches-key-part? [s key-part]
  (if (pattern? key-part) (re-find key-part s) (= s (name key-part))))

(defn- parts-match-vector-key? [sym-ns sym-name [ns-key name-key]]
  (and (string-matches-key-part? sym-ns ns-key)
       (string-matches-key-part? sym-name name-key)))

(defn- form-matches-key? [zloc key context]
  (when-some [sym (form-symbol zloc)]
    (let [full-sym (fully-qualified-symbol sym context)
          sym-name (name sym)
          sym-ns   (or (some-> full-sym namespace) (namespace sym))]
      (cond
        (vector? key)           (parts-match-vector-key? sym-ns sym-name key)
        (pattern? key)          (re-find key sym-name)
        (qualified-symbol? key) (= key full-sym)
        (symbol? key)           (= (name key) sym-name)))))

(defn- inner-indent [zloc key depth idx context]
  (let [top (nth (iterate z/up zloc) depth)]
    (when (and (z/left zloc)
               (form-matches-key? top key context)
               (or (nil? idx) (index-matches-top-argument? zloc depth idx)))
      (let [zup (z/up zloc)]
        (+ (margin zup) (indent-width zup))))))

(defn- nth-form [zloc n]
  (reduce (fn [z f] (when z (f z)))
          (z/leftmost zloc)
          (repeat n z/right)))

(defn- first-form-in-line? [zloc]
  (and (some? zloc)
       (if-let [zloc (z/left* zloc)]
         (if (space? zloc)
           (recur zloc)
           (or (z/linebreak? zloc) (comment? zloc)))
         true)))

(defn- block-indent [zloc key idx context]
  (when (form-matches-key? zloc key context)
    (let [zloc-after-idx (some-> zloc (nth-form (inc idx)))]
      (if (and (or (nil? zloc-after-idx) (first-form-in-line? zloc-after-idx))
               (> (index-of zloc) idx))
        (inner-indent zloc key 0 nil context)
        (list-indent zloc context)))))

(def default-indents
  (merge (read-resource "cljfmt/indents/clojure.clj")
         (read-resource "cljfmt/indents/compojure.clj")
         (read-resource "cljfmt/indents/fuzzy.clj")))

(def default-aligned-forms
  (read-resource "cljfmt/aligned_forms/clojure.clj"))

(def blank-line-forms
  (read-resource "cljfmt/blank_line_forms/clojure.clj"))

(def default-line-breaks
  (read-resource "cljfmt/line_breaks/clojure.clj"))

(def default-options
  {:alias-map                             {}
   :align-binding-columns?                false
   :align-map-columns?                    false
   :align-single-column-lines?            false
   :aligned-forms                         default-aligned-forms
   :max-column-alignment-gap              nil
   :blank-line-forms                      blank-line-forms
   :blank-lines-separate-alignment?       false
   :extra-aligned-forms                   {}
   :extra-blank-line-forms                {}
   :extra-indents                         {}
   :extra-line-breaks                     {}
   :line-breaks                           default-line-breaks
   :line-breaking?                        false
   :function-arguments-indentation        :community
   :indent-line-comments?                 false
   :indentation?                          true
   :indents                               default-indents
   :normalize-newlines-at-file-end?       false
   :insert-missing-whitespace?            true
   :remove-blank-lines-in-forms?          false
   :remove-consecutive-blank-lines?       true
   :remove-multiple-non-indenting-spaces? false
   :remove-surrounding-whitespace?        true
   :remove-trailing-whitespace?           true
   :sort-ns-references?                   false
   :split-keypairs-over-multiple-lines?   false})

(defmulti ^:private indenter-fn
  (fn [_sym _context [type & _args]] type))

(defmethod indenter-fn :inner [sym context [_ depth idx]]
  (fn [zloc] (inner-indent zloc sym depth idx context)))

(defmethod indenter-fn :block [sym context [_ idx]]
  (fn [zloc] (block-indent zloc sym idx context)))

(defmethod indenter-fn :default [sym context [_]]
  (fn [zloc]
    (when (form-matches-key? zloc sym context)
      (list-indent zloc context))))

(defn- make-indenter [[key opts] context]
  (apply some-fn (map (partial indenter-fn key context) opts)))

(defn- indent-order [[key specs]]
  (let [get-depth (fn [[type depth]] (if (= type :inner) depth 0))
        max-depth (transduce (map get-depth) max 0 specs)
        key-order  (cond
                     (qualified-symbol? key) 0
                     (simple-symbol? key)    1
                     (pattern? key)          2)]
    [(- max-depth) key-order (str key)]))

(defn- custom-indent [zloc indenter context]
  (or (when indenter (indenter zloc))
      (list-indent zloc context)))

(defn- indent-amount [zloc indenter context]
  (let [tag (-> zloc z/up z/tag)
        gp  (-> zloc z/up z/up)]
    (cond
      (reader-conditional? gp) (coll-indent zloc)
      (#{:list :fn} tag)       (custom-indent zloc indenter context)
      (= :meta tag)            (indent-amount (z/up zloc) indenter context)
      :else                    (coll-indent zloc))))

(defn- indent-line [zloc indenter context]
  (let [width (indent-amount zloc indenter context)]
    (if (> width 0)
      (z/insert-right* zloc (whitespace width))
      zloc)))

(defn- find-namespace [zloc]
  (some-> zloc root z/down (z/find z/right ns-form?) z/down z/next z/sexpr))

(defn indent
  ([form]
   (indent form default-indents default-options))
  ([form indents]
   (indent form indents default-options))
  ([form indents opts]
   (let [ns-name (or (::ns-name opts) (find-namespace (z/of-node form)))
         sorted-indents (sort-by indent-order indents)
         context (merge (select-keys opts [:function-arguments-indentation
                                           :alias-map :refer-map])
                        {:ns-name ns-name})
         indenter (some->> (seq sorted-indents)
                           (map #(make-indenter % context))
                           (apply some-fn))]
     (transform form edit-all #(should-indent? % opts)
                #(indent-line % indenter context)))))

(defn- map-key? [zloc]
  (and (z/map? (z/up zloc))
       (even? (index-of zloc))
       (not (uneval? zloc))
       (not (z/whitespace-or-comment? zloc))))

(defn- preceded-by-line-break? [zloc]
  (loop [previous (z/left* zloc)]
    (cond
      (line-break? previous)
      true
      (z/whitespace-or-comment? previous)
      (recur (z/left* previous)))))

(defn- map-key-without-line-break? [zloc]
  (and (map-key? zloc) (z/left zloc) (not (preceded-by-line-break? zloc))))

(defn- insert-newline-left [zloc]
  (z/insert-left* zloc (n/newlines 1)))

(defn split-keypairs-over-multiple-lines [form]
  (transform form edit-all map-key-without-line-break? insert-newline-left))

(defn reindent
  ([form]
   (indent (unindent form)))
  ([form indents]
   (indent (unindent form) indents))
  ([form indents opts]
   (indent (unindent form opts) indents opts)))

(defn final? [zloc]
  (and (nil? (z/right* zloc)) (root? (z/up* zloc))))

(defn- trailing-whitespace? [zloc]
  (and (space? zloc)
       (or (z/linebreak? (z/right* zloc)) (final? zloc))))

(defn remove-trailing-whitespace [form]
  (transform form edit-all trailing-whitespace? z/remove*))

(defn normalize-newlines-at-file-end [s]
  (cond-> (str/trimr s)
    (not (str/blank? s)) (str "\n")))

(defn- replace-with-one-space [zloc]
  (z/replace* zloc (whitespace 1)))

(defn- non-indenting-whitespace? [zloc]
  (and (space? zloc)
       (not (indentation? zloc))
       (not (comment? (z/right* zloc)))))

(defn remove-multiple-non-indenting-spaces [form]
  (transform form edit-all non-indenting-whitespace? replace-with-one-space))

(def ^:private ns-reference-symbols
  #{:import :require :require-macros :use})

(defn- ns-reference? [zloc]
  (and (z/list? zloc)
       (some-> zloc z/up ns-form?)
       (-> zloc z/sexpr first ns-reference-symbols)))

(defn- re-indexes [re s]
  (let [matcher    #?(:clj  (re-matcher re s)
                      :cljs (js/RegExp. (.-source re) "g"))
        next-match #?(:clj  #(when (.find matcher)
                               [(.start matcher) (.end matcher)])
                      :cljs #(when-let [result (.exec matcher s)]
                               [(.-index result) (.-lastIndex matcher)]))]
    (take-while some? (repeatedly next-match))))

(defn- re-seq-matcher [re charmap coll]
  {:pre (every? charmap coll)}
  (let [s (apply str (map charmap coll))
        v (vec coll)]
    (for [[start end] (re-indexes re s)]
      {:value (subvec v start end)
       :start start
       :end   end})))

(defn- find-elements-with-comments [nodes]
  (re-seq-matcher #"(CNS*)*E(S*C)?"
                  #(case (n/tag %)
                     (:whitespace :comma) \S
                     :comment \C
                     :newline \N
                     \E)
                  nodes))

(defn- splice-into [coll splices]
  (letfn [(splice [v i splices]
            (when-let [[{:keys [value start end]} & splices] (seq splices)]
              (lazy-cat (subvec v i start) value (splice v end splices))))]
    (splice (vec coll) 0 splices)))

(defn- add-newlines-after-comments [nodes]
  (mapcat #(if (n/comment? %) [% (n/newlines 1)] [%]) nodes))

(defn- remove-newlines-after-comments [nodes]
  (mapcat #(when-not (and %1 (n/comment? %1) (n/linebreak? %2)) [%2])
          (cons nil nodes)
          nodes))

(defn- sort-node-arguments-by [f nodes]
  (let [nodes  (add-newlines-after-comments nodes)
        args   (rest (find-elements-with-comments nodes))
        sorted (sort-by f (map :value args))]
    (->> sorted
         (map #(assoc %1 :value %2) args)
         (splice-into nodes)
         (remove-newlines-after-comments))))

(defn- update-children [zloc f]
  (let [node (z/node zloc)]
    (z/replace zloc (n/replace-children node (f (n/children node))))))

(defn- nodes-string [nodes]
  (apply str (map n/string nodes)))

(defn- remove-node-metadata [nodes]
  (mapcat #(if (= (n/tag %) :meta)
             (rest (n/children %))
             [%])
          nodes))

(defn- node-sort-string [nodes]
  (-> (remove (some-fn n/comment? n/whitespace?) nodes)
      (remove-node-metadata)
      (nodes-string)
      (str/replace #"[\[\]\(\)\{\}]" "")
      (str/trim)
      (str/lower-case)))

(defn sort-arguments [zloc]
  (update-children zloc #(sort-node-arguments-by node-sort-string %)))

(defn sort-ns-references [form]
  (transform form edit-all ns-reference? sort-arguments))

(defn- reduce-columns [zloc f init]
  (loop [zloc zloc, col 0, acc init]
    (if-some [zloc (skip-whitespace-and-commas zloc z/right*)]
      (if (line-break? zloc)
        (recur (z/right* zloc) 0 acc)
        (recur (z/right* zloc) (inc col) (f zloc col acc)))
      acc)))

(defn- count-columns [zloc]
  (inc (reduce-columns zloc #(max %2 %3) 0)))

(defn- trailing-commas [zloc]
  (let [right (z/right* zloc)]
    (if (and right (comma? right))
      (-> right z/node n/string)
      "")))

(defn- node-end-position [zloc]
  (let [lines (str (prior-line-string zloc)
                   (n/string (z/node zloc))
                   (trailing-commas zloc))]
    (transduce (comp (remove #(str/starts-with? % ";"))
                     (map count))
               max 0 (str/split lines #"\r?\n"))))

(defn- single-column-line? [zloc]
  (and (let [zloc (skip-whitespace-and-commas (z/right* zloc) z/right*)]
         (or (nil? zloc) (line-break? zloc)))
       (line-break? (skip-whitespace-and-commas (z/left* zloc) z/left*))))

(defn- node-str-length [zloc]
  (-> zloc z/node n/string count))

(defn- update-space-left [zloc delta]
  (let [left (z/left* zloc)]
    (cond
      (space? left) (let [n (max 0 (+ delta (node-str-length left)))]
                      (z/right* (z/replace* left (n/spaces n))))
      (pos? delta)  (z/insert-space-left zloc delta)
      :else         zloc)))

(defn- nil-if-end [zloc]
  (when (and zloc (not (z/end? zloc))) zloc))

(defn- skip-to-next-line [zloc]
  (->> zloc (z/skip z/next* (complement line-break?)) z/next nil-if-end))

(defn- pad-inside-node [zloc padding]
  (if-some [zloc (z/down zloc)]
    (loop [zloc zloc]
      (if-some [zloc (skip-to-next-line zloc)]
        (recur (update-space-left zloc padding))
        zloc))
    zloc))

(defn- pad-node [zloc padding]
  (-> (update-space-left zloc padding)
      (z/subedit-> (pad-inside-node padding))))

(defn- count-spaces [zloc]
  (if (space? zloc) (node-str-length zloc) 0))

(defn- pad-to-position [zloc start-position {max-gap :max-column-alignment-gap}]
  {:pre [(or (nil? max-gap) (pos-int? max-gap))]}
  (let [delta (- start-position (margin zloc))]
    (if max-gap
      (let [old-gap (count-spaces (z/left* zloc))
            new-gap (+ old-gap delta)]
        (pad-node zloc (if (> new-gap max-gap) (- 1 old-gap) delta)))
      (pad-node zloc delta))))

(defn- edit-column [zloc column f]
  (loop [zloc zloc, col 0]
    (if-some [zloc (skip-whitespace-and-commas zloc z/right*)]
      (let [zloc (if (and (= col column) (not (line-break? zloc)))
                   (f zloc)
                   zloc)
            col  (if (line-break? zloc) 0 (inc col))]
        (if-some [zloc (z/right* zloc)]
          (recur zloc col)
          zloc))
      zloc)))

(defn- end-of-column-group? [zloc]
  (line-break? zloc) (> (count-newlines zloc) 1))

(defn- find-start-of-column-group [zloc]
  (z/skip z/left* #(some-> % z/left* end-of-column-group? not) zloc))

(defn- reduce-column-group [zloc f init]
  (loop [zloc (find-start-of-column-group zloc)
         col 0, acc init]
    (let [zloc (skip-whitespace-and-commas zloc z/right*)]
      (if (or (nil? zloc) (end-of-column-group? zloc))
        acc
        (if (line-break? zloc)
          (recur (z/right* zloc) 0 acc)
          (recur (z/right* zloc) (inc col) (f zloc col acc)))))))

(defn- column-start-position [zloc col opts]
  (let [reduce-fn (if (:blank-lines-separate-alignment? opts)
                    reduce-column-group
                    reduce-columns)
        maximizer (fn [zloc c max-pos]
                    (if (and (= c (dec col))
                             (or (:align-single-column-lines? opts)
                                 (not (single-column-line? zloc))))
                      (max max-pos (node-end-position zloc))
                      max-pos))]
    (inc (reduce-fn zloc maximizer 0))))

(defn- align-one-column
  [zloc col {:keys [blank-lines-separate-alignment?] :as opts}]
  (if-some [zloc (z/down zloc)]
    (let [start-position-fn (if blank-lines-separate-alignment?
                              #(column-start-position % col opts)
                              (constantly
                               (column-start-position zloc col opts)))
          edit-column-fn    #(pad-to-position % (start-position-fn %) opts)]
      (z/up (edit-column zloc col edit-column-fn)))
    zloc))

(defn- align-columns [zloc opts]
  (reduce #(align-one-column %1 %2 opts)
          zloc
          (-> zloc z/down count-columns range rest)))

(defn align-map-columns
  ([form]
   (align-map-columns form default-options))
  ([form opts]
   (transform form edit-all z/map? #(align-columns % opts))))

(defn- matching-form-index? [zloc [k indexes] context]
  (if (= :all indexes)
    (and (or (z/list? zloc)
             (= (z/tag zloc) :fn))
         (form-matches-key? (z/down zloc) k context))
    (and (z/list? (z/up zloc))
         (form-matches-key? zloc k context)
         (contains? (set indexes) (dec (index-of zloc))))))

(defn- format-children [zloc start-idx format-fn]
  (loop [z zloc
         idx 0
         curr (z/down zloc)]
    (if-not curr
      z
      (if (< idx start-idx)
        (recur z (inc idx) (z/right curr))
        (let [curr' (format-fn idx curr)]
          (recur (z/up curr') (inc idx) (z/right curr')))))))

(defn- preceding-whitespaces [z]
  (->> (iterate z/left* z)
       (next)
       (take-while #(some-> % z/tag #{:whitespace :newline :comment}))))

(defn- ensure-blank-line-before [z]
  (let [lefts (preceding-whitespaces z)
        newlines (count (filter #(re-find #"\n" (z/string %)) lefts))]
    (cond
      (>= newlines 2) z
      (= newlines 1) (z/insert-left z (n/newlines 1))
      :else (z/insert-left z (n/newlines 2)))))

(defn- ensure-newline-before [z]
  (let [lefts (preceding-whitespaces z)]
    (if (or (some #(re-find #"\n" (z/string %)) lefts)
            (some #(= :comment (z/tag %)) lefts))
      z
      (let [left (z/left* z)]
        (if (and left (= :whitespace (z/tag left)))
          (z/right (z/replace left (n/newlines 1)))
          (z/insert-left z (n/newlines 1)))))))

(defn- ensure-newline-with-prefix [z prefix]
  (let [lefts (->> (iterate z/left* z)
                   (next)
                   (take-while #(some-> % z/tag #{:whitespace :newline :comment :comma})))]
    (if (some #(and (= :comma (z/tag %)) (= prefix (z/string %))) lefts)
      (ensure-newline-before z)
      (let [z' (ensure-newline-before z)]
        (z/insert-left* z' (n/comma-node prefix))))))

;; Replaces preceding whitespace with a single space.
;; Implicitly relies on the indentation engine running *after* line-breaking
;; in the formatting pipeline to fix the single space into correct structural indentation.
(defn- remove-newline-before [zloc]
  (let [lefts (->> (iterate z/left* zloc)
                   (next)
                   (take-while #(some-> % z/tag #{:whitespace :newline})))]
    (if (seq lefts)
      (let [first-ws (last lefts)
            rest-ws (butlast lefts)
            z-after-replace (z/replace first-ws (n/spaces 1))]
        (loop [z z-after-replace
               to-remove (count rest-ws)]
          (if (pos? to-remove)
            (recur (z/remove* (z/right* z)) (dec to-remove))
            (or (z/right* z) zloc))))
      zloc)))

(defn- meaningful-siblings [zloc]
  (when zloc
    (->> (iterate z/right zloc)
         (take-while identity)
         (remove #(or (z/whitespace? %) (= :newline (z/tag %))))
         (remove uneval?))))

(defn- meaningful-children [zloc]
  (meaningful-siblings (z/down zloc)))

(defn- has-newline-before? [zloc]
  (when zloc
    (boolean (some #(re-find #"\n" (z/string %)) (preceding-whitespaces zloc)))))

(defn- apply-consistent-rule [zloc start-idx opts]
  (let [args (drop (inc start-idx) (meaningful-children zloc))]
    (if (empty? args)
      zloc
      (let [needs-newlines? (or (some #(re-find #"\n" (z/string %)) args)
                                (some has-newline-before? args)
                                (> (count args) (:max-children opts 3)))]
        (format-children zloc (inc start-idx)
                         (fn [_ curr]
                           (if needs-newlines?
                             (ensure-newline-before curr)
                             (remove-newline-before curr))))))))

(defn- apply-pairs-rule [zloc start-idx opts]
  (let [blank-lines? (:blank-lines? opts false)
        split-pairs? (:split-pairs? opts false)
        pair-prefix  (:pair-prefix opts)
        list-form? (z/list? zloc)]
    (format-children zloc (inc start-idx)
                     (fn [idx curr]
                       (let [diff (- idx start-idx)
                             is-pair-start (if list-form? (odd? diff) (even? diff))
                             is-first-pair (= diff (if list-form? 1 0))
                             should-break? (or is-pair-start split-pairs?)]
                         (if should-break?
                           (if (and blank-lines? is-pair-start (not is-first-pair))
                             (ensure-blank-line-before curr)
                             (if (and pair-prefix (not is-pair-start))
                               (ensure-newline-with-prefix curr pair-prefix)
                               (ensure-newline-before curr)))
                           curr))))))

(defn- format-arity-list [zloc opts]
  (let [body-forms (next (meaningful-children zloc))
        needs-newlines? (or (> (count body-forms) (:max-body-forms opts 1))
                            (re-find #"\n" (z/string zloc)))]
    (format-children zloc 1
                     (fn [_ curr]
                       (if needs-newlines?
                         (ensure-newline-before curr)
                         (remove-newline-before curr))))))

(defn- apply-defn-rule [zloc opts]
  (let [all-children (meaningful-children zloc)
        header-nodes (take-while #(not (or (z/vector? %) (z/list? %))) all-children)
        has-docstring? (some #(and (= :token (z/tag %)) (str/starts-with? (z/string %) "\"")) header-nodes)
        has-attr-map? (boolean (some z/map? header-nodes))
        has-meta? (or has-docstring? has-attr-map?)
        args-node (first (filter z/vector? all-children))
        body-forms (if args-node
                     (next (meaningful-siblings args-node))
                     (drop 2 all-children)) ; for multi-arity, everything after name
        needs-newlines? (or has-meta?
                            (:force-args-newline? opts false)
                            (> (count body-forms) (:max-body-forms opts 1))
                            (re-find #"\n" (z/string zloc)))
        args-has-nl? (has-newline-before? args-node)
        first-body-has-nl? (has-newline-before? (first body-forms))
        preserve-args-nl? (and args-has-nl? first-body-has-nl?)]
    (format-children zloc 2
                     (fn [_ curr]
                       (cond
                         (and (= :token (z/tag curr)) (str/starts-with? (z/string curr) "\"")) (ensure-newline-before curr) ; docstring
                         (z/map? curr) (ensure-newline-before curr) ; attr-map
                         (and args-node (= (z/node curr) (z/node args-node)))
                         (if (or has-meta? preserve-args-nl?)
                           (ensure-newline-before curr)
                           (remove-newline-before curr))
                         :else
                         (let [formatted-curr (if (and (not args-node) (z/list? curr))
                                                (format-arity-list curr opts)
                                                curr)]
                           (if needs-newlines? (ensure-newline-before formatted-curr) formatted-curr)))))))

(defn- apply-ns-rule [zloc opts]
  (format-children zloc 2
                   (fn [_ curr]
                     (let [curr' (ensure-newline-before curr)
                           inner-z (z/down curr')
                           inner-kw-str (and inner-z (= :token (z/tag inner-z)) (z/string inner-z))
                           inner-children (meaningful-siblings inner-z)
                           ns-kws (:ns-keywords opts ns-reference-symbols)]
                       (if (and inner-kw-str (str/starts-with? inner-kw-str ":") (ns-kws (keyword (subs inner-kw-str 1))))
                         (let [break? (or (> (count inner-children) (inc (:max-dependencies opts 1)))
                                          (and (:single-dependency-newline? opts)
                                               (= (count inner-children) 2)))]
                           (format-children curr' 1
                                            (fn [idx icurr]
                                              (if (or (z/whitespace? icurr) (= :newline (z/tag icurr)))
                                                icurr
                                                (if break?
                                                  (if (and (:first-entry-same-line? opts) (= idx 1))
                                                    (remove-newline-before icurr)
                                                    (ensure-newline-before icurr))
                                                  (remove-newline-before icurr))))))
                         curr')))))

(defn- apply-always-body-rule [zloc start-idx _opts]
  (let [start-idx (or start-idx 2)]
    (format-children zloc start-idx
                     (fn [_ curr]
                       (ensure-newline-before curr)))))

(defn- line-breaker-fn [_sym _context rule]
  (let [type (first rule)
        opts (if (map? (last rule)) (last rule) {})]
    (case type
      :consistent  (fn [zloc] (apply-consistent-rule zloc (second rule) opts))
      :pairs       (fn [zloc] (apply-pairs-rule zloc (second rule) opts))
      :defn        (fn [zloc] (apply-defn-rule zloc opts))
      :ns          (fn [zloc] (apply-ns-rule zloc opts))
      :always-body (fn [zloc] (apply-always-body-rule zloc (second rule) opts))
      (constantly nil))))

(defn- compile-single-rule [[key opts] context]
  (let [fns (map (fn [rule]
                   (let [inner? (= :inner (if (map? (last rule)) (last (butlast rule)) (last rule)))
                         idx (second rule)
                         base-fn (line-breaker-fn key context rule)]
                     (if inner?
                       (fn [zloc]
                         (if-let [inner-z (nth (meaningful-children zloc) (inc idx) nil)]
                           (if-let [res (base-fn inner-z)]
                             (z/up res)
                             zloc)
                           zloc))
                       base-fn)))
                 opts)]
    (fn [zloc]
      (reduce (fn [z f]
                (if-let [res (f z)]
                  res
                  z))
              zloc
              fns))))

(defn- compile-line-breakers [rules context]
  (let [exact-rules    (into {} (filter #(or (symbol? (key %)) (keyword? (key %))) rules))
        pattern-rules  (filter #(or (pattern? (key %)) (vector? (key %))) rules)
        exact-breakers (into {} (map (fn [[k opts]]
                                       [k (compile-single-rule [k opts] context)])
                                     exact-rules))
        pattern-breakers (map (fn [[k opts]]
                                [k (compile-single-rule [k opts] context)])
                              pattern-rules)]
    (fn [zloc]
      (if (or (z/list? zloc) (z/vector? zloc))
        (if-some [sym (form-symbol (z/down zloc))]
          (let [full-sym (fully-qualified-symbol sym context)
                sym-name (name sym)
                sym-ns   (or (some-> full-sym namespace) (namespace sym))
                breaker (or (get exact-breakers sym)
                            (get exact-breakers full-sym)
                            (some (fn [[k b]]
                                    (when (if (vector? k)
                                            (parts-match-vector-key? sym-ns sym-name k)
                                            (re-find k sym-name))
                                      b))
                                  pattern-breakers))]
            (if breaker
              (breaker zloc)
              zloc))
          zloc)
        zloc))))

(defn- get-line-break-rules [opts]
  (merge (:line-breaks opts) (:extra-line-breaks opts)))

(defn- enforce-line-breaks [form opts]
  (let [rules (get-line-break-rules opts)
        ns-name (or (::ns-name opts) (find-namespace (z/of-node form)))
        context {:alias-map (:alias-map opts)
                 :refer-map (:refer-map opts)
                 :ns-name ns-name}
        breaker (compile-line-breakers rules context)]
    (transform form edit-all #(or (z/list? %) (z/vector? %)) breaker)))

(defn- matching-form? [zloc form-indexes context]
  (and (or (z/list? zloc)
           (= (z/tag zloc) :fn)
           (z/list? (z/up zloc)))
       (some #(matching-form-index? zloc % context) form-indexes)))

(defn align-form-columns [form aligned-forms opts]
  (let [ns-name  (or (::ns-name opts) (find-namespace (z/of-node form)))
        context  {:alias-map (:alias-map opts)
                  :refer-map (:refer-map opts)
                  :ns-name ns-name}
        aligned? #(matching-form? % aligned-forms context)]
    (transform form edit-all aligned? #(align-columns % opts))))

(defn realign-form
  "Realign a rewrite-clj form such that the columns line up into columns."
  ([form]
   (realign-form form default-options))
  ([form opts]
   (-> form z/of-node (align-columns opts) z/root)))

(defn- unalign-from-space [zloc]
  (pad-node (z/right* zloc) (- 1 (node-str-length zloc))))

(defn unalign-form
  "Remove any consecutive non-indenting whitespace within the form."
  [form]
  (-> form z/of-node z/down
      (edit-all non-indenting-whitespace? unalign-from-space z/right*)
      z/root))

(defn- blank-line-in-form? [zloc blank-line-forms context]
  (and (z/linebreak? zloc)
       (> (count-newlines zloc) 1)
       (not (z/map? (z/up zloc)))
       (not (root? (z/up zloc)))
       (not (matching-form? (z/up zloc) blank-line-forms context))))

(defn- replace-with-single-newline [zloc]
  (z/replace zloc (n/newline-node "\n")))

(defn remove-blank-lines-in-forms [form blank-line-forms opts]
  (let [ns-name     (or (::ns-name opts) (find-namespace (z/of-node form)))
        context     {:alias-map (:alias-map opts)
                     :refer-map (:refer-map opts)
                     :ns-name ns-name}
        blank-line? #(blank-line-in-form? % blank-line-forms context)]
    (transform form edit-all blank-line? replace-with-single-newline)))

#?(:clj
   (defn- ns-require-form? [zloc]
     (and (some-> zloc top-level-form ns-form?)
          (some-> zloc z/child-sexprs first (= :require)))))

#?(:clj
   (defn- as-keyword? [zloc]
     (and (= :token (z/tag zloc))
          (= :as (z/sexpr zloc)))))

#?(:clj
   (defn- refer-keyword? [zloc]
     (and (= :token (z/tag zloc))
          (= :refer (z/sexpr zloc)))))

#?(:clj
   (defn- symbol-node? [zloc]
     (some-> zloc z/node n/symbol-node?)))

#?(:clj
   (defn- leftmost-symbol [zloc]
     (some-> zloc z/leftmost (z/find (comp symbol-node? skip-meta)))))

#?(:clj (defn- ns-require-form-parent [grandparent-node]
          (when-not (ns-require-form? grandparent-node)
            (when (or (z/vector? grandparent-node)
                      (z/list? grandparent-node))
              (some-> (z/find (-> grandparent-node z/down skip-meta)
                              (comp skip-meta z/right)
                              symbol-node?)
                      z/sexpr)))))

#?(:clj (defn- join-ns-str [parent-namespace current-ns]
          (if parent-namespace
            (format "%s.%s" parent-namespace current-ns)
            (str current-ns))))

#?(:clj
   (defn- refer-zloc->refer-mapping [refer-zloc]
     (let [refers           (some-> refer-zloc
                                    (z/find-next (comp skip-meta z/right)
                                                 (some-fn z/vector? z/list?))
                                    z/sexpr)
           current-ns       (some-> refer-zloc leftmost-symbol z/sexpr)
           grandparent-node (some-> refer-zloc
                                    (z/find-next z/up (complement meta?))
                                    (z/find-next z/up (complement meta?)))
           parent-ns        (ns-require-form-parent grandparent-node)]
       (when (and (sequential? refers) (symbol? current-ns))
         (let [ns-str (join-ns-str parent-ns current-ns)]
           (->> refers (map (fn [sym] [(str sym) ns-str])) (into {})))))))

#?(:clj (defn- refer-map-for-form [form]
          (when-let [req-zloc (-> form z/of-node (z/find z/next ns-require-form?))]
            (->> (find-all req-zloc refer-keyword?)
                 (map refer-zloc->refer-mapping)
                 (apply merge)))))

#?(:clj
   (defn- as-zloc->alias-mapping [as-zloc]
     (let [alias            (some-> as-zloc
                                    (z/find-next (comp skip-meta z/right)
                                                 symbol-node?)
                                    z/sexpr)
           current-ns       (some-> as-zloc leftmost-symbol z/sexpr)
           grandparent-node (some-> as-zloc
                                    (z/find-next z/up (complement meta?))
                                    (z/find-next z/up (complement meta?)))
           parent-ns        (ns-require-form-parent grandparent-node)]
       (when (and (symbol? alias) (symbol? current-ns))
         {(str alias) (join-ns-str parent-ns current-ns)}))))

#?(:clj
   (defn- alias-map-for-form [form]
     (when-let [req-zloc (-> form z/of-node (z/find z/next ns-require-form?))]
       (->> (find-all req-zloc as-keyword?)
            (map as-zloc->alias-mapping)
            (apply merge)))))

(defn- stringify-map [m]
  (into {} (map (fn [[k v]] [(str k) (str v)])) m))

(defn reformat-form
  "Reformats a rewrite-clj form data structure. Accepts a map of
  [formatting options][1]. See also: [[reformat-string]].

  [1]: https://github.com/weavejester/cljfmt#formatting-options"
  ([form]
   (reformat-form form {}))
  ([form options]
   (let [opts      (merge default-options options)
         indents   (merge (:indents opts) (:extra-indents opts))
         aligned   (merge (:aligned-forms opts) (:extra-aligned-forms opts))
         blank     (merge (:blank-line-forms opts)
                          (:extra-blank-line-forms opts))
         alias-map #?(:clj  (merge (alias-map-for-form form)
                                   (stringify-map (:alias-map opts)))
                      :cljs (stringify-map (:alias-map opts)))
         refer-map #?(:clj  (merge (refer-map-for-form form)
                                   (stringify-map (:refer-map opts)))
                      :cljs (stringify-map (:refer-map opts)))
         ns-name   (find-namespace (z/of-node form))
         opts      (assoc opts :refer-map refer-map :alias-map alias-map
                          ::ns-name ns-name)]
     (-> form
         (cond-> (:sort-ns-references? opts)
           sort-ns-references)
         (cond-> (:split-keypairs-over-multiple-lines? opts)
           split-keypairs-over-multiple-lines)
         (cond-> (and (:line-breaking? opts) (:line-breaks opts))
           (enforce-line-breaks opts))
         (cond-> (:remove-consecutive-blank-lines? opts)
           remove-consecutive-blank-lines)
         (cond-> (:remove-surrounding-whitespace? opts)
           remove-surrounding-whitespace)
         (cond-> (:insert-missing-whitespace? opts)
           insert-missing-whitespace)
         (cond-> (:remove-multiple-non-indenting-spaces? opts)
           remove-multiple-non-indenting-spaces)
         (cond-> (:indentation? opts)
           (reindent indents opts))
         (cond-> (:align-map-columns? opts)
           (align-map-columns opts))
         (cond-> (:align-form-columns? opts)
           (align-form-columns aligned opts))
         (cond-> (:remove-trailing-whitespace? opts)
           remove-trailing-whitespace)
         (cond-> (:remove-blank-lines-in-forms? opts)
           (remove-blank-lines-in-forms blank opts))))))

(defn reformat-string
  "Reformat a string. Accepts a map of [formatting options][1].

  [1]: https://github.com/weavejester/cljfmt#formatting-options"
  ([form-string]
   (reformat-string form-string {}))
  ([form-string options]
   (-> (p/parse-string-all form-string)
       (reformat-form options)
       n/string
       (cond-> (:normalize-newlines-at-file-end? options)
         normalize-newlines-at-file-end))))

(def default-line-separator
  #?(:clj (System/lineSeparator) :cljs \newline))

(defn normalize-newlines [s]
  (str/replace s #"\r\n" "\n"))

(defn replace-newlines [s sep]
  (str/replace s #"\n" sep))

(defn find-line-separator [s]
  (or (re-find #"\r?\n" s) default-line-separator))

(defn wrap-normalize-newlines [f]
  (fn [s]
    (let [sep (find-line-separator s)]
      (-> s normalize-newlines f (replace-newlines sep)))))
