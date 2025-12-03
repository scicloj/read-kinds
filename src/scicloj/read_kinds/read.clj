(ns scicloj.read-kinds.read
  "Convert code into contexts.
  Contexts are maps that contain top-level forms and their evaluated value,
  which will be further annotated with more information."
  (:refer-clojure :exclude [read-string])
  (:require [clojure.string :as str]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node])
  (:import (java.io StringWriter)))

(def evaluators #{:clojure :babashka})

(defn- validate-options [{:keys [evaluator]}]
  (when evaluator
    (assert (contains? evaluators evaluator)
            (str "evaluator must be one of: " evaluators))))

(defn ^:dynamic *on-eval-error*
  "By default, eval errors will be rethrown.
  When *on-eval-error* is bound to nil or a function,
  The exception will be added to the context as an `:error` instead.
  *on-eval-error* may be bound to a function to provide alternative behavior like warning.
  When bound to a function the result will be ignored, but subsequent exceptions will propagate."
  [context ex]
  (throw (ex-info (str "Eval failed: " (ex-message ex))
                  {:id      ::eval-failed
                   :context context}
                  ex)))

(def ^:dynamic *capture-pr-context*)
(def out-orig *out*)
(def err-orig *err*)

(defmacro with-out-err-str [& body]
  `(binding [*out* (new StringWriter)
             *err* (new StringWriter)]
     ~@body))

(defn str-and-reset! [w]
  (locking w
    (let [s (str w)]
      (.setLength (.getBuffer ^StringWriter w) 0)
      s)))

;; TODO need to make items of these in notes
(defmacro with-out-err->context [pr-context & body]
  ;; For a notebook, we capture output globally, and per note.
  (case pr-context
    ;; Capture global *out* and *err*
    :global
    ;; Futures will inherit the current binding,
    ;; which is not affected by altering the root.
    `(with-out-err-str
       ;; Threads may inherit only the root binding
       (with-redefs [*out* *out*
                     *err* *err*]
         (binding [*capture-pr-context* :global]
           ~@body)))
    ;; Capture local *out* and *err*, per note
    :local
    `(let [global-out# *out*
           global-err# *err*]
       (assert (= *capture-pr-context* :global)
               ":global should be captured before (around) :local")
       (with-out-err-str
         (let [result# (do ~@body)]
           (into result#
                 (filter (comp not-empty val))
                 {:out (str *out*)
                  :err (str *err*)
                  :global-out (str-and-reset! global-out#)
                  :global-err (str-and-reset! global-err#)}))))))

(defn print-from-context [context]
  (doseq [[captured print-to] (->> (map (fn [k pr-to]
                                          [(k context) pr-to])
                                        [:out :err :global-out :global-err]
                                        (cycle [out-orig err-orig]))
                                   (filter first))]
    (binding [*out* print-to]
      (print captured)
      (flush)))
  context)

(defn- eval-node
  "Given an Abstract Syntax Tree node, returns a context.
  A context represents a top level form evaluation."
  [node options]
  ;; capturing *out* and *err* as soon as possible
  (with-out-err->context :local
    (let [tag (node/tag node)
          code (node/string node)]
      (case tag
        (:newline :whitespace) {:code code
                                :kind :kind/whitespace}

        :uneval {:code code
                 :kind :kind/uneval}

        ;; extract text from comments
        :comment {:code  code
                  :kind  :kind/comment
                  ;; remove leading semicolons or shebangs, and one non-newline space if present.
                  :value (str/replace-first code #"^(;|#!)*[^\S\r\n]?" "")}
        ;; evaluate for value, capturing exceptions
        ;; TODO doesn't this break namespaced keywords? (sexpr-call
        ;;      without ns/alias inf)
        (let [form (node/sexpr node)
              {:keys [row col end-row end-col]} (meta node)
              context {:line   row
                       :column col
                       ;; TODO for backwards compatibility with clay
                       :region [row col end-row end-col]
                       :code   code
                       :form   form}
              result (try
                       ;; TODO: capture `tap` or not?
                       (let [x (eval form)]
                         {:value x})
                       (catch Throwable ex
                         (when *on-eval-error*
                           (*on-eval-error* context ex))
                         {:exception ex}))]
          (merge context result))))))

(defn- babashka-shebang? [node]
  (-> (node/string node)
      (str/starts-with? "#!/usr/bin/env bb")))

(defn- eval-ast*
  "Given the root Abstract Syntax Tree node,
  returns a vector of contexts that represent evaluation"
  [ast options]
  (let [top-level-nodes (node/children ast)
        ;; TODO: maybe some people want to include the header?
        babashka-shebang (some-> (first top-level-nodes) (babashka-shebang?))
        nodes (if babashka-shebang
                (rest top-level-nodes)
                top-level-nodes)]
    ;; must be eager to restore current bindings
    (mapv #(-> % (eval-node options) print-from-context) nodes)))

(defn eval-ast
  "Evaluates an ast as retrieved via the `read-`functions."
  ([ast] (eval-ast ast {}))
  ([ast options]
   (with-out-err->context :global
     (binding [;; preserve current bindings (they will be reset to
               ;; original)
               *ns* *ns*
               *warn-on-reflection* *warn-on-reflection*
               *unchecked-math* *unchecked-math*]
       (eval-ast* ast options)))))

(defn read-string
  "Parse the first form in a string. The result can be passed to `eval-ast`.
  Suitable for sending text representing one thing for visualization."
  ([code] (read-string code {}))
  ([code options]
   (validate-options options)
   [(parser/parse-string code)]))

(defn read-string-all
  "Parse all forms in a string. The result can be passed to `eval-ast`.
  Suitable for sending a selection of text for visualization.
  When reading a file, prefer using `read-file` to preserve the
  current ns bindings."
  ([code] (read-string-all code {}))
  ([code options]
   (validate-options options)
   ;; preserve current bindings (they will be reset to original)
   (parser/parse-string-all code)))

(defn read-file
  "Similar to `clojure.core/load-file`, but returns a representation
  of the forms, which can be passed to `eval-ast`.
  Suitable for processing an entire namespace."
  [file options]
  (validate-options options)
  ;; preserve current bindings (they will be reset to original)
  (parser/parse-file-all file))
