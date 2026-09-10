(ns code-style-guide.specs
  "Data specs for code-style-guide (https://clojure.org/guides/spec).
  Function specs (s/fdef) live next to each defn in code-style-guide.core."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as str]
            ;; rule maps carry fns under :check/:fix
            [code-style-guide.rule :as-alias rule]))

;; Generators are built inside fns, never in top-level defs:
;; clojure.spec.gen.alpha loads test.check on first use, and the JVM runtime
;; classpath (deps.edn :deps) has no test.check.

;; --- Inputs ---

(def ^:private code-fragments
  ["x = 1" "if x > 42:" "print(x)" "    return y   " "\tindented = True" "# TODO tidy"
   "# TODO(ABC-1) fine" "// FIXME broken" "console.log(v);" "fmt.Println(v)" "status = 404"
   "def f(a):" "function g() {" "}" "" "   " (str/join (repeat 125 "y"))])

;; The text of a source file: lines joined with newlines, with or without a
;; final newline.
(defn- gen-content []
  (gen/fmap (fn [[lines final-newline?]]
              (str (str/join "\n" lines) (when final-newline? "\n")))
            (gen/tuple (gen/vector (gen/one-of [(gen/elements code-fragments)
                                                (gen/string-alphanumeric)])
                                   0 20)
                       (gen/boolean))))

(s/def ::content (s/with-gen string? gen-content))

;; A relative or absolute file path, as a string or java.nio.file.Path.
(defn- gen-path-string []
  (gen/fmap (fn [[dirs base ext]]
              (str/join "/" (conj dirs (cond-> base ext (str "." ext)))))
            (gen/tuple (gen/vector (gen/not-empty (gen/string-alphanumeric)) 0 3)
                       (gen/not-empty (gen/string-alphanumeric))
                       (gen/one-of [(gen/return nil)
                                    (gen/elements ["py" "js" "ts" "go" "java" "rb" "clj"
                                                   "rs" "sh" "bash" "txt"])
                                    (gen/string-alphanumeric)]))))

(s/def ::path-like
  (s/with-gen (s/or :string (s/and string? seq)
                    :path #(instance? java.nio.file.Path %))
    gen-path-string))

(s/def ::lang #{"python" "javascript" "go" "java" "ruby" "clojure" "rust" "shell" "unknown"})

;; --- Violations ---

(s/def ::file string?)
(s/def ::line pos-int?)
(s/def ::id #{"line-length" "trailing-whitespace" "tabs-not-spaces" "consecutive-blank-lines"
              "no-newline-at-end" "todo-without-ticket" "fixme-present" "debug-print"
              "magic-number"})
(s/def ::severity #{"high" "medium" "low"})
(s/def ::message string?)
(s/def ::fixable boolean?)
(s/def ::match string?)
(s/def ::violation
  (s/keys :req-un [::file ::line ::id ::severity ::message ::fixable ::match]))
(s/def ::violations (s/coll-of ::violation :kind sequential? :gen-max 10))

;; --- Rules (core/style-rules) ---

(s/def ::rule/check fn?)
(s/def ::rule/fix (s/nilable fn?))
(s/def ::rule (s/keys :req-un [::id ::rule/check ::message ::severity ::rule/fix]))
(s/def ::rules (s/coll-of ::rule :kind vector? :distinct true))

;; --- CLI option table (the babashka.cli :spec map) ---

(s/def ::desc string?)
(s/def ::default string?)
(s/def ::alias simple-keyword?)
(s/def ::coerce #{:boolean :string :int :long :double :keyword :symbol})
(s/def ::cli-option (s/keys :req-un [::desc] :opt-un [::default ::alias ::coerce]))
(s/def ::cli-spec (s/map-of simple-keyword? ::cli-option))
