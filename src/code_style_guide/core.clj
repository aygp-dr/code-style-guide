(ns code-style-guide.core
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [cheshire.core :as json]
            [code-style-guide.specs :as specs]))

(def style-rules
  [{:id "line-length"
    :check (fn [line _] (> (count line) 120))
    :message "Line exceeds 120 characters"
    :severity "low"
    :fix nil}
   {:id "trailing-whitespace"
    :check (fn [line _] (re-find #"\s+$" line))
    :message "Trailing whitespace"
    :severity "low"
    :fix (fn [line] (str/trimr line))}
   {:id "tabs-not-spaces"
    :check (fn [line _] (re-find #"^\t" line))
    :message "Tab indentation (use spaces)"
    :severity "medium"
    :fix (fn [line] (str/replace line #"\t" "    "))}
   {:id "consecutive-blank-lines"
    :check (fn [line ctx] (and (str/blank? line) (:prev-blank ctx)))
    :message "Consecutive blank lines"
    :severity "low"
    :fix nil}
   {:id "no-newline-at-end"
    :check (fn [line ctx] (and (:last-line ctx) (not (str/blank? line)) (not (:final-newline ctx))))
    :message "File does not end with newline"
    :severity "low"
    :fix nil}
   {:id "todo-without-ticket"
    :check (fn [line _] (and (re-find #"(?i)\bTODO\b" line) (not (re-find #"(?i)TODO\s*[\(\[#]" line))))
    :message "TODO without ticket reference (use TODO(TICKET-123))"
    :severity "medium"
    :fix nil}
   {:id "fixme-present"
    :check (fn [line _] (re-find #"(?i)\bFIXME\b" line))
    :message "FIXME found — should be resolved before merge"
    :severity "high"
    :fix nil}
   {:id "debug-print"
    :check (fn [line ctx]
             (case (:lang ctx)
               "python" (re-find #"^\s*print\(" line)
               "javascript" (re-find #"console\.(log|debug|info)\(" line)
               "go" (re-find #"fmt\.Print" line)
               nil))
    :message "Debug print statement (remove before merge)"
    :severity "high"
    :fix nil}
   {:id "magic-number"
    :check (fn [line _]
             (and (re-find #"[=<>!]\s*\d{2,}" line)
                  (not (re-find #"(?i)(status|port|http|error|code|version|timeout)" line))))
    :message "Magic number (extract to named constant)"
    :severity "medium"
    :fix nil}])

(def ext->lang
  {"py" "python" "js" "javascript" "ts" "javascript"
   "go" "go" "java" "java" "rb" "ruby" "clj" "clojure"
   "rs" "rust" "sh" "shell" "bash" "shell"})

(defn check-content
  "Style violations in `content`, the text of the file at `path` (pure)."
  [content path]
  (let [ext (last (str/split (str (fs/file-name path)) #"\."))
        lang (get ext->lang ext "unknown")
        lines (str/split-lines content)]
    (->> lines
         (map-indexed vector)
         (reduce
          (fn [{:keys [violations prev-blank]} [idx line]]
            (let [ctx {:lang lang
                       :prev-blank prev-blank
                       :last-line (= idx (dec (count lines)))
                       ;; split-lines drops line terminators, so look at the content
                       :final-newline (str/ends-with? content "\n")}
                  new-violations
                  (->> style-rules
                       (filter #(try ((:check %) line ctx) (catch Exception _ false)))
                       (map (fn [rule]
                              {:file (str path)
                               :line (inc idx)
                               :id (:id rule)
                               :severity (:severity rule)
                               :message (:message rule)
                               :fixable (some? (:fix rule))
                               :match (str/trim (subs line 0 (min (count line) 80)))})))]
              {:violations (into violations new-violations)
               :prev-blank (str/blank? line)}))
          {:violations [] :prev-blank false})
         :violations)))

(s/fdef check-content
  :args (s/cat :content ::specs/content :path ::specs/path-like)
  :ret ::specs/violations
  :fn (fn [{{:keys [content] [_ path] :path} :args ret :ret}]
        (let [lines (str/split-lines content)]
          (and (every? #(<= 1 (:line %) (count lines)) ret)
               (every? #(= (str path) (:file %)) ret)
               ;; the final-newline rule fires exactly when the last line
               ;; has text and no newline follows it
               (= (boolean (some #(= "no-newline-at-end" (:id %)) ret))
                  (and (not (str/blank? (last lines)))
                       (not (str/ends-with? content "\n"))))))))

(defn check-file [path]
  (check-content (slurp (str path)) path))

(s/fdef check-file
  :args (s/cat :path ::specs/path-like)
  :ret ::specs/violations)

(defn scan-directory [dir]
  (let [extensions (set (keys ext->lang))
        files (->> (fs/glob dir "**")
                   (filter fs/regular-file?)
                   (filter #(contains? extensions (last (str/split (str (fs/file-name %)) #"\."))))
                   (remove #(str/includes? (str %) "node_modules"))
                   (remove #(str/includes? (str %) ".git"))
                   (remove #(str/includes? (str %) "vendor")))]
    (->> files
         (mapcat check-file)
         (sort-by (juxt :severity :file :line)))))

(s/fdef scan-directory
  :args (s/cat :dir ::specs/path-like)
  :ret ::specs/violations)

(defn format-text [violations]
  (if (empty? violations)
    "No style violations found."
    (str/join "\n"
              (concat
               [(format "Found %d style violation(s):\n" (count violations))]
               (map (fn [{:keys [file line severity message fixable match]}]
                      (format "  %s:%d [%s]%s %s\n    |  %s"
                              file line (str/upper-case severity)
                              (if fixable " (fixable)" "") message match))
                    violations)
               [""
                (format "Summary: %d high, %d medium, %d low (%d auto-fixable)"
                        (count (filter #(= (:severity %) "high") violations))
                        (count (filter #(= (:severity %) "medium") violations))
                        (count (filter #(= (:severity %) "low") violations))
                        (count (filter :fixable violations)))]))))

(s/fdef format-text
  :args (s/cat :violations ::specs/violations)
  :ret string?
  :fn (fn [{{:keys [violations]} :args ret :ret}]
        (if (empty? violations)
          (= "No style violations found." ret)
          (str/includes? ret (format "Found %d style violation(s)" (count violations))))))

(defn format-json [violations]
  (json/generate-string
   {:total (count violations)
    :by-severity {:high (count (filter #(= (:severity %) "high") violations))
                  :medium (count (filter #(= (:severity %) "medium") violations))
                  :low (count (filter #(= (:severity %) "low") violations))}
    :fixable (count (filter :fixable violations))
    :violations violations}
   {:pretty true}))

(s/fdef format-json
  :args (s/cat :violations ::specs/violations)
  :ret string?
  :fn (fn [{{:keys [violations]} :args ret :ret}]
        (let [m (json/parse-string ret true)]
          (and (= (count violations) (:total m))
               (= (count (filter :fixable violations)) (:fixable m))))))

(def cli-spec
  {:dir {:desc "Directory to scan" :default "." :alias :d}
   :format {:desc "Output format: text, json, edn" :default "text" :alias :f}
   :severity {:desc "Minimum severity: low, medium, high" :default "low" :alias :s}
   :fix {:desc "Auto-fix fixable violations" :coerce :boolean :alias :x}
   :help {:desc "Show help" :alias :h :coerce :boolean}})

(defn -main [& args]
  (let [opts (cli/parse-opts args {:spec cli-spec})
        _ (when (:help opts)
            (println "code-style-guide — enforce coding style rules")
            (println)
            (println (cli/format-opts {:spec cli-spec}))
            (System/exit 0))
        severity-rank {"high" 3 "medium" 2 "low" 1}
        min-severity (get severity-rank (:severity opts) 1)
        violations (->> (scan-directory (:dir opts))
                        (filter #(>= (get severity-rank (:severity %) 0) min-severity)))]
    (println
     (case (:format opts)
       "json" (format-json violations)
       "edn" (pr-str violations)
       (format-text violations)))
    (System/exit (if (seq violations) 1 0))))

(s/fdef -main
  :args (s/* string?))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
