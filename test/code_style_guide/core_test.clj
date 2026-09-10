(ns code-style-guide.core-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [code-style-guide.core :as sut]))

;; Exercise every s/fdef :args spec while the unit tests run.
(use-fixtures :once
  (fn [f] (stest/instrument) (try (f) (finally (stest/unstrument)))))

(defn- rule [id]
  (first (filter #(= id (:id %)) sut/style-rules)))

(defn- flags?
  ([id line] (flags? id line {}))
  ([id line ctx] (boolean ((:check (rule id)) line (merge {:lang "unknown"} ctx)))))

;; --- The rule table ---

(deftest rules-are-well-formed
  (is (= 9 (count sut/style-rules)))
  (is (apply distinct? (map :id sut/style-rules)))
  (is (every? #{"high" "medium" "low"} (map :severity sut/style-rules))))

(deftest line-length-rule
  (is (flags? "line-length" (apply str (repeat 121 "x"))))
  (is (not (flags? "line-length" (apply str (repeat 120 "x"))))))

(deftest trailing-whitespace-rule
  (is (flags? "trailing-whitespace" "x = 1   "))
  (is (not (flags? "trailing-whitespace" "x = 1")))
  (is (= "x = 1" ((:fix (rule "trailing-whitespace")) "x = 1 \t"))))

(deftest tabs-not-spaces-rule
  (is (flags? "tabs-not-spaces" "\tx = 1"))
  (is (not (flags? "tabs-not-spaces" "    x = 1")))
  (is (= "    x = 1" ((:fix (rule "tabs-not-spaces")) "\tx = 1"))))

(deftest consecutive-blank-lines-rule
  (is (flags? "consecutive-blank-lines" "" {:prev-blank true}))
  (is (not (flags? "consecutive-blank-lines" "" {:prev-blank false}))))

(deftest todo-without-ticket-rule
  (is (flags? "todo-without-ticket" "# TODO tidy this"))
  (is (not (flags? "todo-without-ticket" "# TODO(ABC-123) tidy this")))
  (is (not (flags? "todo-without-ticket" "# TODO #42 tidy this"))))

(deftest fixme-present-rule
  (is (flags? "fixme-present" "// FIXME: broken"))
  (is (not (flags? "fixme-present" "// fixed it"))))

(deftest debug-print-rule
  (is (flags? "debug-print" "print(x)" {:lang "python"}))
  (is (flags? "debug-print" "  console.log(x);" {:lang "javascript"}))
  (is (flags? "debug-print" "fmt.Println(x)" {:lang "go"}))
  (is (not (flags? "debug-print" "print(x)" {:lang "ruby"}))))

(deftest magic-number-rule
  (is (flags? "magic-number" "if x > 42:"))
  (is (not (flags? "magic-number" "if status == 404:")))
  (is (not (flags? "magic-number" "x = 7"))))

;; --- Checking content and files ---

(deftest check-content-locates-violations
  (let [vs (sut/check-content "x = 1\n\n\nprint(x)   \n" "app.py")
        by-id (group-by :id vs)]
    (is (= [3] (map :line (by-id "consecutive-blank-lines"))))
    (is (= [4] (map :line (by-id "debug-print"))))
    (is (= [true] (map :fixable (by-id "trailing-whitespace"))))
    (is (every? #(= "app.py" (:file %)) vs))))

(deftest check-content-language-comes-from-the-extension
  (let [debug-prints (fn [path] (filter #(= "debug-print" (:id %))
                                        (sut/check-content "console.log(1)" path)))]
    (is (seq (debug-prints "web/app.js")))
    (is (empty? (debug-prints "lib/app.rb")))))

(deftest check-content-missing-final-newline
  (is (= ["no-newline-at-end"] (map :id (sut/check-content "x = 1\ny = 2" "ok.py")))))

(deftest check-content-final-newline-present
  (is (empty? (sut/check-content "x = 1\ny = 2\n" "ok.py")))
  (is (empty? (sut/check-content "x = 1\r\n" "ok.py"))))

(deftest check-file-and-scan-directory
  (let [dir (fs/create-temp-dir)
        a-py (str (fs/path dir "a.py"))]
    (try
      (spit a-py "print(1)")
      (spit (str (fs/path dir "notes.txt")) "print(1)")
      (fs/create-dirs (fs/path dir "node_modules"))
      (spit (str (fs/path dir "node_modules" "b.js")) "console.log(1)")
      (testing "check-file reads and checks one file"
        (is (some #(= "debug-print" (:id %)) (sut/check-file a-py))))
      (testing "scan-directory skips unknown extensions and node_modules"
        (is (= #{a-py} (set (map :file (sut/scan-directory (str dir)))))))
      (finally (fs/delete-tree dir)))))

;; --- Output ---

(def ^:private sample
  [{:file "a.py" :line 3 :id "fixme-present" :severity "high"
    :message "FIXME found" :fixable false :match "# FIXME"}
   {:file "a.py" :line 5 :id "trailing-whitespace" :severity "low"
    :message "Trailing whitespace" :fixable true :match "x = 1"}])

(deftest format-text-output
  (is (= "No style violations found." (sut/format-text [])))
  (let [out (sut/format-text sample)]
    (is (str/includes? out "Found 2 style violation(s)"))
    (is (str/includes? out "a.py:3 [HIGH] FIXME found"))
    (is (str/includes? out "a.py:5 [LOW] (fixable) Trailing whitespace"))
    (is (str/includes? out "Summary: 1 high, 0 medium, 1 low (1 auto-fixable)"))))

(deftest format-json-output
  (let [m (json/parse-string (sut/format-json sample) true)]
    (is (= 2 (:total m)))
    (is (= {:high 1 :medium 0 :low 1} (:by-severity m)))
    (is (= 1 (:fixable m)))
    (is (= 2 (count (:violations m))))))

(deftest cli-spec-defaults
  (is (= "." (get-in sut/cli-spec [:dir :default])))
  (is (= "text" (get-in sut/cli-spec [:format :default])))
  (is (= "low" (get-in sut/cli-spec [:severity :default]))))
