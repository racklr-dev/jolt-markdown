;; jolt.markdown.core test. Run from the repo root:
;;   jolt run test/jolt/markdown/core_test.clj
(ns jolt.markdown.core-test
  (:require [clojure.test :refer [deftest is run-tests]]
            [jolt.markdown.core :as md]))

(deftest inline-plain
  (is (= [{:text "plain text" :style nil}] (md/parse-inline "plain text"))))

(deftest inline-strong
  (is (= [{:text "bold" :style #{:strong}}] (md/parse-inline "**bold**"))))

(deftest inline-em
  (is (= [{:text "em" :style #{:em}}] (md/parse-inline "*em*")))
  (is (= [{:text "em" :style #{:em}}] (md/parse-inline "_em_"))))

(deftest inline-strike
  (is (= [{:text "gone" :style #{:strike}}] (md/parse-inline "~~gone~~"))))

(deftest inline-code
  (is (= [{:text "code" :style #{:code}}] (md/parse-inline "`code`"))))

(deftest inline-link
  (is (= [{:text "docs" :style #{:link} :href "https://x"}]
         (md/parse-inline "[docs](https://x)"))))

(deftest inline-mixed
  (is (= [{:text "a " :style nil}
          {:text "b" :style #{:strong}}
          {:text " c" :style nil}]
         (md/parse-inline "a **b** c"))))

(deftest inline-multi
  (is (= [{:text "run " :style nil}
          {:text "bb" :style #{:code}}
          {:text " then " :style nil}
          {:text "docs" :style #{:link} :href "https://x"}]
         (md/parse-inline "run `bb` then [docs](https://x)"))))

(deftest inline-guards
  (is (= [{:text "snake_case_here" :style nil}] (md/parse-inline "snake_case_here")))
  (is (= [{:text "a * b * c" :style nil}] (md/parse-inline "a * b * c"))))

(deftest blocks-heading
  (is (= [{:type :heading :level 1 :spans [{:text "Title" :style nil}]}]
         (md/parse-blocks "# Title"))))

(deftest blocks-code
  (is (= [{:type :code :lines ["(defn x [] 1)"] :lang "clojure"}]
         (md/parse-blocks "```clojure\n(defn x [] 1)\n```"))))

(deftest blocks-kinds
  (is (= [:bullet :quote :numbered :text]
         (map :type (md/parse-blocks "- a\n> b\n1. c\nplain"))))
  (is (= 1 (-> (md/parse-blocks "1. c") first :n))))

(deftest wrap-refrows-at-space
  (is (= [[{:text "aaaa"}]
          [{:text "bbbb"}]]
         (md/wrap-spans [{:text "aaaa bbbb"}] 6))))

(deftest wrap-hard-breaks-long-word
  (is (= [[{:text "abcd"}]
          [{:text "efgh"}]]
         (md/wrap-spans [{:text "abcdefgh"}] 4))))

(deftest wrap-keeps-style-across-split
  (is (= [[{:text "aaaa" :style #{:strong}}]
          [{:text "bbbb" :style #{:strong}}]]
         (md/wrap-spans [{:text "aaaa bbbb" :style #{:strong}}] 6))))

(deftest wrap-mixed-spans
  (is (= [[{:text "aa" :style nil} {:text "b" :style #{:strong}}]
          [{:text "b" :style #{:strong}} {:text "cc" :style nil}]]
         (md/wrap-spans [{:text "aa" :style nil}
                         {:text "bb" :style #{:strong}}
                         {:text "cc" :style nil}] 3))))

(let [s (run-tests)]
  (when (pos? (+ (:fail s) (:error s)))
    (System/exit 1)))
