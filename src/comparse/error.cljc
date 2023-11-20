(ns comparse.error
  (:require [clojure.string :as str]))

(defprotocol IError
  (-render [this]))

(deftype Expected [label]
  IError
  (-render [_] (str "Expected: " label)))

(deftype ExpectedString [str]
  IError
  (-render [_] (str "Expected: '" str "'")))

(deftype Unexpected [label]
  IError
  (-render [_] (str "Unexpected: " label)))

(deftype UnexpectedString [str]
  IError
  (-render [_] (str "Unexpected: '" str "'")))

(deftype Message [msg]
  IError
  (-render [_] msg))

(deftype Nested [pos user-state errors]
  IError
  (-render [_] "<nested>"))

(defn expected
  "The input does not match the expected input."
  [label]
  (Expected. label))

(defn expected-str
  "The input does not match an expected string constant."
  [str]
  (ExpectedString. str))

(defn unexpected
  "The parser encountered some unexpected input."
  [label]
  (Unexpected. label))

(def unexpected-eof
  (unexpected "end of input"))

(defn unexpected-str
  "The parser encountered some unexpected input."
  [str]
  (UnexpectedString. str))

(defn message
  "The error does not fit the other types."
  [msg]
  (Message. msg))

(defn nested
  "Backtracked after an error occurred."
  [pos user-state errors]
  (Nested. pos user-state errors))

(defn merge-errors [errors1 errors2]
  (concat errors1 errors2))

(defn render-errors [errors]
  ;; TODO: Sort/dedupe
  ;; TODO: combine with "or" / "one of"
  (str/join ", " (map -render errors)))
