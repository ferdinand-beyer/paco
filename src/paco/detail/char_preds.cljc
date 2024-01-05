(ns paco.detail.char-preds
  #?(:cljs (:require-macros [paco.detail.char-preds :refer [test-ranges]])))

(defn code-point
  "Returns the Unicode code point of `ch` as an integer."
  ^long [ch]
  #?(:clj  (unchecked-int (.charValue ^Character ch))
     :cljs (.charCodeAt ^js ch 0)))

(defmacro ^:private test-ranges [ch & ranges]
  (let [cp (gensym "cp")]
    `(let [~cp (code-point ~ch)]
       (or ~@(for [[min-ch max-ch] ranges]
               `(and (<= ~(code-point min-ch) ~cp)
                     (<= ~cp ~(code-point max-ch))))))))

(defn ascii-upper?
  "Returns true if `ch` is a ASCII upper-case letter (A-Z)."
  [ch]
  (test-ranges ch [\A \Z]))

(defn ascii-lower?
  "Returns true if `ch` is a ASCII lower-case letter (a-z)."
  [ch]
  (test-ranges ch [\a \z]))

(defn ascii-letter?
  "Returns true if `ch` is a ASCII letter (a-z, A-Z)."
  [ch]
  (test-ranges ch [\a \z] [\A \Z]))

(defn upper?
  "Returns true if `ch` is a Unicode upper-case letter."
  [ch]
  #?(:clj  (Character/isUpperCase (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Lu}$" ch)))

(defn lower?
  "Returns true if `ch` is a Unicode lower-case letter."
  [ch]
  #?(:clj  (Character/isLowerCase (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Ll}$" ch)))

(defn letter?
  "Returns true if `ch` is a Unicode letter."
  [ch]
  #?(:clj  (Character/isLetter (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{L}$" ch)))

(defn control?
  "Returns true if `ch` is a control character."
  [ch]
  #?(:clj  (Character/isISOControl (.charValue ^Character ch))
     :cljs (.test #"(?u)^\p{Cc}$" ch)))

(defn digit?
  "Returns true if `ch` is a decimal digit (0-9)."
  [ch]
  (test-ranges ch [\0 \9]))

(defn hex?
  "Returns true if `ch` is a hexadecimal digit (0-9, a-f, A-F)."
  [ch]
  (test-ranges ch [\0 \9] [\a \f] [\A \F]))

(defn octal?
  "Returns true if `ch` is an octal digit (0-7)."
  [ch]
  (test-ranges ch [\0 \7]))
