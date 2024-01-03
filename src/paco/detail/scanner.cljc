(ns paco.detail.scanner
  (:refer-clojure :exclude [peek]))

(defprotocol IScanner
  (modcount [scanner])
  (index [scanner])
  (end? [scanner])
  (peek [scanner]))
