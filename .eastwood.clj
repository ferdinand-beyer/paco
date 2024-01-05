(disable-warning
 {:linter :suspicious-expression
  :for-macro 'clojure.core/or
  :if-inside-macroexpansion-of #{'paco.detail.char-preds/test-ranges}
  :within-depth 6
  :reason "test-ranges does not special-case arity 1."})
