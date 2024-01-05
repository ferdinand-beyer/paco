(ns paco.detail.position
  (:refer-clojure :exclude [compare]))


;;? Add token index?
;; Could be useful in nested error messages, to extract a substring
;; from the source, e.g. to display the source line.
(defprotocol IPosition
  (line-index [pos])
  (column-index [pos]))

#?(:clj  (defn- compare-line-col [^long line1 ^long col1 ^long line2 ^long col2]
           (let [d (Long/compare line1 line2)]
             (if (zero? d)
               (Long/compare col1 col2)
               d)))
   :cljs (defn- compare-line-col [line1 col1 line2 col2]
           (let [d (cljs.core/compare line1 line2)]
             (if (zero? d)
               (cljs.core/compare col1 col2)
               d))))

(defrecord Position [^#?(:clj long, :cljs number) line
                     ^#?(:clj long, :cljs number) col]
  Object
  (toString [_]
    (str "line " (unchecked-inc line)
         ", column " (unchecked-inc col)))

  IPosition
  (line-index [_] line)
  (column-index [_] col)

  #?@(:bb []
      :clj [Comparable
            (compareTo [_ other] (compare-line-col
                                  line (.-line ^Position other)
                                  col (.-col ^Position other)))]
      :cljs [IComparable
             (-compare [_ other] (compare-line-col
                                  line (.-line other)
                                  col (.-col other)))]))

(defn position [line-index column-index]
  (Position. line-index column-index))

(defn compare [pos1 pos2]
  (compare-line-col (:line pos1) (:col pos2)
                    (:line pos1) (:col pos2)))
