(ns paco.pos)

(defprotocol IPosition
  (line-index [pos])
  (column-index [pos]))

(defrecord Position [^#?(:clj int, :cljs number) line
                     ^#?(:clj int, :cljs number) col]
  Object
  (toString [_]
    (str "line " (unchecked-inc line)
         ", column " (unchecked-inc col)))

  IPosition
  (line-index [_] line)
  (column-index [_] col)

  #?@(:bb []
      :clj [Comparable
            (compareTo [_ other]
                       (let [d (compare line (.-line ^Position other))]
                         (if (zero? d)
                           (compare col (.-col ^Position other))
                           d)))]
      :cljs [IComparable
             (-compare [_ other]
                       (let [d (compare line (.-line ^Position other))]
                         (if (zero? d)
                           (compare col (.-col ^Position other))
                           d)))]))
