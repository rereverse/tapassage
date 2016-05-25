(ns tapassage.core
  (:import (clojure.lang PersistentQueue)))


(defn sma [period]
  (fn [xf]
    (let [values (volatile! PersistentQueue/EMPTY)
          sum (volatile! 0.0)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (vswap! sum + input)
         (vswap! values conj input)
         (when (> (count @values) period)
           (vswap! sum - (first @values))
           (vswap! values pop))
         (if (= (count @values) period)
           (xf result (/ @sum period))
           result))))))

