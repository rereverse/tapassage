(ns tapassage.core)


(defn sma [period]
  (fn [xf]
    (let [called (volatile! 0)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (if (<= (vswap! called inc) 88)
           (xf result input)
           result))))))
