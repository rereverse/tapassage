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

(defn ema [period]
  (fn [xf]
    (let [prev-ema (volatile! [])
          alpha (/ 2 (+ period 1))]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [ema-f (fn [p] (->> p (- input) (* alpha) (+ p)))
               step (fn [new-ema] (vreset! prev-ema new-ema) (xf result new-ema))]
           (if (vector? @prev-ema)
             (do (vswap! prev-ema conj input)
                 (if (= (count @prev-ema) period)
                   (step (/ (apply + @prev-ema) period))
                   result))
             (step (ema-f @prev-ema)))))))))
