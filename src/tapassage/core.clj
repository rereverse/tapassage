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

(defn- ema-comp [period]
  (fn [xf]
    (let [prev-ema (volatile! [])
          alpha (/ 2 (+ period 1))]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result in]
         (let [input (::x in)
               ema-f (fn [p] (->> p (- input) (* alpha) (+ p)))
               step (fn [new-ema]
                      (vreset! prev-ema new-ema)
                      (xf result (assoc in ::x new-ema)))]
           (if (vector? @prev-ema)
             (do (vswap! prev-ema conj input)
                 (if (= (count @prev-ema) period)
                   (step (/ (apply + @prev-ema) period))
                   result))
             (step (ema-f @prev-ema)))))))))

(defn ema [period]
  (comp
    (map #(hash-map ::x %))
    (ema-comp period)
    (map ::x)))

(defn dema [period]
  (comp
    (ema period)
    (map (fn [x] {::2ema (* 2 x), ::x x}))
    (ema-comp period)
    (map (fn [m] (- (::2ema m) (::x m))))))

(defn tema [period]
  (comp
    (ema period)
    (map (fn [x] {::3ema (* 3 x), ::x x}))
    (ema-comp period)
    (map (fn [m] (assoc m ::3emaema (* 3 (::x m)))))
    (ema-comp period)
    (map (fn [m] (+ (::x m) (- (::3ema m) (::3emaema m)))))))
