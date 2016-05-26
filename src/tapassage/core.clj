(ns tapassage.core
  (:import (clojure.lang PersistentQueue)))


(defn- hcomp [& xfs]
  (fn [xf]
    (let [ts (map #(% (fn [_ input] input)) xfs)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [rs (map #(% nil input) ts)]
           (if (every? some? rs)
             (xf result rs)
             result)))))))

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
               step (fn [new-ema]
                      (vreset! prev-ema new-ema)
                      (xf result new-ema))]
           (if (vector? @prev-ema)
             (do
               (vswap! prev-ema conj input)
               (if (= (count @prev-ema) period)
                 (step (/ (apply + @prev-ema) period))
                 result))
             (step (ema-f @prev-ema)))))))))

(defn dema [period]
  (comp
    (ema period)
    (hcomp
      (map (partial * 2))
      (ema period))
    (map (partial apply -))))

;(defn tema [period]
;  (comp
;    (ema period)
;    (hcomp
;      (map identity)
;      (ema period))
;    (hcomp)))

;(defn tema [period]
;  (comp
;    (ema period)
;    (map (fn [x] {::3ema (* 3 x), ::x x}))
;    (ema-comp period)
;    (map (fn [m] (assoc m ::3emaema (* 3 (::x m)))))
;    (ema-comp period)
;    (map (fn [m] (+ (::x m) (- (::3ema m) (::3emaema m)))))))

(defn wma [period]
  (let [triangles (range 1 (inc period))
        denominator (double (apply + triangles))
        weights (map #(/ % denominator) triangles)]
    (fn [xf]
      (let [values (volatile! PersistentQueue/EMPTY)]
        (fn
          ([] (xf))
          ([result] (xf result))
          ([result input]
           (vswap! values conj input)
           (if (< (count @values) period)
             result
             (do
               (when (> (count @values) period)
                 (vswap! values pop))
               (xf result (apply + (map * weights @values)))))))))))
