(ns tapassage.core
  (:import (clojure.lang PersistentQueue)))

(defn- xfhcomp [& xfs]
  (fn [xf]
    (let [ixf (fn [_ input] input)
          ts (map #(% ixf) xfs)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (let [rs (map #(% nil input) ts)]
           (if (every? some? rs)
             (xf result rs)
             result)))))))

;; MOVING AVERAGES

(defn sma [p]
  (fn [xf]
    (let [values (volatile! PersistentQueue/EMPTY)
          sum (volatile! 0.0)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (vswap! sum + input)
         (vswap! values conj input)
         (when (> (count @values) p)
           (vswap! sum - (first @values))
           (vswap! values pop))
         (if (= (count @values) p)
           (xf result (/ @sum p))
           result))))))

(defn ema [p]
  (fn [xf]
    (let [prev-ema (volatile! [])
          alpha (/ 2 (+ p 1))]
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
               (if (= (count @prev-ema) p)
                 (step (/ (apply + @prev-ema) p))
                 result))
             (step (ema-f @prev-ema)))))))))

(defn dema [p]
  (comp
    (ema p)
    (xfhcomp
      (map (partial * 2))
      (ema p))
    (map (partial apply -))))

(defn tema [p]
  (comp
    (ema p)
    (xfhcomp
      (map identity)
      (ema p))
    (xfhcomp
      (comp (map first) (map (partial * 3)))
      (comp (map second) (map (partial * 3)))
      (comp (map second) (ema p)))
    (map (fn [[ema ema2 ema3]] (+ ema3 (- ema ema2))))))

(defn wma [p]
  (let [triangles (range 1 (inc p))
        denominator (double (apply + triangles))
        weights (map #(/ % denominator) triangles)]
    (fn [xf]
      (let [values (volatile! PersistentQueue/EMPTY)]
        (fn
          ([] (xf))
          ([result] (xf result))
          ([result input]
           (vswap! values conj input)
           (if (< (count @values) p)
             result
             (do
               (when (> (count @values) p)
                 (vswap! values pop))
               (xf result (apply + (map * weights @values)))))))))))

;; OSCILATORS

(defn roc [p]
  (fn [xf]
    (let [values (volatile! PersistentQueue/EMPTY)]
      (fn
        ([] (xf))
        ([result] (xf result))
        ([result input]
         (vswap! values conj input)
         (if (<= (count @values) p)
           result
           (let [prev (first @values)]
             (vswap! values pop)
             (xf result (* 100 (dec (/ input prev)))))))))))
