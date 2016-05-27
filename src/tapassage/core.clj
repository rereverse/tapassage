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
         (let [rs (into [] (map #(% nil input)) ts)]
           (if (every? some? rs)
             (xf result rs)
             result)))))))

(defmacro indicator
  ([trans-fn] `(indicator [] ~trans-fn))
  ([bindings trans-fn]
   `(fn [xf#]
      (let ~bindings
        (fn
          ([] (xf#))
          ([result#] (xf# result#))
          ([result# input#]
           (if-let [r# (~trans-fn input#)]
             (xf# result# r#)
             result#))))))
  {:private true})

(defn sma [p]
  (indicator
    [values (volatile! PersistentQueue/EMPTY)
     sum (volatile! 0.0)]
    (fn [x]
      (vswap! sum + x)
      (vswap! values conj x)
      (when (> (count @values) p)
        (vswap! sum - (first @values))
        (vswap! values pop))
      (when (= (count @values) p)
        (/ @sum p)))))

(defn ema
  ([p] (ema p (/ 2 (+ p 1))))
  ([p alpha]
   (indicator
     [prev-ema (volatile! [])
      ema-f (fn [x] (->> @prev-ema (- x) (* alpha) (+ @prev-ema)))]
     (fn [x]
       (if (vector? @prev-ema)
         (do
           (vswap! prev-ema conj x)
           (when (= (count @prev-ema) p)
             (vreset! prev-ema (/ (apply + @prev-ema) p))))
         (vreset! prev-ema (ema-f x)))))))

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
    (indicator
      [values (volatile! PersistentQueue/EMPTY)]
      (fn [x]
        (vswap! values conj x)
        (when (>= (count @values) p)
          (when (> (count @values) p)
            (vswap! values pop))
          (apply + (map * weights @values)))))))

(defn roc [p]
  (indicator
    [values (volatile! PersistentQueue/EMPTY)]
    (fn [x]
      (vswap! values conj x)
      (when (> (count @values) p)
        (let [prev (first @values)]
          (vswap! values pop)
          (* 100 (dec (/ x prev))))))))

(defn roc-p [p]
  (indicator
    [values (volatile! PersistentQueue/EMPTY)]
    (fn [x]
      (vswap! values conj x)
      (when (> (count @values) p)
        (let [prev (first @values)]
          (vswap! values pop)
          (/ (- x prev) prev))))))

(defn roc-r [p]
  (indicator
    [values (volatile! PersistentQueue/EMPTY)]
    (fn [x]
      (vswap! values conj x)
      (when (> (count @values) p)
        (let [prev (first @values)]
          (vswap! values pop)
          (/ x prev))))))

(defn roc-r-100 [p]
  (comp
    (roc-r p)
    (map (partial * 100))))

(defn trix [p]
  (comp
    (apply comp (repeatedly 3 #(ema p)))
    (roc-p 1)
    (map (partial * 100))))

(def ^:private upward-change
  (indicator
    [prev (volatile! nil)]
    (fn [x]
      (let [p @prev]
        (vreset! prev x)
        (when (some? p)
          (let [d (- x p)]
            (if (pos? d) d 0)))))))

(def ^:private downard-change
  (indicator
    [prev (volatile! nil)]
    (fn [x]
      (let [p @prev]
        (vreset! prev x)
        (when (some? p)
          (let [d (- p x)]
            (if (pos? d) d 0)))))))

(defn rsi [p]
  (let [alpha (/ 1 p)]
    (comp
      (xfhcomp
        (comp upward-change (ema p alpha))
        (comp downard-change (ema p alpha)))
      (map #(/ (first %) (second %)))
      (map #(- 100 (/ 100 (inc %)))))))
