(ns tapassage.core-test
  (:require [clojure.test :refer :all]
            [tapassage.core :refer :all]
            [clojure.algo.generic.math-functions :refer [approx=]])
  (:import [com.tictactec.ta.lib Core MInteger RetCode]))

(def ^:private total-periods 100)
(def ^:private p 13)
(def ^:private input (double-array (repeatedly total-periods rand)))
(def ^:private epsilon 1e-13)

(defn seq-approx= [xs ys]
  (every? (fn [[x y]] (approx= x y epsilon)) (map vector xs ys)))

(defmacro test-indicator
  ([test-name i-name] `(test-indicator ~test-name ~i-name ~i-name))
  ([test-name tp-name ta-name]
   `(deftest ~test-name
      (testing
        (let [out-start-idx# (new MInteger)
              out-len# (new MInteger)
              output# (double-array total-periods)
              ret-code# (.. (new Core) (~ta-name 0 (dec total-periods)
                                         input p out-start-idx# out-len# output#))
              tp# (sequence (~tp-name p) input)
              ta# (take (.-value out-len#) (seq output#))]
          (is (= ret-code# RetCode/Success))
          (is (= (count tp#) (count ta#)))
          (is (seq-approx= tp# ta#)))))))

(test-indicator test-sma sma)
(test-indicator test-ema ema)
(test-indicator test-dema dema)
(test-indicator test-tema tema)
(test-indicator test-wma wma)
(test-indicator test-roc roc)
(test-indicator test-rocp roc-p rocP)
(test-indicator test-trix trix)
