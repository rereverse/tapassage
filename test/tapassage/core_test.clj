(ns tapassage.core-test
  (:require [clojure.test :refer :all]
            [tapassage.core :refer :all]
            [clojure.algo.generic.math-functions :as math])
  (:import [com.tictactec.ta.lib Core MInteger RetCode]))

(def ^:private total-periods 100)
(def ^:private p 13)
(def ^:private input (double-array (repeatedly total-periods rand)))
(def ^:private epsilon 1e-13)

(defn seq-approx= [xs ys]
  (every? (fn [[x y]] (math/approx= x y epsilon)) (map vector xs ys)))

(defmacro test-standard-ta-indicator
  ([i-name] `(test-standard-ta-indicator ~i-name ~i-name))
  ([tp-name ta-name]
   (let [test-name (symbol (str "test-" tp-name))]
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
            (is (seq-approx= tp# ta#))))))))

(test-standard-ta-indicator sma)
(test-standard-ta-indicator ema)
(test-standard-ta-indicator dema)
(test-standard-ta-indicator tema)
(test-standard-ta-indicator wma)
(test-standard-ta-indicator roc)
(test-standard-ta-indicator roc-p rocP)
(test-standard-ta-indicator roc-r rocR)
(test-standard-ta-indicator roc-r100 rocR100)
(test-standard-ta-indicator trix)
(test-standard-ta-indicator rsi)
(test-standard-ta-indicator mom)
