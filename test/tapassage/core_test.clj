(ns tapassage.core-test
  (:require [clojure.test :refer :all]
            [tapassage.core :as tp])
  (:import [com.tictactec.ta.lib Core MInteger RetCode]))

(def ^:private total-periods 100)
(def ^:private ma-period 13)
(def ^:private input (double-array (repeatedly total-periods rand)))

(deftest test-sma
  (testing "SMA"
    (let [out-start-idx (new MInteger)
          out-len (new MInteger)
          output (double-array total-periods)
          ret-code (.. (new Core) (sma 0 (dec total-periods) input ma-period out-start-idx out-len output))
          tp-sma (into [] (tp/sma ma-period) input)]
      (is (= ret-code RetCode/Success))
      (println (.-value out-len))
      (is (= (count tp-sma) out-len)))))
