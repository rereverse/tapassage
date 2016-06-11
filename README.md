# tapassage

A Clojure library designed to (eventually :0)) implement all
indicators in famous TA-lib as transducers over input data feed.

## Usage

[Simple moving average](http://www.investopedia.com/terms/s/sma.asp) of period 3 over few values:

    (into [] (sma 3) [1 5 9 12 4 3 5 6])
    => [5.0 8.666666666666666 8.333333333333334 6.333333333333333 4.0 4.666666666666667]

Or, the angle of linear line should be 45 degrees, right?

    (into [] (lin-reg-angle 2) (take 10 (iterate inc 1)))
    => [45.0 45.0 45.0 45.0 45.0 45.0 45.0 45.0 45.0]

Every multiple of two is 100% more than the previous one (rate of change)

    (into [] (roc-p 1) (take 10 (iterate (partial * 2) 1)))
    => [1 1 1 1 1 1 1 1 1]

Simple moving average of exponential moving average of few values (sma 3 (ema 3 xs))

    (into [] (comp
               (ema 3)
               (sma 3)) [1 2 3 4 5 5 5 4 3 2 1])
    => [3.0 3.8333333333333335 4.416666666666667 4.541666666666667 4.270833333333333 3.6354166666666665 2.8177083333333335]

All of the transducers are stateful. Don't worry too much about the
0 and 1 arity, basically just implement the step arity. See `indicator` macro.

## License

Copyright © 2016 Tomas Zaoral

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
