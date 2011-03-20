(ns covenant.valuation
  "A valuation model for Covenant contracts.

Note that I've gone with a binding modification approach to model
extensibility because protocols make it difficult to formulate
a more traditionally functional approach. The one major downside
of the current approach is the need to use def/defn to reliably
bind variables in a multithreaded context. This restriction should
be ameliorated in future versions of Clojure with enhanced binding
management capabilities (ie, automatic binding inheritence in a multithreaded
environment).
"

  (:require [covenant.core :as c]
            [covenant.observable :as o])
  (:import [covenant.core Zero One Give And Or Cond Scale When Anytime Until]))

;; valuation model

(extend-protocol o/Observable
  Zero
  (values [_ _] (repeat 0))
  Give
  (values [contract t] (map #(* -1 %) (o/values (.contract contract) t)))
  And
  (values [contract t] (map (fn [a b] (+ a b))
                               (o/values (.contract-a contract) t)
                               (o/values (.contract-b contract) t)))
  Or
  (values [contract t] (map (fn [a b] (max a b))
                               (o/values (.contract-a contract) t)
                               (o/values (.contract-b contract) t)))
  Cond
  (values [contract t] (map (fn [c a b] (if c a b))
                               (o/values (.condition contract) t)
                               (o/values (.contract-a contract) t)
                               (o/values (.contract-b contract) t)))
  Scale
  (values [contract t] (map (fn [o c] (* o c))
                               (o/values (.observable contract) t)
                               (o/values (.contract contract) t))))

(def *currency*)

(defn *exch*
  "Given a time series and a currency, return a seq of the exchange
rate from currency to the model currency (*currency*) at each point
in the time series"
  [currency time-series])

(defn *discount*
  "Given a seq of observable values and a seq of contract values over
time, return a seq of the 'fair' contract values over time. This usually
means adjusting contract values down to account for uncertainty about the
observables underlying the contract values."
  [observable-values contract-values])

(defn *snell*
  "Given a seq of observable values and a seq of contract values over
time, calculate the 'Snell envelope' of the contract values under the
observable values. At a high level, the returned seq should reflect the
adjustments to the contract values given that they may be realized
any time the observable values are true."
  [observable-values contract-values])

(defn *absorb*
  "Given a seq of observable values and a seq of contract values over time
return a seq of the 'fair' contract values given that the contract values
given that they may be realized until the first True value in the observable."
  [observable-values contract-values])

(extend-protocol o/Observable
  One
  (values [contract t] (*exch* (.currency contract) t))
  When
  (values [contract t]
          (*discount*
                        (o/values (.observable contract) t)
                        (o/values (.contract contract) t)))
  Anytime
  (values [contract t] (*snell*
                           (o/values (.observable contract) t)
                           (o/values (.contract contract) t)))
  Until
  (values [contract t] (*absorb*
                           (o/values (.observable contract) t)
                           (o/values (.contract contract) t))))

(defn use-default-model
  "Use a model that makes the following assumptions:

- the exchange rate between any two currencies is 1:1
- we have perfect information about the observables underlying a contract
- an 'anytime' contract will always be acquired as soon as possible
- an 'until' contract will always be acquired as late as possible (but will always be acquired)

This means that:

- `*exch` will return a constant `seq` of `1`
- `*discount*` will always return the unmodified contract values `seq`
- `*snell*` is equivalent to `*discount*`
- `*anytime*` will return the unmodified contract values `seq` until the
  first time the observed values `seq` is true. After that it will
  return a constantly 0 `seq`.

Note that there's a very good chance I've completely whiffed on snell and anytime -
IANA financial engineer - feedback requested!
"
  []
  (def *currency* :usd)

  (defn *exch*
    [currency time-series]
    (map (constantly 1) time-series))

  (defn *discount*
    [observable-values contract-values]
    (let [[_ value]
          ;; find the value of the contract the first time the
          ;; observable is true
          (some (fn [[obs _ :as pair]] (if obs pair nil))
                (map vector observable-values contract-values))]
      ;; return a seq that is constantly value until the first time
      ;; the observable is true, and then is constantly 0
      (map (fn [h] (if h (or value 0) 0))
           (drop-last
            (reductions (fn [h [obs _]] (and h (not obs)))
                        true
                        (map vector observable-values contract-values))))))

  (defn *snell*
    [observable-values contract-values])

  (defn *absorb*
    [observable-values contract-values]))
