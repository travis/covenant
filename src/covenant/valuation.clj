(ns covenant.valuation
  (:require [covenant.core :as c]
            [covenant.observable :as o])
  (:import [covenant.core Zero One Give And Or Cond Scale When Anytime Until]))

;; valuation model

(extend-protocol Observable
  Zero
  (values [_ _] (repeat 0))
  One
  (values [contract t] (*exch* (.currency contract) t))
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

(def *currency* :usd)

(defn *exch*
  ""
  [currency time-series]
  (map (constantly 1) time-series))

(defn *discount*
  [observable-values contract-values]
  contract-values)

(defn *snell*
  [observable-values contract-values]
  contract-values)

(defn *absorb*
  [observable-values contract-values]
  contract-values)

(extend-protocol Observable
  One
  (values [contract t] (*exch* (.currency contract) t))
  When
  (values [contract t] (*discount*
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

