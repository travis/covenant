(ns covenant.examples.bond
  "A simple example to demonstrate the library. Model a 4 payment bond with
an interest rate of 1.5%."
  (:require
   [covenant.core :as c]
   [covenant.valuation :as v]
   [covenant.observable :as o]))


(def total 100)

(def principal-payment (/ total 4))
(def principal-payments (o/const principal-payment))

(def principal (o/schedule
                {0 total
                 1 total
                 2 (- total principal-payment)
                 3 (- total (* principal-payment 2))
                 4 (- total (* principal-payment 3))
                 5 (- total (* principal-payment 4))}))

;; an observable of the interest payments on our bond
(def interest-payments (o/* (o/const 0.015) principal))

;; an observable of the payments on our bond
(def payments (o/+ interest-payments principal-payments))

;; a contract for the interest payment at a specific time
(defn payment-at
  [time]
  (c/when (o/at time) (c/scale payments (c/one :usd))))

(comment
  (o/values (o/const 0.013) [0 1 2 3 4 5])
  (o/values (o/* (o/const 5) (o/const 3)) [0 1 2 3 4 5])
  (o/values interest-payments [0 1 2 3 4 5])
  (o/values payments [0 1 2 3 4 5])
  (o/values (payment-at 2) [0 1 2 3 4 5])
  (o/values (payment-at 6) [0 1 2 3 4 5])
  )

(def four-payment-bond
     (c/and (c/when (o/at 0) (c/scale (o/const 100) (c/one :usd)))
            (c/give (c/and (payment-at 1)
                           (c/and (payment-at 2)
                                  (c/and (payment-at 3)
                                         (payment-at 4)))))))

(comment
  (o/values four-payment-bond [0 1 2 3 4 5]))

