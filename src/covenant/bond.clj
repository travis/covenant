(ns covenant.bond
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
(def interest-payments (o/* (o/const 0.015) principal))

(def payments (o/+ interest-payments principal-payments))

(defn interest-payment-at
  [time]
  (c/when (o/at time) (c/scale payments (c/one :usd))))

(comment
  (o/values (o/const 0.013) [0 1 2 3 4 5])
  (o/values (o/* (o/const 5) (o/const 3)) [0 1 2 3 4 5])
  (o/values interest-payments [0 1 2 3 4 5])
  (o/values payments [0 1 2 3 4 5]))

(def four-payment-bond
     (c/and (c/when (o/at 0) (c/scale (o/const 100) (c/one :usd)))
            (c/give (c/and (interest-payment-at 1)
                           (c/and (interest-payment-at 2)
                                  (c/and (interest-payment-at 3)
                                         (interest-payment-at 4)))))))
(comment
 (v/valuation four-payment-bond [1 2 3 4 ]))

