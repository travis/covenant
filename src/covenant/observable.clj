(ns covenant.observable
  "Introduce the primitive Observables we'll use to interact with the world.

An Observable
is an \"objective... possibly time varying\" quantity whose true
value may or may not be known at the time the contract is entered
into. The Nasdaq, the value of the principal on a mortgage and the
temperature outside my front door are all Observables, and it is easy
to imagine writing contracts against each of them.

We introduce a protocol for Observables that specifies a single function, values.
Given a time series, the values function should return the value of the
Observable at each point in time in the series.
"
  (:refer-clojure :exclude [+ - * / max min])
  (:require [clojure.string :as s]))

(defprotocol Observable
  (values [observable time-series] "Given a time series, return the value of the
Observable at each point in time in the series."))

(defmacro defobservable
  "Similar to defcontract, introduce a type and a helper function. Observables
should also include an implementation for the values function, and this
is supported directly by this macro."
  [name args values & [comment]]
  (let [tname (symbol (s/capitalize (str name)))]
    `(do
       (deftype ~tname ~args Observable ~values)
       (defn ~name ~(str comment) ~args (new ~tname ~@args)))))

(defobservable const [c]
  (values [observable t] (map (constantly c) t)))

(deftype Lift [f args]
  Observable
  (values [observable t] (apply map f (map #(values % t) args))))
(defn lift [f & args] (Lift. f args))

(comment
 (values (lift (fn [a b c] (+ a b c)) (const 5) (const 6) (const 7)) (repeat 5 1)))

;; numerical observables

(defn + [& args] (apply lift clojure.core/+ args))
(defn - [& args] (apply lift clojure.core/- args))
(defn * [& args] (apply lift clojure.core/* args))
(defn / [& args] (apply lift clojure.core// args))
(defn max [& args] (apply lift clojure.core/max args))
(defn min [& args] (apply lift clojure.core/min args))

(comment
  (values (+ (const 5) (const 6)) [1 2 3 4 5]))

;; date observables

(defobservable date []
  (values [_ t] t))

(defn at [t] (lift #(= % t) (date)))

;; miscellaneous observables useful enough to be included here

(defobservable schedule
  [sched]
  (values [_ t] (map sched t)))
