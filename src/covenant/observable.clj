(ns covenant.observable
  (:refer-clojure :exclude [+ - * / max min])
  (:require [clojure.string :as s]))

(defprotocol Observable
  (values [observable time-series]))

(defmacro defobservable
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

;;(values (lift (fn [a b c] (+ a b c)) (const 5) (const 6) (const 7)) (repeat 5 1))

;; numerical

(defn + [& args] (apply lift clojure.core/+ args))
(defn - [& args] (apply lift clojure.core/- args))
(defn * [& args] (apply lift clojure.core/* args))
(defn / [& args] (apply lift clojure.core// args))
(defn max [& args] (apply lift clojure.core/max args))
(defn min [& args] (apply lift clojure.core/min args))

(comment
  (values (+ (const 5) (const 6)) [1 2 3 4 5])
  )

;; date

(defobservable date []
  (values [_ t] t))

(defn at [t] (lift #(= % t) (date)))

(defobservable schedule
  [sched]
  (values [_ t] (map #(sched %) t)))
