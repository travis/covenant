(ns covenant.core
  (:refer-clojure :exclude [and or when cond])
  (:require [clojure.string :as s]))

(defmacro defcontract
  [name args & [comment]]
  (let [tname (symbol (s/capitalize (str name)))]
    `(do
       (deftype ~tname ~args)
       (defn ~name ~(str comment) ~args (new ~tname ~@args)))))

(defcontract zero []
  "A contract to receive nothing.")
(defcontract one [currency]
  "A contract to receive one unit of currency.")
(defcontract give [contract]
  "A contract to give a counterparty a specific contract.")
(defcontract and [contract-a contract-b]
  "A contract to receive both contract-a and contract-b.")
(defcontract or [contract-a contract-b]
  "A contract representing a choice between receiving contract-a and receiving contract-b.")
(defcontract cond [condition contract-a contract-b]
  "A contract to receive either contract-a or contract-b according to a conditional (that is, boolean) observable.")
(defcontract scale [observable contract]
  "A contract to receive a contract scaled up by the value of an observable.")
(defcontract when [observable contract]
  "A contract to receive another contract at a particular time.")
(defcontract anytime [observable contract]
  "A contract granting the option to receive another contract any time during a particular period.")
(defcontract until [observable contract]
  "A contract granting the option to receive another contract until a particular time.")


