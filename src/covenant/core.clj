(ns covenant.core
  "The core of the library is small - only 10 primitives.

We define the primitives using a custom def form, which we introduce
in this module."
  (:refer-clojure :exclude [and or when cond])
  (:require [clojure.string :as s]))

(defmacro defcontract
  "Define a type and a helper function for instantiating it.

The helper function isn't strictly necessary, but makes it
easier to use higher order functional programming facilities when
creating new contracts and provides a place to hang a docstring.

The code we'd like to generate looks like:

 (deftype Zero [])
 (defn zero \"A contract representing 0\" [])
"
  [name args & [comment]]
  (let [tname (symbol (s/capitalize (str name)))]
    `(do
       (deftype ~tname ~args)
       (defn ~name ~(str comment) ~args (new ~tname ~@args)))))

;; Now, define each of the contract primitives.
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


