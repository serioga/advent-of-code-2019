(ns advent.day-09-1
  "--- Day 9: Sensor Boost ---

You've just said goodbye to the rebooted rover and left Mars when you receive a faint distress
signal coming from the asteroid belt. It must be the Ceres monitoring station!

In order to lock on to the signal, you'll need to boost your sensors. The Elves send up the latest
BOOST program - Basic Operation Of System Test.

While BOOST (your puzzle input) is capable of boosting your sensors, for tenuous safety reasons, it
refuses to do so until the computer it runs on passes some checks to demonstrate it is a complete
Intcode computer.

Your existing Intcode computer is missing one key feature: it needs support for parameters in
relative mode.

Parameters in mode 2, relative mode, behave very similarly to parameters in position mode: the
parameter is interpreted as a position. Like position mode, parameters in relative mode can be read
from or written to.

The important difference is that relative mode parameters don't count from address 0. Instead, they
count from a value called the relative base. The relative base starts at 0.

The address a relative mode parameter refers to is itself plus the current relative base. When the
relative base is 0, relative mode parameters and position mode parameters with the same value refer
to the same address.

For example, given a relative base of 50, a relative mode parameter of -7 refers to memory address
50 + -7 = 43.

The relative base is modified with the relative base offset instruction:

    Opcode 9 adjusts the relative base by the value of its only parameter. The relative base
    increases (or decreases, if the value is negative) by the value of the parameter.

For example, if the relative base is 2000, then after the instruction 109,19, the relative base
would be 2019. If the next instruction were 204,-34, then the value at address 1985 would be output.

Your Intcode computer will also need a few other capabilities:

  - The computer's available memory should be much larger than the initial program. Memory beyond
    the initial program starts with the value 0 and can be read or written like any other memory.
    (It is invalid to try to access memory at a negative address, though.)
  - The computer should have support for large numbers. Some instructions near the beginning of the
    BOOST program will verify this capability.

Here are some example programs that use these features:

  - 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99 takes no input and produces a copy of
    itself as output.
  - 1102,34915192,34915192,7,4,7,99,0 should output a 16-digit number.
  - 104,1125899906842624,99 should output the large number in the middle.

The BOOST program will ask for a single input; run it in test mode by providing it the value 1. It
will perform a series of checks on each opcode, output any opcodes (and the associated parameter
modes) that seem to be functioning incorrectly, and finally output a BOOST keycode.

Once your Intcode computer is fully functional, the BOOST program should report no malfunctioning
opcodes when run in test mode; it should only output a single value, the BOOST keycode. What BOOST
keycode does it produce?"

  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(defn init-state
  ([mem]
   (init-state mem nil))

  ([mem in]
   #:state{:addr 0
           :mem (vec mem)
           :in (cond-> in
                 (not (coll? in)) vector)
           :out []
           :exit-code nil
           :relative-base 0}))


(defn read-mem
  ([{:state/keys [addr] :as state}]
   (read-mem state addr))

  ([{:state/keys [mem]}, addr]
   (get mem addr 0)))


(defn write-ex
  [arr i v]
  (let [i-max (dec (count arr))]
    (if (<= i i-max)
      (assoc arr i v)
      (-> arr
        (into (repeat (dec (- i i-max)) 0))
        (conj v)))))


(defn write-mem
  [{:state/keys [mem] :as state}, addr, v]
  (assoc state :state/mem (write-ex mem addr v)))


(defn move-addr
  [state n]
  (update state :state/addr + n))


(defn jump-to-addr
  [state addr]
  (assoc state :state/addr addr))


(defn halt?
  [state]
  (some? (:state/exit-code state)))


(defn last-output
  [state]
  (-> state :state/out peek))


(defn op-code
  [v]
  (if (< v 99) v, (rem v 100)))


(defn exp [x n]
  (if (zero? n)
    1
    (* x (exp x (dec n)))))


(defn nth-param-mode
  {:test (fn []
           (t/is (= 0 (nth-param-mode 1002 1)))
           (t/is (= 1 (nth-param-mode 1002 2)))
           (t/is (= 0 (nth-param-mode 1002 3))))}
  ^long [v n]
  (-> v
    (quot (exp 10 (inc n)))
    (rem 10)))

#_(comment
    (t/test-var #'nth-param-mode))


(defn param-addr
  [{:state/keys [addr] :as state}, n]
  (case (nth-param-mode (read-mem state addr) n)
    0 (read-mem state (+ addr n))
    1 (+ addr n)
    2 (+
        (read-mem state (+ addr n))
        (:state/relative-base state))))


(defn read-param
  [state, n]
  (read-mem state (param-addr state n)))


(defmulti operate
  "Execute current operation and return modified state."
  (fn current-op-code
    [state]
    (-> state read-mem op-code)))


(defn apply-binary-op
  "Execute +/* operation."
  [state, op]
  (let [a (read-param state 1)
        b (read-param state 2)
        res-addr (param-addr state 3)]
    (-> state
      (write-mem res-addr (op a b))
      (move-addr 4))))


(defn apply-logical-op
  [state, pred]
  (if (pred (read-param state 1))
    (jump-to-addr state (read-param state 2))
    (move-addr state 3)))


(defn apply-compare-op
  [state, op]
  (let [a (read-param state 1)
        b (read-param state 2)
        res-addr (param-addr state 3)]
    (-> state
      (write-mem res-addr (if (op a b) 1, 0))
      (move-addr 4))))


(defmethod operate 1
  [state]
  (apply-binary-op state +))


(defmethod operate 2
  [state]
  (apply-binary-op state *))


(defmethod operate 3
  [{:state/keys [in] :as state}]
  (let [res-addr (param-addr state 1)
        v (first in)]
    (println "Input" res-addr "<<" v)
    (-> state
      (write-mem res-addr v)
      (update :state/in subvec 1)
      (move-addr 2))))


(defmethod operate 4
  [state]
  (let [v (read-param state 1)]
    (println "Output" v)
    (-> state
      (update :state/out conj v)
      (move-addr 2))))


(defmethod operate 5
  [state]
  (apply-logical-op state (complement zero?)))


(defmethod operate 6
  [state]
  (apply-logical-op state zero?))


(defmethod operate 7
  [state]
  (apply-compare-op state <))


(defmethod operate 8
  [state]
  (apply-compare-op state =))


(defmethod operate 9
  [state]
  (let [offset (read-param state 1)]
    #_(println "Adjusts the relative base" offset)
    (-> state
      (update :state/relative-base + offset)
      (move-addr 2))))


(defmethod operate 99
  [state]
  (assoc state :state/exit-code (read-mem state 0)))


(defn run-program
  "Execute program until `(stop? state)` is true and return state."
  [state stop?]
  (->>
    (iterate operate state)
    (some #(when (stop? %) %))))


(defn run-tests
  "Execute diagnostic program and return last output."
  {:test (fn []
           (t/is (== 99 (run-tests [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99])))
           (t/is (== 1219070632396864 (run-tests [1102, 34915192, 34915192, 7, 4, 7, 99, 0])))
           (t/is (== 1125899906842624 (run-tests [104, 1125899906842624, 99]))))}

  ([mem]
   (-> (init-state mem)
     (run-program halt?)
     (last-output)))

  ([mem, in]
   (-> (init-state mem in)
     (run-program halt?)
     (last-output))))

#_(comment
    (t/test-var #'run-tests))


(def input
  (-> "advent/day_09_input.txt"
    (io/resource)
    (slurp)
    (string/split #",")
    (->> (map edn/read-string))))


(defn solve
  [in]
  (run-tests input in))


(comment
  (solve 1)
  (t/run-tests)
  (do input))

