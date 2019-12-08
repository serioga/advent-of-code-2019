(ns advent.day-05-2a
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(def input
  (-> "advent/day_05_input.txt"
    (io/resource)
    (slurp)
    (string/split #",")
    (->> (map edn/read-string))))


(defn init-state
  ([mem]
   (init-state mem nil))

  ([mem in]
   #:state{:addr 0
           :mem (vec mem)
           :in (cond-> in
                 (not (coll? in)) vector)
           :out []
           :exit-code nil}))


(defn read-mem
  ([{:state/keys [addr] :as state}]
   (read-mem state addr))

  ([{:state/keys [mem]}, addr]
   (mem addr))

  ([{:state/keys [mem]}, addr, offset]
   (mem (+ addr offset))))


(defn write-mem
  [state addr v]
  (assoc-in state [:state/mem addr] v))


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


(defn position-param
  [{:state/keys [addr] :as state}, n]
  (read-mem state
    (read-mem state addr n)))


(defn immediate-param
  [{:state/keys [addr] :as state}, n]
  (read-mem state, addr, n))


(defn read-param
  [state, n]
  (case (nth-param-mode (read-mem state) n)
    0 (position-param state n)
    1 (immediate-param state n)))


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
        res-addr (immediate-param state 3)]
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
        res-addr (immediate-param state 3)]
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
  (let [res-addr (immediate-param state 1)
        v (first in)]
    (println "Input" res-addr "<<" v)
    (-> state
      (write-mem res-addr v)
      (update :state/in subvec 1)
      (move-addr 2))))


(defmethod operate 4
  [state]
  (let [v (position-param state 1)]
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


(defmethod operate 99
  [state]
  (assoc state :state/exit-code (read-mem state 0)))


(defn run-program-until
  "Execute program until `(stop? state)` is true and return state."
  [state stop?]
  (->>
    (iterate operate state)
    (reduce
      (fn [_ state]
        (cond-> state
          (stop? state) reduced)))))


(defn run-program
  "Execute program until `(stop? state)` is true and return state."
  [state stop?]
  (->>
    (iterate operate state)
    (some #(when (stop? %) %))))


(defn run-tests
  "Execute diagnostic program and return last output."
  {:test (fn []
           (t/is (== 1 (run-tests [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] 8)))
           (t/is (== 0 (run-tests [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] 9)))
           (t/is (== 1 (run-tests [3, 3, 1108, -1, 8, 3, 4, 3, 99] 8)))
           (t/is (== 0 (run-tests [3, 3, 1108, -1, 8, 3, 4, 3, 99] 9)))
           (t/is (== 0 (run-tests [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] 0)))
           (t/is (== 1 (run-tests [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] 2)))
           (t/is (== 0 (run-tests [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] 0)))
           (t/is (== 1 (run-tests [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] 2)))
           #_(t/is (== 999 (run-tests [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                       1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                       999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] 7)))
           (t/is (== 1000 (run-tests [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                      1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                      999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] 8)))
           (t/is (== 1001 (run-tests [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                      1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                      999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] 9))))}
  [mem, in]
  (-> (init-state mem in)
    (run-program halt?)
    (last-output)))


#_(comment
    (t/test-var #'run-tests))


(defn solve
  [init-id]
  (run-tests input init-id))


(comment
  (time (solve 5))
  (time (solve 1))
  (t/run-tests))

