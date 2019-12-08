(ns advent.day-07-2
  "--- Part Two ---

It's no good - in this configuration, the amplifiers can't generate a large enough output signal to
produce the thrust you'll need. The Elves quickly talk you through rewiring the amplifiers into a
feedback loop:

      O-------O  O-------O  O-------O  O-------O  O-------O
0 -+->| Amp A |->| Amp B |->| Amp C |->| Amp D |->| Amp E |-.
   |  O-------O  O-------O  O-------O  O-------O  O-------O |
   |                                                        |
   '--------------------------------------------------------+
                                                            |
                                                            v
                                                     (to thrusters)

Most of the amplifiers are connected as they were before; amplifier A's output is connected to
amplifier B's input, and so on. However, the output from amplifier E is now connected into amplifier
A's input. This creates the feedback loop: the signal will be sent through the amplifiers many
times.

In feedback loop mode, the amplifiers need totally different phase settings: integers from 5 to 9,
again each used exactly once. These settings will cause the Amplifier Controller Software to
repeatedly take input and produce output many times before halting. Provide each amplifier its phase
setting at its first input instruction; all further input/output instructions are for signals.

Don't restart the Amplifier Controller Software on any amplifier during this process. Each one
should continue receiving and sending signals until it halts.

All signals sent or received in this process will be between pairs of amplifiers except the very
first signal and the very last signal. To start the process, a 0 signal is sent to amplifier A's
input exactly once.

Eventually, the software on the amplifiers will halt after they have processed the final loop. When
this happens, the last output signal from amplifier E is sent to the thrusters. Your job is to find
the largest output signal that can be sent to the thrusters using the new phase settings and
feedback loop arrangement.

Here are some example programs:

    Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):

    3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5

    Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):

    3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
    -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
    53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10

Try every combination of the new phase settings on the amplifier feedback loop. What is the highest
signal that can be sent to the thrusters?"

  (:require
    [advent.day-05-2a :refer [halt?
                              init-state
                              last-output
                              run-program]]
    [advent.day-07-1 :refer [input]]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(defn create-amplifier
  [program phase]
  (init-state program phase))


(defn set-input
  [amplifier in]
  (update amplifier :state/in conj in))


(defn reset-output
  [amplifier]
  (assoc amplifier :state/out []))


(defn test-amplifier
  [amplifier in]
  (-> amplifier
    (reset-output)
    (set-input in)
    (run-program (some-fn last-output halt?))))


(defn feedback-loop
  "Signal in single loop."
  [amplifiers in]
  (loop [amplifiers amplifiers
         signal in
         i 0]
    (if-let [amp (some->
                   (get amplifiers i)
                   (test-amplifier signal))]
      (recur
        (assoc amplifiers i amp)
        (last-output amp)
        (inc i))
      amplifiers)))


(defn init-amplifiers
  [program phases]
  (mapv (partial create-amplifier program) phases))


(defn thruster-signal
  {:test (fn []
           (t/is (= 139629729
                   (thruster-signal
                     [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
                      27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]
                     [9, 8, 7, 6, 5])))
           (t/is (= 18216
                   (thruster-signal
                     [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
                      -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
                      53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10]
                     [9, 7, 8, 5, 6]))))}
  ([program phases]
   (thruster-signal program phases 0))

  ([program phases in]
   (loop [amplifiers (init-amplifiers program phases)
          signal in]
     (let [res (feedback-loop amplifiers signal)
           out (-> res peek last-output)]
       (if out
         (recur res out)
         (-> amplifiers peek last-output))))))

#_(comment
    (t/test-var #'thruster-signal))


(def phases (range 5 10))


(defn max-thruster-signal
  {:test (fn []
           (t/is (= 139629729 (max-thruster-signal
                                [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
                                 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5])))
           (t/is (= 18216 (max-thruster-signal
                            [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
                             -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
                             53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10]))))}
  [program]
  (reduce max
    (for [a phases, b phases, c phases, d phases, e phases
          :when (distinct? a b c d e)]
      (thruster-signal program [a b c d e] 0))))

#_(comment
    (t/test-var #'max-thruster-signal))


(defn solve
  []
  (max-thruster-signal input))


(comment
  (solve)
  (t/run-tests))

