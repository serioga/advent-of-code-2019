(ns advent.day-05-2
  "--- Part Two ---

The air conditioner comes online! Its cold air feels good for a while, but then the TEST alarms
start to go off. Since the air conditioner can't vent its heat anywhere but back into the
spacecraft, it's actually making the air inside the ship warmer.

Instead, you'll need to use the TEST to extend the thermal radiators. Fortunately, the diagnostic
program (your puzzle input) is already equipped for this. Unfortunately, your Intcode computer is
not.

Your computer is only missing a few opcodes:

  - Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to
    the value from the second parameter. Otherwise, it does nothing.
  - Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to
    the value from the second parameter. Otherwise, it does nothing.
  - Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in
    the position given by the third parameter. Otherwise, it stores 0.
  - Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the
    position given by the third parameter. Otherwise, it stores 0.

Like all instructions, these instructions need to support parameter modes as described above.

Normally, after an instruction is finished, the instruction pointer increases by the number of
values in that instruction. However, if the instruction modifies the instruction pointer, that value
is used and the instruction pointer is not automatically increased.

For example, here are several programs that take one input, compare it to the value 8, and then
produce one output:

  - 3,9,8,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is equal to 8;
    output 1 (if it is) or 0 (if it is not).
  - 3,9,7,9,10,9,4,9,99,-1,8 - Using position mode, consider whether the input is less than 8;
    output 1 (if it is) or 0 (if it is not).
  - 3,3,1108,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is equal to 8;
    output 1 (if it is) or 0 (if it is not).
  - 3,3,1107,-1,8,3,4,3,99 - Using immediate mode, consider whether the input is less than 8;
    output 1 (if it is) or 0 (if it is not).

Here are some jump tests that take an input, then output 0 if the input was zero or 1 if the input
was non-zero:

  - 3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9 (using position mode)
  - 3,3,1105,-1,9,1101,0,0,12,4,12,99,1 (using immediate mode)

Here's a larger example:

3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99

The above example program uses an input instruction to ask for a single number. The program will
then output 999 if the input value is below 8, output 1000 if the input value is equal to 8, or
output 1001 if the input value is greater than 8.

This time, when the TEST diagnostic program runs its input instruction to get the ID of the system
to test, provide it 5, the ID for the ship's thermal radiator controller. This diagnostic test suite
only outputs one number, the diagnostic code.

What is the diagnostic code for system ID 5?"

  (:require
    [advent.day-05-1 :refer [clone
                             immediate-param
                             input
                             op-code
                             read-mem
                             read-param
                             run-binary-op!
                             run-input!
                             run-output!
                             write-mem!]]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(defn run-jump-if-true
  "jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value
   from the second parameter. Otherwise, it does nothing."
  ^long [^ints program, ^long addr]
  (if-not (zero? (read-param program addr 1))
    (read-param program addr 2)
    (+ addr 3)))


(defn run-jump-if-false
  "jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from
   the second parameter. Otherwise, it does nothing."
  ^long [^ints program, ^long addr]
  (if (zero? (read-param program addr 1))
    (read-param program addr 2)
    (+ addr 3)))


(defn run-less-than!
  "less than: if the first parameter is less than the second parameter, it stores 1 in the position
   given by the third parameter. Otherwise, it stores 0."
  ^long [^ints program!, ^long addr]
  (let [res (if (< (read-param program! addr 1) (read-param program! addr 2))
              1, 0)]
    (write-mem! program! (immediate-param program! addr 3) res)
    (+ addr 4)))


(defn run-equals!
  "equals: if the first parameter is equal to the second parameter, it stores 1 in the position
   given by the third parameter. Otherwise, it stores 0."
  ^long [^ints program!, ^long addr]
  (let [res (if (= (read-param program! addr 1) (read-param program! addr 2))
              1, 0)]
    (write-mem! program! (immediate-param program! addr 3) res)
    (+ addr 4)))


(defn run-program
  "Execute program and return value at position 0"
  {:test (fn []
           (t/is (== 3500 (run-program [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50] nil)))
           (t/is (== 2 (run-program [1, 0, 0, 0, 99] nil)))
           (t/is (== 2 (run-program [2, 3, 0, 3, 99] nil)))
           (t/is (== 2 (run-program [2, 4, 4, 5, 99, 0] nil)))
           (t/is (== 30 (run-program [1, 1, 1, 4, 99, 5, 6, 0, 99] nil)))
           (t/is (== 1002 (run-program [1002, 4, 3, 4, 33] nil)))
           (t/is (== 1 (run-program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] 8)))
           (t/is (== 0 (run-program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8] 9)))
           (t/is (== 1 (run-program [3, 3, 1108, -1, 8, 3, 4, 3, 99] 8)))
           (t/is (== 0 (run-program [3, 3, 1108, -1, 8, 3, 4, 3, 99] 9)))
           (t/is (== 0 (run-program [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] 0)))
           (t/is (== 1 (run-program [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9] 2)))
           (t/is (== 0 (run-program [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] 0)))
           (t/is (== 1 (run-program [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1] 2)))
           #_(t/is (== 999 (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                         1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                         999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] 7)))
           (t/is (== 1000 (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] 8)))
           (t/is (== 1001 (run-program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                                        1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                        999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99] 9))))}
  [program, in]
  (let [*io (atom in)]
    (loop [program! (clone program)
           addr 0]
      (if-some [next-addr (case (op-code (read-mem program! addr))
                            1 (run-binary-op! program! addr +)
                            2 (run-binary-op! program! addr *)
                            3 (run-input! program! addr *io)
                            4 (run-output! program! addr *io)
                            5 (run-jump-if-true program! addr)
                            6 (run-jump-if-false program! addr)
                            7 (run-less-than! program! addr)
                            8 (run-equals! program! addr)
                            99 nil)]
        (recur program! (long next-addr))
        (if (some? in)
          @*io
          (read-mem program! 0))))))

#_(comment
    (t/test-var #'run-program))


(defn solve
  [init-id]
  (run-program input init-id))


(comment
  (solve 5)
  (t/run-tests))
