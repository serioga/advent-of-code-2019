(ns advent.day-04-1
  "--- Day 4: Secure Container ---

  You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had
  written the password on a sticky note, but someone threw it out.

  However, they do remember a few key facts about the password:

    It is a six-digit number.
    The value is within the range given in your puzzle input.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same
    (like 111123 or 135679).

  Other than the range rule, the following are true:

    111111 meets these criteria (double 11, never decreases).
    223450 does not meet these criteria (decreasing pair of digits 50).
    123789 does not meet these criteria (no double).

  How many different passwords within the range given in your puzzle input meet these criteria?"

  (:require
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(defn char-code
  [^Character c]
  (Character/getNumericValue c))


(defn password?
  {:test (fn []
           (t/is (true? (password? 111111)))
           (t/is (false? (password? 223450)))
           (t/is (false? (password? 123789))))}
  [n]
  (let [s (str n)]
    (and
      (apply <= (mapv char-code s))
      (< (count (set s)) (count s)))))

#_(comment
    (t/test-var #'password?))


(defn count-passwords
  {:test (fn []
           (t/is (= 1 (count-passwords 111111 111111)))
           (t/is (= 3 (count-passwords 122345 122347))))}
  [start end]
  (count (->> (range start (inc end))
           (filter password?))))


(defn solve
  []
  (count-passwords 307237 769058))


(comment
  (time (solve))
  (t/run-tests))
