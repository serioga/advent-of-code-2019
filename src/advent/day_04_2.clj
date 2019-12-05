(ns advent.day-04-2
  "--- Part Two ---

An Elf just remembered one more important detail: the two adjacent matching digits are not part of a
larger group of matching digits.

Given this additional criterion, but still ignoring the range rule, the following are now true:

  - 112233 meets these criteria because the digits never decrease and all repeated digits are
    exactly two digits long.
  - 123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
  - 111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double
    22).

How many different passwords within the range given in your puzzle input meet all of the criteria?"

  (:require
    [advent.day-04-1 :refer [char-code]]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(defn has-exact-digit-count?
  "Check if string contains a char repeated only twice."
  {:test (fn []
           (t/is (true? (has-exact-digit-count? "112233")))
           (t/is (false? (has-exact-digit-count? "123444"))))}
  [s]
  (boolean
    (some #(= % 2)
      (vals (frequencies s)))))

#_(comment
    (t/test-var #'has-exact-digit-count?))


(defn has-double?
  "Check if string contains a group of exactly two chars."
  {:test (fn []
           (t/is (true? (has-double? "112233")))
           (t/is (false? (has-double? "123444"))))}
  [s]
  (boolean
    (some #(= 2 (count %))
      (partition-by identity s))))

#_(comment
    (t/test-var #'has-double?))


(defn password?
  {:test (fn []
           (t/is (true? (password? 112233)))
           (t/is (false? (password? 123444)))
           (t/is (true? (password? 111122))))}
  [n]
  (let [s (str n)]
    (and
      (apply <= (mapv char-code s))
      (has-double? s))))

#_(comment
    (t/test-var #'password?))


(defn count-passwords
  [start end]
  (count (->> (range start (inc end))
           (filter password?))))


(defn solve
  []
  (count-passwords 307237 769058))


(comment
  (time (solve))
  (t/run-tests))
