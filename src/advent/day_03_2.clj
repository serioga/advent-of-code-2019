(ns advent.day-03-2
  "--- Part Two ---

It turns out that this circuit is very timing-sensitive; you actually need to minimize the signal
delay.

To do this, calculate the number of steps each wire takes to reach each intersection; choose the
intersection where the sum of both wires' steps is lowest. If a wire visits a position on the grid
multiple times, use the steps value from the first time it visits that position when calculating the
total value of a specific intersection.

The number of steps a wire takes is the total number of grid squares the wire has entered to get to
that location, including the intersection being considered. Again consider the example from above:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

In the above example, the intersection closest to the central port is reached after 8+5+5+2 = 20
steps by the first wire and 7+6+4+3 = 20 steps by the second wire for a total of 20+20 = 40 steps.

However, the top-right intersection is better: the first wire takes only 8+5+2 = 15 and the second
wire takes only 7+6+2 = 15, a total of 15+15 = 30 steps.

Here are the best steps for the extra examples from above:

    R75,D30,R83,U83,L12,D49,R71,U7,L72
    U62,R66,U55,R34,D71,R55,D58,R83 = 610 steps
    R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
    U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = 410 steps

What is the fewest combined steps the wires must take to reach an intersection?
"
  (:require
    [advent.day-03-1 :refer [build-wire, find-cross, input, ->pt]]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(defn wire-len-to
  "The number of steps a wire takes is the total number of grid squares the wire has entered to get
  to that location `px`, including the intersection being considered."
  {:test (fn []
           (t/is (= 20 (wire-len-to
                         (build-wire "R8,U5,L5,D3")
                         (->pt 3 3))))
           (t/is (= 20 (wire-len-to
                         (build-wire "U7,R6,D4,L4")
                         (->pt 3 3))))
           (t/is (= 15 (wire-len-to
                         (build-wire "R8,U5,L5,D3")
                         (->pt 6 5))))
           (t/is (= 15 (wire-len-to
                         (build-wire "U7,R6,D4,L4")
                         (->pt 6 5)))))}
  [wire px]
  (count
    (take-while #(not= % px) wire)))

#_(comment
    (t/test-var #'wire-len-to))


(defn find-best-steps
  "Find best intersection of two wires."
  {:test (fn []
           (t/is (= 610 (find-best-steps
                          (build-wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                          (build-wire "U62,R66,U55,R34,D71,R55,D58,R83"))))
           (t/is (= 410 (find-best-steps
                          (build-wire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
                          (build-wire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))}
  [w1 w2]
  (apply min (mapv (fn
                     [px]
                     (+ (wire-len-to w1 px) (wire-len-to w2 px)))
               (find-cross w1 w2))))

#_(comment
    (t/test-var #'find-best-steps))


(defn solve
  []
  (let [{:keys [path1 path2]} input]

    (find-best-steps
      (build-wire path1)
      (build-wire path2))))


(comment
  (time (solve))
  (t/run-tests)
  (build-wire "R8,U5,L5,D3")
  (build-wire "U7,R6,D4,L4")
  (part1/find-cross
    (build-wire "R8,U5,L5,D3")
    (build-wire "U7,R6,D4,L4")))
