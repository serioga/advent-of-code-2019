(ns advent.day-03-1
  "--- Day 3: Crossed Wires ---

The gravity assist was successful, and you're well on your way to the Venus refuelling station.
During the rush back on Earth, the fuel management system wasn't completely installed, so that's
next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a
central port and extend outward on a grid. You trace the path each wire takes as it leaves the
central port, one wire per line of text (your puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need
to find the intersection point closest to the central port. Because the wires are on a grid, use the
Manhattan distance for this measurement. While the wires do technically cross right at the central
port where they both start, this point does not count, nor does a wire count as crossing with
itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it
goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........

Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:

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

These wires cross at two locations (marked X), but the lower-left one is closer to the central port:
its distance is 3 + 3 = 6.

Here are a few more examples:

    R75,D30,R83,U83,L12,D49,R71,U7,L72
    U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
    R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
    U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

What is the Manhattan distance from the central port to the closest intersection?
"
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


;; Input preparation

(defn ^:private path-cmd
  {:test (fn []
           (t/is (= [\R 75] (path-cmd "R75"))))}
  [s]
  (let [code (first s)
        num (edn/read-string (subs s 1))]
    [code num]))


(defn ^:private compile-path
  [s]
  (->> (string/split s #",")
    (map path-cmd)))


#_(comment
    (t/test-var #'path-cmd)
    (compile-path "R75,D30,R83,U83,L12,D49,R71,U7,L72"))


(def input
  (let [[s1 s2] (-> "advent/day_03_input.txt"
                  (io/resource)
                  (slurp)
                  (string/split-lines))]
    {:path1 s1
     :path2 s2}))


;; Task solution

(defn ->pt
  "Construct grid point."
  [x y]
  [x y])


(def pt0 (->pt 0 0))


(defn pt-x [p] (p 0))
(defn pt-y [p] (p 1))


(defn upd-x [f p] (update p 0 f))
(defn upd-y [f p] (update p 1 f))


(defn abs
  [n]
  (Math/abs ^int n))


(defn dist
  "Manhattan distance between two points."
  [p1 p2]
  (let [dx (abs (- (pt-x p1) (pt-x p2)))
        dy (abs (- (pt-y p1) (pt-y p2)))]
    (+ dx dy)))


(def path-update-point
  {\R (partial upd-x inc)
   \L (partial upd-x dec)
   \U (partial upd-y inc)
   \D (partial upd-y dec)})


(defn new-wire
  []
  (vector pt0))


(defn apply-path-cmd
  "Generate additional points in the wire from single command like `R8`."
  [wire [code num]]
  (into wire
    (->> (iterate (path-update-point code) (peek wire))
      (take (inc num))
      (drop 1))))


(defn build-wire
  "Build collection of wire points from path instructions."
  [path]
  (reduce apply-path-cmd (new-wire) (compile-path path)))


(defn find-cross
  [w1 w2]
  (-> (set/intersection (set w1) (set w2))
    (disj pt0)))


(defn find-cross-min-dist
  {:test (fn []
           (t/is (= 6
                   (find-cross-min-dist
                     (build-wire "R8,U5,L5,D3")
                     (build-wire "U7,R6,D4,L4"))))
           (t/is (= 159
                   (find-cross-min-dist
                     (build-wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                     (build-wire "U62,R66,U55,R34,D71,R55,D58,R83"))))
           (t/is (= 135
                   (find-cross-min-dist
                     (build-wire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
                     (build-wire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))}
  [w1 w2]
  (apply min (mapv
               (partial dist pt0)
               (find-cross w1 w2))))

#_(comment
    (t/test-var #'find-cross-min-dist))


(defn solve
  []
  (let [{:keys [path1 path2]} input]
    (find-cross-min-dist
      (build-wire path1)
      (build-wire path2))))


(comment
  (time (solve))
  (t/run-tests)
  (new-wire)
  (build-wire "R8,U5,L5,D3")
  (build-wire "U7,R6,D4,L4")
  (find-cross
    (build-wire "R8,U5,L5,D3")
    (build-wire "U7,R6,D4,L4"))
  (dist (->pt 1 1) (->pt 1 1)))
