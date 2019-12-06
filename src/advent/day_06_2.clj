(ns advent.day-06-2
  "--- Part Two ---

Now, you just need to figure out how many orbital transfers you (YOU) need to take to get to Santa (SAN).

You start at the object YOU are orbiting; your destination is the object SAN is orbiting. An orbital
transfer lets you move from any object to an object orbiting or orbited by that object.

For example, suppose you have the following map:

COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN

Visually, the above map of orbits looks like this:

                          YOU
                         /
        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \\
                I - SAN

In this example, YOU are in orbit around K, and SAN is in orbit around I. To move from K to I, a
minimum of 4 orbital transfers are required:

    K to J
    J to E
    E to D
    D to I

Afterward, the map of orbits looks like this:

        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \\
                I - SAN
                 \\
                  YOU

What is the minimum number of orbital transfers required to move from the object YOU are orbiting to
the object SAN is orbiting? (Between the objects they are orbiting - not between YOU and SAN.)"

  (:require
    [advent.day-06-1 :refer [input
                             object-orbits-map
                             total-orbits-seq]]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(defn path-between
  [orbits source dest]
  (let [dest-orbits (total-orbits-seq orbits dest)

        source-path (into '()
                      (take-while (complement (set dest-orbits))
                        (total-orbits-seq orbits source)))

        common (get orbits (first source-path))

        dest-path (into '()
                    (take-while #(not= % common) dest-orbits))]

    (into (conj source-path common) dest-path)))


(defn count-orbital-transfers
  {:test (fn []
           (let [orbits (object-orbits-map ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H"
                                            "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"])]
             (t/is (= 4) (count-orbital-transfers orbits "YOU" "SAN"))))}
  [orbits source dest]
  (-> orbits
    (path-between source dest)
    (count)
    (- 3)))

#_(comment
    (t/test-var #'count-orbital-transfers))


(defn solve
  []
  (-> input
    (object-orbits-map)
    (count-orbital-transfers "YOU" "SAN")))


(comment
  (time (solve))
  (t/run-tests)
  (-> input
    (object-orbits-map)
    (select-keys ["YOU" "SAN"]))
  (-> ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"]
    (object-orbits-map)
    (total-orbits-seq "YOU"))
  (-> ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"]
    (object-orbits-map)
    (total-orbits-seq "SAN"))
  (let [orbits (object-orbits-map ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H"
                                   "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"])]
    (path-between orbits "YOU" "SAN")))
