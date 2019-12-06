(ns advent.day-06-1
  "--- Day 6: Universal Orbit Map ---

You've landed at the Universal Orbit Map facility on Mercury. Because navigation in space often
involves transferring between orbits, the orbit maps here are useful for finding efficient routes
between, for example, you and Santa. You download a map of the local orbits (your puzzle input).

Except for the universal Center of Mass (COM), every object in space is in orbit around exactly one
other object. An orbit looks roughly like this:

```
                  \\
                   \\
                    |
                    |
AAA--> o            o <--BBB
                    |
                    |
                   /
                  /
```

In this diagram, the object BBB is in orbit around AAA. The path that BBB takes around AAA (drawn
with lines) is only partly shown. In the map data, this orbital relationship is written AAA)BBB,
which means \"BBB is in orbit around AAA\".

Before you use your map data to plot a course, you need to make sure it wasn't corrupted during the
download. To verify maps, the Universal Orbit Map facility uses orbit count checksums - the total
number of direct orbits (like the one shown above) and indirect orbits.

Whenever A orbits B and B orbits C, then A indirectly orbits C. This chain can be any number of
objects long: if A orbits B, B orbits C, and C orbits D, then A indirectly orbits D.

For example, suppose you have the following map:

```
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
```

Visually, the above map of orbits looks like this:

```
        G - H       J - K - L
       /           /
COM - B - C - D - E - F
               \\
                I
```

In this visual representation, when two objects are connected by a line, the one on the right
directly orbits the one on the left.

Here, we can count the total number of orbits as follows:

  - D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
  - L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of 7 orbits.
  - COM orbits nothing.

The total number of direct and indirect orbits in this example is 42.

What is the total number of direct and indirect orbits in your map data?"

  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.test :as t]))

(set! *warn-on-reflection* true)


(def input
  (-> "advent/day_06_input.txt"
    (io/resource)
    (slurp)
    (string/split-lines)))


(defn object-orbits-map
  "Convert input to map [object name -> orbit center object]"
  {:test (fn []
           (t/is (=
                   {"B" "COM" "L" "K"}
                   (-> (object-orbits-map ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G"
                                           "G)H" "D)I" "E)J" "J)K" "K)L"])
                     (select-keys ["B" "L"])))))}
  [in]
  (->> in
    (mapv #(-> %
             (string/split #"\)")
             (reverse)
             (vec)))
    (into {})))

#_(comment
    (t/test-var #'object-orbits-map))


(defn object-children
  "Convert object orbits map to map of object children."
  {:test (fn []
           (t/is (=
                   {"COM" ["B"], "B" ["C" "G"]}
                   (object-children {"B" "COM", "C" "B", "G" "B"}))))}
  [orbits]
  (into {}
    (for [[k v] (group-by second orbits)]
      [k (mapv first v)])))

#_(comment
    (t/test-var #'object-children))


(defn total-orbits-seq
  "Sequence of all parent orbits for `obj`."
  [orbits obj]
  (->>
    (iterate orbits obj)
    (take-while some?)))


(defn count-total-orbits
  "Total number of direct and indirect orbits for `obj` or for all objects in orbits map."
  {:test (fn []
           (let [orbits (object-orbits-map ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G"
                                            "G)H" "D)I" "E)J" "J)K" "K)L"])]
             (t/is (= 3 (count-total-orbits orbits "D")))
             (t/is (= 7 (count-total-orbits orbits "L")))
             (t/is (= 0 (count-total-orbits orbits "COM")))
             (t/is (= 42 (count-total-orbits orbits)))))}

  ([orbits obj]
   (dec (count (total-orbits-seq orbits obj))))

  ([orbits]
   (reduce + 0
     (map (comp
            (partial count-total-orbits orbits)
            first)
       orbits))))

#_(comment
    (t/test-var #'count-total-orbits))


(defn solve
  []
  (-> input
    (object-orbits-map)
    (count-total-orbits)))


(comment
  (time (solve))
  (t/run-tests)
  input
  (count input)
  (into {}
    (list (reverse (string/split "B)C" #"\)"))))
  (-> ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"]
    (object-orbits-map))
  (-> ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"]
    (object-orbits-map)
    (total-orbits-seq "D"))
  (count (object-orbits-map input)))
