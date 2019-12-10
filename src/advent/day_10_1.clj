(ns advent.day-10-1
  "--- Day 10: Monitoring Station ---

You fly into the asteroid belt and reach the Ceres monitoring station. The Elves here have an
emergency: they're having trouble tracking all of the asteroids and can't be sure they're safe.

The Elves would like to build a new monitoring station in a nearby area of space; they hand you a
map of all of the asteroids in that region (your puzzle input).

The map indicates whether each position is empty (.) or contains an asteroid (#). The asteroids are
much smaller than they appear on the map, and every asteroid is exactly in the center of its marked
position. The asteroids can be described with X,Y coordinates where X is the distance from the left
edge and Y is the distance from the top edge (so the top-left corner is 0,0 and the position
immediately to its right is 1,0).

Your job is to figure out which asteroid would be the best place to build a new monitoring station.
A monitoring station can detect any asteroid to which it has direct line of sight - that is, there
cannot be another asteroid exactly between them. This line of sight can be at any angle, not just
lines aligned to the grid or diagonally. The best location is the asteroid that can detect the
largest number of other asteroids.

For example, consider the following map:

.#..#
.....
#####
....#
...##

The best location for a new monitoring station on this map is the highlighted asteroid at 3,4
because it can detect 8 asteroids, more than any other location. (The only asteroid it cannot detect
is the one at 1,0; its view of this asteroid is blocked by the asteroid at 2,2.) All other asteroids
are worse locations; they can detect 7 or fewer other asteroids. Here is the number of other
asteroids a monitoring station on each asteroid could detect:

.7..7
.....
67775
....7
...87

Here is an asteroid (#) and some examples of the ways its line of sight might be blocked. If there
were another asteroid at the location of a capital letter, the locations marked with the
corresponding lowercase letter would be blocked and could not be detected:

#.........
...A......
...B..a...
.EDCG....a
..F.c.b...
.....c....
..efd.c.gb
.......c..
....f...c.
...e..d..c

Here are some larger examples:

    Best is 5,8 with 33 other asteroids detected:

    ......#.#.
    #..#.#....
    ..#######.
    .#.#.###..
    .#..#.....
    ..#....#.#
    #..#....#.
    .##.#..###
    ##...#..#.
    .#....####

    Best is 1,2 with 35 other asteroids detected:

    #.#...#.#.
    .###....#.
    .#....#...
    ##.#.#.#.#
    ....#.#.#.
    .##..###.#
    ..#...##..
    ..##....##
    ......#...
    .####.###.

    Best is 6,3 with 41 other asteroids detected:

    .#..#..###
    ####.###.#
    ....###.#.
    ..###.##.#
    ##.##.#.#.
    ....###..#
    ..#.#..#.#
    #..#.#.###
    .##...##.#
    .....#.#..

    Best is 11,13 with 210 other asteroids detected:

    .#..##.###...#######
    ##.############..##.
    .#.######.########.#
    .###.#######.####.#.
    #####.##.#.##.###.##
    ..#####..#.#########
    ####################
    #.####....###.#.#.##
    ##.#################
    #####.##.###..####..
    ..######..##.#######
    ####.##.####...##..#
    .#####..#.######.###
    ##...#.##########...
    #.##########.#######
    .####.#.###.###.#.##
    ....##.##.###..#####
    .#.#.###########.###
    #.#.#.#####.####.###
    ###.##.####.##.#..##

Find the best location for a new monitoring station. How many other asteroids can be detected from
that location?"
  (:require [clojure.string :as string]))

(set! *warn-on-reflection* true)


(defn parse-asteroid-map
  "Collect set of asteroid coordinates [x y]."
  [s]
  (let [lines (string/split-lines s)
        w (count (first lines))
        h (count lines)]
    (set
      (for [x (range w)
            y (range h)
            :when (#{\#} (-> lines (get y) (get x)))]
        [x y]))))


(defn remove-invisible
  "Return `others` without hidden for `from` by `to`."
  [[x-from y-from], [x-to y-to], others]
  (let [dx' (- x-to x-from)
        dy' (- y-to y-from)
        hidden? (fn [[x y]] (let [dx (- x x-from)
                                  dy (- y y-from)]
                              (println [x-from y-from] [x-to y-to] [x y] "-" dx' dy' "-" dx dy "-" (and
                                                                                                     (not (zero? dy'))
                                                                                                     (zero? (rem dy dy'))
                                                                                                     (pos? (* dy dy')))
                                (and
                                  (or
                                    (= dx dx')
                                    (and
                                      (not (zero? dx'))
                                      (zero? (rem dx dx'))
                                      (pos? (* dx dx'))))
                                  (or
                                    (= dy dy')
                                    (and
                                      (not (zero? dy'))
                                      (zero? (rem dy dy'))
                                      (pos? (* dy dy'))))
                                  (not
                                    (and
                                      (= x x-to)
                                      (= y y-to)))))
                              (and
                                (or
                                  (and
                                    (zero? dx')
                                    (zero? dx))
                                  (and
                                    (not (zero? dx'))
                                    (zero? (rem dx dx'))
                                    (pos? (* dx dx'))))
                                (or
                                  (and
                                    (zero? dy')
                                    (zero? dy))
                                  (and
                                    (not (zero? dy'))
                                    (zero? (rem dy dy'))
                                    (pos? (* dy dy'))))
                                (not
                                  (and
                                    (= x x-to)
                                    (= y y-to))))))]
    (into #{}
      (remove hidden? others))))


(defn collect-visible
  [from, asteroid-map]
  (reduce (fn [m to]
            (println to m)
            (remove-invisible from to m))
    (disj asteroid-map from)
    (disj asteroid-map from)))


(comment
  (get ".#." 1)
  (rem -6 3)
  (rem 3 6)
  (rem 3 1)
  (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##")
  (let [m (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##")
        from [4 2]
        to [3 2]]
    (remove-invisible from to (-> m (disj from))))
  (let [m (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##")
        from [0 2]]
    (collect-visible from m))
  (count
    (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##"))
  (parse-asteroid-map ".#..#\n.....\n#####"))