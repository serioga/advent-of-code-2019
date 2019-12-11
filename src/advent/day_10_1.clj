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
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [clojure.test :as t]))

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


(defn abs [x] (if (neg? x) (- x) x))


(defn beyond? [a b] (and
                      (pos? (* a b))
                      (< (abs a) (abs b))))


(defn diag? [x y] (= (abs x) (abs y)))

#_(comment
    (beyond? -1 -2)
    (beyond? 0 1))


(defn dist-cover?
  [dx' dy' dx dy]
  (cond
    ; same location
    (and (= dx' dx) (= dy' dy)) false

    ; vertical
    (zero? dx') (and
                  (zero? dx)
                  (beyond? dy' dy))

    ; horizontal
    (zero? dy') (and
                  (zero? dy)
                  (beyond? dx' dx))

    ; diagonal
    (diag? dx' dy') (and
                      (diag? dx dy)
                      (beyond? dx' dx)
                      (beyond? dy' dy))

    ; other directions
    :else (let [k (fn [d' d] (when (and
                                     (zero? (rem d d'))
                                     (beyond? d' d))
                               (quot d d')))
                kx (k dx' dx)
                ky (k dy' dy)]

            (and
              (some? kx)
              (some? ky)
              (= kx ky)))))

#_(comment
    (dist-cover? 2 2 3 3)
    (dist-cover? 4 1 8 2)
    (dist-cover? -4 -1 -12 -3)
    (dist-cover? 8 2 4 1)
    (dist-cover? 1 2 3 4))


(defn remove-invisible
  "Return `others` without hidden for `from` by `to`."
  [[x-from y-from], [x-to y-to], others]
  (let [dx' (- x-to x-from)
        dy' (- y-to y-from)
        hidden? (fn [[x y]] (let [dx (- x x-from)
                                  dy (- y y-from)]
                              #_(if (dist-cover? dx' dy' dx dy)
                                  (println dx' dy' dx dy))
                              #_(println [x-to y-to] [x y] dx' dy' dx dy (dist-cover? dx' dy' dx dy))
                              (dist-cover? dx' dy' dx dy)))]
    (into #{}
      (remove hidden? others))))


(defn collect-visible
  [asteroid-map, from]
  (let [others (disj asteroid-map from)]
    (reduce (fn [m to]
              #_(println to m)
              (remove-invisible from to m))
      others, others)))


(defn find-best
  {:test (fn []
           (t/is (= [[3 4] 8] (-> ".#..#\n.....\n#####\n....#\n...##"
                                (parse-asteroid-map)
                                (find-best))))
           (t/is (= [[5 8] 33] (-> "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
                                 (parse-asteroid-map)
                                 (find-best))))
           (t/is (= [[1 2] 35] (-> "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
                                 (parse-asteroid-map)
                                 (find-best))))
           (t/is (= [[6 3] 41] (-> ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
                                 (parse-asteroid-map)
                                 (find-best))))
           (t/is (= [[11 13] 210] (-> ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
                                    (parse-asteroid-map)
                                    (find-best)))))}
  [asteroid-map]
  (let [vmap (->> asteroid-map
               (map (juxt identity #(count (collect-visible asteroid-map %))))
               (into {}))
        cmap (group-by second vmap)
        cmax (apply max (keys cmap))]
    (first (get cmap cmax))))

#_(comment
    (t/test-var #'find-best))


(defn solve
  [in]
  (find-best
    (parse-asteroid-map in)))

(def input
  (-> "advent/day_10_input.txt"
    (io/resource)
    (slurp)))

(comment
  (do input)
  (solve input)
  (solve ".#..#\n.....\n#####\n....#\n...##")
  (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##")
  (parse-asteroid-map input)
  (let [m (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##")
        from [4 2]
        to [3 2]]
    (remove-invisible from to (-> m (disj from))))
  (let [m (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##")
        from [1 0]]
    (collect-visible m from))
  (count
    (parse-asteroid-map ".#..#\n.....\n#####\n....#\n...##"))
  (parse-asteroid-map ".#..#\n.....\n#####"))
