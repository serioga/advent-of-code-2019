(ns advent.day-08-2
  "--- Part Two ---

Now you're ready to decode the image. The image is rendered by stacking the layers and aligning the
pixels with the same positions in each layer. The digits indicate the color of the corresponding
pixel: 0 is black, 1 is white, and 2 is transparent.

The layers are rendered with the first layer in front and the last layer in back. So, if a given
position has a transparent pixel in the first and second layers, a black pixel in the third layer,
and a white pixel in the fourth layer, the final image would have a black pixel at that position.

For example, given an image 2 pixels wide and 2 pixels tall, the image data 0222112222120000
corresponds to the following image layers:

Layer 1: 02
         22

Layer 2: 11
         22

Layer 3: 22
         12

Layer 4: 00
         00

Then, the full image can be found by determining the top visible pixel in each position:

    The top-left pixel is black because the top layer is 0.
    The top-right pixel is white because the top layer is 2 (transparent), but the second layer is 1.
    The bottom-left pixel is white because the top two layers are 2, but the third layer is 1.
    The bottom-right pixel is black because the only visible pixel in that position is 0 (from layer 4).

So, the final image looks like this:

01
10

What message is produced after decoding your image?"

  (:require
    [advent.day-08-1 :refer [data->image
                             input
                             layers]]))

(set! *warn-on-reflection* true)


(defn mix-color
  [a b]
  (if (= 2 a) b, a))


(defn render-layers
  [layers]
  (reduce (fn [a b]
            (vec (map-indexed (fn [i, pix] (mix-color pix (b i)))
                   a)))
    layers))


(defn str-pix
  [x]
  (if (= 1 x) "m " "  "))


(defn print-image
  [layer width]
  (doseq [row (partition width layer)]
    (println (apply str (map str-pix row)))))


(defn solve
  [input w h]
  (-> (layers input w, h)
    (render-layers)
    (print-image w)))


(comment
  (solve input 25 6)

  (layers (data->image "0222112222120000") 2 2)

  (-> (data->image "0222112222120000")
    (layers 2 2)
    (render-layers)
    (print-image 2)))

