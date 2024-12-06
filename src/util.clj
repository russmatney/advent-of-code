(ns util
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn parse-split-lines [sep data]
  (map #(string/split % (re-pattern sep)) data))

(defn parse-ints [data]
  (map read-string data))

(defn partition-by-newlines [lines]
  (->> lines
       (partition-by #{""})
       (remove (comp #{""} first))))

(defn parse-input
  ([f] (parse-input f nil))
  ([f {:keys [skip-split-lines? split split? ints? partition? trim? one-line?]}]
   (let [split (or split (when split? " "))
         parsed
         (cond-> f
           true                    slurp
           (not skip-split-lines?) string/split-lines
           trim?                   (#(map string/trim %)))]
     (cond
       (and split ints?)
       (->> parsed (parse-split-lines split)
            (map #(->> % (parse-ints) (into []))))

       (and partition? ints?)
       (->> parsed partition-by-newlines
            (map #(->> % (parse-ints) (into []))))

       partition? (partition-by-newlines parsed)
       split      (parse-split-lines split parsed)
       ints?      (parse-ints parsed)
       one-line?  (first parsed)
       :else      parsed))))

(defmacro input
  "Parses a file in the same dir as this is called.
  Expects a simple fname input, ex: `(input \"input.txt\")`
  Returns a seq of lines.
  "
  ([fname] (input fname nil))
  ([fname opts]
   `(-> *file*
        io/file
        .getParent
        (str "/" ~fname)
        (parse-input ~opts))))

(defn draw-grid
  ([points draw-fn] (draw-grid points draw-fn nil))
  ([points draw-fn {:keys [reverse-y]}]
   (println "\n--------------------------Drawing grid-----------------")
   (let [max-x (->> points (map first) (apply max))
         min-x (->> points (map first) (apply min))
         max-y (->> points (map second) (apply max))
         min-y (->> points (map second) (apply min))
         rendered
         (cond->> (range min-y (inc max-y))
           reverse-y reverse
           true      (map (fn [y]
                            (str y ":\t"
                                 (->>
                                   (range min-x (inc max-x))
                                   (map (if draw-fn
                                          #(draw-fn [% y])
                                          (fn [x] (cond
                                                    (points [x y]) "#"
                                                    :else          "."))))
                                   (apply str))
                                 "\n")))
           true      (apply str))]
     (println rendered)
     rendered)))
