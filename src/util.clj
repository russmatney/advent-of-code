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
  ([f {:keys [skip-split-lines? split split? ints? partition? trim?]}]
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
