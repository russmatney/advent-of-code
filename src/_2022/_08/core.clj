(ns _2022._08.core
  (:require
   [clojure.string :as string]))

(defn input [f]
  (->>
    (slurp (str "src/_2022/_08/" f))
    (string/split-lines)
    (map-indexed (fn [y line]
                   (->> line
                        (map-indexed
                          (fn [x tree]
                            [[x y] (read-string (str tree))])))))
    (mapcat identity)
    (into {})))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn point-at-edge? [n p]
  (or
    (zero? p)
    (#{(- n 1)} p)))

(defn visible-in-dir? [this-tree dir grid [x y]]
  (let [[cx cy]    (case dir
                     :left  [(dec x) y]
                     :right [(inc x) y]
                     :up    [x (dec y)]
                     :down  [x (inc y)])
        check-tree (get grid [cx cy])]
    (cond (not check-tree) true

          (> this-tree check-tree)
          (visible-in-dir? this-tree dir grid [cx cy])

          :else false)))

(comment
  (visible-in-dir? 5 :left (input "example.txt") [1 1])
  (get (input "example.txt") [0 1]))

(defn is-visible? [grid n [x y]]
  (let [this-tree (get grid [x y])]
    (cond
      (or (point-at-edge? n x) (point-at-edge? n y)) true

      :else (or
              (visible-in-dir? this-tree :left grid [x y])
              (visible-in-dir? this-tree :right grid [x y])
              (visible-in-dir? this-tree :up grid [x y])
              (visible-in-dir? this-tree :down grid [x y])))))

(defn visible-trees [f]
  (let [grid   (input f)
        n      (-> (str "src/_2022/_08/" f) slurp string/split-lines count)
        points (for [x (range n) y (range n)] [x y])]
    (->> points
         (filter
           (fn [point]
             (is-visible? grid n point))))))

(comment
  (count
    (visible-trees "example.txt"))

  (count
    (visible-trees "input.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn score-in-dir [score this-tree dir grid [x y]]
  (let [[cx cy]    (case dir
                     :left  [(dec x) y]
                     :right [(inc x) y]
                     :up    [x (dec y)]
                     :down  [x (inc y)])
        check-tree (get grid [cx cy])]
    (cond
      (not check-tree) score

      (> this-tree check-tree)
      (score-in-dir (inc score) this-tree dir grid [cx cy])

      :else (inc score))))

(comment
  (score-in-dir 0 5 :right (input "example.txt") [1 1])
  (get (input "example.txt") [0 1]))

(defn scenic-score [grid n [x y]]
  (let [this-tree (get grid [x y])]
    (cond
      ;; skip all edge trees
      (or (point-at-edge? n x) (point-at-edge? n y)) 0

      :else (*
              (score-in-dir 0 this-tree :left grid [x y])
              (score-in-dir 0 this-tree :right grid [x y])
              (score-in-dir 0 this-tree :up grid [x y])
              (score-in-dir 0 this-tree :down grid [x y])))))

(defn most-scenic-tree [f]
  (let [grid   (input f)
        n      (-> (str "src/_2022/_08/" f) slurp string/split-lines count)
        points (for [x (range n) y (range n)] [x y])]
    (->> points
         (map
           (fn [point]
             (scenic-score grid n point)))
         (sort >)
         first)))

(comment
  (most-scenic-tree "example.txt")
  (most-scenic-tree "input.txt"))


(comment
  (re-seq #"\w+" "hello there [bracket text]")
  ;; => ("hello" "there" "bracket" "text")

  (re-seq #"\[([ |\w]+)\]" "hello there [bracket text] [more bracket text]")
  ;; => (["[bracket text]" "bracket text"] ["[more bracket text]" "more bracket text"])

  (->> "some-str" (re-seq #"\w+"))
  ;; ("some" "str")
  (->> "some-str" (re-seq #"(\w+)"))
  ;; (["some" "some"] ["str" "str"])
  (->> "some-str" (re-seq #"s(\w+)"))
  ;; (["some" "ome"] ["str" "tr"])
  )
