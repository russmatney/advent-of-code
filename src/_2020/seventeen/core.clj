(ns _2020.seventeen.core
  (:require [util :refer [input]]
            [clojure.math.combinatorics :as combo]))


(comment
  (input "example.txt"))

(defn initial-cube [f]
  (->> (input f)
       (map-indexed
         (fn [y line]
           (->> line
                (map-indexed
                  (fn [x char]
                    {:state
                     (case char
                       \. :inactive
                       \# :active)
                     :x x
                     :y y
                     :z 0}))
                (into []))))
       (into [])
       ((fn [slice]
          ;; wrap to get to z dim
          [slice]))
       ))

(defn print-cube [cube]
  (println "cube!")
  (doall
    (->> cube
         (map (fn [slice]
                (print "\nz: " (some->> slice first first :z))
                (doall
                  (->> slice
                       (map (fn [row]
                              (print "\n")
                              (doall
                                (->> row
                                     (map (comp print (fn [cube]
                                                        (case (:state cube)
                                                          :inactive "."
                                                          :active   "#"))))))))))))))
  (print "\n"))

(comment
  (initial-cube "example.txt")
  (print-cube (initial-cube "example.txt"))
  )

(defn flat-cube [f]
  (->>
    (initial-cube f)
    (flatten)))

(comment
  (flat-cube "example.txt"))

(defn ->index-key [c]
  (str "x" (:x c) "y" (:y c) "z" (:z c)))

(defn ->indexed [c]
  [(->index-key c) c])

(defn indexed-cube [f]
  (->> f
       flat-cube
       (map ->indexed)
       (into {})))

(comment
  (indexed-cube "example.txt"))

(defn active-cells [indexed-cube]
  (->> indexed-cube
       vals
       (filter (comp #{:active} :state))))

(defn ->neighbors [{:keys [x y z] :as _c}]
  (let [xs (range (- x 1) (+ x 1 1))
        ys (range (- y 1) (+ y 1 1))
        zs (range (- z 1) (+ z 1 1))]
    (->> (combo/cartesian-product xs ys zs)
         (map (fn [[x y z]]
                {:x x :y y :z z}))
         (remove (fn [{cx :x cy :y cz :z}]
                   (and (= x cx)
                        (= y cy)
                        (= z cz)))))))

(comment
  (count (->neighbors {:x -1 :y 0 :z 0}))
  )

(defn update-cell [cube cell]
  (let [
        nbs              (->> (->neighbors cell)
                              (map (fn [c]
                                     ;; (println "c" c
                                     ;;          "key" (->index-key c)
                                     ;;          "found" (get cube (->index-key c))
                                     ;;          )
                                     (get cube (->index-key c)))))
        active-neighbors (->> nbs
                              (filter (comp #{:active} :state)))]
    ;; (println "\n\n\n")
    ;; (println "cube" (keys cube))
    ;; (println "cell" cell)
    ;; (println "nbs" (count nbs))
    ;; (println "active-nbs" (count active-neighbors))
    (cond
      (= (:state cell) :active)
      (if (or (= 2 (count active-neighbors))
              (= 3 (count active-neighbors)))
        cell
        (assoc cell :state :inactive))

      (or (= (:state cell) :inactive)
          (nil? (:state cell)))
      (if (= (count active-neighbors) 3)
        (assoc cell :state :active)
        (assoc cell :state :inactive))
      ))
  )

(defn expand-cube [cube]
  (let [min-x (->> cube vals (map :x) (apply min) dec)
        max-x (->> cube vals (map :x) (apply max) inc)

        min-y (->> cube vals (map :y) (apply min) dec)
        max-y (->> cube vals (map :y) (apply max) inc)

        min-z (->> cube vals (map :z) (apply min) dec)
        max-z (->> cube vals (map :z) (apply max) inc)

        new-cells
        (->> (combo/cartesian-product (range min-x (+ max-x 1))
                                      (range  min-y (+ max-y 1))
                                      (range min-z (+ max-z 1)))
             (map (fn [[x y z]]
                    {:state :inactive :x x :y y :z z}))
             (map ->indexed)
             (into {}))]
    (merge new-cells cube)))

(comment
  (-> (indexed-cube "example.txt")
      expand-cube
      count)

  (merge {:x 1} {:x 2})
  )

(defn run-cycle [f n]
  (let [cube (indexed-cube f)]
    (->> cube
         (iterate
           (fn [cube]
             (let [cube   (expand-cube cube)
                   active (active-cells cube)]
               (println "active-cells" (count active))
               (->> cube
                    (reduce
                      (fn [updated-cube [k cell]]
                        (let [cell
                              ;; pass cube for this iteration, not updated-cube
                              (update-cell cube cell)]
                          ;; (println "updated cell" cell)
                          (assoc updated-cube k cell)))
                      cube)))))
         (take (+ n 1))
         last)
    ))

(comment
  (println "hi")
  (run-cycle "example.txt" 1)
  (run-cycle "example.txt" 6)

  (->>
    (run-cycle "example.txt" 6)
    vals
    (filter (comp #{:active} :state))
    count
    )
  ;; 112

  (->>
    (run-cycle "input.txt" 6)
    vals
    (filter (comp #{:active} :state))
    count
    )
  ;; 446 too low
  ;; 448!!!
  )
