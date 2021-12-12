(ns _2021.twelve.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(comment
  (input "example.txt")
  (input "input.txt"))

(defn parse-traversals [f]
  (->> f input
       (map (fn [edge] (string/split edge #"-")))
       (into #{})))

(def example-traversals (parse-traversals "example.txt"))
(def example-2-traversals (parse-traversals "example-2.txt"))
(def example-3-traversals (parse-traversals "example-3.txt"))
(def input-traversals (parse-traversals "input.txt"))

(defn cave-type? [s]
  (cond
    (#{"start"} s)       :cave/start
    (#{"end"} s)         :cave/end
    (re-seq #"[A-Z]+" s) :cave/big
    (re-seq #"[a-z]+" s) :cave/small))

(defn linked-caves [trvrsls current-cave]
  (->> trvrsls
       (filter #(->> % (into #{}) ((fn [s] (s current-cave))) seq))
       (map #(->> % (remove #{current-cave}) first))
       (into #{})))

(defn has-end? [path]
  (when (->> path (filter #{"end"}) seq)
    true))

(defn step-path-1 [trvrsls path]
  (let [current  (last path)
        visited? (into #{} path)
        next     (->> (linked-caves trvrsls current)
                      (remove (fn [cave]
                                (let [t (cave-type? cave)]
                                  (or
                                    (#{:cave/start} t)
                                    (and (#{:cave/small} (cave-type? cave))
                                         (visited? cave)))))))]
    (->> next (map #(conj path %)))))

(comment
  (step-path-1 example-traversals ["start" "A" "c" "A"]))

(defn collect-paths [trvrsls step-path-f]
  (loop [paths     [["start"]]
         acc-paths []]
    (let [grouped    (->> paths (group-by has-end?))
          unfinished (get grouped nil)
          finished   (get grouped true)]
      (if-not (->> unfinished seq)
        ;; no more to finish
        (concat acc-paths finished)

        ;; step the unfinished, acc the finished
        (recur (->> unfinished
                    (map (partial step-path-f trvrsls))
                    (apply concat))
               (concat acc-paths finished))))))

(comment
  (->> (collect-paths example-traversals step-path-1) count)
  (->> (collect-paths example-2-traversals step-path-1) count)
  (->> (collect-paths example-3-traversals step-path-1) count)
  (->> (collect-paths input-traversals step-path-1) count))

;;

(defn step-path-2
  "Visit _one_ small cave more than once"
  [trvrsls path]
  (let [current                       (last path)
        visited-any-small-cave-twice? (->> path
                                           (filter (comp #{:cave/small} cave-type?))
                                           (group-by identity)
                                           (filter (comp #{2} count second))
                                           seq)
        visited?                      (into #{} path)
        cant-visit?
        (fn [cave]
          (let [t (cave-type? cave)]
            (or
              ;; don't revisit "start"
              (#{:cave/start} t)

              ;; is small cave
              (and
                (#{:cave/small} (cave-type? cave))
                (and visited-any-small-cave-twice? (visited? cave))))))]

    (->>
      current
      (linked-caves trvrsls)
      (remove cant-visit?)
      (map #(conj path %)))))

(comment
  (step-path-2 example-traversals ["start" "A" "b" "A" "c" "A"])
  (step-path-2 example-traversals ["start" "A" "b" "A" "c" "A" "c" "A"])
  (step-path-2 example-traversals ["start" "A" "b" "A" "c" "A" "b" "A"])
  (step-path-2 example-traversals ["start" "A" "c" "A" "c" "A"])
  )

(comment
  (collect-paths example-traversals step-path-2)
  (->> (collect-paths example-traversals step-path-2) count)
  (->> (collect-paths example-2-traversals step-path-2) count)
  (->> (collect-paths example-3-traversals step-path-2) count)
  (->> (collect-paths input-traversals step-path-2) count)
  )
