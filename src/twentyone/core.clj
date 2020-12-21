(ns twentyone.core
  (:require
   [wing.core :as wing]
   [util :refer [input]]
   [clojure.string :as string]
   [clojure.set :as set]))

(comment)

(defn parse-foods [f]
  (->> f
       input
       (map (fn [line]
              (let [[_ ingredients allergens]
                    (-> (re-seq #"([\w| ]+) \(contains ([\w|,| ]+)\)"
                                line) first)]
                {:ingredients
                 (->> (string/split ingredients #" ")
                      (map string/trim)
                      set)
                 :allergens
                 (->> (string/split allergens #", ")
                      (map string/trim)
                      set)})))))

(comment
  (parse-foods "example.txt"))

(defn ingredients-by-allergens [f]
  (->> f
       parse-foods
       (mapcat
         (fn [{:keys [ingredients allergens]}]
           (->> allergens (map (fn [al] [al ingredients])))))
       (wing/group-by first second)
       (map (juxt key (comp #(apply set/intersection %) second)))))

(defn unique-allergen-sources [f]
  (->> (ingredients-by-allergens f)
       (map second)
       (apply set/union)))

(comment
  (ingredients-by-allergens "example.txt")
  (unique-allergen-sources "example.txt"))

(defn count-non-allergen-ings [f]
  (let [allergen-sources (unique-allergen-sources f)
        foods            (parse-foods f)
        ]
    (->> foods
         (mapcat (comp #(into [] %) :ingredients))
         (remove allergen-sources)
         count
         )) )

(comment
  ;; part 1
  (count-non-allergen-ings "example.txt")
  (count-non-allergen-ings "input.txt")
  ;; 2324
  )

;; part 2

(defn assigned-allergens [f]
  (let [i-by-a (ingredients-by-allergens f)
        ]
    (loop [assigned   {}
           unassigned i-by-a]
      (if-not (seq unassigned)
        assigned
        (let [[al ings] (->> unassigned
                             (filter (comp #{1} count second))
                             first)
              ing       (first ings)]
          (recur
            (assoc assigned al ing)
            (->> unassigned
                 (map (fn [[al ings]]
                        [al (disj ings ing)]))
                 (filter (comp seq second)))))))))

(comment
  (assigned-allergens "example.txt")
  (assigned-allergens "input.txt")
  )

(defn canonical-dangerous-ingredients-list [f]
  (->> f
       assigned-allergens
       (sort-by first)
       vals
       (string/join ",")))

(comment
  (canonical-dangerous-ingredients-list "example.txt")
  (canonical-dangerous-ingredients-list "input.txt")
  ;; not: bxjvzk,sp,hsksz,qzzzf,fmpgn
  )
