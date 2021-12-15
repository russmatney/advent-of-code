(ns grid)

(defn parse-digit-grid [lines]
  (->> lines
       (map #(->> % (re-seq #"\d") (map read-string) vec))
       vec))

(defn num-at [rows {:keys [x y]}]
  (when-let [row (nth rows y nil)]
    (when-let [num (nth row x nil)]
      num)))

(defn valid-neighbors [rows {:keys [x y]}]
  (for [nx    (range (dec x) (inc (inc x)))
        ny    (range (dec y) (inc (inc y)))
        :when (and (num-at rows {:x nx :y ny})
                   ;; exclude the passed coord
                   (not (and (#{nx} x) (#{ny} y))))]
    {:x nx :y ny}))
