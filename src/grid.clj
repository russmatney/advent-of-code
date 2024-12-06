(ns grid)

(defn parse-digit-grid [lines]
  (->> lines
       (map #(->> % (re-seq #"\d") (map read-string) vec))
       vec))

(defn parse-grid [lines]
  (->> lines
       (map #(->> % (re-seq #".") vec))
       vec))

(defn val-at [rows {:keys [x y]}]
  (when-let [row (nth rows y nil)]
    (when-let [num (nth row x nil)]
      num)))

(defn all-coords [rows]
  (for [y (range 0 (count rows))
        x (range 0 (count (first rows)))]
    {:x x :y y :val (val-at rows {:x x :y y})}))

(defn valid-neighbors [rows {:keys [x y]}]
  (for [nx    (range (dec x) (inc (inc x)))
        ny    (range (dec y) (inc (inc y)))
        :when (and (val-at rows {:x nx :y ny})
                   ;; exclude the passed coord
                   (not (and (#{nx} x) (#{ny} y))))]
    (let [nbr-coord {:x nx :y ny}]
      (assoc nbr-coord
             :val (val-at rows nbr-coord)
             :dir {:x (- nx x)
                   :y (- ny y)}))))
