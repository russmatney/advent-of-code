(ns twenty.core
  (:require [util :refer [input partition-by-newlines]]))

(defn parse-tile [lines]
  (let [id    (->> lines first (re-seq #"Tile (\d\d\d\d):")
                   first second)
        image (->> lines rest)
        edges {:top    (first image)
               :bottom (last image)
               :left   (apply str (map (fn [line] (first line)) image))
               :right  (apply str (map (fn [line] (last line)) image))}]
    {:id    id
     :image image
     :edges edges}))

(defn tiles [f]
  (->> f
       input
       partition-by-newlines
       (map parse-tile)))

(comment
  (tiles "example.txt"))

(defn matching-edge-counts [f]
  (->> (tiles f)
       (mapcat (comp vals :edges))
       (group-by (fn [x] x))
       (map (fn [[edge grp]]
              [edge (count grp)]))
       (into {})))

(comment
  (matching-edge-counts "example.txt")
  (->>
    (matching-edge-counts "input.txt")
    (filter (comp #(> % 1) second)))

  ;; no edges match more than twice
  )

(defn matching-edges? [edges [_edge-side edge]]
  (->> edges
       (map (fn [[side line]]
              (cond
                (= line edge)
                [side line false]
                (= line (apply str (reverse edge)))
                [side line true])))
       (remove nil?)))

(defn matching-edges-for-tile [tile edges]
  (->> tile
       :edges
       (map
         (fn [[side edge]]
           {:side    side
            :edge    edge
            :matches (matching-edges? edges [side edge])}))
       (filter (comp seq :matches))))

(defn matching-tiles [tile-a tile-b]
  (matching-edges-for-tile tile-a (:edges tile-b)))

(comment
  (matching-tiles
    {:edges {:top ".##.#."}}
    {:edges {:bottom ".##.#."
             :left   ".####."}}))

(defn add-to-puzzle
  "All we need to know is what 4 tiles have 2 unmatched edges.
  So updating the puzzle is removing edges that matched in place."
  [puzzle tile]
  (if-not (seq puzzle)
    {:puzzle   (->> [[(:id tile) tile]] (into {}))
     :no-match nil}
    (if-let
        [matches-found
         (some->>
           puzzle
           (map
             (fn [[t-id t]]
               (let [res (matching-tiles tile t)]
                 (when (>  (count res) 1)
                   (println "Found multiple matches on this tile!" res))
                 (when (seq res)
                   (let [{:keys [side ;; new-tile-side
                                 ;; edge ;; new-tile-edge
                                 matches]}
                         (first res)]
                     (when (>  (count matches) 1)
                       (println "Found multiple matches!" tile side))

                     (when (seq matches)
                       (let [[match-tile-side] (first matches)]
                         {:new-tile-side   side
                          :match-tile-side match-tile-side
                          :match-tile-id   t-id})))))))
           (remove nil?))]
      (do
        (println "matches-found" (count matches-found))
        {:puzzle (reduce
                   (fn [puzzle match-found]
                     (let [{:keys [match-tile-id match-tile-side
                                   new-tile-side]} match-found]
                       ;; clear the used-edges
                       (-> puzzle
                           ;; update new tile's matching edges
                           (assoc-in [(:id tile) :edges new-tile-side] nil)
                           (assoc-in [match-tile-id :edges match-tile-side] nil))))
                   ;; add new tile
                   (assoc puzzle (:id tile) tile)
                   matches-found)})
      {:puzzle   puzzle
       :no-match true})))

(defn build-puzzle [f]
  (loop [ts     (tiles f)
         puzzle []
         seen   {}]
    (if-not (seq ts)
      puzzle
      (let [tile (first ts)]
        (println "tile:" (:id tile)
                 "(" (count ts) "not yet placed,"
                 (count puzzle) "placed.)")

        (if (and
              (get seen (:id tile))
              (> (get seen (:id tile)) 2))
          (do
            (println "exiting early" seen)
            puzzle)
          (let [{:keys [puzzle no-match]}
                (add-to-puzzle puzzle tile)]
            (recur
              (if no-match (concat (rest ts) [tile]) (rest ts))
              puzzle
              (update seen (:id tile) (fn [x] (if x (inc x) 1))))))))))

(comment
  (build-puzzle "example.txt")
  (->> (build-puzzle "example.txt")
       vals
       (filter (comp #{2} count #(remove nil? %) vals :edges))
       )

  (->> (build-puzzle "input.txt")
       vals
       (filter (comp #{2} count #(remove nil? %) vals :edges))
       (map :id)
       (map read-string)
       (apply *)))
