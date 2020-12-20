(ns twenty.core
  (:require [util :refer [input partition-by-newlines]]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn edges-for-image [image]
  {:top    (first image)
   :bottom (last image)
   :left   (apply str (map (fn [line] (first line)) image))
   :right  (apply str (map (fn [line] (last line)) image))})

(defn parse-tile [lines]
  (let [id    (->> lines first (re-seq #"Tile (\d\d\d\d):")
                   first second)
        image (->> lines rest)
        edges (edges-for-image image)]
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

  ;; no edges match more than once! yay!
  )

(defn matching-edges [edges [_edge-side edge]]
  (->> edges
       (map (fn [[side line]]
              (cond
                (= line edge) {:line      line
                               :side      side
                               :reversed? false}

                (= line (apply str (reverse edge))) {:line      line
                                                     :side      side
                                                     :reversed? true})))
       (remove nil?)))

(defn matching-edges-for-tile [tile edges]
  (->> tile
       :edges
       (map
         (fn [[side edge]]
           {:side    side
            :edge    edge
            :matches (matching-edges edges [side edge])}))
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
               (when-let [res (some-> (matching-tiles tile t) first)]
                 (let [{:keys [side matches]} res]
                   (when (seq matches)
                     (let [m (first matches)]
                       {:new-tile-side   side
                        :match-tile-side (:side m)
                        :match           m
                        :match-tile-id   t-id}))))))
           (remove nil?))]
      {:puzzle (reduce
                 (fn [puzzle match-found]
                   (let [{:keys [match-tile-id
                                 match-tile-side
                                 new-tile-side
                                 match]} match-found]
                     ;; clear the used-edges
                     (-> puzzle
                         ;; update new tile's matching edges
                         ;; (assoc-in [(:id tile) :edges new-tile-side] nil)
                         (assoc-in [(:id tile) :matched-edges new-tile-side]
                                   {:id        match-tile-id
                                    :side      match-tile-side
                                    :reversed? (:reversed? match)})
                         (assoc-in [match-tile-id :matched-edges match-tile-side]
                                   {:id        (:id tile)
                                    :side      new-tile-side
                                    ;; not sure reversed is right here, might be losing data or have to check again
                                    :reversed? (:reversed? match)}))))
                 ;; add new tile
                 (assoc puzzle (:id tile) tile)
                 matches-found)}
      {:puzzle   puzzle
       :no-match true})))

(defn assign-matching-edges [f]
  (loop [ts     (tiles f)
         puzzle []
         seen   {}]
    (if-not (seq ts)
      puzzle
      (let [tile (first ts)]
        (if (and
              (get seen (:id tile))
              (> (get seen (:id tile)) 2))
          (do
            (println "error: tile seen too many times, exiting early" seen)
            puzzle)
          (let [{:keys [puzzle no-match]}
                (add-to-puzzle puzzle tile)]
            (recur
              (if no-match (concat (rest ts) [tile]) (rest ts))
              puzzle
              (update seen (:id tile) (fn [x] (if x (inc x) 1))))))))))

(comment
  (assign-matching-edges "example.txt")

  (->> (assign-matching-edges "example.txt")
       vals
       (filter (comp #{2} count vals :matched-edges)))

  (->> (assign-matching-edges "example.txt")
       vals
       (filter (comp #{2} count vals :matched-edges)))

  (->> (assign-matching-edges "input.txt")
       vals
       (filter (comp #{2} count vals :matched-edges))
       (map :id)
       (map read-string)
       (apply *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-puzz [len]
  (->>
    (repeat len (->> (repeat len nil) (into [])))
    (into []))
  )

(defn flip-tile [direction tile]
  (let [updated-image
        (case direction
          :vertical
          (->> (:image tile) (reverse) (into []))
          :horizontal
          (->> (:image tile) (map (comp #(apply str %) reverse)) (into [])))
        updated-matched-edges
        (case direction
          :vertical
          (-> tile :matched-edges
              (assoc :top (-> tile :matched-edges :bottom))
              (assoc :bottom (-> tile :matched-edges :top))
              (->> (remove (comp nil? val))
                   (into {}))
              )
          :horizontal
          (-> tile :matched-edges
              (assoc :right (-> tile :matched-edges :left))
              (assoc :left (-> tile :matched-edges :right))
              (->> (remove (comp nil? val))
                   (into {}))))]
    (-> tile
        (assoc :edges (edges-for-image updated-image))
        (assoc :matched-edges updated-matched-edges)
        (assoc :image updated-image))))

(comment
  (let [pieces (assign-matching-edges "example.txt")]
    (println "hi")
    (println (:image (val (first pieces))))
    ;; (val (first pieces))
    (flip-tile :vertical (val (first pieces)))
    (flip-tile :horizontal (val (first pieces)))
    ))

(defn rotate-image [img]
  (let [cs        (->> img
                       (map-indexed
                         (fn [y line]
                           (->> line
                                (map-indexed
                                  (fn [x char]
                                    {:y y :x x :char char})))))
                       flatten)
        max-x     (->> cs (map :x) (apply max))
        max-y     (->> cs (map :y) (apply max))
        new-image (->> (repeat (+ max-x 1)
                               (->> (repeat (+ max-y 1) nil)
                                    (into [])))
                       (into []))]
    (->> (reduce
           (fn [img {:keys [x y char]}]
             (let [new-y x
                   new-x (- max-y y)]
               (assoc-in img [new-y new-x] char)))
           new-image
           cs)
         (map (fn [row]
                (apply str row)))
         (into []))))

(defn rotate-tile-once
  "Rotates a tile one time, counterclockwise, such that:
  top->right, right->bottom, bottom->left, left->top."
  [tile]
  (let [updated-image         (rotate-image (:image tile))
        matched               (:matched-edges tile)
        updated-matched-edges (-> matched
                                  (assoc :right (:top matched))
                                  (assoc :bottom (:right matched))
                                  (assoc :left (:bottom matched))
                                  (assoc :top (:left matched))
                                  (->> (remove (comp nil? val))
                                       (into {})))]
    (-> tile
        (assoc :edges (edges-for-image updated-image))
        (assoc :matched-edges updated-matched-edges)
        (assoc :image updated-image))))

(comment
  (let [pieces (assign-matching-edges "example.txt")]
    (println "hi")
    (println (:image (val (first pieces))))

    (:image (val (first pieces)))
    ;; (:image (rotate-tile-once (val (first pieces))))
    ))

(defn rotate-tile [times tile]
  (->> (iterate rotate-tile-once tile)
       (take (+ times 1))
       last))

(comment
  (let [pieces (assign-matching-edges "example.txt")]
    (println "hi")
    (println (:image (val (first pieces))))
    (:image (val (first pieces)))
    ;; (:image (rotate-tile 1 (val (first pieces))))
    ;; (:image (rotate-tile 2 (val (first pieces))))
    ;; (:image (rotate-tile 4 (val (first pieces))))
    ))

(defn update-tile-to-match
  "Find edge matching `matched-edge`,
  orient based on side and the edge found,
  update matched edges to reflect new orientation."
  [side matched-edge tile]
  (let [matching-edge            (->> (matching-edges (:edges tile)
                                                      [side matched-edge])
                                      first)
        {:keys [side reversed?]} matching-edge]

    ;; (println side)
    ;; (println matched-edge)
    ;; (println tile)
    ;; tile
    matching-edge))

(comment
  (let [pieces (assign-matching-edges "example.txt")]
    (-> (update-tile-to-match :right "#...##.#.#" (get pieces "1489"))
        ;; :matched-edges
        )))

(defn merge-tile
  "Adds the passed tile to the puzz at the correct index.
  Removes the used matched-edge.
  Modifies/Adds matched edges from the passed tile according to the orientation.
  "
  [puzz [matched-tile-side {:keys [tile]}]]
  (let [tile-in-puzzle
        (some->> puzz
                 flatten
                 (filter
                   (comp #{(:id tile)} :id matched-tile-side :matched-edges))
                 first)

        matched-edge (-> tile-in-puzzle :edges matched-tile-side)

        [match-x match-y]
        (->> puzz
             (map-indexed
               (fn [x ts]
                 (->> ts
                      (map-indexed
                        (fn [y t]
                          {:x x :y y :t t})))))
             flatten
             (filter (comp #{(:id tile-in-puzzle)} :id :t))
             (map (fn [{:keys [x y]}] [x y]))
             first)

        [x-diff y-diff] (case matched-tile-side
                          :right  [1 0]
                          :left   [-1 0]
                          :bottom [0 1]
                          :top    [0 -1])
        [x y]           [(+ x-diff match-x) (+ y-diff match-y)]

        ;; does the new tile need to be reversed? rotated? flipped?
        ;; update :matched-edges to reflect the new side, same tile ref

        oriented-tile (update-tile-to-match matched-tile-side matched-edge tile)

        ]

    (update-in puzz [y x] oriented-tile)
    )
  )

(comment
  (let [f               "example.txt"
        pieces          (assign-matching-edges f)
        len             (-> pieces count Math/sqrt int)
        puzz            (init-puzz len)
        top-left-corner (->> pieces
                             vals
                             (filter (comp #{2} count vals :matched-edges))
                             (filter
                               (comp #(set/subset? #{:bottom :right} %) set keys
                                     :matched-edges))
                             first)
        match           (->> top-left-corner
                             :matched-edges
                             :right
                             :id
                             (get pieces))]
    (merge-tile (assoc-in puzz [0 0] top-left-corner)
                [:right {:tile      match
                         :reversed? false
                         :side      :left}])
    ;; top-left-corner
    ;; match
    )
  )

(defn merge-tiles [puzz tiles-to-merge]
  (reduce merge-tile puzz tiles-to-merge))

(defn build-puzzle [f]
  (let [pieces          (assign-matching-edges f)
        len             (-> pieces count Math/sqrt int)
        puzz            (init-puzz len)
        top-left-corner (->> pieces
                             vals
                             (filter (comp #{2} count vals :matched-edges))
                             (filter
                               (comp #(set/subset? #{:bottom :right} %) set keys
                                     :matched-edges))
                             first)]
    (loop [puzz (assoc-in puzz [0 0] top-left-corner)]
      ;; each loop, update the puzz :image and :matched-edges
      (if-not (some->> puzz flatten (map :matched-edges) seq)
        puzz
        (let [tiles-to-merge (->> puzz :matched-edges
                                  (map (fn [[side {:keys [id] :as match}]]
                                         [side (assoc match :tile
                                                      (get pieces id))])))]
          (recur (merge-tiles puzz tiles-to-merge)))))))

(comment
  (->> [[{:matched-edges [1 2]}]] flatten (map :matched-edges) seq)

  (build-puzzle "example.txt")

  (set/subset? #{:right}
               #{:bottom :right}
               )

  (build-puzzle "input.txt")
  )
