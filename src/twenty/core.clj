(ns twenty.core
  (:require [util :refer [input partition-by-newlines]]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as string]))

(defn edges-for-image
  "Here we reverse the bottom and left for rotational consistency....
  .... or maybe for insanity?"
  [image]
  {:top    (first image)
   :bottom (apply str (reverse (last image)))
   :left   (apply str (reverse (map (fn [line] (first line)) image)))
   ;; :bottom (apply str (last image))
   ;; :left   (apply str (map (fn [line] (first line)) image))
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
  (let [edges (->> (tiles f)
                   (mapcat (comp vals :edges)))]
    (->>
      edges
      (concat (map (comp #(apply str %) reverse) edges))
      (group-by (fn [x] x))
      (map (fn [[edge grp]]
             [edge (count grp)]))
      (into {}))))

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

(defn matches-for-tiles [tile-a tile-b]
  (matching-edges-for-tile tile-a (:edges tile-b)))

(comment
  (matches-for-tiles
    {:edges {:top ".##.#."}}
    {:edges {:bottom ".##.#."
             :left   ".####."}}))

(defn set-matched-edges
  [tiles tile]
  (if-not (seq tiles)
    {:tiles-with-edges (->> [[(:id tile) tile]] (into {}))
     :no-match         nil}
    (if-let
        [matches-found
         (some->>
           tiles
           (mapcat
             (fn [[t-id t]]
               (when-let [edge-matches (matches-for-tiles tile t)]
                 (->> edge-matches
                      (map (fn [edge-match]
                             (let [{:keys [side matches]} edge-match]
                               (when (seq matches)
                                 (when (> (count matches) 1)
                                   (println "unhandled matches!!"))
                                 (let [m (first matches)]
                                   {:new-tile-side   side
                                    :match-tile-side (:side m)
                                    :match           m
                                    :match-tile-id   t-id})))))))))
           (remove nil?))]
      {:tiles-with-edges (reduce
                           (fn [tiles match-found]
                             (let [{:keys [match-tile-id
                                           match-tile-side
                                           new-tile-side
                                           ]} match-found]
                               (-> tiles
                                   (assoc-in
                                     [(:id tile) :matched-edges new-tile-side]
                                     {:id match-tile-id :side match-tile-side})
                                   (assoc-in
                                     [match-tile-id :matched-edges match-tile-side]
                                     {:id (:id tile) :side new-tile-side}))))
                           ;; add new tile
                           (assoc tiles (:id tile) tile)
                           matches-found)}
      {:tiles-with-edges tiles
       :no-match         true})))

(defn assign-matching-edges [f]
  (loop [ts               (tiles f)
         tiles-with-edges []
         seen             {}]
    (if-not (seq ts)
      tiles-with-edges
      (let [tile (first ts)]
        (if (and
              (get seen (:id tile))
              (> (get seen (:id tile)) 2))
          (do
            (println "error: tile seen too many times, exiting early" seen)
            tiles-with-edges)
          (let [{:keys [tiles-with-edges no-match]}
                (set-matched-edges tiles-with-edges tile)]
            (recur
              (if no-match (concat (rest ts) [tile]) (rest ts))
              tiles-with-edges
              (update seen (:id tile) (fn [x] (if x (inc x) 1))))))))))

(comment
  (println "xxx")
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

(defn print-tile [tile]
  (println "Tile: " (:id tile))
  (doall
    (->> tile
         :image
         (map (comp println #(str "\t" %)))))
  tile)

(def rotations
  {;; [match-with matched-edge] rotations
   ;; :right -> distance from :left
   [:right :right]   2
   [:right :bottom]  1
   [:right :left]    0
   [:right :top]     3
   ;; :bottom -> distance from :top
   [:bottom :bottom] 2
   [:bottom :right]  3
   [:bottom :top]    0
   [:bottom :left]   1
   ;; :top
   [:top :top]       2
   [:top :left]      3
   [:top :bottom]    0
   [:top :right]     1
   ;; :left
   [:left :left]     2
   [:left :top]      1
   [:left :right]    0
   [:left :bottom]   3})

(defn update-tile-to-match
  "Find edge matching `matched-edge`,
  orient based on side and the edge found,
  update matched edges to reflect new orientation."
  [current-tile side tile]
  (let [target-edge     (-> current-tile :edges side)
        matching-edge   (->> (matching-edges
                               (:edges tile)
                               [side target-edge])
                             first)
        match-side      (:side matching-edge)
        match-reversed? (:reversed? matching-edge)
        rots            (rotations [side match-side])]

    (println "match-reversed?" match-reversed?)
    (println "rots" rots)
    (when (= rots nil)
      (println "no rots!"))

    (cond-> tile
      (and (not match-reversed?) (= rots 0))
      ((fn [t]
         (println "edge case running!")
         ;; TODO i think there's one more case here
         (if (#{:right} side)
           (flip-tile :vertical t)
           (flip-tile :horizontal t))))

      true
      ((fn [t]
         (rotate-tile rots t))))))

(comment
  (build-puzzle "example.txt")
  )

(comment
  (println "xxxx")
  (let [tlc (get (assign-matching-edges "example.txt") "2971")
        p   (get (assign-matching-edges "example.txt") "1489")
        ]
    (print-tile tlc)
    (print-tile p)
    (-> (update-tile-to-match tlc :right p)
        print-tile
        )
    ))

(defn add-to-puzzle
  "Adds the passed tile to the puzz to the right of the passed current-tile,
  which should already be in the puzzle."
  [puzz [x y] [side current-tile] new-tile]
  (println "merging tile:" (:id new-tile)
           "current (last): "(:id current-tile)
           "side:" side)
  (let [oriented-tile (update-tile-to-match current-tile side new-tile)]
    {:puzzle       (-> puzz (assoc-in [y x] oriented-tile))
     :updated-tile oriented-tile}))

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
    (loop [puzz             (assoc-in puzz [0 0] top-left-corner)
           current-tile     [:right top-left-corner]
           tile-id-to-merge (-> top-left-corner :matched-edges :right :id)
           at-index         [1 0]
           i                0]
      (if (or (not tile-id-to-merge)
              (= len (second at-index)))
        puzz
        (let [new-tile (get pieces tile-id-to-merge)
              {:keys [puzzle updated-tile]}
              (add-to-puzzle puzz at-index current-tile new-tile)

              [idx-x idx-y]         at-index
              [new-row? next-index] (if (= idx-x (dec len))
                                      [true [0 (+ 1 idx-y)]]
                                      [false [(inc idx-x) idx-y]])
              first-of-row          (when new-row?
                                      (-> puzzle (nth idx-y) (nth 0)))]

          (recur puzzle
                 (if new-row?
                   [:bottom first-of-row]
                   [:right updated-tile])
                 ;; new tile matched-edges have not been rotated
                 (if new-row?
                   (-> first-of-row :matched-edges :bottom :id)
                   (-> updated-tile :matched-edges :right :id))
                 next-index
                 (inc i)))))))

(comment
  (println "hi")
  (build-puzzle "example.txt")

  (build-puzzle "input.txt")
  )

(defn puzzle-image [puzz]
  (->> puzz
       (map-indexed vector)
       (reduce
         (fn [img [index row]]
           (let [images (->> row (map :image))
                 ct     (count (first images))
                 ;; images (concat [(apply str (repeat ct " "))] images)
                 ;; ct     (+ 1 ct)
                 ]
             ;; (println images)
             ;; (println "ct" ct "index" index)
             (reduce
               (fn [img i]
                 ;; (println "appending to row index" (+ (* ct index) i))
                 ;; (println img)
                 ;; (println i)
                 ;; (println "images map nth i" (->> images (map #(nth % i))))
                 (update img (+ (* ct index) i)
                         (fn [s]
                           (apply str s
                                  (->> images (map #(nth % i))
                                       ;; (string/join " ")
                                       )
                                  ))))
               img
               (range ct))
             ))
         [])))

(comment
  (repeat 10 " ")
  (range 6)
  (-> "example.txt"
      build-puzzle
      puzzle-image)

  "..#.#....###.#.#.......##....."
  )

(defn remove-image-borders [puzzle]
  (->> puzzle
       (map (fn [row]
              (->> row
                   (map (fn [tile]
                          (update tile :image
                                  (fn [img]
                                    (->> img
                                         rest
                                         butlast
                                         (map (fn [line]
                                                (->> line
                                                     rest
                                                     butlast
                                                     (apply str))))
                                         (into []))))))
                   (into []))))
       (into [])))

(comment
  (-> "example.txt"
      build-puzzle
      remove-image-borders
      puzzle-image
      )
  )
