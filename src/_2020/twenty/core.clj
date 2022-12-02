(ns _2020.twenty.core
  (:require [util :refer [input partition-by-newlines]]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn edges-for-image
  "Here we reverse the bottom and left for rotational consistency....
  .... or maybe for insanity?"
  [image]
  {:top    (first image)
   :bottom (apply str (reverse (last image)))
   :left   (apply str (reverse (map (fn [line] (first line)) image)))
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
  (assign-matching-edges "example.txt")

  ;; part 1
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
    (into [])))

(defn flip-image [direction image]
  (case direction
    :vertical
    (->> image (reverse) (into []))
    :horizontal
    (->> image (map (comp #(apply str %) reverse)) (into []))))

(defn flip-tile [direction tile]
  (let [updated-image (flip-image direction (:image tile))
        updated-matched-edges
        (case direction
          :vertical
          (-> tile :matched-edges
              (assoc :top (-> tile :matched-edges :bottom))
              (assoc :bottom (-> tile :matched-edges :top))
              (->> (remove (comp nil? val))
                   (into {})))
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

(defn rotate-image-once [img]
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

(defn rotate-image [times tile]
  (->> (iterate rotate-image-once tile)
       (take (+ times 1))
       last))

(defn rotate-tile-once
  "Rotates a tile one time, counterclockwise, such that:
  top->right, right->bottom, bottom->left, left->top."
  [tile]
  (let [updated-image         (rotate-image-once (:image tile))
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
    (println (:image (val (first pieces))))

    (:image (val (first pieces)))
    ;; (:image (rotate-tile-once (val (first pieces))))
    ))

(defn rotate-tile [times tile]
  (->> (iterate rotate-tile-once tile)
       (take (+ times 1))
       last))

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
   [:bottom :left]   1})

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
      (not match-reversed?)
      ((fn [t]
         (println "edge case running!")
         (cond
           (and (#{:right :bottom} side)
                (#{:left :right} match-side))
           (flip-tile :vertical t)

           ;; (and (#{:right} side)
           ;;      (#{:bottom :top} match-side))
           (and (#{:bottom :right} side)
                (#{:top :bottom} match-side))
           (flip-tile :horizontal t)

           :else t)))

      true
      ((fn [t]
         (rotate-tile rots t))))))

(comment
  (println "xxxx")
  (let [tlc (get (assign-matching-edges "example.txt") "2971")
        p   (get (assign-matching-edges "example.txt") "1489")
        ]
    (print-tile tlc)
    (print-tile p)
    (-> (update-tile-to-match tlc :right p)
        print-tile)))

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

(defn puzzle-image [puzz]
  (->> puzz
       (map-indexed vector)
       (reduce
         (fn [img [index row]]
           (let [images (->> row (map :image))
                 ct     (count (first images))]
             (reduce
               (fn [img i]
                 (update img (+ (* ct index) i)
                         (fn [s]
                           (apply str s
                                  (->> images (map #(nth % i)))))))
               img
               (range ct))))
         [])))

(defn puzzle-image-debug [puzz]
  (->> puzz
       (map-indexed vector)
       (reduce
         (fn [img [index row]]
           (let [images (->> row (map :image))
                 ct     (count (first images))]
             (reduce
               (fn [img i]
                 (update img (+ (* ct index) i)
                         (fn [s]
                           (apply str s
                                  (->> images (map #(nth % i))
                                       (string/join " "))))))
               img
               (range ct))))
         [])))

(comment
  (-> "example.txt" build-puzzle puzzle-image-debug)
  (-> "input.txt" build-puzzle puzzle-image-debug)
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
      puzzle-image))

(defn monster []
  (-> "monster.txt"
      input
      ((fn [[one two three]]
         (let [ct    (count two)
               three (str "." three "...")
               one   (apply str (concat (repeat (- ct 2) " ") [one "."]))]
           (->> [one two three]
                (map #(string/replace % " " "."))))))))

(defn monster-shapes []
  (let [mon     (monster)
        flipped (flip-image :vertical mon)]
    [mon
     (rotate-image 1 mon)
     (rotate-image 2 mon)
     (rotate-image 3 mon)
     flipped
     (rotate-image 1 flipped)
     (rotate-image 2 flipped)
     (rotate-image 3 flipped)]))

(defn monster-sightings [img shape]
  ;; (println "shape-str" (->> shape string/join))
  ;; (println "img-str" (string/join img))
  (let [shape-width (-> shape first count)
        img-width   (-> img first count)
        width-diff  (- img-width shape-width)

        _         (println "width-diff" width-diff)
        shape-reg (re-pattern (string/join (apply str (repeat width-diff ".")) shape))
        _         (println "shape-reg" shape-reg)
        img-str   (string/join img)]
    (loop [img       img-str
           sightings 0]
      (let [match (re-find shape-reg img)]
        (if-not match
          sightings
          ;; drop the first match + 1 from the string and loop
          (let [idx (string/index-of img match)
                img (->> img (drop (inc idx)) (apply str))]
            (recur img (inc sightings))))))))

(comment
  (let [img "hello"
        idx (string/index-of img "l")]
    (->> img (drop (+ idx 1)) (apply str)))
  (let [s "..#..#"]
    (re-seq (re-pattern s) "...####.........#..#..#..#"))

  (let [f     "example.txt"
        img   (-> f build-puzzle remove-image-borders puzzle-image)
        shape (-> (monster-shapes) rest rest first)]
    (monster-sightings img shape)))

(defn get-best-monster [img]
  (->> (monster-shapes)
       (map (fn [shape]
              {:shape     shape
               :sightings (monster-sightings img shape)}))
       (sort-by :sightings >)
       first))

(comment
  (let [f   "example.txt"
        img (-> f build-puzzle remove-image-borders puzzle-image)]
    (get-best-monster img))

  (let [f   "input.txt"
        img (-> f build-puzzle remove-image-borders puzzle-image)]
    (get-best-monster img)))

(defn count-hashes [img]
  (-> img
      string/join
      (->>
        (filter #{\#})
        count)))

(defn non-monster-hash-count [f]
  (let [puzzle       (remove-image-borders (build-puzzle f))
        puzzle-image (puzzle-image puzzle)

        {:keys [shape sightings]} (get-best-monster puzzle-image)

        image-hashes (count-hashes puzzle-image)
        shape-hashes (count-hashes shape)
        ]
    (- image-hashes (* sightings shape-hashes))))

(comment
  (println "wh")
  (non-monster-hash-count "example.txt")
  ;; 288 too high
  ;; 273
  (non-monster-hash-count "input.txt")
  ;; 2891 (probably) too high
  ;; 2786 too high
  ;; 2366
  )
