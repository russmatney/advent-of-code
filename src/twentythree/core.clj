(ns twentythree.core
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input "963275481")
(def example "389125467")

(defn cups [s]
  (->> s (map (comp read-string str))))

(defn cups-million [s]
  (let [cs      (cups s)
        max     (apply max cs)
        million 1000000
        ]
    (concat cs (range max million))))

(defn cups-million-map [s]
  (let [cs (cups-million s)]
    (->> cs
         (map-indexed vector)
         (into {}))))

(comment
  (cups input)
  (cups example)

  (->
    (cups-million example)
    (nth 1)
    )

  (->
    (cups-million example)
    (nth 102346)
    )

  (->
    (cups-million-map example)
    (get 702346)
    )

  (mod 13 9)
  )

(defn play-round [cs ix]
  (let [ct      (count cs)
        ix      (mod ix ct)
        highest (apply max cs)
        cyc-cs  (take (* ct 2) (cycle cs))
        ;; pre-ix-cs         (take ix cyc-cs)

        post-ix-cs (drop ix cyc-cs)
        current    (first post-ix-cs)
        pickup     (take 3 (rest post-ix-cs))
        rst        (drop 4 post-ix-cs)

        dest (->> (iterate (fn [curr]
                             (let [x (dec curr)]
                               (if (zero? x)
                                 highest
                                 x)))
                           (if (= 1 current)
                             highest
                             (dec current)))
                  (drop-while
                    (fn [x]
                      ((set pickup) x)))
                  first)

        cs-to-dest (->> rst
                        (take-while #(not (= % dest))))

        new-cs (concat
                 [current]
                 cs-to-dest
                 [dest]
                 pickup)

        cs-from-dest (->> rst
                          (drop-while #(not (= % dest)))
                          rest)
        tail         (take (- ct (count new-cs)) cs-from-dest)

        full-cs (->> (concat new-cs tail)
                     cycle
                     (drop-while #(not (= current %)))
                     (drop (- ct ix))
                     (take ct))]
    full-cs))

(defn play-rounds [cs rounds]
  (->>
    (iterate
      (fn [[cs ix]]
        [(play-round cs ix) (inc ix)])
      [cs 0])
    (take (+ 1 rounds))
    last
    first))

(comment
  (println "hellloo\n\n")
  (play-rounds (cups example) 100)

  (play-rounds (cups example) 10000000) ;; 639147258
  )

(defn after-1 [cs]
  (let [ct (count cs)]
    (->> cs
         cycle
         (drop-while #(not (= 1 %)))
         rest
         (take (dec ct)))))

(comment
  (->>
    (play-rounds (cups example) 10)
    after-1)
  (->>
    (play-rounds (cups example) 100)
    after-1)

  (->>
    (play-rounds (cups input) 100)
    after-1
    string/join)
  97632548
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(comment
  (do
    (println "starting... for" example)
    (let [in        example
          ix->label (cups-million-map in)
          label->ix (set/map-invert ix->label)
          max-label (apply max (vals ix->label))
          ]
      (println "created ix->label and label->ix")
      (label->ix 12346)
      (ix->label 12337)

      (loop [current-ix 0
             max-ix     (label->ix max-label)]
        (let [current-val    (ix->label current-ix)
              picked-up-ixs  (range (+ current-ix 1) (+ current-ix 4))
              picked-up-vals (->> picked-up-ixs (map ix->label))
              current-label  (ix->label current-ix)
              dest-val       (->> (if (= current-label 1)
                                    max-label
                                    (dec current-label))
                                  (iterate (fn [x]
                                             (let [next (dec x)]
                                               (if (zero? next)
                                                 max-label
                                                 next))))
                                  (drop-while #((set picked-up-vals) %))
                                  first)
              dest-ix        (label->ix dest-val)

              ;; ix-update
              ;; (->>
              ;;   ()
              ;;   (merge {dest-ix dest-val}))
              ]
          (println
            "current-idx" current-ix
            "current-val" current-val
            "\n"
            "picked-up-ixs" picked-up-ixs
            "picked-up-vals" picked-up-vals
            "\n"
            "dest-val" dest-val
            "dest-ix" dest-ix)

          ;; move pickup ixs to after dest ix
          ;; update current ix

          ))))

  (-> {:hi :bye} set/map-invert)


  (->> (cups-million example)
       (map-indexed (fn [i x] [i x]))
       (into {})
       ((fn [mp]
          (get mp 623450))))

  )
