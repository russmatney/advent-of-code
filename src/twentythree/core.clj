(ns twentythree.core
  (:require [clojure.string :as string]))

(def input "963275481")
(def example "389125467")

(defn cups [s]
  (->> s (map (comp read-string str))))

(comment
  (cups input)
  (cups example)

  (mod 13 9)
  )

(defn play-round [cs ix]
  (let [ct                (count cs)
        ix                (mod ix ct)
        cyc-cs            (take (* ct 2) (cycle cs))
        ;; pre-ix-cs         (take ix cyc-cs)
        post-ix-cs        (drop ix cyc-cs)
        current           (first post-ix-cs)
        pickup            (take 3 (rest post-ix-cs))
        rst               (drop 4 post-ix-cs)
        dest-cups         (->> cs (remove (set pickup)))
        dest-less-than    (->> dest-cups (filter #(< % current))
                               sort reverse)
        dest-greater-than (->> dest-cups (filter #(> % current))
                               sort reverse)
        dest              (if (seq dest-less-than)
                            (first dest-less-than)
                            (first dest-greater-than))
        cs-to-dest        (->> rst
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

        ;; _       (println (- ct ix))
        ;; _       (println (concat new-cs tail))
        ;; _       (println
        ;;           (->> (concat new-cs tail)
        ;;                cycle
        ;;                (drop-while #(not (= current %)))
        ;;                (drop (- ct ix))
        ;;                (take ct)))
        full-cs (->> (concat new-cs tail)
                     cycle
                     (drop-while #(not (= current %)))
                     (drop (- ct ix))
                     (take ct))

        ]
    ;; (println "pre-ix-cs" pre-ix-cs
    ;;          "current" current
    ;;          "cs-to-dest" cs-to-dest
    ;;          "cs-from-dest" cs-from-dest
    ;;          "dest" dest
    ;;          "pickup" pickup
    ;;          "tail" tail)

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
  (play-rounds (cups example) 10)
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
