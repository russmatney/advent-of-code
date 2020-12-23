(ns twentythree.core
  (:require [clojure.string :as string]))

(def input "963275481")
(def example "389125467")

(defn cups [s]
  (->> s (map (comp read-string str))))

(comment
  (cups input)
  (cups example))

(defn play-round [cs ix]
  (let [ct      (count cs)
        ix      (mod ix ct)
        highest (apply max cs)
        cyc-cs  (take (* ct 2) (cycle cs))

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
    string/join))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cups-million [s]
  (let [cs      (cups s)
        max     (apply max cs)
        million 1000000
        ]
    (concat cs (range (+ 1 max) (+ million 1)))))

(defn ->val->next-val [cs]
  (->> cs
       (reduce
         (fn [{:keys [last pointers]} c]
           {:last     c
            :pointers (assoc pointers last c)})
         {:last     (last cs)
          :pointers {}})
       :pointers
       (into {})))


(defn take-pts [pts start n]
  (->> (iterate pts start)
       rest
       (take n)))

(comment
  (take-pts {1 3
             3 6
             7 4
             6 7} 3 4))

(defn play-rounds-2 [in max-rounds]
  (let [;; cs                (cups in)
        cs                (cups-million in)
        label->next-label (->val->next-val cs)
        max-label         (apply max (keys label->next-label))]
    (loop [label->next-label label->next-label
           current-label     (first cs)
           round             0]
      (if (= round max-rounds)
        (take-pts label->next-label 1 9)
        (let [picked-up       (take-pts label->next-label current-label 3)
              first-picked-up (first picked-up)
              last-picked-up  (last picked-up)
              next-current    (label->next-label last-picked-up)

              dest       (->> (if (= current-label 1)
                                max-label
                                (dec current-label))
                              (iterate (fn [x]
                                         (let [next (dec x)]
                                           (if (zero? next)
                                             max-label
                                             next))))
                              (drop-while #((set picked-up) %))
                              first)
              after-dest (label->next-label dest)]
          (recur
            (-> label->next-label
                (assoc current-label next-current)
                (assoc dest first-picked-up)
                (assoc last-picked-up after-dest))
            next-current
            (inc round)))))))

(comment
  (play-rounds-2 example 10)
  (play-rounds-2 example 100)
  (play-rounds-2 example 10000000)
  (->> (play-rounds-2 input 10000000)
       (take 2)
       (apply *))
  (play-rounds-2 input 100)

  )
