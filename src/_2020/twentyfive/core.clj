(ns _2020.twentyfive.core
  (:require [util :refer [input]]))

;; to transform a subject number...
;; for i in loop-size:
;;   set value to itself * subject number
;;   set value to remainder after dividing by 20201227

;; cryptographic handshake:
;; card transforms sn 7 into card's pubkey with card's loop-size
;; door transforms sn 7 into door's pubkey with door's loop-size
;; public keys are shared
;; card transforms the sn of the door's pubkey and card's loop-size
;; door transforms the sn of the card's pubkey and door's loop-size
;; result is same encryption key

(defn pks [f]
  (->> f input (map read-string)))

(comment
  (pks "example.txt")
  (pks "input.txt"))


(defn tx-sn [sn loop-size]
  (let [blah 20201227]
    (loop [val             1
           remaining-loops loop-size]
      (if (> remaining-loops 0)
        (recur (mod (* sn val) blah) (dec remaining-loops))
        val))))

(comment
  (mod (* 7 7 7 7) 20201227)
  (mod (* 7 7 7 7 7 7 7 7 7 7 7) 20201227)
  (tx-sn 7 8)
  (tx-sn 7 11)
  )

(defn find-loop-size [sn pk]
  (loop [loop-size 1
         val       1]
    (let [new-val (mod (* val sn) 20201227)]
      (if (= pk new-val)
        loop-size
        (recur (inc loop-size) new-val)))))

(comment
  (println "hi")
  (def --pk1 5764801)
  (def --pk2 17807724)
  (def --sn 7)
  (find-loop-size --sn --pk1)
  (find-loop-size --sn --pk2)

  (->
    (find-loop-size --sn --pk1)
    ((fn [ls]
       (tx-sn --pk2 ls))))

  (->
    (find-loop-size --sn --pk2)
    ((fn [ls]
       (tx-sn --pk1 ls)))))

(defn find-encryption-key [f]
  (let [[pk1 pk2] (-> f pks)
        _         (println "two pks" pk1 pk2)
        ls1       (find-loop-size 7 pk1)
        _         (println "ls1" ls1)
        ls2       (find-loop-size 7 pk2)
        _         (println "ls2" ls2)
        enc1      (tx-sn pk2 ls1)
        enc2      (tx-sn pk1 ls2)
        ]
    (println "enc keys" enc1 enc2)
    enc1))

(comment
  (find-encryption-key "example.txt")
  (find-encryption-key "input.txt")
  )
