(ns _2021.ten.core
  (:require [util :refer [input]]))


(comment

  (input "example.txt")
  (input "input.txt"))

(defn parse [f]
  (->> f input)
  )

(comment
  (parse "example.txt")
  (parse "input.txt"))

(def closers->score
  {\) 3
   \} 1197
   \] 57
   \> 25137})

(def closers->openers
  {\) \(
   \} \{
   \] \[
   \> \<})

(def is-opener? #{\( \{ \[ \<})

(defn corrupted-by [line]
  (loop [seen-openers []
         line         line]
    (if-not (seq line)
      nil
      (let [next (first line)
            rst  (rest line)]
        ;; (println "seen-openers" seen-openers)
        (cond
          (is-opener? next)
          (recur (cons next seen-openers) rst)

          ;; is-closer?
          (closers->openers next)
          (let [matching-opener (closers->openers next)]
            (if (#{matching-opener} (first seen-openers))
              (recur (rest seen-openers) rst)
              ;; corrupted!
              next))

          :else :wut)))))

(comment

  (let [my-stack [4 3 2]]
    (cons 5 my-stack)
    )

  (corrupted-by "{([(<{}[<>[]}>{[]{[(<()>")

  (->>
    (parse "input.txt")
    (filter corrupted-by)
    (map corrupted-by)
    (map closers->score)
    (apply +)))
