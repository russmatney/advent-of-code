(ns _2020.eighteen.core
  (:require [util :refer [input]]
            [clojure.string :as string]))


(comment
  (input "example.txt")
  (input "input.txt")
  )

(defn split-first-paren-group
  "Expects the str to start with an opening paren: '('."
  [s]
  (let [closing-index
        (->> s
             rest ;; skip this paren
             (reduce (fn [{:keys [par-count idx]} c]
                       (let [new-idx (inc idx)]
                         (cond
                           (= c \()
                           {:idx       new-idx
                            :par-count (inc par-count)}

                           (= c \))
                           (if (= par-count 0)
                             ;; exit early!
                             (reduced new-idx)
                             {:idx       new-idx
                              :par-count (dec par-count)})

                           :else
                           {:idx       new-idx
                            :par-count par-count})))
                     {:par-count 0
                      :idx       0}))]
    [(->> s (take closing-index) (drop 1) (apply str))
     (->> s (drop (+ closing-index 1)) (apply str))]
    ))

(comment
  (split-first-paren-group "( blah whatever) rest of the thing")
  (split-first-paren-group "( blah(nested() ()) whatever) rest of the thing")
  )

(defn eval-line [s]
  (let [s (string/replace s " " "")]
    (loop [remaining   s
           current-val nil
           pending-op  nil]
      (if (seq remaining)
        (let [c (->> remaining (take 1) first)]
          (cond
            ;; is an operator
            (or (= c \+) (= c \*))
            (recur (rest remaining) current-val c)

            ;; is a number
            (re-seq #"^(\d+)" (apply str remaining))
            (let [num     (->> remaining (apply str) (re-seq #"^(\d+)") first second)
                  num-len (count num)
                  num     (read-string num)]
              (cond
                (nil? current-val)
                (recur (drop num-len remaining) num nil)

                pending-op
                (recur (drop num-len remaining)
                       (case pending-op
                         \+ (+ current-val num)
                         \* (* current-val num)) nil)))

            ;; paren - grab str until the correct closing, eval, replace in orginial string
            (= c \()
            (let [[paren-group remaining] (split-first-paren-group remaining)
                  val                     (eval-line paren-group)]
              (recur (str val remaining) current-val pending-op))

            ;; should never be hit
            :else
            current-val))
        current-val))))

(comment
  (= \* "*")
  (re-seq #"\d" "3")
  (eval-line "2 * 3 + 4 * 5")
  (eval-line "2 * 3 + (4 * 5)")

  (->> (input "example.txt")
       (map eval-line))
  (->> (input "input.txt")
       (map eval-line)
       (apply +)))


(defn add-precedence-parens [s]
  (let [s (string/replace s " " "")]
    (loop [remaining  s
           last-term  nil
           new-str    ""
           pending-op nil]
      (if (seq remaining)
        (let [c (->> remaining (take 1) first)]
          (cond
            (or (= c \+) (= c \*))
            (recur (rest remaining) last-term new-str c)

            (re-seq #"^(\d+)" (apply str remaining))
            (let [num (->> remaining (apply str) (re-seq #"^(\d+)") first second)]
              (cond
                (nil? last-term)
                (recur (rest remaining) num new-str pending-op)

                (= pending-op \+)
                ;; open paren, add last term and +, signal for next pass to close after term
                (recur (rest remaining) (str "(" last-term "+" num ")")
                       new-str nil)

                (= pending-op \*)
                ;; write last term, this op, signal to write next term
                (recur (rest remaining) num
                       (str new-str last-term "*") nil)))

            (= c \()
            (let [[paren-group remaining] (split-first-paren-group remaining)
                  val                     (add-precedence-parens paren-group)
                  term                    (str "(" val ")")
                  [new-str last-term]
                  (cond
                    (nil? last-term)  [new-str term]
                    (= pending-op \+) [new-str (str "(" last-term "+" term ")")]
                    (= pending-op \*) [(str new-str last-term "*") term])]
              (recur remaining last-term new-str pending-op))
            ))
        (cond
          last-term
          (str new-str last-term)

          :else
          new-str)))))


(comment
  (add-precedence-parens "1 * (2 + 3) + 4")
  (add-precedence-parens "(2 + 4 * 9) * (6 + 9 * 8 + 6) + 6")

  (add-precedence-parens "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  (add-precedence-parens "2 * 3 + 4")
  (add-precedence-parens "2 + 3 + 4")
  (add-precedence-parens "2 * 3 + (4 * 5)")
  (eval-line (add-precedence-parens "2 * 3 + 4")) ;; should be 14
  (eval-line (add-precedence-parens "2 * 3 + (4 * 5)")) ;; should be 46

  (->> (input "example.txt")
       (map (comp eval-line add-precedence-parens)))

  (->> (input "input.txt")
       (map (comp eval-line add-precedence-parens))
       (apply +))

  )
