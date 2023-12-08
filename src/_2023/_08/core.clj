(ns _2023._08.core
  (:require [util :as util]
            [clojure.string :as string]))

(defn input [fname]
  (util/parse-input (str "src/_2023/_08/" fname ".txt") {:partition? true}))

(comment
  (input "input")
  (input "example"))

(defn parse [fname]
  (let [[steps network] (input fname)
        steps           (->> (first steps) (into []))

        network (reduce
                  (fn [net line]
                    (let [key          (->> line (take 3) (apply str))
                          [left right] (->> line (re-seq #"([A-Z0-9]{3}), ([A-Z0-9]{3})")
                                            (map rest) first)]
                      (assoc net key {:left left :right right})))
                  {}
                  network)]
    {:steps   steps
     :network network}))

(comment
  (parse "input")
  (parse "example")
  (->> "AAA = (BBB, CCC)" (re-seq #"([A-Z]{3}), ([A-Z]{3})")))

(defn count-steps
  ([fname] (count-steps fname "AAA" #{"ZZZ"}))
  ([fname start-node is-finished?]
   (let [{:keys [steps network]} (parse fname)
         total-steps             (count steps)]
     (loop [ct       1
            step-idx 0
            node     start-node]
       (let [next-dir  (case (get steps step-idx)
                         \L :left \R :right)
             next-node (get (network node) next-dir)]
         (if (is-finished? next-node)
           ct
           (recur (inc ct)
                  (mod (inc step-idx) total-steps)
                  next-node)))))))

(comment
  (mod 8 5)
  (count-steps "example")
  (count-steps "input"))

;; pulled lcm code from 2020 day 13....
;; https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd [a b]
  (if (zero? b) a
      (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& ns] (reduce lcm (remove nil? ns)))

(comment
  (lcmv 2 3 4 nil))

(defn ghost-steps [fname]
  (let [{:keys [network]} (parse fname)
        first-nodes       (->> network keys (filter #(string/ends-with? % "A")) (into #{}))
        dists-to-z        (->> first-nodes
                               (map #(count-steps
                                       fname % (fn [node] (string/ends-with? node "Z")))))]
    (apply lcmv dists-to-z)))

(comment
  (ghost-steps "example2")
  (ghost-steps "input"))
