(ns _2023._08.core
  (:require [util :as util]))

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
                          [left right] (->> line (re-seq #"([A-Z]{3}), ([A-Z]{3})")
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

(defn count-steps-a-to-z [fname]
  (let [{:keys [steps network]} (parse fname)
        total-steps             (count steps)]

    (loop [ct       1
           step-idx 0
           node     "AAA"]
      (let [next-dir  (case (get steps step-idx)
                        \L :left \R :right)
            next-node (get (network node) next-dir)]
        (if (#{"ZZZ"} next-node)
          ct
          (recur (inc ct)
                 (mod (inc step-idx) total-steps)
                 next-node))))))

(comment
  (mod 8 5)

  (count-steps-a-to-z "example")
  (count-steps-a-to-z "input")
  )
