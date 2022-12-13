(ns _2022._13.core
  (:require [util :as util]))

(defn path [f]
  (str "src/_2022/_13/" f))

(defn signals [f]
  (-> (path f)
      (util/parse-input {:partition? true})
      (->>
        (map-indexed
          (fn [i pair]
            (let [[left right] pair]
              [(inc i)
               {:left  (read-string left)
                :right (read-string right)}])))
        (into {}))))

(comment
  (signals "example.txt")
  (signals "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compare-sides [left right]
  (cond
    (and (nil? left) (not (nil? right)))
    :correct

    (and (not (nil? left)) (nil? right))
    :incorrect

    (and (int? left) (int? right))
    (cond
      (< left right) :correct
      (= left right) :continue
      (> left right) :incorrect)

    (and (vector? left) (vector? right))
    (->> (range (max (count left) (count right)))
         (map (fn [i]
                (let [left-val  (get left i)
                      right-val (get right i)]
                  (compare-sides left-val right-val))))
         (filter #{:correct :incorrect})
         first)

    (and (int? left) (vector? right))
    (compare-sides [left] right)

    (and (vector? left) (int? right))
    (compare-sides left [right])

    :else
    (println "compare what?" left right)))

(comment
  (compare-sides [1,1,3,1,1] [1,1,5,1,1])
  (compare-sides [[1],[2,3,4]] [[1],4])
  (compare-sides [9] [[8,7,6]])
  (compare-sides [[4,4],4,4] [[4,4],4,4,4])
  (compare-sides [7,7,7,7] [7,7,7])
  (compare-sides [] [3])
  (compare-sides [[[]]] [[]])
  (compare-sides [1,[2,[3,[4,[5,6,7]]]],8,9]
                 [1,[2,[3,[4,[5,6,0]]]],8,9])
  (compare-sides [[[[10,6],3,[9,6,7,9,7]],[[2,4,10,7,1],[7,9],[8,2,9,9,2],5,[1]],5,[]],[[[8,6,6,9,1],1],[7,[8,3],9,4,[0,3,10,9,7]]],[[[10,1]],0,[],[[4,1],[3],[10,6,4],10]],[8,[7],2,9],[[],2,[[3,6,3,6],4,[8,7,4,7,2],3]]]
                 [[8,9,[[]]],[],[]])
  (compare-sides [[[0,[1,10,0,4]],1,[[0,10,3,9],4,8],[[5,7,1,8,0],[9,1,10,7,2]]],[6,3]]
                 [[[10,10,[3,10,7]]],[6,5,[[10,1,10]],1,1],[10,[9]],[[5,7,0,[],[6,0,10,10]],1,6],[9,8,[]]]))

(defn sum-correct-order-idxs [f]
  (->> (signals f)
       (map (fn [[i {:keys [left right]}]]
              [i (compare-sides left right)]))
       (filter (comp #{:correct} second))
       (map first)
       (reduce +)))

(comment
  (sum-correct-order-idxs "example.txt")
  (sum-correct-order-idxs "input.txt"))
