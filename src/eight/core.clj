(ns eight.core
  (:require [util :refer [input]]
            [clojure.string :as string]))

(defn build-program [f]
  (->> (input f)
       (map (fn [s]
              (let [[cmd arg] (->> (string/split s #" ") (into []))]
                {:cmd cmd
                 :arg (read-string arg)})))
       (map-indexed vector)))

(comment
  (build-program "example.txt")
  (build-program "input.txt")
  (read-string "-1")
  (read-string "+1")
  )

(def exec
  {"nop" (fn [{:keys [line acc]} _arg]
            (println "no-op")
           {:next-line (+ line 1)
            :acc       acc})
   "acc" (fn [{:keys [line acc]} arg]
            (println "acc" acc arg)
           {:next-line (+ line 1)
            :acc       (+ acc arg)})
   "jmp" (fn [{:keys [line acc]} arg]
           (println "jump" line arg)
           {:next-line (+ line arg)
            :acc       acc})})

(defn run-program [program]
  (loop [line         0
         acc          0
         lines-called #{}]
    (if (contains? lines-called line)
      acc
      (let [{:keys [cmd arg]} (second (nth program line))
            _                 (println cmd)
            _                 (println arg)
            {:keys [next-line acc]}
            ((exec cmd) {:line line :acc acc} arg)]
        (recur next-line acc (conj lines-called line))))))

(comment
  (-> (build-program "example.txt")
      run-program)

  (-> (build-program "input.txt")
      run-program)

  (-> (build-program "example.txt")
      (nth 0))

  ((exec "nop") {:line 0 :acc 3} 3)
  )
