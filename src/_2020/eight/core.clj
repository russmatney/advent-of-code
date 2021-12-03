(ns _2020.eight.core
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
           {:next-line (+ line 1)
            :acc       acc})
   "acc" (fn [{:keys [line acc]} arg]
           {:next-line (+ line 1)
            :acc       (+ acc arg)})
   "jmp" (fn [{:keys [line acc]} arg]
           {:next-line (+ line arg)
            :acc       acc})})

(defn run-program [program]
  (loop [line         0
         acc          0
         lines-called #{}]
    (if (contains? lines-called line)
      acc
      (let [{:keys [cmd arg]} (second (nth program line))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn analyze-program [program]
  (loop [line         0
         acc          0
         lines-called #{}
         non-acc-cmds []]
    (if (contains? lines-called line)
      {:acc          acc
       :non-acc-cmds non-acc-cmds}
      (let [{:keys [cmd arg]} (second (nth program line))
            {:keys [next-line acc]}
            ((exec cmd) {:line line :acc acc} arg)

            non-acc-cmds
            (if (#{"jmp" "nop"} cmd)
              (conj non-acc-cmds {:cmd cmd :line line})
              non-acc-cmds)]
        (recur next-line acc (conj lines-called line) non-acc-cmds)))))

(defn run-with-overwrite
  [program {:keys [overwrite-line overwrite-cmd]}]
  (loop [line         0
         acc          0
         lines-called #{}]
    (cond
      (contains? lines-called line)
      (do
        (println "stoppin', cuz we loopin', baby.")
        false)

      (>= line (count program))
      (do
        (println "I won't be back. (termination)")
        acc)

      :else
      (let [c (second (nth program line))
            {:keys [cmd arg]}
            (if (= line overwrite-line)
              (assoc c :cmd overwrite-cmd)
              c)
            {:keys [next-line acc]}
            ((exec cmd) {:line line :acc acc} arg)]
        (recur next-line acc (conj lines-called line))))))

(comment
  (let [program                (build-program "input.txt")
        {:keys [non-acc-cmds]} (analyze-program program)
        non-acc-cmds           (sort-by :line > non-acc-cmds)]

    (loop [non-acc-cmds non-acc-cmds]
      (let [next               (first non-acc-cmds)
            {:keys [line cmd]} next
            result
            (run-with-overwrite
              program {:overwrite-line line
                       :overwrite-cmd  (if (#{"nop"} cmd)
                                         "jmp" "nop")})]
        (if result
          result
          (recur (rest non-acc-cmds)))))))
