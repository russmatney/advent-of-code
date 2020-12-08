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
         max-jmp-nop  0
         max-cmd      nil]
    (if (contains? lines-called line)
      {:acc         acc
       :max-jmp-nop max-jmp-nop
       :max-cmd     max-cmd}
      (let [{:keys [cmd arg]} (second (nth program line))
            {:keys [next-line acc]}
            ((exec cmd) {:line line :acc acc} arg)

            [max-jmp-nop max-cmd]
            (if (and (#{"jmp" "nop"} cmd)
                     (> line max-jmp-nop))
              [line cmd]
              [max-jmp-nop max-cmd])]
        (recur next-line acc (conj lines-called line) max-jmp-nop max-cmd)))))

(defn run-with-overwrite
  [program {:keys [overwrite-line overwrite-cmd]}]
  (loop [line         0
         acc          0
         lines-called #{}]
    (cond
      (contains? lines-called line)
      (do
        (println "stoppin', cuz we loopin', baby.")
        acc)

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
  (let [program                       (build-program "input.txt")
        {:keys [max-jmp-nop max-cmd]} (analyze-program program)]

    (run-with-overwrite program {:overwrite-line max-jmp-nop
                                 :overwrite-cmd  (if (#{"nop"} max-cmd)
                                                   "jmp" "nop")})

    )
  ;; 1314 too low....hmmmm
  )
