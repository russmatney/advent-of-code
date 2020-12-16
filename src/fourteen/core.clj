(ns fourteen.core
  (:require [util :refer [input]]))

(comment
  (input "example.txt"))

(defn parse-program [f]
  (->> (input f)
       (map (fn [s]
              (let [mask-res (re-seq #"mask = (\w+)" s)
                    mem-res  (re-seq #"mem\[(\d+)\] = (\d+)" s)]
                (cond
                  mask-res
                  {:cmd "mask"
                   :val (-> mask-res first second)}

                  mem-res
                  {:cmd  "mem"
                   :slot (-> mem-res first second read-string)
                   :val  (-> mem-res first rest second read-string)}))))))

(comment
  (re-seq #"mask = (\w+)" "mask = 123X")
  (re-seq #"mask = (\w+)" "mem[123] = 123")
  (re-seq #"mem\[(\d+)\] = (\d+)" "mem[123] = 123")

  (parse-program "example.txt")
  )

(defn apply-bitmask [bitmask val]
  (reduce (fn [val [i char]]
            (case char
              \X val
              \0 (bit-clear val i)
              \1 (bit-set val i)))
          val (map-indexed vector (reverse bitmask))))

(comment
  (Integer/toBinaryString 7)
  (Integer/toUnsignedLong "111")
  (Integer/toBinaryString 7)
  )

(defn run-command [agg {:keys [cmd val slot]}]
  (case cmd
    ;; update bitmask
    "mask" (assoc agg :bit val)

    ;; apply bitmask, write to agg
    "mem" (let [val (apply-bitmask (:bit agg) val)]
            (assoc-in agg [:mem slot] val))))

(defn run-program [f]
  (let [cmds (parse-program f)]
    (reduce run-command {:mem {}} cmds)))

(defn sum-final-values [f]
  (->> (run-program f)
       :mem
       vals
       (apply +)))

(comment
  (-> 0
      (bit-clear 1)
      (bit-clear 6))

  (run-program "example.txt")
  (run-program "input.txt")
  (sum-final-values "input.txt")
  )
