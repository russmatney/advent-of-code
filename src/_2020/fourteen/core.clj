(ns _2020.fourteen.core
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn assoc-str [s n char]
  (str (subs s 0 n) char (subs s (+ n 1) (count s))))

(comment
  (assoc-str "hello" 2 "X"))

(defn expand-floating [s]
  (loop [addrs [""]
         src   s]
    (if-let [next-char (some-> src first)]
      (let [addrs
            (if (= next-char \X)
              (->> addrs
                   (map (fn [addr]
                          [(str addr 0)
                           (str addr 1)]
                          ))
                   flatten)
              (->> addrs (map (fn [addr] (str addr next-char)))))]
        (recur addrs (rest src)))
      addrs)))

(comment
  (first (reverse (reverse "X1101X")))

  (expand-floating (reverse (reverse "X1101X")))

  )

(defn pad-bit-str [s]
  (let [len    (count s)
        pad    (- 36 len)
        prefix (apply str (repeat pad "0"))]
    (str prefix s)))

(comment
  (pad-bit-str "111010101010111111111"))

(defn get-all-slots [bitmask slot]
  (let [slot-str        (pad-bit-str (Integer/toBinaryString slot))
        masked-slot-str (reduce
                          (fn [slot-str [i char]]
                            (case char
                              \0 slot-str
                              \1 (assoc-str slot-str i \1)
                              \X (assoc-str slot-str i \X)))
                          (apply str slot-str)
                          (map-indexed vector bitmask))
        ]
    (expand-floating masked-slot-str)
    #_(->> (expand-floating masked-slot-str)
           (map #(Integer/parseInt % 2)))))

(comment
  (Integer/parseInt "011010001011100100000001001011100001" 2)

  (format "%036d" 42)
  (Integer/toBinaryString 42)
  (get-all-slots
    "000000000000000000000000000000X1001X"
    42))

(defn apply-memory-bitmask [agg slot val]
  (let [bitmask (:bit agg)
        slots   (get-all-slots bitmask slot)]
    (update agg :mem
            (fn [mem]
              (reduce (fn [mem slot]
                        (assoc mem slot val))
                      mem
                      slots)))))

(defn run [agg {:keys [cmd val slot]}]
  (case cmd
    ;; update bitmask
    "mask" (assoc agg :bit val)

    ;; apply bitmask, write to agg
    "mem" (apply-memory-bitmask agg slot val)))

(defn program [f]
  (let [cmds (parse-program f)]
    (reduce run {:mem {}} cmds)))

(defn sum [f]
  (->> (program f)
       :mem
       vals
       (apply +)))

(comment
  (program "example-two.txt")
  (sum "example-two.txt")
  (sum "input.txt")
  )
