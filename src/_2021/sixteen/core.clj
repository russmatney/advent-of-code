(ns _2021.sixteen.core
  (:require [util :refer [input]]))

(def input-data (first (input "input.txt")))

(def hex->bin
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn chars->dec [chars]
  (->> chars (apply str) (#(Integer/parseInt % 2))))

(comment
  (chars->dec (seq "000000000011011"))
  (Integer/parseInt "001" 2)
  (Integer/parseInt "110" 2)
  (Integer/parseInt "11011" 2)

  (Integer/parseInt "11010001010" 2)
  (Integer/parseInt "1010" 2)
  (Integer/parseInt "0101001000100100" 2)
  )


(defn input->bin [inp]
  (->> inp
       (map hex->bin)
       (apply str)))

(comment
  input-data
  (input->bin "D2FE28")
  (input->bin "38006F45291200")
  (input->bin "EE00D40C823060")
  (input->bin "8A004A801A8002F478")
  (input->bin "620080001611562C8802118E34")
  (input->bin "C0015000016115A2E0802F182340")
  (input->bin "A0016C880162017C3686B18A3D4780")
  (input->bin input-data)
  )

(defn parse-literal-value
  "Take 5 at a time until the first char is 0."
  [bin]
  (let [trimmed (->> bin (drop-while #(#{\0} %)))]
    (loop [literal-bin '()
           bin         trimmed
           len-used    (- (count bin) (count trimmed))
           done?       nil]
      (println "building up literal-bin for bin" (apply str literal-bin) (apply str bin))
      (println "done?" done?)
      (cond
        (or done? (< (count bin) 5))
        [(chars->dec literal-bin) len-used (apply str bin)]

        ;; (not (zero? (mod (count bin) 5)))
        ;; (recur literal-bin (rest bin) len-used done?)

        :else
        (let [next (take 5 bin)
              lit  (concat literal-bin (rest next))]
          (println "new lit" lit)
          (println "first next" (first next) (#{\0} (first next)))
          (recur lit (drop 5 bin) (+ len-used 5) (#{\0} (first next))))))))

(comment
  (mod 15 5)

  (parse-literal-value "101111111000101000")
  (parse-literal-value "10111111100010100010101001010101")
  (parse-literal-value "0101010110001011")
  (#{\0} \0)
  (#{\0} \1)
  (print "wut\n\n")
  )

(defn parse-packet [bin]
  (let [version                     (->> bin (take 3) chars->dec)
        type-id                     (->> bin (drop 3) (take 3) chars->dec)
        operator?                   (not (#{4} type-id))
        [literal literal-used rest] (if-not operator?
                                      (parse-literal-value (->> bin (drop 6)))
                                      [nil 0 nil])
        length-type-id              (when operator?
                                      (->> bin (drop 6) first))
        sub-packet-len              (when (#{\0} length-type-id)
                                      (->> bin (drop 7) (take 15) chars->dec))
        sub-packet-num              (when (#{\1} length-type-id)
                                      (->> bin (drop 7) (take 11) chars->dec))
        rest-bin                    (->> bin (drop
                                               (+ 3 3 (if operator? 1 0)
                                                  literal-used
                                                  (if sub-packet-len 15 0)
                                                  (if sub-packet-num 11 0)))
                                         (apply str))

        [sub-packets rest-bin]
        (cond sub-packet-len
              (->> rest-bin (take sub-packet-len) (apply str)
                   ((fn [bin]
                      (loop [packets []
                             b       bin]
                        (if (zero? (count b))
                          [packets (->> rest-bin (drop sub-packet-len) (apply str))]
                          (let [{:keys [rest-bin] :as packet} (parse-packet b)]
                            (recur (conj packets packet) rest-bin)))))))
              sub-packet-num
              (loop [packets   []
                     b         rest-bin
                     remaining sub-packet-num]
                (if (zero? remaining)
                  [packets b]
                  (let [{:keys [rest-bin] :as packet} (parse-packet b)]
                    (recur (conj packets packet) rest-bin (dec remaining)))))

              :else [[] rest-bin])]
    {:binary            bin
     :rest-bin          rest-bin
     :version           version
     :version-sum       (apply + version (->> sub-packets (map :version-sum)))
     :type-id           type-id
     :literal-value     literal
     :literal-rest      rest
     :operator?         operator?
     :sub-packet-length sub-packet-len
     :sub-packet-number sub-packet-num
     :sub-packets       sub-packets
     :length-type-id    length-type-id}))

(comment
  (parse-packet (input->bin "D2FE28"))
  (parse-packet (input->bin "38006F45291200"))
  (parse-packet (input->bin "EE00D40C823060"))
  (parse-packet (input->bin "8A004A801A8002F478"))

  (parse-packet (input->bin "620080001611562C8802118E34"))
  (parse-packet (input->bin "C0015000016115A2E0802F182340"))
  (parse-packet (input->bin "A0016C880162017C3686B18A3D4780"))
  (parse-packet (input->bin input-data))

  (println "\n\n")
  (Integer/parseInt "0110001111100011001000001000110001101101" 2)
  )
