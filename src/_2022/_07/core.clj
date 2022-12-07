(ns _2022._07.core
  (:require
   [clojure.string :as string]))

(defn input [f]
  (->
    (slurp (str "src/_2022/_07/" f))
    ;; split on \n\$ to grab each command + output
    (string/split #"\n?\$ ")
    (->> (remove empty?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cd

(defn handle-cd [state cmd]
  (let [dir (-> cmd (string/split #" ") second)]
    (update state :cwd
            (cond
              (#{".."} dir) (fn [cwd] (->> cwd butlast (into [])))
              (#{"/"} dir)  (constantly ["/"])
              :else         (fn [cwd] (->> (concat cwd [dir]) (into [])))))))

(comment
  (-> ["/" "a" "b"] butlast (into []) (concat ["c"]))

  (handle-cd {:cwd ["/" "a"]} "cd b")
  (handle-cd {:cwd ["a" "b"]} "cd ..")
  (handle-cd {:cwd ["aaaa" "bbb" "cc"]} "cd ..")
  (handle-cd {:cwd ["aaaa" "bbb" "cc"]} "cd /"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ls

;; parse file

(defn parse-ls-file [ls-file]
  (-> (re-seq #"^(\d+) (.*)$" ls-file) first second read-string))

(comment (parse-ls-file "436456 s.log"))

;; parse dir

(defn parse-ls-dir [state ls-file]
  (let [dir (-> (re-seq #"^dir (.+)$" ls-file) first last)]
    (into [] (concat (:cwd state) [dir]))))

(defn handle-ls [state cmd]
  (let [contents   (-> cmd string/split-lines rest)
        files-size (->> contents
                        (filter #(re-seq #"^\d" %))
                        (map parse-ls-file)
                        (reduce +))
        dirs       (->> contents
                        (filter #(re-seq #"^dir" %))
                        (map (partial parse-ls-dir state))
                        (into #{}))]
    (assoc-in state [:dirs (:cwd state)]
              {:dirs       dirs
               :files-size (or files-size 0)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch

(defn calc-dir-size [state-dirs dir-path]
  (let [{:keys [files-size dirs]} (get state-dirs dir-path)
        nested-size               (if (seq dirs)
                                    (->> dirs
                                         (map (partial calc-dir-size state-dirs))
                                         (reduce +))
                                    0)]
    (+ (or files-size 0) (or nested-size 0))))

(defn set-dir-sizes [dirs]
  (reduce
    (fn [state-dirs dir-path]
      (let [dir-size (calc-dir-size state-dirs dir-path)]
        (assoc-in state-dirs [dir-path :dir-size] dir-size)))
    dirs
    (->> dirs keys)))

(defn handle-command
  [state cmd]
  (cond (re-seq #"^cd" cmd) (handle-cd state cmd)
        (re-seq #"^ls" cmd) (handle-ls state cmd)))

(defn handle-commands [f]
  (->>
    (input f)
    (reduce handle-command {:cwd [] :dirs {}})
    :dirs
    set-dir-sizes))

(comment (handle-commands "example.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 1

(defn part1 [f]
  (->>
    (handle-commands f)
    (filter (fn [[_path {:keys [dir-size]}]] (> 100000 dir-size)))
    (map (comp :dir-size second))
    (reduce +)))

(comment
  (part1 "example.txt")
  (part1 "input.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2

(defn used-space [dirs]
  (-> dirs (get ["/"]) :dir-size))

(defn unused-space [dirs]
  (- 70000000 (used-space dirs)))

(def required-space 30000000)

(defn part2 [f]
  (let [dirs     (handle-commands f)
        required (- required-space (unused-space dirs))]
    (->> dirs
         (filter (comp (fn [size] (>= size required))
                       :dir-size second))
         (sort-by (comp :dir-size second) <)
         first second :dir-size)))

(comment
  (part2 "example.txt")
  (part2 "input.txt")
  ;; 5025657
  (->> "input.txt" handle-commands
       #_unused-space)

  (get *1 ["/"]))
