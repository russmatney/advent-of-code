(ns _2022._07.core
  (:require [util :as util]
            [clojure.string :as string]))

(defn input [f]
  (->
    (slurp (str "src/_2022/_07/" f))
    (string/split #"\n?\$ ")
    (->> (remove empty?))))

(comment
  (input "input.txt")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cd

(defn handle-cd [state cmd]
  (let [dir (-> cmd (string/split #" ") second)]
    (update state :cwd
            (cond
              (#{".."} dir)
              (fn [cwd]
                (string/replace cwd (-> (re-seq #"\/([a-z]*\/)$" cwd) first second) ""))

              (#{"/"} dir) (constantly "/")

              :else
              (fn [cwd]
                (->>
                  ;; TODO compress/handle ..
                  (str cwd dir "/")))))))

(comment

  (re-seq #"\/([a-z]*\/)$" "/a/b/c/d/e/")

  (handle-cd {:cwd ""} "cd /")
  (handle-cd {:cwd "/"} "cd /")
  (handle-cd {:cwd "/a"} "cd /")
  (handle-cd {:cwd ""} "cd a")
  (handle-cd {:cwd "a/"} "cd b")
  (handle-cd {:cwd "a/b/"} "cd b")
  (handle-cd {:cwd "a/b/"} "cd ..")
  (handle-cd {:cwd "aaaa/bbb/ccc/baadfsd/"} "cd ..")
  (handle-cd {:cwd "aaaa/bbb/ccc/baadfsd/"} "cd /")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ls

;; parse file

(defn parse-ls-file [state ls-file]
  (let [[size fname] (-> (re-seq #"^(\d+) (.*)$" ls-file)
                         first rest)]
    {:size     (read-string size)
     :filename (str (:cwd state) fname)}))

(comment (parse-ls-file {:cwd "/"} "436456 s.log"))

;; parse dir

(defn parse-ls-dir [state ls-file]
  (let [dir (-> (re-seq #"^dir (.*)$" ls-file)
                first last)]
    {:dir (str (:cwd state) dir "/")}))

(defn handle-ls [state cmd]
  (let [contents   (-> cmd string/split-lines rest)
        files      (->> contents
                        (filter #(re-seq #"^\d" %))
                        (map (partial parse-ls-file state))
                        seq)
        files-size (when (seq files)
                     (->> files (map :size) (reduce +)))
        dirs       (->> contents
                        (filter #(re-seq #"^dir" %))
                        (map (partial parse-ls-dir state))
                        seq)]
    (assoc-in state [:dirs (:cwd state)]
              {:dirs       dirs
               :files      files
               :files-size (or files-size 0)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dispatch

(defn handle-command
  [state cmd]
  (cond
    (re-seq #"^cd" cmd)
    (handle-cd state cmd)

    (re-seq #"^ls" cmd)
    (handle-ls state cmd)))

(comment
  (->>
    (input "example.txt")
    (reduce handle-command {:cwd  ""
                            :dirs {}})))

(defn handle-commands [f]
  (->>
    (input f)
    (reduce handle-command {:cwd "" :dirs {}})))

(defn calc-dir-size [state-dirs dir-path]
  (let [{:keys [files-size dirs]} (get state-dirs dir-path)
        nested-size               (if (seq dirs)
                                    (->> dirs
                                         (map :dir)
                                         (map (partial calc-dir-size state-dirs))
                                         (reduce +))
                                    0)]
    (println
      "calc-dir-size" dir-path
      "files-size" files-size
      "nested-size" nested-size
      "dirs" dirs)
    (+ (or files-size 0) (or nested-size 0))))

(defn set-dir-sizes [state]
  (reduce
    (fn [state-dirs dir-path]
      (println dir-path "....calcing....")
      (let [dir-size (calc-dir-size state-dirs dir-path)]
        (println dir-path dir-size)
        (println "\n")
        (assoc-in state-dirs [dir-path :dir-size] dir-size)))
    (:dirs state)
    (->> state :dirs keys)))

(defn part1 [f]
  (->>
    (handle-commands f)
    set-dir-sizes
    (filter (fn [[_path {:keys [dir-size]}]] (> 100000 dir-size)))
    (map (comp :dir-size second))
    (reduce +)))

(comment
  (part1 "example.txt")
  (part1 "input.txt")
  *1
  )

(defn used-space [dirs]
  #_(-> dirs (get "/") :dir-size)
  (->> dirs
       vals
       (tree-seq (constantly true) :files)
       (mapcat identity)
       (mapcat :files)
       (map :size)
       (reduce +)))

(defn unused-space [dirs]
  (- 70000000 (used-space dirs)))

(def required-space 30000000)

(defn part2 [f]
  (let [dir-sizes    (-> f handle-commands set-dir-sizes)
        unused       (unused-space dir-sizes)
        required     (- required-space unused)
        _            (println "unused" unused)
        _            (println "required" required)
        large-enough (->> dir-sizes
                          (filter (comp (fn [size]
                                          (>= size required))
                                        :dir-size second)))]
    (println "ct lg enf" (count large-enough))
    (->> large-enough
         (sort-by (comp :dir-size second) <)
         first)))

(comment
  (part2 "example.txt")
  (part2 "input.txt")
  ;; 4415 too low
  ;; 4420059 too low
  (->>
    "input.txt"
    handle-commands
    set-dir-sizes
    #_unused-space)

  (get *1 "/")

  (- 70000000 48381165)
  (- 70000000 44274331)

  44274331

  (-> "input.txt"
      handle-commands
      set-dir-sizes
      unused-space)

  ;; all files
  (->>
    "input.txt"
    handle-commands
    :dirs
    vals
    (tree-seq (constantly true) :files)
    (mapcat identity)
    (mapcat :files))

  (->>
    "input.txt"
    handle-commands
    :dirs
    vals
    (tree-seq (constantly true) :files)
    (mapcat identity)
    (mapcat :files)
    (map :size)
    (reduce +)
    )
  44274331 ;; all files


  )
