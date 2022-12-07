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
            (cond (#{".."} dir)
                  (fn [cwd]
                    (string/replace cwd (-> (re-seq #"\/([a-z]*\/)$" cwd) first second) ""))

                  :else
                  (fn [cwd]
                    (->>
                      ;; TODO compress/handle ..
                      (str cwd (when-not (#{"/"} dir) dir) "/")))))))

(comment

  (re-seq #"\/([a-z]*\/)$" "/a/b/c/d/e/")

  (handle-cd {:cwd ""} "cd /")
  (handle-cd {:cwd ""} "cd a")
  (handle-cd {:cwd "a/"} "cd b")
  (handle-cd {:cwd "a/b/"} "cd b")
  (handle-cd {:cwd "a/b/"} "cd ..")
  (handle-cd {:cwd "aaaa/bbb/ccc/baadfsd/"} "cd ..")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ls

;; parse file

(defn parse-ls-file [ls-file]
  (let [[size name] (-> (re-seq #"^(\d+) (.*)$" ls-file)
                        first rest)]
    {:size     size
     :filename name}))

(comment (parse-ls-file "436456 s.log"))

;; parse dir

(defn parse-ls-dir [state ls-file]
  (let [dir (-> (re-seq #"^dir (.*)$" ls-file)
                first last)]
    {:dir (str (:cwd state) dir "/")}))

(defn handle-ls [state cmd]
  (let [contents   (-> cmd string/split-lines rest)
        files      (->> contents (filter #(re-seq #"^\d" %)) (map parse-ls-file))
        files-size (->> files (map (comp read-string :size)) (reduce +))
        dirs       (->> contents
                        (filter #(re-seq #"^dir" %))
                        (map (partial parse-ls-dir state)))]
    (update-in state [:dirs (:cwd state)]
               (fn [_] {:files      files
                        :dirs       dirs
                        :files-size files-size}))))

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

(defn set-dir-files-sizes [state-dirs dir-path]
  (let [{:keys [file-size dirs]} (get state-dirs dir-path)
        dir-files-size
        (+ file-size (->> dirs
                          (map :dir)
                          (map (partial set-dir-files-sizes state-dirs))
                          (map :dir-files-size)
                          (reduce +)))]
    (update state-dirs dir-path :dir-files-size dir-files-size)))

(defn handle-commands [f]
  (->>
    (input f)
    (reduce handle-command {:cwd "" :dirs {}})
    ))

(comment
  (let [state (handle-commands "example.txt")]
    (reduce set-dir-files-sizes (:dirs state) (->> state :dirs keys)))

  (handle-commands "input.txt"))
