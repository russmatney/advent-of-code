(ns tasks
  (:require [babashka.curl :as curl]
            [babashka.pods :as pods]
            [babashka.fs :as fs]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; badge generation
;; originally based on:
;; https://github.com/genmeblog/advent-of-code/blob/master/badges/badges.bb
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pods/load-pod 'retrogradeorbit/bootleg "0.1.9")

(require '[pod.retrogradeorbit.bootleg.utils :as bootleg]
         '[pod.retrogradeorbit.hickory.select :as s])

(defn cookie []
  (when-not (fs/exists? "resources/.session")
    (fs/create-file "resources/.session"))
  (str/trim (slurp "resources/.session")))

(defn get-stars
  "Return a string representing number of stars earned for a given `year`"
  [year]
  (let [parsed (-> (str "https://adventofcode.com/" year)
                   (curl/get {:headers {"Cookie" (str "session=" (cookie))}})
                   :body
                   (bootleg/convert-to :hickory))]
    (-> (s/select (s/class "star-count") parsed)
        first
        :content
        first
        (or "0*"))))

(comment
  (get-stars "2022")

  (-> (str "https://adventofcode.com/" "2022")
      (curl/get {:headers {"Cookie" (str "session=" (cookie))}})
      :body
      (bootleg/convert-to :hickory))

  )

(defn badge-style []
  {"color"      "009900" ;; right side
   "labelColor" "0f0f23" ;; left side
   "style"      "flat"
   "logo"       (str "data:image/png;base64," (slurp "badges/aoc-favicon-base64"))})

(defn make-badge [year stars]
  (let [params (merge {"label" year "message" stars} (badge-style))]
    (:body (curl/get "http://img.shields.io/static/v1" {:query-params params}))))

(defn save-badge! [year]
  (let [path  (str "badges/" year ".svg")
        stars (get-stars year)
        badge (make-badge year stars)]
    (spit path badge)))

(def yrs ["2022" "2021" "2020"
          ;; "2019" "2018" "2017" "2016" "2015"
          ])

(defn gen-badges []
  (run! save-badge! yrs))


(comment
  (gen-badges))
