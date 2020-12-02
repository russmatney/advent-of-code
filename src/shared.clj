(ns shared
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defmacro input [fname]
  `(-> *file*
       io/file
       .getParent
       (str "/" ~fname)
       slurp
       string/split-lines))
