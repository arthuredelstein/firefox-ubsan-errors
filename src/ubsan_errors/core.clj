(ns ubsan-errors.core
  (:require [clojure.java.io :as io])
  (:import [java.io File]))

(def ^:dynamic *results* "../download_taskcluster_logs/results/")

(defn file-line-seq
  "Returns a seq of the lines in the file at path."
  [path]
  (line-seq (io/reader path)))

(defn list-files
  "Get the list of files found in directory at path."
  [path]
  (->> path
       io/as-file
       .listFiles
       seq))

(defn select-runtime-error-lines
  "Select lines with the string 'runtime error:'."
  [lines]
  (filter #(.contains % "runtime error:") lines))

(defn extract-runtime-error
  "For a 'runtime error:' line, extract ['file:row:col' 'error message']."
  [line]
  (try
    (->
     (re-find #"(\S+): runtime error: (.*?$)" line)
     (subvec 1))
    (catch Exception e
      (println line)
      ;; A few runtime errors report no source line, oddly.
      (->
       (re-find #"runtime error: (.*?$)" line)
       (assoc 0 nil)))))

(defn runtime-errors
  "Extract all runtime errors in all the log files."
  []
  (->> (list-files *results*)
       (mapcat file-line-seq)
       (select-runtime-error-lines)
       (map extract-runtime-error)))
