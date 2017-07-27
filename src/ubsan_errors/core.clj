(ns ubsan-errors.core
  (:require [clojure.java.io :as io])
  (:import [java.io File]))

(def ^:dynamic *results* "../download_taskcluster_logs/results/")

(defn file-line-seq [path]
  (line-seq (io/reader path)))

(defn log-files [path]
  (->> path
       io/as-file
       .listFiles
       seq))

(defn select-runtime-error-lines [lines]
  (filter #(.contains % "runtime error:") lines))

(defn extract-runtime-error [line]
  (try
    (->
     (re-find #"(\S+): runtime error: (.*?$)" line)
     (subvec 1))
    (catch Exception e
      (println "failed to extract: " line)
      nil)))

(defn runtime-errors []
  (->> (log-files *results*)
       (mapcat file-line-seq)
       (select-runtime-error-lines)
       (map extract-runtime-error)
       (remove nil?)))
