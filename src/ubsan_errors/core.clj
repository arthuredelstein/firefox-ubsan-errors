(ns ubsan-errors.core
  (:require [clojure.java.io :as io]))

(def ^:dynamic *results* "./logs/")

(defn fmap
  "Map function f over values of map m."
  [m f]
  (into {} (for [[k v] m] [k (f v)])))

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
     (subvec 1 3))
    (catch Exception e
      ;; A few runtime errors report no source line, oddly.
      (vector
       nil
       (second (re-find #"runtime error: (.*?$)" line))))))

(defn runtime-errors
  "Extract all runtime errors in all the log files."
  []
  (->> (list-files *results*)
       (mapcat file-line-seq)
       (select-runtime-error-lines)
       (map extract-runtime-error)))

(defn print-summary
  "Print a table of rows with
   [runtime error location, count, typical error message]"
  [runtime-errs]
  (let [;; group by file and line number
        resf (group-by first runtime-errs)
        ;; clean up redundant data
        resfs (fmap resf #(map second %))
        ;; produce [file:line:col [number representative-err-msg]]
        resfsr (for [[line mesgs] resfs]
                 [line (count mesgs) (first mesgs)])]
    (doseq [[line c m] (reverse (sort-by second resfsr))]
      (let [line2 (when line
                    (.replace line
                              "/home/worker/workspace/build/src/" " "))]
        (println line2 "\t" c "\t" m)))))

(defn save-summary
  "Save a table of rows with
  [runtime error location, count typical error message]"
  [runtime-errs]
  (spit "firefox-ubsan-error-summary.txt"
        (with-out-str
          (print-summary runtime-errs))))
