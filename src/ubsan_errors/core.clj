(ns ubsan-errors.core
  (:require [clojure.java.io :as io]
            [ubsan-errors.logs :as logs]))

(defn list-files
  "Get the list of files found in directory at path."
  [path]
  (->> path
       io/as-file
       .listFiles
       seq))

(defn file-line-seq
  "Returns a seq of the lines in the file at path."
  [path]
  (line-seq (io/reader path)))

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
  "Extract all runtime errors in all the log files. Returns
   a map with entries [location [error-message count]]."
  [dir]
  (->> (list-files dir)
       (mapcat file-line-seq)
       (select-runtime-error-lines)
       (map extract-runtime-error)
       (reduce
        (fn [m [loc err]]
          (update-in m [loc]
                     (fn [[e ct]]
                       (if ct
                         [e (inc ct)]
                         [err 1]))))
        (sorted-map))))

(defn print-summary
  "Print a table of rows with
   [runtime error location, count, typical error message]"
  [runtime-err-map]
  (doseq [[line [m c]] runtime-err-map]
    (let [line2
          (when line
            (.replace
             line
             "/builds/worker/workspace/build/src/" " "))]
      (println line2 "\t" c "\t" m))))

(defn save-summary
  "Save a table of rows with
  [runtime error location, count typical error message]"
  [filename runtime-errs]
  (spit filename
        (with-out-str
          (print-summary runtime-errs)))
  (println "Wrote" filename))

(defn -main
  "The main program. Pass the task-group-id."
  [task-group-id]
  (let [dir (logs/download-task-logs! task-group-id)]
    (save-summary
     (str "firefox-ubsan-errors-" task-group-id ".txt")
     (runtime-errors dir))))
