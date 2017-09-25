(ns ubsan-errors.logs
  (:require [clojure.data.json :as json]
            [clj-http.client :as http]))

(defn task-group-url
  "A url for listing the task group. Optional
   continuation token (otherwise provide nil)."
  [task-group-id continuation-token]
  (str "https://queue.taskcluster.net/v1/task-group/"
       task-group-id "/list"
       (if continuation-token
         (str "?continuationToken=" continuation-token)
         "")))

(defn fetch-tasks
  "Fetch all tasks from the task group."
  [task-group-id]
  (loop [continuation-token nil tasks '()]
    (let [url (task-group-url task-group-id continuation-token)
          raw (:body (http/get url))
          data (json/read-str raw :key-fn keyword)
          tasks+ (concat tasks (data :tasks))
          new-continuation-token (data :continuationToken)]
      (if new-continuation-token
        (recur new-continuation-token tasks+)
        tasks+))))

(defn task-log-url
  "A URL for downloading the log for a given task with
   a 'run' index."
  [task-id run]
  (str "https://public-artifacts.taskcluster.net/"
       task-id "/" run
       "/public/logs/live_backing.log"))

(defn task-log-url2 [task-id run]
  "A second possible URL for downloading the log for
   the given task with a 'run' index."
  (str "https://queue.taskcluster.net/v1/task/" task-id
       "/runs/" run
       "/artifacts/public/logs/live_backing.log.gz"))

(defn get-body
  "Attempt to download contents at a URL and
   return nil if it fails."
  [url]
  (try
    (:body (http/get url))
    (catch Exception e nil)))

(defn download-task-log!
  "Download the log file for a single task."
  [task]
  (let [task-status (task :status)
        task-state (task-status :state)
        task-id (task-status :taskId)
        runs-count (count (task-status :runs))
        last-run (dec runs-count)]
    (println task-id task-state)
    (if (#{"exception" "unscheduled"} task-state)
      (println task-state last-run)
      (let [body (or (get-body (task-log-url task-id last-run))
                     (get-body (task-log-url2 task-id last-run)))]
        (when (nil? body)
          (throw (Exception. "download failed.")))
        (spit (str "./logs/" task-id) body)))))

(defn download-task-logs!
  "Download the log files for all tasks in the tasks group."
  [task-group-id]
  (clojure.java.io/make-parents "./logs/any-file.txt")
  (dorun (map download-task-log! (fetch-tasks task-group-id))))

