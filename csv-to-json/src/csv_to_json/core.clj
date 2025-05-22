(ns csv-to-json.core
  "Reads data from a CSV file, uses cleaner to clean it up and
  uses writer to write three JSON files."
  (:gen-class)
  (:require
   [csv-to-json.cleaner :as cleaner]
   [csv-to-json.writer :as writer]
   [clojure.data.csv :as csv]
   [clojure.java.io :as jio]))


;; Read data from CSV

(defn empty-string-to-nil
  "Turns empty strings to nil, non-empty are unchanged."
  [s]
  (when-not (and (string? s) (empty? s)) s))

(defn load-csv
  "Returns a data structure loaded from a CSV file at filepath."
  [filepath]
  (with-open [reader (jio/reader filepath)]
    (->> (csv/read-csv reader)
         (map (fn [row] (map empty-string-to-nil row)))
         (doall))))


;; Check that CSV file exists and isn't empty

(defn error-from-empty
  "Checks if file is empty."
  [data]
  (if (empty? data)
    (println "Error: File is empty")
    data))

(defn try-load-csv
  "Tries to load CSV. If file doesn't exist or is empty, prints error message."
  [filepath]
  (if (.exists (jio/as-file filepath))
    (error-from-empty (load-csv filepath))
    (println "Error: Could not find file")))


;; Convert CSV data vector of vectors to vector of maps with correct keys
;; Change status to int: "completed" = 2, "failed" = 1, "inprogress" = 0;

(def titles-and-keys
  {"Etunimi" :first_name
   "Sukunimi" :last_name
   "E-mail" :email
   "Kurssin nimi" :course_name
   "Kurssi alkaa" :start_date
   "Kurssi päättyy" :end_date
   "Status" :status
   "Arvosana" :grade
   "Kurssin suorituspäivämäärä" :date})

(def status-int
  {"completed" 2
   "failed" 1
   "inprogress" 0})

(defn status-from-text
  "Changes status to number."
  [status]
  (get status-int status))

(defn set-status
  "Changes status to number."
  [row]
  (update row :status status-from-text))

(defn vectors-to-maps
  "Converts a vector of vectors into a vector of maps. Sets status as integer."
  [[header & body]]
  (let [header-repeated (repeat (replace titles-and-keys header))]
    (->> body
         (mapv zipmap header-repeated)
         (map set-status))))

;; Main

(defn -main
  "Loads CSV and writes JSONs of the data."
  [& args]
  (println (writer/write-jsons (cleaner/clean-data (vectors-to-maps (try-load-csv (first args)))))))