(ns csv-to-json.core
  (:gen-class)
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as jio]
   [clojure.data.json :as json]))

;;

(defn dissoc-nils
  "Drops keys with nil values, or nil keys, from the hashmap H. TODO"
  [h]
  (into {} (filter (fn [[k v]] (and v k)) h)))


;; Read data from CSV

(defn empty-string-to-nil
  "Turns empty strings to nil, non-empty are unchanged."
  [s]
  (if (and (string? s) (empty? s))
    nil
    s))

(defn load-csv
  "Returns a data structure loaded from a CSV file at FILEPATH."
  [filepath]
  (with-open [reader (jio/reader filepath)]
    (->> (csv/read-csv reader)
         (map (fn [row] (map empty-string-to-nil row)))
         (doall))))


;; Check that CSV file exists and isn't empty

(defn error-from-empty-csv
  "Checks if CSV is empty. TODO put this later"
  [data]
  (if (empty? data)
    (println "Virhe tyhjä")
    data))

(defn try-load-csv
  "Tries to load CSV. If file doesn't exist or is empty, prints error message."
  [filepath]
  (if (.exists (jio/as-file filepath))
    (error-from-empty-csv (load-csv filepath))
    (println "Virhe")))


;; Convert CSV data vector of vectors to vector of maps with correct keys
;; Change status to number: "completed" = 2, "failed" = 1, "inprogress" = 0;

(defn key-from-title
  "Returns correct key for the given TITLE of a column."
  [title]
  (case title
    "Etunimi" :first_name
    "Sukunimi" :last_name
    "E-mail" :email
    "Kurssin nimi" :course_name
    "Kurssi alkaa" :start_date
    "Kurssi päättyy" :end_date
    "Status" :status
    "Arvosana" :grade
    "Kurssin suorituspäivämäärä" :date
    :default))

(defn status-from-text
  "Changes status to number."
  [status]
  (case status
    "completed" 2
    "failed" 1
    "inprogress" 0
    -1))

(defn set-status
  "Changes status to number."
  [row]
  (update row :status status-from-text))

(defn vectors-to-maps
  "Converts a vector of vectors into a vector of maps. Sets status as integer."
  [v]
  (let [header (first v)]
    (map set-status (mapv zipmap (repeat (map key-from-title header)) (rest v)))))


;; Clean up data before converting to JSON

;; 1. Remove rows where all required information is not present
;; 3. Change grade to "failed" when course completion date 
;;    is outside course start and end dates
;; 4. Remove grade if course status is not "completed"

(defn required-info?
  "Checks if key of PAIR is required info."
  [pair]
  (contains? #{:first_name :laste_name :email :course_name :start_date :end_date :status} (key pair)))

(defn valid-row?
  "Checks if all required information is present."
  [row]
  (not-any? nil? (vals (filter required-info? row))))

(defn remove-missing-required
  "Removes rows missing required information."
  [data]
  (filter valid-row? data))

;; 2. Remove duplicate course+student combinations

(defn email-and-course-name
  "Creates a map of email and course name."
  [row]
  (select-keys row [:email :course_name]))

(defn group-by-student-course
  "Groups rows by student email and course name."
  [row]
  (group-by email-and-course-name row))

(defn desc
  [a b]
  (compare b a))

(defn sort-group
  "Sorts group of rows so that the latest completed, 
   or if one doesn't exist the latest failed, attempt is first."
  [group]
  (->> group
   (sort-by :date desc)
   (sort-by :status desc)))

(defn sort-row-groups
  "Creates a vector of vectors, each of which includes one group of duplicates,
   ordered by status and date."
  [data]
  (map sort-group (vals (group-by-student-course data))))

(defn remove-duplicates
  "Removes duplicate rows."
  [data]
  (map first (sort-row-groups data)))


(defn clean-data
  "Cleans up data per above instructions."
  [data]
  (remove-duplicates(remove-missing-required data)))


;; Write to JSON

;; write-json {:key1 "val1" :key2 "val2"} "output/testi1.json"
(defn write-json
  "Writes given map DATA to json at FILEPATH."
  [data, filepath]
  (with-open [wrtr (jio/writer filepath)]
    (.write wrtr (json/write-str data))))


;;

(defn -main
  "Loads CSV and writes JSONs of the data."
  [& args]
  (println (clean-data (vectors-to-maps (try-load-csv (first args))))))