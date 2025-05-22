(ns csv-to-json.core
  (:gen-class)
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as jio]
   [clojure.data.json :as json]))


;; Read data from CSV

(defn empty-string-to-nil
  "Turns empty strings to nil, non-empty are unchanged."
  [s]
  (when-not (and (string? s) (empty? s)) s))

(defn load-csv
  "Returns a data structure loaded from a CSV file at FILEPATH."
  [filepath]
  (with-open [reader (jio/reader filepath)]
    (->> (csv/read-csv reader)
         (map (fn [row] (map empty-string-to-nil row)))
         (doall))))


;; Check that CSV file exists and isn't empty

(defn error-from-empty
  "Checks if CSV is empty."
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
  [[header & body]]
  (let [header-repeated (repeat (map key-from-title header))]
    (->> body
         (mapv zipmap header-repeated)
         (map set-status))))


;; Clean up data before converting to JSON

;; 1. Remove rows where all required information is not present

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

;; 3. Change status to "failed" when course completion date 
;;    is outside course start and end dates

(defn status-to-fail
  "Changes status to 'failed' (1)."
  [row]
  (assoc row :status 1))

(defn outside-completion-to-fail
  "Checks if course completion date is outside course dates, 
   if not changes status to 'failed'."
  [row]
  (if (nil? (get row :date))
    row
    (if (and (>= 0 (compare (get row :start_date) (get row :date))) (>= 0 (compare (get row :date) (get row :end_date))))
      row
      (status-to-fail row))))

(defn fail-courses-outside-dates
  "Removes grades from rows not marked 'completed'."
  [data]
  (map outside-completion-to-fail data))

;; 4. Remove grade if course status is not "completed"

(defn grade-to-nil
  "Changes grade to nil."
  [row]
  (assoc row :grade nil))

(defn incomplete-to-nil-grade
  "Checks if status is 'completed', and if not changes grade to nil."
  [row]
  (if (= 2 (get row :status))
    row
    (grade-to-nil row)))

(defn remove-incomplete-grades
  "Removes grades from rows not marked 'completed'."
  [data]
  (map incomplete-to-nil-grade data))

;;

(defn clean-data
  "Cleans up data per above instructions."
  [data]
  (->> data
       (remove-missing-required)
       (remove-duplicates)
       (fail-courses-outside-dates)
       (remove-incomplete-grades)))


;; Write to JSON

;; write-json-to-file {:key1 "val1" :key2 "val2"} "output/testi1.json"
(defn write-json-to-file
  "Writes given map DATA to json at FILEPATH."
  [data filepath]
  (with-open [wrtr (jio/writer filepath)]
    (.write wrtr (json/write-str (into [] data)))))

;; 1. courses.json: 
;;[{
;;  "name": <kurssin nimi>,
;;  "start_date": <kurssin alkamispäivämäärä>,
;;  "end_date": <kurssin päättymispäivämäärä>,
;;  "results": {
;;    "completed": <suoritettujen lukumäärä>,
;;    "failed": <hylättyjen lukumäärä>,
;;    "inprogress": <keskeneräisten suoritusten lukumäärä>
;;  },
;;  "grades": {
;;     1: <arvosanan 1 suorittaneiden lukumäärä>,
;;     2: <arvosanan 2 suorittaneiden lukumäärä>,
;;     3: <arvosanan 3 suorittaneiden lukumäärä>,
;;     4: <arvosanan 4 suorittaneiden lukumäärä>,
;;     5: <arvosanan 5 suorittaneiden lukumäärä>
;;  },
;;  “first_completion_date”: <ensimmäisen suorituksen päivämäärä>,
;;  “most_recent_completion_date”: <viimeisimmän suorituksen päivämäärä>
;;}]

(defn courses-data
  "Collects data for courses.json. TODO"
  [row]
  (identity {:name (get row :course_name)
     :start_date (get row :start_date)
     :end_date (get row :end_date)}))

(defn write-courses
  "Writes courses.json"
  [data]
  (write-json-to-file (map courses-data data) "output/courses.json"))

(defn write-jsons
  "Writes three JSON files."
  [data]
  (write-courses data))

;;

(defn -main
  "Loads CSV and writes JSONs of the data."
  [& args]
  (println (vectors-to-maps (try-load-csv (first args)))))
  ;(println (write-jsons (clean-data (vectors-to-maps (try-load-csv (first args)))))))