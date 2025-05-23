(ns csv-to-json.writer
  "Contains functions to change the data so it can be written to JSON."
  (:require 
   [clojure.java.io :as jio]
   [clojure.data.json :as json]))

;; Write to JSON

(defn write-json-to-file
  "Writes given map data to json at filepath."
  [data filepath]
  (with-open [wrtr (jio/writer filepath)]
    (.write wrtr (json/write-str (into [] data)))))

;; Counting helpers

(defn count-if
  "Counts how many pass pred."
  [pred? coll]
  (count (filter pred? coll)))

(defn count-amount
  "Counts how many in data have value at key."
  [data key value]
  (count-if (fn [c] (= (get c key) value)) data))

(defn count-results
  "Counts how many of each result (status) has been given."
  [rows json]
  (conj json {"results" {"completed" (count-amount rows :status 2)
                         "failed" (count-amount rows :status 1)
                         "inprogress"  (count-amount rows :status 0)}}))

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

(defn count-grades
  "Counts how many of each grade has been given."
  [course json]
  (conj json {"grades" {1 (count-amount course :grade "1")
                        2 (count-amount course :grade "2")
                        3 (count-amount course :grade "3")
                        4 (count-amount course :grade "4")
                        5 (count-amount course :grade "5")}}))

(defn filter-nil-dates
  "Filter out rows with nil completion dates"
  [course]
  (filter (fn [c] (some? (get c :date))) course))

(defn completion-dates
  "Adds first and last completion date."
  [course json]
  (let [course-by-date (sort-by :date (filter-nil-dates course))
        first-date (get (first course-by-date) :date)
        last-date (get (last course-by-date) :date)]
    (conj json {"first_completion_date" first-date} {"last_completion_date" last-date})))

(defn course-info
  "Adds name, start date and end date.
   Calls to add results, grades and completion dates."
  [course]
  (let [basic-info {:name (get (first course) :course_name)
                    :start_date (get (first course) :start_date)
                    :end_date (get (first course) :end_date)}]
    (->> basic-info
         (count-results course)
         (count-grades course)
         (completion-dates course))))

(defn courses-info
  "Collects info for each course."
  [data]
  (map course-info (vals data)))

(defn get-courses-info
  "Groups by course name and gets info for courses.json."
  [data]
  (->> data
       (group-by :course_name)
       (courses-info)))

(defn write-courses
  "Writes courses.json"
  [data]
  (write-json-to-file (get-courses-info data) "output/courses.json"))


;; 2. users.json:
;;[{
;;  "first_name": <opiskelijan etunimi>,
;;  "last_name": <opiskelijan etunimi>,
;;  "email": <email>,
;;  "course_results": {
;;    "completed": <käyttäjän suorittamien kurssien lukumäärä>,
;;    "failed": <hylättyjen kurssien lukumäärä>,
;;    "inprogress": <keskeneräisten suoritusten lukumäärä>
;;  },
;;  "grade_avg": <arvosanojen keskiarvo suoritetuilta kursseilta, mikäli kursseja on suoritettu>
;;}]

(defn grades
  "Creates a list of non-nil grades obtained by the user."
  [user]
  (->> user
       (map (fn [c] (get c :grade)))
       (filter some?)
       (map read-string)))

(defn grade-avg
  "Counts grade average for the user."
  [user json]
  (let [grades (grades user)]
    (if (empty? grades)
      (conj json {"grade_avg" nil})
      (conj json {"grade_avg" (/ (apply + grades) (count grades))}))))

(defn user-info
  "Adds name, start date and end date.
   Calls to add results, grades and completion dates."
  [user]
  (let [basic-info {:first_name (get (first user) :first_name)
                    :laste_name (get (first user) :last_name)
                    :email (get (first user) :email)}]
    (->> basic-info
         (grade-avg user)
         (count-results user))))

(defn users-info
  "Collects info for each user."
  [data]
  (map user-info (vals data)))

(defn get-users-info
  "Groups by course name and gets info for users.json."
  [data]
  (->> data
       (group-by :email)
       (users-info)))

(defn write-users
  "Writes users.json"
  [data]
  (write-json-to-file (get-users-info data) "output/users.json"))


;; 3. results.json:
;;[{
;;  "course_name": <kurssin nimi>,
;;  "email": <oppijan email>,
;;  "status": <suoritukset status>,
;;  "grade": <suorituksen arvosana>,
;;  "date": <suorituspäivämäärä ISO 8601 -muodossa>
;;}]

(defn status-from-int
  "Gets string value from status int."
  [status]
  (get {2 "completed"
        1 "failed"
        0 "inprogress"} status))

(defn set-string-status
  "Changes status back to string."
  [row]
  (update row :status status-from-int))

(defn grade-to-int
  "Changes grade from string to int."
  [row]
  (let [grade (get row :grade)]
    (if (nil? grade)
      row
      (assoc row :grade (read-string grade)))))

(defn filter-result
  "Keeps only the keys needed for results.json."
  [result]
  (select-keys result [:course_name :email :status :grade :date]))

(defn result-info
  "Filters unneeded info from results."
  [data]
  (->> data
       (map filter-result)
       (map set-string-status)
       (map grade-to-int)))

(defn write-results
  "Writes results.json"
  [data]
  (write-json-to-file (result-info data) "output/results.json"))

;; Main writer

(defn write-jsons
  "Writes three JSON files."
  [data]
  (or (write-courses data) (write-users data) (write-results data)))