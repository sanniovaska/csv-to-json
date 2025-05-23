(ns csv-to-json.cleaner
  "Contains functions to remove and change data so it is valid.")

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

(defn group-by-student-course
  "Group data by student email and course name."
  [data]
  (group-by (juxt :email :course_name) data))

(defn desc
  [a b]
  (compare b a))

(defn sort-group
  "Sorts group of rows so that the latest completed, 
   or if one doesn't exist the latest failed, attempt is first."
  [group]
  (sort-by (juxt :status :date) desc group))

(defn sort-row-groups
  "Creates a vector of vectors, each of which includes one group of duplicates,
   ordered by status and date."
  [data]
  (map sort-group (vals (group-by-student-course data))))

(defn remove-duplicates
  "Removes duplicate rows."
  [data]
  (map first (sort-row-groups data)))

;; 3. Change status to "failed" and date to nil when course completion date 
;;    is outside course start and end dates.

(defn status-to-fail
  "Changes status to 'failed' (1)."
  [row]
  (assoc row :status 1))

(defn date-to-nil
  "Changes date to nil."
  [row]
  (assoc row :date nil))

(defn earlier?
  [a b]
  (>= 0 (compare a b)))

(defn outside-completion-to-fail
  "Checks if course completion date is outside course dates, 
   if not changes status to 'failed'."
  [row]
  (let [date (get row :date)
        start-date (get row :start_date)
        end-date (get row :end_date)]
    (if (nil? date)
    row
    (if (and (earlier? start-date date) (earlier? date end-date) ())
      row
      (date-to-nil (status-to-fail row))))))

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

;; Main cleaner

(defn clean-data
  "Cleans up data per above instructions."
  [data]
  (->> data
       (remove-missing-required)
       (remove-duplicates)
       (fail-courses-outside-dates)
       (remove-incomplete-grades)))