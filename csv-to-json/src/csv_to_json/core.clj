(ns csv-to-json.core
  (:gen-class)
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as jio]
   [clojure.data.json :as json]))

;; Write to JSON

;; write-json {:key1 "val1" :key2 "val2"} "output/testi1.json"
(defn write-json
  "Writes given map DATA to json at FILEPATH."
  [data, filepath]
  (with-open [wrtr (jio/writer filepath)]
    (.write wrtr (json/write-str data))))


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


;; Main

(defn -main
  "Loads CSV and prints JSONs of the data."
  [& args]
  (println (try-load-csv (first args))))