(ns csv-to-json.core
  (:gen-class)
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as jio]))

(defn empty-string-to-nil
  "Turns empty strings to nil, non-empty are unchanged."
  [s]
  (if (and (string? s) (empty? s))
    nil
    s))

(defn dissoc-nils
  "Drops keys with nil values, or nil keys, from the hashmap H. TODO"
  [h]
  (into {} (filter (fn [[k v]] (and v k)) h)))


(defn load-csv
  "Returns a data structure loaded from a CSV file at FILEPATH."
  [filepath]
  (with-open [reader (jio/reader filepath)]
    (->> (csv/read-csv reader)
         (map (fn [row] (map empty-string-to-nil row)))
         (doall))))

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

(defn -main
  "Loads CSV and prints JSONs of the data."
  [& args]
  (println (try-load-csv (first args))))