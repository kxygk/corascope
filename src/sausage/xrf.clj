(ns sausage.xrf
    (:require
     [clojure.data.csv]
     [clojure.java.io]))

(defn load-xrf-scan-file
  [csv-file]  
  (let [full-csv-table (-> csv-file
                           (.getCanonicalPath)
                           (clojure.java.io/reader)
                           (clojure.data.csv/read-csv :separator \tab))
        header (into [] (take 2 full-csv-table))
        columns (map #(-> %
                          (clojure.string/split #" ")
                          (first)
                          keyword)
                     (first (drop 2 full-csv-table)))
        count-table (drop 3 full-csv-table)
        data (map #(zipmap columns %) count-table)]
    {:header header
     :columns columns
     :data data}))


(defn build-csv-row
  "take one measurement data and shove it into a vector or string - for CSV exports"
  [columns
   measurement-data]
  (into [] (map #(% measurement-data) columns)))

(defn save-xrf-scan
  ""
  [directory
   xrf-scan
   file-name]
  (let [header (:header xrf-scan)
        columns (:columns xrf-scan)
        data (:data xrf-scan)]

    (clojure.data.csv/write-csv (clojure.java.io/writer (str directory "/" file-name))
                                (into (merge header (into [] (map name columns)))
                                      (into [] (map #(build-csv-row columns %) data)))
                                :separator \tab)))

(defn start-position
  "End position of the scan - in (mm)"
  [xrf-scan]
  (-> xrf-scan
      :data
      first
      :position
      read-string))

(defn end-position
  "End position of the scan - in (mm)"
  [xrf-scan]
  (-> xrf-scan
      :data
      last
      :position
      read-string))
