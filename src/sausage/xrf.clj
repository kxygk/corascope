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
     :element-counts data}))


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
        data (:element-counts xrf-scan)]

    (clojure.data.csv/write-csv (clojure.java.io/writer (str directory "/" file-name))
                                (into (merge header (into [] (map name columns)))
                                      (into [] (map #(build-csv-row columns %) data)))
                                :separator \tab)))

(defn start-position
  "End position of the scan - in (mm)"
  [xrf-scan]
  (-> xrf-scan
      :element-counts
      first
      :position
      read-string))

(defn end-position
  "End position of the scan - in (mm)"
  [xrf-scan]
  (-> xrf-scan
      :element-counts
      last
      :position
      read-string))

(defn shift-string-number ;; TODO Make this unnecessary..
  [shift
   string-number]
  (str (- (read-string string-number)
          shift)))

(defn crop
  [xrf-scan-element-counts ;; TODO: Rewrite so it operates on 'xrf-scan'
   crop-left-mm
   crop-right-mm] ;; TODO rewrite as threaded ->
  (let [end-mm (- (-> xrf-scan-element-counts last :position read-string)
                  crop-right-mm)]
    (filter #(< 0
                (read-string (:position %)))
            (map #(update % :position (partial shift-string-number crop-left-mm))
                 (filter #(>= end-mm
                             (read-string (:position %)))
                         xrf-scan-element-counts)
                 )
            )
  )
  )


(partial shift-string-number 100)
