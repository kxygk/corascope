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
    (assert (= (count columns)
               (count (set columns)))
            "BAD NEWS: Your XRF scan has non-unique columns names. This isn't supported!")
    {:header header
     :columns (set columns)
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
  (str (+ (read-string string-number)
          shift)))

(defn shift-scan-point
  [shift
   scan-point]
  (update scan-point :position (partial shift-string-number shift)))

(defn crop
  [xrf-scan-element-counts ;; TODO: Rewrite so it operates on 'xrf-scan'
   length-mm
   crop-left-mm
   crop-right-mm] ;; TODO rewrite as threaded ->
  (filter #(< 0
              (read-string (:position %)))
          (map #(shift-scan-point (- crop-left-mm)
                                  %)
               (filter #(>= (- length-mm
                               crop-right-mm)
                            (read-string (:position %)))
                       xrf-scan-element-counts))))


(defn join-horizontally
  ""
  [xrfA
   xrfB-start-mm
   xrfB]
  (let [element-countsA (:element-counts xrfA)
        shifted-element-countsB (map (partial shift-scan-point xrfB-start-mm)
                                     (:element-counts xrfB))
        merged-counts (concat element-countsA
                              shifted-element-countsB)]
    (assoc xrfA
           :element-counts
           merged-counts)))
