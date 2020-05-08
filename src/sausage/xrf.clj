(ns sausage.xrf
  (:require
   [clojure.data.csv]
   [clojure.java.io]
   [sausage.state :as state] ;; base XRF subscriptions are here b/c of `length-mm`
   [cljfx.api :as fx])
  (:import javafx.stage.FileChooser
           javafx.stage.Stage))

(defn element-counts
  "Given an XRF-SCAN and a ELEMENT (keyword)
  RETURN:
  [[position element-count]
   [position element-count]
   ...
   [position element-count]]"
  [context
   core-number
   element]
  (map #(vector (read-string (:position-mm %))
                (read-string (element %)))
       (fx/sub context
               state/xrf-element-counts
               core-number)))

(defn max-element-count-in-core
  [context
   core-number
   element]
  (if (or (nil? (fx/sub context
                        state/xrf-scan
                        core-number))
          (nil? (element (fx/sub context
                                 state/xrf-columns
                                 core-number))))
    0.0 ;; skips cores that have no XRF-scan loaded in
    (apply max (map second
                    (fx/sub context
                            element-counts
                            core-number
                            element)))))

(defn max-element-count-all-cores
  [context
   element]
  (apply max 0 (filter some? (map #(fx/sub  context
                                            max-element-count-in-core
                                            %
                                            element)
                                  (range (fx/sub context
                                                 state/num-cores))))))

(defn- load-xrf-scan-file
  [csv-file]  
  (let [full-csv-table (-> csv-file
                           (.getCanonicalPath)
                           (clojure.java.io/reader)
                           (clojure.data.csv/read-csv :separator \tab))
        header (into [] (take 2 full-csv-table))
        columns (map #(-> %
                          (clojure.string/replace " " "-")
                          (clojure.string/replace "(" "")
                          (clojure.string/replace ")" "")
                          keyword)
                     (first (drop 2 full-csv-table)))
        count-table (drop 3 full-csv-table)
        data (map #(zipmap columns %) count-table)]
    (assert (= (count columns)
               (count (set columns)))
            "BAD NEWS: Your XRF scan has non-unique columns names. This isn't supported!")
    {:file-name (.getName csv-file)
     :header header
     :columns (set columns)
     :element-counts data}))

(defn load-data
  [snapshot
   event]
  ;;  @(fx/on-fx-thread
  (let [core-number (:core-number event)
        file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (if (nil? file)
      snapshot ;; if no file was selected ie. file-selection-window was just closed
      (let [xrf-scan (sausage.xrf/load-xrf-scan-file file)
            columns (:columns xrf-scan)]
        (-> snapshot
            (fx/swap-context assoc-in [:cores
                                       core-number
                                       :xrf-scan]
                             xrf-scan))))))

(defn- build-csv-row
  "take one measurement data and shove it into a vector or string - for CSV exports"
  [columns
   measurement-data]
  (into [] (map #(% measurement-data) columns)))

(defn- save-xrf-scan
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

(defn- shift-string-number ;; TODO Make this unnecessary..
  [shift
   string-number]
  (str (+ (read-string string-number)
          shift)))

(defn- shift-scan-point
  [shift
   scan-point]
  (update scan-point :position-mm (partial shift-string-number shift)))

(defn crop
  [snapshot
   {:keys [core-number
           crop-left-mm
           crop-right-mm]}]
  (if (nil? (fx/sub snapshot
                    state/xrf-scan
                    core-number))
    snapshot
    (let [length-mm (fx/sub snapshot
                            state/length-mm
                            core-number)]
      (fx/swap-context snapshot
                       assoc-in ;; Maybe switch to `update-in` ??
                       [:cores
                        core-number
                        :xrf-scan
                        :element-counts]
                       (->> (fx/sub snapshot
                                    state/xrf-element-counts
                                    core-number)
                            ;; filter out right pixels
                            (filter #(<= (read-string (:position-mm %))
                                         (- length-mm
                                            crop-right-mm)))
                            ;; shift all points down by crop
                            (map #(shift-scan-point (- crop-left-mm)
                                                    %))
                            ;; filter out all below zero
                            (filter #(< 0
                                        (read-string (:position-mm %)))))))))

(defn- join-horizontally
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

(defn merge-cores
  "Attach the FROM-CORE-NUMBER xrf-scan horizontally
  to the right of the INTO-CORE-NUMBER xrf-scan
  This updates the INTO-CORE-NUMBER image and leaves
  the FROM-CORE-NUMBER image untouched"
  [snapshot
   {:keys [into-core-number
           from-core-number]}]
  (-> snapshot
      (fx/swap-context update-in
                       [:cores
                        into-core-number
                        :seams]
                       #(conj % (fx/sub snapshot
                                        state/start-mm
                                        from-core-number)))
      (fx/swap-context assoc-in
                       [:cores
                        into-core-number
                        :xrf-scan]
                       ;; The merge order is backwards here ..
                       ;; b/c the core images are stored backwards
                       (join-horizontally (fx/sub snapshot
                                                  state/xrf-scan
                                                  into-core-number)
                                          (fx/sub snapshot
                                                  state/start-mm
                                                  from-core-number)
                                          (fx/sub snapshot
                                                  state/xrf-scan
                                                  from-core-number)))))
