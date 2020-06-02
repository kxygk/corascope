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
  (mapv #(vector (read-string (:position-mm %))
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
          (nil? (element (set (fx/sub context
                                 state/xrf-columns
                                 core-number)))))
    0.0 ;; skips cores that have no XRF-scan loaded in
    (apply max (mapv second
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
(defn add-column-with-filename
  [element-counts
   file-name]
  (map (fn [data-point]
         (update data-point     ;; save file name to column
                 :pre-merge-file-source
                 #(if (nil? %) ;; unless it's already there
                    file-name
                    %)))
       element-counts))

(defn- load-xrf-scan-file
  "Note: XRF files are tab separated CSV files
  with an extrenious tab at the end
  Hence the `butlast` in there.."
  [csv-file]  
  (let [full-csv-table (-> csv-file
                           (.getCanonicalPath)
                           (clojure.java.io/reader)
                           (clojure.data.csv/read-csv :separator \tab))
        file-name (.getName csv-file)
        header (into [] (take 2 full-csv-table))
        columns (map #(-> %
                          (clojure.string/replace " " "-")
                          (clojure.string/replace "(" "")
                          (clojure.string/replace ")" "")
                          keyword)
                     (butlast (first (drop 2 full-csv-table))))
        count-table (drop 3 full-csv-table)
        data (map #(zipmap columns %) count-table)]
    (assert (= (count columns)
               (count (set columns)))
            "BAD NEWS: Your XRF scan has non-unique columns names")
    {:file-name file-name
     :header header
     :columns (into [:pre-merge-file-source] columns)
     :element-counts (-> data
                         (add-column-with-filename file-name))}))

(defn load-data
  [snapshot
   event]
  ;;  @(fx/on-fx-thread
  (let [core-number (:core-number event)
        file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   (.setInitialDirectory (fx/sub snapshot
                                                 state/last-used-path)))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (if (nil? file)
      snapshot ;; if no file was selected ie. file-selection-window was just closed
      (let [xrf-scan (sausage.xrf/load-xrf-scan-file file)
            columns (:columns xrf-scan)]
        (-> snapshot
            (fx/swap-context assoc
                             :last-used-path
                             (.getParentFile file))
            (fx/swap-context assoc-in
                             [:cores
                              core-number
                              :xrf-scan]
                             xrf-scan))))))

(defn build-csv-row
  "take one measurement data and shove it into a vector or string - for CSV exports"
  [columns
   measurement-data]
  (into [] (map #(% measurement-data) columns)))

(defn save-data
  ""
  [snapshot
   {:keys [core-number]}]
  ;; Side Effect
  (let [header (fx/sub snapshot
                       state/xrf-header
                       core-number)
        columns (fx/sub snapshot
                        state/xrf-columns
                        core-number)
        element-counts-csv (->> (fx/sub snapshot
                                        state/xrf-element-counts
                                        core-number)
                                (map #(build-csv-row columns %))
                                (into []))
        columns-csv (->> columns
                         (map name)
                         (into []))
        file (-> (doto (FileChooser.)
                   (.setTitle "Save Optical Image")
                   (.setInitialFileName (str (fx/sub snapshot
                                                     state/autosave-filename
                                                     core-number)
                                             ".txt"))
                   (.setInitialDirectory  (fx/sub snapshot
                                                  state/last-used-path)))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showSaveDialog (Stage.)))]
    (with-open [csv-writer (clojure.java.io/writer (.getCanonicalPath file))]
      (clojure.data.csv/write-csv csv-writer
                                  (concat []
                                          header
                                          [columns-csv]
                                          element-counts-csv)
                                  :separator \tab)))
    ;; return state unchanged
    snapshot)

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
