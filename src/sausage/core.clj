(ns sausage.core
  (:require
   [sausage.plot]
   [sausage.optical]
   [sausage.xrf]
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view])
  (:import javafx.stage.DirectoryChooser
           javafx.stage.FileChooser
           javafx.stage.Stage)
  (:gen-class :main true))

;; GLOBAL CONSTANTS
;; Can be freely adjusted to tweak the display

;; The cores are displayed/scaled based on their lengths so uninitialized cores
;; need a "size" so that they can take up space to display properly
;; Otherwise nothing shows up and there is no "Load" buttons.
;; (TODO: Think of a less goofy UI solution)
(def fixed-default-core-length 300.0)
(def fixed-workspace-settings-height 30.0)
(def fixed-margin-width 456) ;; dialed in to fit the periodic table
(def fixed-core-header-height 48)
(def fixed-optical-scan-height 133.0)  ;; needs to be fixed so the core displays line up
(def fixed-element-count-height 300.0)  ;; needs to be fixed so the core displays line up
(def fixed-slider-height 18)
(def fixed-element-selector-width 50)

(def *state
  ""
  (atom {:working-directory ""
         :width 400
         :full-width? false
         :height 400
         :mm-per-pixel 0.5 ;; Common parameter across all cores
         :mm-xrf-step-size 5 ;; Common parameter across all cores
         :cores []
         :displays [{:type :optical
                     :height fixed-optical-scan-height
                     :scan-line? true}
                    {:type :element-count
                     :height fixed-element-count-height
                     :merge-seams? true
                     :element :Mn
                     :max-count 1000}
                    {:type :optical
                     :height fixed-optical-scan-height
                     :scan-line? true}
                    {:type :element-count
                     :height fixed-element-count-height
                     :merge-seams? true
                     :element :Mn
                     :max-count 1000}]}
        ))

(defmulti event-handler
  "CLJFX -  Event Handlers

  When defining CLJFX event like `on-value-changed` you have two options
  - 1 You can point to *Function* (or lambda) to run when the event happens
  ex:
  ```
  {:fx/type :check-box
               :selected done
               :on-selected-changed #(swap! *state assoc-in [:by-id id :done] %)
  ```
  - 2 You can point to a custom *Map Events*
  ex:
  ```
  {:fx/type :check-box
               :selected done
               :on-selected-changed {:event/type ::set-done :id id}}
  ```
  You will then need to create an event handler function
  ex:
  ```
  (defn map-event-handler [event]
  (case (:event/type event)
    ::set-done (swap! *state assoc-in
                             [:by-id (:id event) :done]
                             (:fx/event event))))
  ```
  And this function will then be registered with the renderer
  ex:
  ```
  (fx/mount-renderer
  *state
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)
    :opts {:fx.opt/map-event-handler map-event-handler}))
  ```
  The map you paired to the event will be passed to the registered event handler
  It will needs to designate its type with the `event/type` key
  And the actual even will be passed in under the `fx/event` key
  Any additional keys you place in the map will also get passed along
  lik `:id` in the example

  In the simple example we registered a function,
  however here I register a multimethod
  Then we simply switch on the `event/type`"
  :event/type)

(defmethod event-handler ::width-changed
  [event]
  (swap! *state assoc :width (:fx/event event)))

(defmethod event-handler ::height-changed
  [event]
  (swap! *state assoc :height (:fx/event event)))

;; TODO Get this all hooked up properly
(defmethod event-handler ::set-working-directory [_]
  @(fx/on-fx-thread
    (let [file (-> (doto (DirectoryChooser.)
                     (.setTitle "Set a working directory")
                     #_(.setInitialDirectory (File. "/home/")))
                   ;; Could also grab primary stage instance to make this dialog blocking
                   (.showDialog (Stage.)))
          path (.getCanonicalPath file)]
      (.mkdirs file) ;; maybe should be moved to the save-workspace method
      (swap! *state assoc :working-directory path))))


;;## Example:
;;Pixel index:  0 1 2 3 4 5 6 7 8
;;             |-|-|-|-|-|-|-|-|-|
;;             0mm               4.5mm
                                        ;                   |~~~~~~~~~|
;;XRF-Scan          |---------|
;;                  1.25mm    3.75mm
;;
;; Optical scan   : mm/pixel = 0.5mm
;;
;; Unscanned Left : 2 Pixels / 1.0mm      ;; Third Pixel overlaps and remains
;;
;; Unscanned Right: 1 Pixel  / 0,5mm      ;; Second Pixel overlaps and remains
;;
;; Note: Cropping needs to be conservative, so unscanned areas need to be
;;       /under-selected/ so that when you merge data doesn't overlap
;;
;;## Degenerate cases:
;;# Scan starts on pixel edge
;; scan-start-mm: 1.0mm
;; scan-end-mm  : 4.0mm
;;
;;Pixel number: 1 2 3 4 5 6 7 8 9
;;             |-|-|-|-|-|-|-|-|-|
;;             0mm               4.5mm
                                        ;                  |~~~~~~~~~~~|
;;XRF-Scan         |-----------|
;;                 1.0mm       4.0mm
;;
;; scan-start-pix 2nd pixel
;; scan-end-pix 8th pixel
;;
;; unscanned-left-pix: 2 pixel
;; unscanned-right-pix:1 pixel
;;
;;# Method annotated from example
(defmethod event-handler ::update-unscanned-areas [event]
  (let [core-number (:core-number event)
        image-width-pix (-> @*state
                            :cores
                            (.get core-number)
                            :optical
                            :image
                            .getWidth) ;; 9 Pixels
        mm-per-pixel (:mm-per-pixel @*state) ;; 0.5 mm/pixel
        scan (-> @*state
                 :cores
                 (.get core-number)
                 :xrf-scan
                 :element-counts)
        scan-start-mm (-> scan
                          first
                          :position
                          read-string) ;; 1.25mm
        scan-end-mm (-> scan
                        last
                        :position
                        read-string)  ;; 3.75mm
        ]
    (swap! *state
           assoc-in [:cores
                     core-number
                     :optical
                     :unscanned-left-pix]
           (int (Math/floor (/ scan-start-mm    ;; (floor 2.5)
                               mm-per-pixel)))) ;; 2 Pixels entirely unscanned
    (swap! *state
           assoc-in [:cores
                     core-number
                     :optical
                     :unscanned-right-pix]
           (- image-width-pix
              (int (Math/ceil (/ scan-end-mm         ;; 9 - (ceil 7.5)
                                 mm-per-pixel))))))) ;; 9-8 = 1 Pixel entirely unscanned

;; Updates the core's 'display image' based on
;; the internally stored 'optical image'
;;
;; NOTE: This is done because we want to avoid
;; operations on the underlying 'optical image'
(defmethod event-handler ::update-display-image
  [event]
  (let [core-number (:core-number event)
        optical (-> @*state
                    :cores
                    (get core-number)
                    :optical
                    :image)]
    (swap! *state assoc-in [:cores
                            core-number
                            :optical
                            :display]
           (-> optical
               sausage.optical/flip-image
               sausage.optical/to-fx-image))))

(defn overlap?
  "Check if CORE-A and CORE-B overlap in any way
  RETURN: TRUE/FALSE"
  [core-a
   core-b]
  (if (>= (-> core-b :start-mm)
          (+ (-> core-a :start-mm)
             (-> core-a :length-mm))) ;; end-mm
    false
    true))

(defn overlapping-cores
  "Given a vector of CORES, it will check which overlap
  and gives you two lists:
  - not-overlapping: a selection of cores that don't overlap (biased towards 0mm)
  - overlapping: the remaining cores that did overlap (and may mutually overlap)
  RETURNS: [[not-overlapping][overlapping]]"
  [cores]
  (if (empty? cores)
    [[][]]
    (let [current-core (first cores)
          later-cores (rest cores)
          [overlapping-current-core
           not-overlapping-current-core] (split-with (partial overlap? current-core)
                                                     later-cores)
          [current-level-cores
           other-level-cores] (overlapping-cores not-overlapping-current-core)]
      [(into [current-core]
             current-level-cores)
       (into (into [] overlapping-current-core)
             other-level-cores)])))

(defn pyramid-stack
  "Stacks CORES into a 'pyramid'.
  Lower rows as tightly as possible - biased to 0mm
  ie. it takes the first core, then chooses the next core that fits next to it
  and then the next.. etc.
  Cores that don't fit on the first row we again try to fit on the next row.
  This happens recursively.
  RETURNS:
  [[core-x core-y core-z] ;; first row
   [core-a core-b]        ;; second row
   [core-c]]               ;; third row ... etc.
  "
  [cores]
  (let [[current-level-cores
         other-cores] (overlapping-cores cores)]
    (if (empty? other-cores)
      [current-level-cores
       other-cores]
      (into [current-level-cores]
            (pyramid-stack other-cores)))))

(defn inject-index
  "Given a VECTOR-OF-MAPS it will add and index key into each map.
  key is named: :tracking-index
  After processing the maps you can use the injected index
  to figure out which one it was"
  [vector-of-maps]
  (map #(assoc %1
               :tracking-index
               %2)
       vector-of-maps
       (range (count vector-of-maps))))

(defn reduce-to-indeces
  "Given a MAP a tracking :tracking-index
  RETURN: index
  Given a VECTOR recursively call this function
  RETURN: vector of vector of vector... of indeces
  EXAMPLE OUTPUT:
  [[0 2 4]
   [1]
   [3 5]]"
  [element]
  (if (map? element)
    (:tracking-index element)
    (map reduce-to-indeces element)))

(defn sort-cores
  "Given a vector of CORES,
  RETURN: a vector sorted by :start-mm
  NOTE: The core-number/index will change"
  [cores]
  (into [] (sort #(< (:start-mm %1)
                     (:start-mm %2))
                 cores)))

;; Adjusts the CORE list so that the cores are in order based on :start-mm
;; Then (re)generates a pyramid/stacked layout so we can then look up which
;; core shows up on which level
(defmethod event-handler ::sort-cores [event]
  (println "Sorting Cores")
  (swap! *state
         update
         :cores
         sort-cores)
  (swap! *state
         assoc
         :layout
         (-> @*state
             :cores
             inject-index
             pyramid-stack
             reduce-to-indeces)))

(defn- does-vector-contain-value
  "Local helper - Checks if a VECTOR contains a VALUE
  RETURNS: TRUE/FALSE"
  [value vector]
  (reduce #(or %1 (= %2 value))
          false
          vector))

(defn get-core-row
  "Give a CORE-NUMBER and LAYOUT
  RETURN: Which row the core should be displayed on"
  [core-number layout]
  (let [does-row-have-core? (map (partial does-vector-contain-value
                                          core-number)
                                 layout)
        row-with-core (first (keep-indexed #(if (true? %2) %1)
                                           does-row-have-core?))]
    row-with-core))

(defn update-core-length
  "Given a CORE and the optical image's MM-PER-PIXEL
  RETURN: A core with an updated :length-mm
  NOTE: This is either derived from the optical scan size
        or the xrf-scan size
  DEGENERATE: If there is no :optical or :xrf-scan then
              the length is reset to a global default  "
  [core
   mm-per-pixel]
  (if (nil? (-> core :optical))
    (if (nil? (-> core :xrf-scan))
      (assoc core :length-mm fixed-default-core-length)
      (assoc core :length-mm (* mm-per-pixel
                                (-> core
                                    :xrf-scan
                                    :element-counts
                                    last
                                    :position
                                    read-string))))
    (assoc core :length-mm (* mm-per-pixel
                              (-> core
                                  :optical
                                  :image
                                  .getWidth)))))

;; Makes sure the :length-mm is set properly based on the
;; :optical scan and the :xrf-scan
(defmethod event-handler ::update-core-length [event]
  (let [core-number (:core-number event)
        core (-> @*state
                 :cores
                 (get core-number))]
    (swap! *state
           assoc-in [:cores
                     core-number]
           (update-core-length core
                               (-> @*state
                                   :mm-per-pixel))))
  (event-dispatcher {:event/type ::sort-cores}))

;; File Picker copied from here:
;; https://github.com/cljfx/cljfx/pull/40#issuecomment-544256262
;;
;; User has opted to load a new project. Use FileChooser so they can select the file.
;; Then trigger the central project loading event, passing in the selected file.
(defmethod event-handler ::load-optical-image [event]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (if (some? file)
      (let[optical-image (sausage.optical/load-image file)
           core-number (:core-number event)]
        (swap! *state
               assoc-in [:cores
                         core-number
                         :optical
                         :image]
               optical-image)
        (event-dispatcher {:event/type ::update-display-image
                        :core-number core-number})
        (event-dispatcher {:event/type ::update-core-length
                        :core-number core-number})

        (if (-> @*state
                :cores
                (.get core-number)
                :xrf-scan)
          (event-dispatcher {:event/type ::update-unscanned-areas
                          :core-number core-number}))))))

(defn element-counts
  "Given an XRF-SCAN and a ELEMENT (keyword)
  RETURN:
  [[position element-count]
   [position element-count]
   ...
   [position element-count]]"
  [xrf-scan
   element]
  (map #(vector (read-string (:position %))
                (read-string (element %)))
       (:element-counts xrf-scan)))

(defn get-max-count-for-element-in-core-scan
  "Given a CORE and an ELEMENT
  Returns the maximum count detected for this element"
  [core
   element]
  (if (nil? (-> core :xrf-scan))
    0.0
    (let [pos-count-pairs (element-counts (-> core
                                              :xrf-scan)
                                          element)
          counts (map second pos-count-pairs)]
      (apply max counts))))

(defn get-max-count-for-element-in-cores ;; TODO Collapse these function into one maybe..
  "Given a list of CORES and an ELEMENT
  Returns the maximum count detected for this element"
  [cores
   element]
  (apply max
         (map #(get-max-count-for-element-in-core-scan %
                                                       element)
              cores)))

(defn update-selection
  [selection]
  (let [element (:element selection)
        cores (-> @*state
                  :cores)]
    {:element element
     :max-count (get-max-count-for-element-in-cores cores
                                                    element)}))

(defn- update-max-element-in-display
  [cores
   display]
  (if (= :element-count
         (:type display))
    (assoc display
           :max-count
           (get-max-count-for-element-in-cores cores
                                               (:element display)))
    display))

(defmethod event-handler ::update-max-element-count [event]
  (swap! *state
         update
         :displays
         #(mapv (partial update-max-element-in-display (-> @*state
                                                           :cores))
                %)))

(defmethod event-handler ::load-xrf-scan [event]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (if (some? file)
      (let [core-number (:core-number event)
            xrf-scan (sausage.xrf/load-xrf-scan-file file)
            columns (:columns xrf-scan)]
        (swap! *state
               update
               :columns
               clojure.set/union
               columns)
        (swap! *state
               assoc-in [:cores
                         core-number
                         :xrf-scan]
               xrf-scan)
        (event-dispatcher {:event/type ::update-core-length
                        :core-number core-number})
        (event-dispatcher {:event/type ::update-max-element-count})
        (if (-> @*state
                :cores
                (.get core-number)
                :optical)
          (event-dispatcher {:event/type ::update-unscanned-areas
                          :core-number core-number})
          (swap! *state
                 assoc-in [:cores
                           core-number
                           :length-mm]
                 (sausage.xrf/end-position xrf-scan)))))))

(defmethod event-handler ::add-core [event]
  (let [num-cores (-> @*state
                      :cores
                      count)]
    (swap! *state assoc-in [:cores
                            num-cores]
           {:start-mm (if (nil? (-> @*state :cores last))
                        0.0 ;; if no core to tack on to, then set to zero
                        (+ (-> @*state :cores last :start-mm)
                           (-> @*state :cores last :length-mm)))
            :optical nil
            :xrf-scan nil
            :crop-left 0
            :crop-right 0
            :length-mm fixed-default-core-length
            :seams []})
    (event-dispatcher {:event/type ::sort-cores})))

(defmethod event-handler ::remove-core [event]
  (let [core-number (:core-number event)]
    (if (nil? core-number)
      (swap! *state assoc :cores
             (pop (:cores @*state)))
      (swap! *state
             update
             :cores
             #(vec (concat (subvec % 0 (:core-number event))
                           (subvec % (inc (:core-number event))))))))
  (if (empty? (:cores @*state))
    (event-dispatcher {:event/type ::add-core}) ;; already does sorting
    (event-dispatcher {:event/type ::sort-cores})))

(defn merge-cores
  "Given a CORE-A and CORE-B and a MM-PER-PIXEL for their respective images
  RESULT: One core with optical and xrf-scans merged"
  [mm-per-pixel
   core-a
   core-b]
  (let [gap-pmm (- (-> core-b ;; gap between the end of one core and start of next
                       :start-mm)
                   (+ (-> core-a
                          :start-mm)
                      (-> core-a
                          :length-mm)))
        gap-pix (/ gap-pmm
                   mm-per-pixel)
        merged-image (sausage.optical/join-horizontally (-> core-b
                                                            :optical
                                                            :image)
                                                        (-> core-a
                                                            :optical
                                                            :image)
                                                        gap-pix)
        merged-xrf-scan (sausage.xrf/join-horizontally (-> core-a
                                                           :xrf-scan)
                                                       (-> core-b
                                                           :start-mm)
                                                       (-> core-b
                                                           :xrf-scan))]


    (-> core-a
        (update :seams #(into % [(-> core-a :length-mm)]))
        (assoc-in [:optical :image] merged-image)
        (assoc-in [:xrf-scan] merged-xrf-scan)
        (update-core-length mm-per-pixel))))

;; TODO: Make sure this doesn't get called when cores overlap
(defmethod event-handler ::merge-all-cores
  [event]
  (swap! *state
         assoc
         :cores
         [(reduce (partial merge-cores (-> @*state :mm-per-pixel))
                  (-> @*state
                      :cores
                      (get 0))
                  (rest (-> @*state
                            :cores)))])
  (event-dispatcher {:event/type ::update-display-image
                  :core-number 0})
  (event-dispatcher {:event/type ::update-core-length
                  :core-number 0}))

(defn workspace-settings-display
  "Top level settings for the workspace where all data will be stored in"
  [{:keys [working-directory
           height]}]
  {:fx/type :h-box
   :children [{:fx/type :button
               :pref-width fixed-margin-width
               :min-width fixed-margin-width
               :pref-height height
               :min-height height
               :max-height height
               :on-action {:event/type ::set-working-directory}
               :text "Set"}
              {:fx/type :text-field
               :editable false
               :pref-height height ;; TODO: What's with all these pref/min dimensions...?
               :prompt-text "Select a working directory.."
               :pref-column-count 999
               :text working-directory}
              {:fx/type :button
               :pref-height height
               :pref-width height ;; make square button
               :min-width  height
               :on-action {:event/type ::remove-core}
               :text "-"}
              {:fx/type :button
               :pref-height height
               :pref-width height ;; make square button
               :min-width height
               :on-action {:event/type ::add-core}
               :text "+"}]})

(defn optical-image-display
  "display and options for the optical image"
  [{:keys [core-number
           scan-line?
           width
           height
           optical
           crop-left
           crop-right]}]
  {:fx/type :h-box
   :pref-height height
   :children (if (nil? optical)
               [{:fx/type :v-box
                 :children [{:fx/type :button
                             :pref-width width
                             :pref-height height
                             :on-action {:event/type ::load-optical-image
                                         :core-number core-number}
                             :text "Load image"}]}]
               [{:fx/type :group
                 :children [{:fx/type :pane
                             :children (filter identity ;; TODO Find way to do conditional GUI elements without filter
                                               (let [crop-left-pix (* crop-left
                                                                      width)
                                                     crop-right-pix (* crop-right
                                                                       width)]
                                                 [{:fx/type :image-view
                                                   :fit-width width
                                                   :fit-height height
                                                   :image (:display optical)}
                                                  ;; Center Line
                                                  (if (true? scan-line?)
                                                    {:fx/type :line
                                                     :start-x 1 ;; pixel offset
                                                     :start-y (/ height
                                                                 2.0)
                                                     :end-x (dec width)
                                                     :end-y (/ height
                                                               2.0)
                                                     :stroke-dash-array [10 10]
                                                     :stroke "white"})
                                                  ;; Right Crop
                                                  (if (pos? crop-right-pix)
                                                    {:fx/type :line
                                                     :start-x (- width
                                                                 crop-right-pix)
                                                     :start-y 0
                                                     :end-x (- width
                                                               crop-right-pix)
                                                     :end-y (dec height) ;; this is inclusive!
                                                     :stroke "red"})
                                                  (if (pos? crop-right-pix)
                                                    {:fx/type :rectangle
                                                     :x (- width
                                                           crop-right-pix)
                                                     :y 0
                                                     :height height
                                                     :width crop-right-pix
                                                     :opacity 0.10
                                                     :fill "red"})
                                                  ;; Left Crop
                                                  (if (pos? crop-left-pix);;(not (nil? (:crop-pixels-left optical)))
                                                    {:fx/type :line
                                                     :start-x crop-left-pix
                                                     :start-y 0
                                                     :end-x crop-left-pix
                                                     :end-y (dec height) ;; this is inclusive!
                                                     :stroke "red"})
                                                  (if (pos? crop-left-pix);;(not (nil? (:crop-pixels-left optical)))
                                                    {:fx/type :rectangle
                                                     :x 0
                                                     :y 0
                                                     :height height
                                                     :width crop-left-pix
                                                     :opacity 0.10
                                                     :fill "red"})
                                                  ;; Left Unscanned Area
                                                  (if (and (not (nil? (:unscanned-left-pix optical)))
                                                           (pos? (:unscanned-left-pix optical)))
                                                    (let [left-width (* (/ (:unscanned-left-pix optical)
                                                                           (.getWidth (:image optical)))
                                                                        width)]
                                                      {:fx/type :line
                                                       :start-x left-width
                                                       :start-y 0
                                                       :end-x left-width
                                                       :end-y (dec height) ;; this is inclusive!
                                                       :stroke "brown"}))
                                                  (if (and (not (nil? (:unscanned-left-pix optical)))
                                                           (pos? (:unscanned-left-pix optical)))
                                                    (let [left-width (* (/ (:unscanned-left-pix optical)
                                                                           (.getWidth (:image optical)))
                                                                        width)]
                                                      {:fx/type :rectangle
                                                       :x 0
                                                       :y 0
                                                       :height height
                                                       :width left-width
                                                       :opacity 0.10
                                                       :fill "yellow"}))
                                                  ;; Right Unscanned Area
                                                  (if (and (not (nil? (:unscanned-left-pix optical)))
                                                           (pos? (:unscanned-right-pix optical)))
                                                    (let [right-width (* (/ (:unscanned-right-pix optical)
                                                                            (.getWidth (:image optical)))
                                                                         width)]
                                                      {:fx/type :line
                                                       :start-x (- width
                                                                   right-width);;right-width
                                                       :start-y 0
                                                       :end-x (- width
                                                                 right-width);;right-width
                                                       :end-y (dec height) ;; this is inclusive!
                                                       :stroke "brown"}))
                                                  (if (and (not (nil? (:unscanned-left-pix optical)))
                                                           (pos? (:unscanned-right-pix optical)))
                                                    (let [right-width (* (/ (:unscanned-right-pix optical)
                                                                            (.getWidth (:image optical)))
                                                                         width)]
                                                      {:fx/type :rectangle
                                                       :x (- width
                                                             right-width)
                                                       :y 0
                                                       :height height
                                                       :width right-width
                                                       :opacity 0.10
                                                       :fill "yellow"}))
                                                  ]))}]}])})

(defn xrf-scan-display
  "display and options for XRF scan data"
  [{:keys [core-number
           width
           height
           xrf-scan
           selection
           max-element-count
           core-length-mm
           crop-left
           crop-right
           merge-seams?
           seams]}]
  {:fx/type :h-box
   :children [{:fx/type :v-box
               :children (if xrf-scan
                           [{:fx/type :image-view
                             :fit-width width
                             :fit-height height
                             :image (sausage.plot/plot-points width
                                                              height
                                                              (element-counts xrf-scan
                                                                              (keyword selection))
                                                              core-length-mm
                                                              max-element-count
                                                              crop-left
                                                              crop-right
                                                              (if merge-seams?
                                                                seams
                                                                []))}]
                           [{:fx/type :button
                             :pref-width width
                             :pref-height height
                             :on-action {:event/type ::load-xrf-scan
                                         :core-number core-number}
                             :text "Load XRF Scan"}])}]})

(defmethod event-handler ::adjust-right-crop
  [event]
  (swap! *state assoc-in [:cores
                          (:core-number event)
                          :crop-right]
         (- 1 (:fx/event event))))

(defmethod event-handler ::adjust-left-crop
  [event]
  (swap! *state assoc-in [:cores
                          (:core-number event)
                          :crop-left]
         (:fx/event event)))

;; Crop both :optical and :xrf-data
;; 1 - always crop areas that are optically scanned but have no xrf data
;; 2 - optionally crop more based on the slider positions
;; TODO: Simplify this .. with some destructuring or something
(defmethod event-handler ::crop
  [event]
  (let [core-number (:core-number event)
        core (-> @*state
                 :cores
                 (get core-number))
        xrf-scan-element-counts (-> core
                                    :xrf-scan
                                    :element-counts)
        image (-> core
                  :optical
                  :image)
        image-width-pix (-> image
                            .getWidth)
        crop-slider-left (-> core
                             :crop-left)
        crop-slider-right (-> core
                              :crop-right)
        crop-slider-left-pix (int (Math/floor (* crop-slider-left
                                                 image-width-pix)))
        crop-slider-right-pix (int (Math/floor (* crop-slider-right
                                                  image-width-pix)))
        unscanned-left-pix (-> core
                               :optical
                               :unscanned-left-pix)
        unscanned-right-pix (-> core
                                :optical
                                :unscanned-right-pix)
        crop-left-pix (if (> unscanned-left-pix
                             crop-slider-left-pix)
                        unscanned-left-pix
                        crop-slider-left-pix)
        crop-right-pix (if (> unscanned-right-pix
                              crop-slider-right-pix)
                         unscanned-right-pix
                         crop-slider-right-pix)
        mm-per-pixel (-> @*state
                         :mm-per-pixel)
        crop-left-mm (* crop-left-pix
                        mm-per-pixel)
        crop-right-mm (* crop-right-pix
                         mm-per-pixel)]
    (swap! *state assoc-in [:cores
                            core-number
                            :optical
                            :image]
           (sausage.optical/crop image
                                 crop-left-pix
                                 crop-right-pix))
    (swap! *state assoc-in [:cores
                            core-number
                            :xrf-scan
                            :element-counts]
           (sausage.xrf/crop xrf-scan-element-counts
                             (-> core
                                 :length-mm)
                             crop-left-mm
                             crop-right-mm))
    (if (== 0
            core-number) ;; shift the merge seams
      (swap! *state update-in [:cores 0 :seams] #(map (partial + (- crop-left-mm)) %)))

    (event-dispatcher {:event/type ::update-core-length
                    :core-number core-number})
    (event-dispatcher {:event/type ::update-display-image
                    :core-number core-number})
    (swap! *state
           assoc-in [:cores
                     core-number
                     :optical
                     :unscanned-left-pix]
           0.0)
    (swap! *state
           assoc-in [:cores
                     core-number
                     :optical
                     :unscanned-right-pix]
           0.0)
    (swap! *state
           assoc-in [:cores
                     core-number
                     :crop-left]
           0.0)
    (swap! *state
           assoc-in [:cores
                     core-number
                     :crop-right]
           0.0)))

(defn crop-slider
  "The sliders that visually help the user cropping the data"
  [{:keys [core-number
           width
           height
           crop-left
           crop-right]}]
  {:fx/type :h-box
   :pref-height height
   :min-height height
   :max-height height
   :pref-width width
   :min-width width
   :max-width width
   :children [{:fx/type :slider
               :max 0.5
               :min 0.0
               :show-tick-labels false
               :show-tick-marks false
               :pref-width (* 0.5
                              width)
               :min-width (* 0.5
                             width)
               :pref-height height
               :min-height height
               :value crop-left
               :on-value-changed {:event/type ::adjust-left-crop
                                  :core-number core-number}}
              {:fx/type :slider
               :max 1.0
               :min 0.5
               :show-tick-labels false
               :show-tick-marks false
               :pref-width (* 0.5 width)
               :min-width (* 0.5 width)
               :pref-height height
               :min-height height
               :value (- 1 crop-right)
               :on-value-changed {:event/type ::adjust-right-crop
                                  :core-number core-number}}]})

;; Update :start-mm based on user input and then resorts/lays-out the cores
(defmethod event-handler ::update-core-start [event]
  (try
    (let [core-number (:core-number event)
          input-value-mm (:fx/event event)
          mm-per-pixel (-> @*state
                           :mm-per-pixel)
          rounded-to-pixel (Math/round (/ input-value-mm
                                          mm-per-pixel))
          corrected-start-mm (* rounded-to-pixel
                                mm-per-pixel)]
      (println "corrected" corrected-start-mm)
      (swap! *state
             assoc-in [:cores
                       core-number
                       :start-mm]
             corrected-start-mm))
    (catch Exception ex
      (println "Invalid core-start input")))
  (event-dispatcher {:event/type ::sort-cores}))

(defn core-header-display
  "The options bar at the top of every core"
  [{:keys [core-number
           width
           start-mm
           mm-per-pixel
           crop-left
           crop-right]}]
  (let [height (- fixed-core-header-height
                  fixed-slider-height)]
    {:fx/type :v-box
     :children [{:fx/type :h-box
                 :pref-height height
                 :min-height height
                 :max-height height
                 :alignment :center-left
                 :children [{:fx/type :spinner
                             :editable true
                             :value-factory {:fx/type :double-spinner-value-factory
                                             :amount-to-step-by mm-per-pixel
                                             :min 0.0
                                             :max Double/MAX_VALUE
                                             :value start-mm}
                              :on-value-changed {:event/type ::update-core-start
                                                                :core-number core-number}
                             }
                            {:fx/type :separator
                             :orientation :horizontal}
                            {:fx/type :text
                             :text "(mm)"}
                            {:fx/type :button
                             :pref-width (* 0.10
                                            width)
                             :pref-height height
                             :on-action {:event/type ::crop
                                         :core-number core-number}
                             :text "Crop"}
                            {:fx/type :pane
                             :h-box/hgrow :always}
                            {:fx/type :button
                             :text "X"
                             :on-action {:event/type ::remove-core
                                         :core-number core-number}}
                            ]}
                {:fx/type crop-slider
                 :core-number core-number
                 :width width
                 :height fixed-slider-height
                 :crop-left  crop-left
                 :crop-right crop-right}]}))

(defn core-display
  "The cummulative core display"
  [{:keys [horizontal-zoom-factor
           mm-per-pixel
           height
           display-row
           core-number
           directory
           core
           displays]}]
  (let [width (* horizontal-zoom-factor
                 (-> core :length-mm))] ;; TODO: Convert to pixels
    {:fx/type :v-box
     :layout-x (* horizontal-zoom-factor
                  (-> core :start-mm))  ;; TODO: Convert to pixels
     :layout-y (* height
                  display-row)  ;; TODO: Convert to pixels
     :children (into [{:fx/type core-header-display
                       :core-number core-number
                       :width width
                       :start-mm (:start-mm core)
                       :mm-per-pixel mm-per-pixel
                       :crop-left (:crop-left core)
                       :crop-right (:crop-right core)}]
                     (map #(case (:type %)
                             :optical {:fx/type optical-image-display
                                       :core-number core-number
                                       :scan-line? (:scan-line? %)
                                       :width width
                                       :height (:height %) ;;fixed-optical-scan-height
                                       :optical (:optical core)
                                       :crop-left  (:crop-left core)
                                       :crop-right (:crop-right core)}
                             :element-count {:fx/type xrf-scan-display
                                             :core-number core-number
                                             :height (:height %) ;;fixed-optical-scan-height
                                             :width width
                                             :xrf-scan (:xrf-scan core)
                                             :selection (:element %)
                                             :max-element-count (:max-count %)
                                             :core-length-mm (:length-mm core)
                                             :crop-left  (:crop-left core)
                                             :crop-right (:crop-right core)
                                             :merge-seams? (:merge-seams? %)
                                             :seams (:seams core)})
                          displays))}))

(defmethod event-handler ::toggle-full-width
  [event]
  (swap! *state assoc :full-width? (:fx/event event)))

(defmethod event-handler ::toggle-scan-line
  [event]
  (swap! *state
         assoc-in
         [:displays
          (:display-number event)
          :scan-line?]
         (:fx/event event)))

(defmethod event-handler ::toggle-merge-seams
  [event]
  (swap! *state
         assoc-in
         [:displays
          (:display-number event)
          :merge-seams?]
         (:fx/event event)))

(def periodic-table
  [[:H  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil :He]
   [:Li :Be nil nil nil nil nil nil nil nil nil nil :B  :C  :N  :O  :F  :Ne]
   [:Na :Mg nil nil nil nil nil nil nil nil nil nil :Al :Si :P  :S  :Cl :Ar]
   [:K  :Ca :Sc :Ti :V  :Cr :Mn :Fe :Co :Ni :Cu :Zn :Ga :Ge :As :Se :Br :Kr]
   [:Rb :Sr :Y  :Zr :Nb :Mo :Tc :Ru :Rh :Pd :Ag :Cd :In :Sn :Sb :Te :I  :Xe]
   [:Cs :Ba :La :Hf :Ta :W  :Re :Os :Ir :Pt :Au :Hg :Tl :Pb :Bi :Po :At :Rn]
   [:Fr :Ra :Ac :Rf :Db :Sg :Bh :Hs :Mt :Ds :Rg :Cn :Nh :Fl :Mc :Lv :Ts :Og]
   [:.. :Th :.. :U]])

(defmethod event-handler ::update-selected-element [event]
  (swap! *state
         assoc-in
         [:displays
          (:display-number event)
          :element]
         (:element event))
  (event-dispatcher {:event/type ::update-max-element-count}))

(defn periodic-buttons
  "Periodic table as a grid of buttons
  Excess  columns in the xrf-scan file are then appended at the end"
  [{:keys [columns
           display-number]}]
  (let [non-elements (clojure.set/difference  (set (-> @*state :columns))
                                              (set (flatten periodic-table)))]
    {:fx/type :grid-pane
     :children
     (into (filter some?
                   (apply concat
                          (map-indexed (fn [valence-electrons period]
                                         (map-indexed (fn [group element]
                                                        (if (some? element)
                                                          {:fx/type :button
                                                           :padding 2.0
                                                           :min-width 25
                                                           :max-height 25
                                                           :pref-width 25
                                                           :grid-pane/column group
                                                           :grid-pane/row valence-electrons
                                                           :disable (not (contains? columns element))
                                                           :on-action {:event/type ::update-selected-element
                                                                       :display-number display-number
                                                                       :element element}
                                                           :text (name element)}))
                                                      period
                                                      ))
                                       periodic-table)))
           (if (some? non-elements)
             (map-indexed (fn [row-after-table non-element]
                            (let [columns 6]
                              {:fx/type :button
                               :grid-pane/column-span (/ (count (first periodic-table))
                                                         columns)
                               :max-height 25
                               :max-width Double/MAX_VALUE
                               :grid-pane/column (* (int (mod row-after-table columns))
                                                    (/ (count (first periodic-table))
                                                       columns))
                               :grid-pane/row (+ (count periodic-table)
                                                 (int (/ row-after-table columns)))
                               :on-action {:event/type ::update-selected-element
                                           :display-number display-number
                                           :element non-element}
                               :text (name non-element)}))
                          non-elements)
             [] ))}))

(defn core-header-options
  ""
  [{:keys [;;display-number
           width
           can-merge?]}]
  {:fx/type :h-box
   :pref-height fixed-core-header-height
   :min-height fixed-core-header-height
   :max-height fixed-core-header-height
   :alignment :center-left
   :children [{:fx/type :button
               :max-height Double/MAX_VALUE
               :on-action {:event/type ::merge-all-cores}
               :disable true
               :text "Crop"}
              {:fx/type :button
               :max-height Double/MAX_VALUE
               :on-action {:event/type ::merge-all-cores}
               :disable (not can-merge?)
               :text ">> Merge"}
              {:fx/type :check-box
               :text "Full Width"
               :on-selected-changed {:event/type ::toggle-full-width}}]})

(defn optical-image-options
  ""
  [{:keys [display-number
           height
           scan-line?]}]
  {:fx/type :v-box
   :children [{:fx/type :check-box
               :text "Scan Line"
               :selected scan-line?
               :on-selected-changed {:event/type ::toggle-scan-line
                                     :display-number display-number}}]})

(defn xrf-scan-options
  ""
  [{:keys [display-number
           height
           columns
           merge-seams?]}]
  {:fx/type :v-box
   :children [{:fx/type periodic-buttons
               :display-number display-number
               :columns columns}
              {:fx/type :check-box
               :text "Merge Seams"
               :selected merge-seams?
               :on-selected-changed {:event/type ::toggle-merge-seams
                                     :display-number display-number}}]})

(defmethod event-handler ::remove-display [event]
  (swap! *state
         update
         :displays
         #(vec (concat (subvec % 0 (:display-number event))
                       (subvec % (inc (:display-number event))))))
  )

(defn display-options-header
  [{:keys [display-number
           display-name]}]
  {:fx/type :v-box
   :alignment :center-left
   :children [{:fx/type :separator
               :orientation :horizontal}
              {:fx/type :h-box
               :children [{:fx/type :text
                           :text display-name}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :button
                           :text "X"
                           :on-action {:event/type ::remove-display
                                       :display-number display-number}}]}]})


(defmethod event-handler ::add-display [event]
  (swap! *state
         update
         :displays
         #(conj %
                (case (:display-type event)
                  :optical
                  {:type :optical
                   :height fixed-optical-scan-height
                   :scan-line? true}
                  :element-count
                  {:type :element-count
                   :height fixed-element-count-height
                   :merge-seams? true
                   :element :Mn
                   :max-count 1000})))
  (event-dispatcher {:event/type ::update-max-element-count}))

(defn add-display-options
  [_]
  {:fx/type :h-box
   :children [{:fx/type :button
               :on-action {:event/type ::add-display
                           :display-type :optical}
               :text " + Optical"}
              {:fx/type :button
               :on-action {:event/type ::add-display
                           :display-type :element-count}
               :text " +  Element Count"}]})

(defn margin
  "The right margin with global options/toggles.
  First come static options
  Followed by per-display options"
  [{:keys [width
           height
           columns
           can-merge?
           displays
           ]}]
  {:fx/type :v-box
   :pref-width width
   :min-width width
   :max-width width
   :children [{:fx/type core-header-options
               :can-merge? can-merge?}
              {:fx/type :v-box
               :children (flatten (map-indexed (fn [display-number
                                                    display]
                                                 {:fx/type :v-box
                                                  :pref-height (:height display);; height
                                                  :min-height (:height display) ;;height
                                                  :max-height (:height display) ;;height
                                                  :children [{:fx/type display-options-header
                                                              :display-number display-number
                                                              :display-name (name (:type display))}
                                                             (case (:type display)
                                                               :optical {:fx/type optical-image-options
                                                                         :display-number display-number
                                                                         :height (:height display)
                                                                         :scan-line? (:scan-line? display)}
                                                               :element-count {:fx/type xrf-scan-options
                                                                               :display-number display-number
                                                                               :height (:height display)
                                                                               :columns columns
                                                                               :merge-seams? (:merge-seams? display)})]})
                                               displays))}
              {:fx/type add-display-options}]})

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [width
           full-width?
           height
           working-directory
           mm-per-pixel
           layout
           columns
           cores
           displays]}]
  (let [horizontal-zoom-factor (if full-width?
                                 1
                                 (/ (- width fixed-margin-width)
                                    (+ (-> @*state :cores last :start-mm)
                                       (-> @*state :cores last :length-mm))))
        core-display-height (reduce #(+ %1 (:height %2))
                                    (+ fixed-core-header-height
                                       fixed-slider-height)
                                    displays)]
    {:fx/type :stage
     :title "Sausage Scanner"
     :showing true
     :scene {:fx/type :scene
             :on-width-changed {:event/type ::width-changed}
             :on-height-changed {:event/type ::height-changed}
             :root {:fx/type :v-box
                    :children[{:fx/type workspace-settings-display
                               :working-directory working-directory
                               :height fixed-workspace-settings-height}
                              {:fx/type :h-box
                               :children [{:fx/type :scroll-pane
                                           :hbar-policy :never
                                           :vbar-policy :never
                                           :pref-viewport-width (- width fixed-margin-width)
                                           :content {:fx/type :pane
                                                     :children (map-indexed (fn [index core]
                                                                              {:fx/type core-display
                                                                               :core-number index
                                                                               :horizontal-zoom-factor horizontal-zoom-factor
                                                                               :mm-per-pixel mm-per-pixel
                                                                               :height core-display-height
                                                                               :display-row (get-core-row index
                                                                                                          layout)
                                                                               :core core
                                                                               :displays displays})
                                                                            cores)}}
                                          {:fx/type margin
                                           :width fixed-margin-width
                                           :height core-display-height
                                           :columns columns
                                           :can-merge? (-> layout
                                                           second
                                                           empty?)
                                           :displays displays}
                                          ]}]}}}))

(def event-dispatcher
  (-> event-handler
      ;; adds the current state to every processed event
      ;; the event handler can then operate on the current state
      ;; and doesn't need to do it own dereferencing
      (fx/wrap-co-effects {:state #(deref *state)})
      ))

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-dispatcher}))


(defn -main [& args]
  ;; Add a first empty core
  ;; The UI paradigm doesn't make much sense with zero cores
  (event-dispatcher {:event/type ::add-core})
  (fx/mount-renderer
   *state
   renderer))

