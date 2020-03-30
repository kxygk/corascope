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
  (:gen-class))

(def *state
  ""
  (atom {:working-directory ""
         :width 400
         :full-width? false
         :height 400
         :mm-per-pixel 0.5 ;; Common parameter across all cores
         :mm-xrf-step-size 5 ;; Common parameter across all cores
         :cores [{:optical nil
                  :scan-line? true
                  :merge-seams? true
                  :xrf-scan nil
                  :length-mm 0.0
                  :start-mm 0.0
                  :crop-left 0
                  :crop-right 0
                  :seams []}]
         :selections [{:element "Mn"
                       :max-count 1000}]}))


                 ;; {:optical nil #_{:image nil ;; BoofCV image
                 ;;                  :display nil ;; JFX image
                 ;;                  :unscanned-pixels-left 0
                 ;;                  :unscanned-pixels-right 0
                 ;;                  :scan-line-pixel-offset-from-center 0}
                 ;;  :xrf-scan nil #_{:header
                 ;;                   :columns
                 ;;                   :element-counts
                 ;;                   :missing-steps-left 0

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

(defmethod event-handler ::update-display-image [event]
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

(defmethod event-handler ::reset-core-start [event]
  (let [core-number (:core-number event)
        cores (-> @*state :cores)
        previous-core (-> cores (get (dec core-number)))]
    (println "updating start of core" core-number)
    (swap! *state assoc-in [:cores
                            core-number
                            :start-mm]
           (if (zero? core-number)
             0.0
             (+ (-> previous-core
                    :start-mm)
                (-> previous-core
                    :length-mm))))
    (if (some? (get cores (inc core-number))) ;; reset all subsequent start measurements
      (event-handler {:event/type ::reset-core-start
                      :core-number (inc core-number)}))))

(defn update-core-length
  ""
  [core
   mm-per-pixel]
  (if (nil? (-> core :optical))
    (if (nil? (-> core :xrf-scan))
      (assoc core :length-mm nil)
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
                                   :mm-per-pixel)))
    (event-handler {:event/type ::reset-core-start
                    :core-number core-number})))

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
        (event-handler {:event/type ::update-display-image
                        :core-number core-number})
        (event-handler {:event/type ::update-core-length
                        :core-number core-number})

        (if (-> @*state
                :cores
                (.get core-number)
                :xrf-scan)
          (event-handler {:event/type ::update-unscanned-areas
                          :core-number core-number}))))))

(defn element-counts
  "Given an XRF-SCAN and a ELEMENT
  Returns a vector of [position element-count] pairs
  These can then be plotted"
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
                                                    (keyword element))}))

(defmethod event-handler ::update-max-element-count [event]
  (swap! *state
         update
         :selections
         #(into [] (map (fn [selection]
                           (let [element (:element selection)
                                 cores (-> @*state
                                           :cores)]
                             {:element element
                              :max-count (get-max-count-for-element-in-cores cores
                                                                             (keyword element))}))
                         %))))


(defmethod event-handler ::load-xrf-scan [event]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (if (some? file)
      (let [core-number (:core-number event)
            xrf-scan (sausage.xrf/load-xrf-scan-file file)]
        (swap! *state
               assoc-in [:cores
                         core-number
                         :xrf-scan]
               xrf-scan)
        (event-handler {:event/type ::update-core-length
                        :core-number core-number})
        (event-handler {:event/type ::update-max-element-count})
        (if (-> @*state
                :cores
                (.get core-number)
                :optical)
          (event-handler {:event/type ::update-unscanned-areas
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
           {:optical nil
            :xrf-scan nil
            :crop-left 0
            :crop-right 0
            :length-mm 0.0
            :seams []})
    (event-handler {:event/type ::reset-core-start
                    :core-number num-cores})))

(defmethod event-handler ::remove-core [event]
    (swap! *state assoc :cores
           (pop (:cores @*state)))
  (if (empty? (:cores @*state))
    (event-handler {:event/type ::add-core})))

(defn merge-cores
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
  (event-handler {:event/type ::update-display-image
                  :core-number 0})
  (event-handler {:event/type ::update-core-length
                 :core-number 0}))

(defn workspace-settings-display
  "Top level settings for the workspace where all data will be stored in"
  [{:keys [working-directory
           fixed-left-margin-width
           height]}]
  {:fx/type :h-box
   :children [{:fx/type :button
               :pref-width fixed-left-margin-width
               :min-width fixed-left-margin-width
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
                                                     :end-y height
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
                                                     :end-y height
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
                                                       :end-y height
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
                                                       :end-y height
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


(defn xrf-columns-list ;; TODO: Figure out how the hell this works!
  "List of Elements (and other stuff)"
  [{:keys [items
           selection
           selection-mode
           selection-number
           height]}]
  {:fx/type fx.ext.list-view/with-selection-props
   :props (case selection-mode
            :single (cond-> {:selection-mode :single
                             :on-selected-item-changed {:event/type ::select-single
                                                        :selection-number selection-number}}
                      (seq selection)
                      (assoc :selected-item (-> selection sort first))))
   :desc {:fx/type :list-view
          :cell-factory (fn [path]
                          {:text path})
          :pref-height height
          :max-width 99999
          :items items
          :orientation :vertical}})

(defmethod event-handler ::select-multiple
  [event]
  "multi select not implemented")

(defmethod event-handler ::select-single
  [event]
  (swap! *state assoc-in [:selections 0 :element] (:fx/event event))
  (event-handler {:event/type ::update-max-element-count}))

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
                           [{:fx/type fx/ext-instance-factory
                             :create #(sausage.plot/plot-points width
                                                                height
                                                                (element-counts xrf-scan
                                                                                (keyword selection))
                                                                core-length-mm
                                                                max-element-count
                                                                crop-left
                                                                crop-right
                                                                (if merge-seams?
                                                                  seams
                                                                  [])
                                                                )}]
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

(defmethod event-handler ::crop
  [event]
  (let [core-number (:core-number event) ;; TODO destructure from function argument
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

    (event-handler {:event/type ::update-core-length
                    :core-number core-number})
    (event-handler {:event/type ::update-display-image
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
  ""
  [{:keys [core-number
           width
           height
           optical
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
               :max 0.45
               :min 0.0
               :show-tick-labels false
               :show-tick-marks false
               :pref-width (* 0.45
                              width)
               :min-width (* 0.45
                             width)
               :pref-height height
               :min-height height
               :value crop-left
               :on-value-changed {:event/type ::adjust-left-crop
                                  :core-number core-number}}
              {:fx/type :button
               :pref-width (* 0.10
                              width)
               :pref-height height
               :on-action {:event/type ::crop
                           :core-number core-number}
               :text "Crop"}
              {:fx/type :slider
               :max 1.0
               :min 0.55
               :show-tick-labels false
               :show-tick-marks false
               :pref-width (* 0.45 width)
               :min-width (* 0.45 width)
               :pref-height height
               :min-height height
               :value (- 1 crop-right)
               :on-value-changed {:event/type ::adjust-right-crop
                                  :core-number core-number}}]})

(defmethod event-handler ::update-core-start [event]
  (try
    (let [core-number (:core-number event)
          input-value-mm (-> (:fx/event event)
                             read-string ;;  returns a 'long'
                             double)     ;; 'Math/round' can't take a long...
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
      (event-handler {:event/type ::reset-core-start
                      :core-number (:core-number event)}))))

(defn core-options-display
  ""
  [{:keys [core-number
           width
           height
           start-mm]}]
  {:fx/type :h-box
   :pref-height height
   :min-height height
   :max-height height
   :alignment :center-left
   :children [{:fx/type :text-field
               :editable true
               :pref-height height
               :prompt-text "Core Start (mm)"
               :pref-column-count 9
               :text (str start-mm)
               :on-text-changed {:event/type ::update-core-start
                                 :core-number core-number}
               ;; :value-factory {:fx/type :integer-spinner-value-factory
               ;;                 :value 10
               ;;                 :min 0
               ;;                 :max 100}
               }
              {:fx/type :separator
               :orientation :horizontal}
              {:fx/type :text
               :text "Core Start (mm)"}
              {:fx/type :separator
               :orientation :vertical}]})

(defn core-display
  ""
  [{:keys [width
           full-width?
           height
           scan-line?
           merge-seams?
           fixed-core-options-height
           fixed-optical-scan-height
           fixed-slider-height
           core-number
           directory
           core
           selections]}]
  (let [width (if (or (nil? (:optical core))
                      (not full-width?))
              width
              (-> core :optical :display .getWidth))]
  {:fx/type :v-box
   :children [
              {:fx/type core-options-display
               :core-number core-number
               :width width
               :height fixed-core-options-height
               :start-mm (:start-mm core)}
              {:fx/type optical-image-display
               :core-number core-number
               :scan-line? scan-line?
               :width width
               :height fixed-optical-scan-height
               :optical (:optical core)
               :crop-left  (:crop-left core)
               :crop-right (:crop-right core)}
              {:fx/type crop-slider
               :core-number core-number
               :width width
               :height fixed-slider-height
               :optical (:optical core)
               :crop-left  (:crop-left core)
               :crop-right (:crop-right core)}
              {:fx/type xrf-scan-display
               :core-number core-number
               :height (- height
                          fixed-core-options-height
                          fixed-optical-scan-height
                          fixed-slider-height)
               :width width
               :xrf-scan (:xrf-scan core)
               :selection (:element (get selections 0))
               :max-element-count (:max-count (get selections 0))
               :core-length-mm (:length-mm core)
               :crop-left  (:crop-left core)
               :crop-right (:crop-right core)
               :merge-seams? merge-seams?
               :seams (:seams core)}]}))

(defmethod event-handler ::toggle-full-width
  [event]
  (swap! *state assoc :full-width? (:fx/event event)))

(defmethod event-handler ::toggle-scan-line
  [event]
  (swap! *state assoc :scan-line? (:fx/event event)))

(defmethod event-handler ::toggle-merge-seams
  [event]
  (swap! *state assoc :merge-seams? (:fx/event event)))

(defn margin
  ""
  [{:keys [width
           height
           fixed-core-options-height
           fixed-optical-scan-height
           fixed-slider-height
           elements
           selection
           ]}]
  {:fx/type :v-box
   :pref-width width
   :min-width width
   :max-width width
   :children [{:fx/type :button
;;               :pref-width width
               :pref-height fixed-core-options-height
               :min-height fixed-core-options-height
               :max-height fixed-core-options-height
               :on-action {:event/type ::merge-all-cores}
               :text ">> Merge"}
              {:fx/type :v-box
;;               :alignment :center-left
               :pref-height fixed-optical-scan-height
               :children [{:fx/type :separator}
                          {:fx/type :check-box
                           :text "Full Width"
                           :on-selected-changed {:event/type ::toggle-full-width}}
                          {:fx/type :check-box
                           :text "Scan Line"
                           :on-selected-changed {:event/type ::toggle-scan-line}}
                          {:fx/type :check-box
                           :text "Merge Seams"
                           :on-selected-changed {:event/type ::toggle-merge-seams}}]}
              {:fx/type :h-box
               :alignment :center
               :pref-height fixed-slider-height
               :children [{:fx/type :text
                           :text "-"}]}
              {:fx/type xrf-columns-list
               :items elements
               :selection-mode :single
               :selection selection
               :selection-number 0
               :height (- height
                          fixed-core-options-height
                          fixed-optical-scan-height
                          fixed-slider-height
                          )}]})

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [width
           full-width?
           height
           scan-line?
           merge-seams?
           working-directory
           cores
           selections]}]
  (let [fixed-workspace-settings-height 30
        fixed-left-margin-width 150
        fixed-core-options-height 30
        fixed-optical-scan-height 133.0  ;; needs to be fixed so the core displays line up
        fixed-slider-height 50
        fixed-element-selector-width 50
        core-display-width (if full-width?
                             (- width
                              fixed-left-margin-width)
                             (/ (- width
                                   fixed-left-margin-width)
                                (count cores)))
        core-display-height (- height
                               fixed-workspace-settings-height)]
    {:fx/type :stage
     :title "Sausage Scanner"
      :showing true
      :scene {:fx/type :scene
              :on-width-changed {:event/type ::width-changed}
              :on-height-changed {:event/type ::height-changed}
              :root {:fx/type :v-box
                     :children[{:fx/type workspace-settings-display
                                :working-directory working-directory
                                :fixed-left-margin-width fixed-left-margin-width
                                :height fixed-workspace-settings-height}
                               {:fx/type :h-box
                                :children [{:fx/type :scroll-pane
                                            :hbar-policy :never
                                            :vbar-policy :never
                                            :content {:fx/type :h-box
                                                      :children (into [] (map-indexed (fn [index core]
                                                                               {:fx/type core-display
                                                                                :core-number index
                                                                                :scan-line? scan-line?
                                                                                :merge-seams? merge-seams?
                                                                                :width core-display-width
                                                                                :full-width? full-width?
                                                                                :height core-display-height
                                                                                :fixed-core-options-height fixed-core-options-height
                                                                                :fixed-optical-scan-height fixed-optical-scan-height
                                                                                :fixed-slider-height fixed-slider-height
                                                                                :core core
                                                                                :selections selections})
                                                                                      cores))}}
                                           {:fx/type margin
                                            :width fixed-left-margin-width
                                            :height core-display-height
                                            :fixed-core-options-height fixed-core-options-height
                                            :fixed-optical-scan-height fixed-optical-scan-height
                                            :fixed-slider-height fixed-slider-height
                                            :elements (map name
                                                           (:columns (:xrf-scan (get cores 0))))
                                            :selection (:element (get selections 0))}]}]}}}))

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-handler}))


(defn -main [& args]
  (fx/mount-renderer
   *state
   renderer))
