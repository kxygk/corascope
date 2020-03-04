(ns sausage.core
  (:require
   [sausage.plot]
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [clojure.data.csv]
   [clojure.java.io])
  (:import [javafx.scene.input KeyCode KeyEvent]
           boofcv.io.image.ConvertBufferedImage
           boofcv.io.image.UtilImageIO
           boofcv.struct.image.ImageType
           boofcv.core.image.ConvertImage
           boofcv.struct.image.GrayU8
           boofcv.alg.misc.ImageMiscOps
           javafx.embed.swing.SwingFXUtils
           ;;           javafx.scene.paint Color
           javafx.stage.DirectoryChooser
           javafx.stage.FileChooser
           java.io.File
           java.io.FileWriter
           javafx.stage.Stage
           javax.imageio.ImageIO

           ))

(def *state
  ""
  (atom {:working-directory ""
         :width 400
         :height 400
         :display-width 300
         :cores [
                 {:optical-image nil ;; BoofCV image
                  :display-image nil ;; JFX image
                  :scan-line-pixel-offset-from-center 0
                  :xrf-scan nil
                  :crop-right 0.2 ;; what fraction off the "right" isn't part of the core
                  :crop-left 0.0}
                 {:optical-image nil ;; BoofCV image
                  :display-image nil ;; JFX image
                  :scan-line-pixel-offset-from-center 0
                  :xrf-scan nil
                  :crop-right 0.2 ;; what fraction off the "right" isn't part of the core
                  :crop-left 0.0}
                 ]
         :selections [{:element "Mn"
                       :max-count 1000}]
         }))

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
  (swap! *state assoc :width (:fx/event event))
  (swap! *state assoc :display-width (:fx/event event)))
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

(defn save-image
  ""
  [directory
   fx-image
   name]
  (ImageIO/write (SwingFXUtils/fromFXImage fx-image
                                           nil)
                 "tiff"
                 (File. (str directory
                             "/" name))))
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

(defn save-to-working-directory
  "Saves the current 'state' to the working directory (optical image, data table etc.)"
  []
  (let [working-directory (-> @*state
                              :working-directory)
        optical-image (->  @*state
                           :optical-image)
        xrf-scan (-> @*state
                     :xrf-scan )]
    (if optical-image (save-image working-directory
                                  optical-image
                                  "optical.tiff"))
    (if xrf-scan (save-xrf-scan working-directory
                                xrf-scan
                                "xrf-scan.txt"))))


(defn to-fx-image
  [boofcv-image]
  (-> boofcv-image
      (ConvertBufferedImage/convertTo nil true)
      (SwingFXUtils/toFXImage nil)))

(defn load-image
  [file]
  (println "loading image")
  (println file)
  (-> file
      (.getCanonicalPath)
      (boofcv.io.image.UtilImageIO/loadImage)
      ;;      (ImageMiscOps/flipHorizontal)
      (ConvertBufferedImage/convertFrom true
                                        (ImageType/pl 3
                                                      GrayU8))))

(defmethod event-handler ::update-display-image [event]
  (let [core-number (:core-number event)
        optical (-> @*state
                    :cores
                    (get core-number)
                    :optical-image)]
    (-> optical
        (.getBand 0)
        ImageMiscOps/flipHorizontal)
    (-> optical
        (.getBand 1)
        ImageMiscOps/flipHorizontal)
    (-> optical
        (.getBand 2)
        ImageMiscOps/flipHorizontal)
    (swap! *state assoc-in [:cores
                            core-number
                            :display-image]
           (to-fx-image optical))))

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
                 (.showOpenDialog (Stage.)))
        optical-image (load-image file)
        core-number (:core-number event)]
    (swap! *state
           assoc-in [:cores
                     core-number
                     :optical-image]
           optical-image)
    (event-handler {:event/type ::update-display-image
                    :core-number core-number})))

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

(defn element-counts
  [xrf-scan
   element]
  (map #(vector (read-string (:position %))
                (read-string (element %)))
       (:data xrf-scan)))

(defmethod event-handler ::update-max-element-count [event]
  ;;  @(fx/on-fx-thread
  (println "updating max element")
  (swap! *state
         assoc-in [:selections
                   (:selection-number event)
                   :max-count]
         (apply max (map second (element-counts (-> @*state
                                                    :cores
                                                    (get 0)
                                                    :xrf-scan)
                                                (keyword (-> @*state
                                                             :selections
                                                             (get 0)
                                                             :element)))))))

(defmethod event-handler ::load-xrf-scan [event]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (swap! *state
           assoc-in [:cores
                     (:core-number event)
                     :xrf-scan]
           (load-xrf-scan-file file))
    ))

(defn end-position
  [data-table]
  (read-string (:position (last data-table))))

(defmethod event-handler ::add-core [event]
  (let [num-cores (-> @*state
                      :cores
                      count)]
    (swap! *state assoc-in [:cores
                            num-cores]
           {:optical-image nil ;; BoofCV image
                  :display-image nil ;; JFX image
                  :scan-line-pixel-offset-from-center 0
                  :xrf-scan nil
                  :crop-right 0.0
                  :crop-left 0.0})))

(defmethod event-handler ::remove-core [event]
    (swap! *state assoc :cores
           (pop (:cores @*state)))
  (if (empty? (:cores @*state))
    (event-handler {:event/type ::add-core})))

(defn workspace-settings-display
  "Top level settings for the workspace where all data will be stored in"
  [{:keys [working-directory
           fixed-left-margin-width
           height]}]
  {:fx/type :h-box
   ;; :width width
   :children [{:fx/type :button
               :pref-width fixed-left-margin-width
               :min-width fixed-left-margin-width
               :pref-height height
               :on-action {:event/type ::set-working-directory}
               :text "Set"}
              {:fx/type :text-field
               :editable false
               :pref-height height
               :prompt-text "Select a working directory.."
               :pref-column-count 999
               :text working-directory}
              {:fx/type :button
               :pref-height height
               :pref-width height ;; make square button
               :min-width height
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
           width
           height
           display-image
           crop-right
           crop-left
           scan-line-pixel-offset-from-center]}]
  {:fx/type :h-box
   :pref-height height
   :children (if display-image
                 [{:fx/type :group
                  :children [{:fx/type :image-view
                              :fit-width width
                              :fit-height height
                              :image display-image}
                             ;; Center Line
                             {:fx/type :line
                              :start-x 0
                              :start-y (/ height
                                          2.0)
                              :end-x width
                              :end-y (/ height
                                        2.0)
                              :stroke-dash-array [10 10]
                              :stroke "white"}
                             ;; Right Crop
                             {:fx/type :line
                              :start-x ( - width
                                        (* crop-right
                                           width))
                              :start-y 0
                              :end-x ( - width
                                      (* crop-right
                                         width))
                              :end-y height
                              :stroke "red"}
                             {:fx/type :rectangle
                              :x ( - width
                                  (* crop-right
                                     width))
                              :y 0
                              :height height
                              :width (* crop-right
                                        width)
                              :opacity 0.05
                              :fill "red"}
                             ;; Left Crop
                             {:fx/type :line
                              :start-x (* crop-left
                                          width)
                              :start-y 0
                              :end-x (* crop-left
                                        width)
                              :end-y height
                              :stroke "red"}
                             {:fx/type :rectangle
                              :x 0
                              :y 0
                              :height height
                              :width (* crop-left
                                        width)
                              :opacity 0.05
                              :fill "red"}]}]
                 [{:fx/type :v-box
                   :children [{:fx/type :button
                               :pref-width width
                               :pref-height height
                               :on-action {:event/type ::load-optical-image
                                           :core-number core-number}
                               :text "Load image"}]}])})


(defn xrf-columns-list
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
  (event-handler {:event/type ::update-max-element-count
                  :selection-number (:selection-number event)}))


(defn xrf-scan-display
  "display and options for XRF scan data"
  [{:keys [core-number
           width
           height
           xrf-scan
           selection
           crop-right
           crop-left
           load-event
           max-element-count]}]
  {:fx/type :h-box
   :children [{:fx/type :v-box
               :children (if xrf-scan
                           [{:fx/type fx/ext-instance-factory
                             :create #(sausage.plot/plot-points width
                                                                height
                                                                (element-counts xrf-scan (keyword selection))
                                                                (end-position (:data xrf-scan))
                                                                (* max-element-count
                                                                   1.3)
                                                                crop-right
                                                                crop-left
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

(defn crop-slider
  ""
  [{:keys [core-number
           width
           height
           crop-right
           crop-left]}]
  {:fx/type :h-box
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
               :on-action {:event/type ::load-xrf-scan
                           :core-number core-number}
               :text "Crop"}
              {:fx/type :slider
               :max 1.0
               :min 0.55
               :show-tick-labels false
               :show-tick-marks false
               :pref-width (* 0.45 width) ;;width
               :min-width (* 0.45 width) ;;width
               :pref-height height
               :min-height height
               :value (- 1 crop-right)
               :on-value-changed {:event/type ::adjust-right-crop
                                  :core-number core-number}}]})

(defn core-display
  ""
  [{:keys [width
           height
           fixed-optical-scan-height
           fixed-slider-height
           core-number
           height
           directory
           core
           selections]}]
  {:fx/type :v-box
   :children [
              {:fx/type optical-image-display
               :core-number core-number
               :width width
               :height fixed-optical-scan-height
               :display-image (:display-image core)
               :crop-right (:crop-right core)
               :crop-left (:crop-left core)
               :scan-line-pixel-offset-from-center (:scan-line-pixel-offset-from-center core)}
              {:fx/type crop-slider
               :core-number core-number
               :width width
               :height fixed-slider-height
               :crop-right (:crop-right core)
               :crop-left (:crop-left core)}
              {:fx/type xrf-scan-display
               :core-number core-number
               :height (- height
                          fixed-optical-scan-height
                          fixed-slider-height)
               :width width
               :xrf-scan (:xrf-scan core)
               :selection (:element (get selections 0))
               :crop-right (:crop-right core)
               :crop-left (:crop-left core)
               :load-event ::load-primary-xrf-scan
               :max-element-count (:max-count (get selections 0))}
              ]})

(defn left-margin
  ""
  [{:keys [width
           height
           fixed-optical-scan-height
           fixed-slider-height
           elements
           selection
           ]}]
  {:fx/type :v-box
   :pref-width width
   :pref-height fixed-optical-scan-height
   :children [{:fx/type :h-box
               :alignment :center
               :pref-height fixed-optical-scan-height
               :children [{:fx/type :text
                          :text "Optical"}]}
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
                          fixed-optical-scan-height
                          fixed-slider-height)}]})

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [width
           display-width
           height
           working-directory
           cores
           selections]}]
  (let [fixed-workspace-settings-height 50
        fixed-left-margin-width 150
        fixed-optical-scan-height 133.0  ;; needs to be fixed so the core displays line up
        fixed-slider-height 50
        fixed-element-selector-width 50
        core-display-width (/ (- width
                                 fixed-left-margin-width)
                              (count cores))
        core-display-height (- height
                               fixed-workspace-settings-height)]
    {:fx/type :stage
      :showing true
      :on-width-changed {:event/type ::width-changed}
      :on-height-changed {:event/type ::height-changed}
      :scene {:fx/type :scene
              :root {:fx/type :v-box
                     :children[{:fx/type workspace-settings-display
                                :working-directory working-directory
                                :fixed-left-margin-width fixed-left-margin-width
                                :height fixed-workspace-settings-height}
                               {:fx/type :h-box
                                :children (into []
                                                (concat [{:fx/type left-margin
                                                          :width fixed-left-margin-width
                                                          :height core-display-height
                                                          :fixed-optical-scan-height fixed-optical-scan-height
                                                          :fixed-slider-height fixed-slider-height
                                                          :elements (map name
                                                                         (:columns (:xrf-scan (get cores 0))))
                                                          :selection (:element (get selections 0))}]
                                                         (map-indexed (fn [index core]
                                                                        {:fx/type core-display
                                                                         :core-number index
                                                                         :width core-display-width
                                                                         :height (- height
                                                                                    fixed-workspace-settings-height)
                                                                         :fixed-optical-scan-height fixed-optical-scan-height
                                                                         :fixed-slider-height fixed-slider-height
                                                                         :core core
                                                                         :selections selections})
                                                                      (:cores @*state))))}]}}}))

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-handler}))

(fx/mount-renderer
 *state
 renderer)
