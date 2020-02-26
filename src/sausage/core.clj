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
         :options-width 100
         :display-width 300
         :optical-image nil ;; BoofCV image
         :display-image nil ;; JFX image
         :scan-line-pixel-offset-from-center 0
         :xrf-scan nil
         :crop-right 0.2 ;; what fraction off the "right" isn't part of the core
         :crop-left 0.0
         :selection "Mn"
         :max-element-count 0

         :merge-optical-image nil
         :merge-display-image nil
         :merge-xrf-scan nil
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
  (swap! *state assoc :display-width (- (:fx/event event)
                                        (:options-width @*state))))
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

(defmethod event-handler ::update-primary-display-image [_]
  (let [optical (-> @*state
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
    (swap! *state assoc :display-image (-> @*state
                                           :optical-image
                                           to-fx-image))))


(defmethod event-handler ::update-merge-display-image [_]
  (let [optical (-> @*state
                    :merge-optical-image)]
    (-> optical
        (.getBand 0)
        ImageMiscOps/flipHorizontal)
    (-> optical
        (.getBand 1)
        ImageMiscOps/flipHorizontal)
    (-> optical
        (.getBand 2)
        ImageMiscOps/flipHorizontal)
    (swap! *state assoc :merge-display-image (-> @*state
                                                 :merge-optical-image
                                                 to-fx-image))))


;; File Picker copied from here:
;; https://github.com/cljfx/cljfx/pull/40#issuecomment-544256262
;;
;; User has opted to load a new project. Use FileChooser so they can select the file.
;; Then trigger the central project loading event, passing in the selected file.
(defmethod event-handler ::load-primary-optical-image [_]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))
        optical-image (load-image file)]
    (swap! *state assoc-in [:optical-image] optical-image)
    (event-handler {:event/type ::update-primary-display-image})
    (event-handler {:event/type ::update-max-element-count})))

(defmethod event-handler ::load-merge-optical-image [_]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))
        optical-image (load-image file)]
    (swap! *state assoc-in [:merge-optical-image] optical-image)
    (event-handler {:event/type ::update-merge-display-image})))


(defn load-xrf-scan
  [csv-file]  
  ;; (def table (clojure.data.csv/read-csv (clojure.java.io/reader (str (:directory @*state) "BatchAO96-12-0-134cm-Mo.txt")) :separator \tab))

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

:max-element-count 0

(defn element-counts
  [xrf-scan
   element]
  (map #(vector (read-string (:position %))
                (read-string (element %)))
       (:data xrf-scan)))

(defmethod event-handler ::update-max-element-count [_]
  ;;  @(fx/on-fx-thread
  (println "updating max element")
  (swap! *state assoc :max-element-count (apply max (map second (element-counts (-> @*state
                                                                                    :xrf-scan)
                                                                                (keyword (-> @*state
                                                                                             :selection)))))))


(defmethod event-handler ::load-primary-xrf-scan [_]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (swap! *state assoc :xrf-scan (load-xrf-scan file))))

(defmethod event-handler ::load-merge-xrf-scan [_]
  ;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (swap! *state assoc :merge-xrf-scan (load-xrf-scan file))))


(defn end-position
  [data-table]
  (read-string (:position (last data-table))))

(defn workspace-settings-display
  "Top level settings for the workspace where all data will be stored in"
  [{:keys [options-width
           working-directory]}]
  {:fx/type :h-box
   ;; :width width
   :children [{:fx/type :button
               :pref-width options-width
               :min-width options-width
               :pref-height 40
               :on-action {:event/type ::set-working-directory}
               :text "Set"}
              {:fx/type :text-field
               :editable false
               :pref-height 40
               :prompt-text "Select a working directory.."
               :pref-column-count 100
               :text working-directory}]})

(defn optical-image-display
  "display and options for the optical image"
  [{:keys [display-width
           options-width
           display-image
           crop-right
           scan-line-pixel-offset-from-center
           load-event]}]
  {:fx/type :h-box
   :children (filter identity
                     [{:fx/type :v-box
                       :children [{:fx/type :button
                                   :on-action {:event/type load-event}
                                   :pref-width options-width
                                   :min-width options-width
                                   :text "Load image"}]}
                      (if display-image
                        (let [image-height (.getHeight display-image)]
                          {:fx/type :group
                           :children [{:fx/type :image-view
                                       :fit-width display-width
                                       :image display-image}
                                      {:fx/type :line
                                       :start-x 0
                                       :start-y (/ image-height
                                                   2.0)
                                       :end-x display-width
                                       :end-y (/ image-height
                                                 2.0)
                                       :stroke-dash-array [10 10]
                                       :stroke "white"}
                                      {:fx/type :rectangle
                                       :x ( - display-width
                                           (* crop-right
                                              display-width))
                                       :y 0
                                       :height image-height
                                       :width (* crop-right
                                                 display-width)
                                       :opacity 0.15
                                       :fill "red"}
                                      ]}))])})


(defn xrf-columns-list
  "List of Elements (and other stuff)"
  [{:keys [items
           selection
           selection-mode
           options-width]}]
  {:fx/type fx.ext.list-view/with-selection-props
   :props (case selection-mode
            :single (cond-> {:selection-mode :single
                             :on-selected-item-changed {:event/type ::select-single}}
                      (seq selection)
                      (assoc :selected-item (-> selection sort first))))
   :desc {:fx/type :list-view
          :cell-factory (fn [path]
                          {:text path})
          :max-width 99999
          :items items
          :orientation :horizontal}})
(defmethod event-handler ::select-multiple
  [event]
  "multi select not implemented")

(defmethod event-handler ::select-single
  [event]
  (swap! *state assoc :selection (:fx/event event))
  (event-handler {:event/type ::update-max-element-count}))


(defn xrf-scan-display
  "display and options for XRF scan data"
  [{:keys [display-width
           options-width
           height
           xrf-scan
           selection
           crop-right
           load-event
           max-element-count]}]
  {:fx/type :h-box
   :children (filter identity
                     [{:fx/type :v-box
                       :children [{:fx/type :button
                                   :on-action {:event/type load-event}
                                   :pref-width options-width
                                   :min-width options-width
                                   :text "Load XRF Scan"}
                                  #_{:fx/type xrf-columns-list
                                     :items (map name (:columns (:xrf-scan @*state)))
                                     :selection-mode :single
                                     :selection selection
                                     :pref-width options-width
                                     :min-width options-width}]}
                      (if xrf-scan
                        {:fx/type fx/ext-instance-factory
                         :create #(sausage.plot/plot-points display-width
                                                            (- height 50)
                                                            (element-counts xrf-scan (keyword selection))
                                                            (end-position (:data xrf-scan))
                                                            max-element-count ;;(apply max (map second (element-counts xrf-scan (keyword selection))))
                                                            crop-right
                                                            )})])})


(defn crop-slider
  ""
  [{:keys [display-width
           options-width
           crop-right]}]
  (let [slider-height 10]
    {:fx/type :h-box
     :children (filter identity
                       [{:fx/type :v-box
                         :children [{:fx/type :button
                                     :pref-height slider-height
                                     :on-action {:event/type ::auto-crop}
                                     :pref-width options-width
                                     :min-width options-width
                                     :text "Auto"}]}
                        {:fx/type :slider
                         :max 1.0
                         :min 0.0
                         :show-tick-labels false
                         :show-tick-marks false
                         :pref-height slider-height
                         :pref-width display-width
                         :min-width display-width
                         :value (- 1 crop-right)
                         :on-value-changed {:event/type ::adjust-crop}}
                        ])}))
(defmethod event-handler ::auto-crop
  [event]
  (println "Magic not implemented yet")
  (swap! *state assoc :crop-right 0))

(defmethod event-handler ::adjust-crop
  [event]
  (swap! *state assoc :crop-right (- 1 (:fx/event event))))




(defn primary-display
  ""
  [{:keys [width
           display-width
           options-width
           height
           directory
           display-image
           xrf-scan
           selection
           crop-right
           scan-line-pixel-offset-from-center
           max-element-count]}]
  {:fx/type :v-box
   :children [
              {:fx/type optical-image-display
               :options-width options-width
               :display-width display-width
               :display-image display-image
               :crop-right crop-right
               :scan-line-pixel-offset-from-center scan-line-pixel-offset-from-center
               :load-event ::load-primary-optical-image}
              {:fx/type crop-slider
               :options-width options-width
               :display-width display-width
               :crop-right crop-right}
              {:fx/type xrf-scan-display
               :height (if display-image
                         (- height
                            (.getHeight display-image)
                            100 ;; workspace setting fixed size
                            )
                         height)
               :options-width options-width
               :display-width display-width
               :xrf-scan xrf-scan
               :selection selection
               :crop-right crop-right
               :load-event ::load-primary-xrf-scan
               :max-element-count max-element-count}
              ]})

(defn secondary-merge-display
  ""
  [{:keys [width
           display-width
           options-width
           height
           directory
           display-image
           xrf-scan
           selection
           crop-right
           scan-line-pixel-offset-from-center
           max-element-count]}]
  {:fx/type :v-box
   :children [
              {:fx/type optical-image-display
               :options-width options-width
               :display-width display-width
               :display-image display-image
               :crop-right crop-right
               :scan-line-pixel-offset-from-center scan-line-pixel-offset-from-center
               :load-event ::load-merge-optical-image}
              {:fx/type crop-slider
               :options-width options-width
               :display-width display-width
               :crop-right crop-right}
              {:fx/type xrf-scan-display
               :height (if display-image
                         (- height
                            (.getHeight display-image)
                            100 ;; workspace setting fixed size
                            )
                         height)
               :options-width options-width
               :display-width display-width
               :xrf-scan xrf-scan
               :selection selection
               :crop-right crop-right
               :load-event ::load-merge-xrf-scan
               :max-element-count max-element-count}]})

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [width
           display-width
           options-width
           height
           working-directory
           display-image
           xrf-scan
           selection
           crop-right
           scan-line-pixel-offset-from-center
           merge-optical-image
           merge-display-image
           merge-xrf-scan
           max-element-count]}]
  {:fx/type :stage
   :showing true
   :on-width-changed {:event/type ::width-changed}
   :on-height-changed {:event/type ::height-changed}
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children[{:fx/type workspace-settings-display
                             :options-width options-width
                             :working-directory working-directory}
                            {:fx/type :h-box
                             :children [{:fx/type primary-display
                                         :width width
                                         :display-width (/ display-width 2.0)
                                         :options-width options-width
                                         :height height
                                         :display-image display-image
                                         :xrf-scan xrf-scan
                                         :selection selection
                                         :crop-right crop-right
                                         :scan-line-pixel-offset-from-center scan-line-pixel-offset-from-center
                                         :max-element-count max-element-count}
                                        {:fx/type secondary-merge-display
                                         :width width
                                         :display-width (/ display-width 2.0)
                                         :options-width options-width
                                         :height height
                                         :display-image merge-display-image
                                         :xrf-scan merge-xrf-scan
                                         :selection selection
                                         :crop-right crop-right
                                         :scan-line-pixel-offset-from-center scan-line-pixel-offset-from-center
                                         :max-element-count max-element-count}
                                        ]}

                            {:fx/type xrf-columns-list
                             :items (map name (:columns xrf-scan))
                             :selection-mode :single
                             :selection selection
                             :heigth 20
                             :pref-width options-width
                             :min-width options-width}
                            ]}}})


(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-handler}))

(fx/mount-renderer
 *state
 renderer)

;; Technically shouldn't be necessary
;; But I get weird artifacts unless i rereun this here
(renderer)

