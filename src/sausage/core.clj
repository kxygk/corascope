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

;; (let [image-url  "https://geokon-gh.github.io/web/chengdu.jpg"
;;       color (ConvertBufferedImage/convertFrom (UtilImageIO/loadImage image-url)
;;                                               true,
;;                                               (ImageType/pl 3 GrayU8))]
;;   (SwingFXUtils/toFXImage (UtilImageIO/loadImage image-url)
;;                           nil))


(def *state
  ""
  (atom {:working-directory ""
         :width 400
         :height 400
         :options-width 100
         :display-width 300
         :optical {:optical-image nil
                   :display-image nil ;; optical image + overlays
                   :scan-line-pixel-offset-from-center 0
                   :scan-line-pixel-average nil
                   }
         :xrf-scan nil
         :crop-right 0.2 ;; what fraction off the "right" isn't part of the core
         :selection "Mn"}))

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
  (let [working-directory (:working-directory @*state)
        optical-image (:optical-image (:optical @*state))
        xrf-scan (:xrf-scan @*state)]
    (if optical-image (save-image working-directory
                                  optical-image
                                  "optical.tiff"))
    (if xrf-scan (save-xrf-scan working-directory
                                xrf-scan
                                "xrf-scan.txt"))))

(defn add-scan-line
  "TODO"
  [image-input
   scan-line-pixel-offset-from-center]
  (let [image (.clone image-input)
        height (.getHeight image)
        width (.getWidth image)
        scan-line-height (Math/ceil (/ height 2))
        pixel-x (range 0
                       width)]
    (run! #(if (> 10 (mod % 50))
             (do (.set (.getBand image
                                 0)
                       %
                       (+ scan-line-height
                          scan-line-pixel-offset-from-center)
                       255)
                 (.set (.getBand image
                                 1)
                       %
                       (+ scan-line-height
                          scan-line-pixel-offset-from-center)
                       0)
                 (.set (.getBand image
                                 2)
                       %
                       (+ scan-line-height
                          scan-line-pixel-offset-from-center)
                       0)))
          pixel-x)
    image))

(defn make-region-more-red
  "Makes the region more red.."
  [start-x
   end-x
   start-y
   end-y
   image]
  (run!
   #(let [current-pixel-value (.get (.getBand image
                                              0)
                                    (:x %)
                                    (:y %))
          new-pixel-value (+ current-pixel-value
                             (/ (- 255
                                   current-pixel-value)
                                3))]
      (.set (.getBand image
                      0)
            (:x %)
            (:y %)
            new-pixel-value))
  (for [x (range start-x end-x)
        y (range start-y end-y)]
    {:x x :y y})))


(defn add-crop-region
  "TODO"
  [image
   crop-right]
  (let [width (.getWidth image)
        height (.getHeight image)]
    (make-region-more-red (- width
                             (* width
                                crop-right))
                          width
                          0
                          height
                          image))
  image)

(defn add-overlay
  "TODO"
  [image
   scan-line-pixel-offset-from-center
   crop-right]
  (-> image
      (add-scan-line scan-line-pixel-offset-from-center)
      (add-crop-region crop-right)))

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
      (ConvertBufferedImage/convertFrom true
                                        (ImageType/pl 3
                                                      GrayU8))))

(defmethod event-handler ::update-display-image [_]
  (swap! *state assoc-in [:optical :display-image] (to-fx-image (add-overlay (-> @*state
                                                                                 :optical
                                                                                 :optical-image)
                                                                             (-> @*state
                                                                                 :optical
                                                                                 :scan-line-pixel-offset-from-center)
                                                                             (-> @*state
                                                                                 :crop-right)))))

;; File Picker copied from here:
;; https://github.com/cljfx/cljfx/pull/40#issuecomment-544256262
;;
;; User has opted to load a new project. Use FileChooser so they can select the file.
;; Then trigger the central project loading event, passing in the selected file.
(defmethod event-handler ::load-new-optical-image [_]
;;  @(fx/on-fx-thread
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   #_(.setInitialDirectory (File. "/home/")))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))
        optical-image (load-image file)]
    (swap! *state assoc-in [:optical :optical-image] optical-image)
    (event-handler {:event/type ::update-display-image})))


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
        

(defmethod event-handler ::load-new-xrf-scan [_]
;;  @(fx/on-fx-thread
    (let [file (-> (doto (FileChooser.)
                     (.setTitle "Open a project")
                     #_(.setInitialDirectory (File. "/home/")))
                   ;; Could also grab primary stage instance to make this dialog blocking
                   (.showOpenDialog (Stage.)))]
      (swap! *state assoc :xrf-scan (load-xrf-scan file))))


(defn element-counts
  [xrf-scan
   element]
 (map #(vector (read-string (:position %))
               (read-string (element %)))
      (:data xrf-scan)))

(defn end-position
  [data-table]
  (read-string (:position (last data-table))))

(def table (clojure.data.csv/read-csv (clojure.java.io/reader (str (:directory @*state) "BatchAO96-12-0-134cm-Mo.txt")) :separator \tab))

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
           display-image]}]
  {:fx/type :h-box
   :children (filter identity
                     [{:fx/type :v-box
                       :children [{:fx/type :button
                                   :on-action {:event/type ::load-new-optical-image}
                                   :pref-width options-width
                                   :min-width options-width
                                   :text "Load image"}]}
                      (if display-image
                        {:fx/type :group
                         :children [{:fx/type :image-view
                                        :fit-width display-width
                                        :image display-image}
                                    #_{:fx/type :rectangle
                                     :width 20
                                     :opacity 0.2
                                     :fill "red"
                                     :height 20}]})])})

(defmethod event-handler ::select-multiple
  [event]
  "multi select not implemented")

(defmethod event-handler ::select-single
  [event]
  (swap! *state assoc :selection (:fx/event event)))
  
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
;;          :pref-width options-width
;;          :min-width options-width
          :max-width 99999
          :items items}})



(defn xrf-scan-display
  "display and options for XRF scan data"
  [{:keys [display-width
           options-width
           height
           xrf-scan
           selection
           crop-right]}]
  {:fx/type :h-box
   :children (filter identity
                     [{:fx/type :v-box
                       :children [{:fx/type :button
                                   :on-action {:event/type ::load-new-xrf-scan}
                                   :pref-width options-width
                                   :min-width options-width
                                   :text "Load XRF Scan"}
                                  {:fx/type xrf-columns-list
                                   :items (map name (:columns (:xrf-scan @*state)))
                                   :selection-mode :single
                                   :selection selection
                                   :pref-width options-width
                                   :min-width options-width}]}
                      (if xrf-scan
                        {:fx/type fx/ext-instance-factory
                         :create #(sausage.plot/plot-points display-width
                                                            height
                                                            (element-counts xrf-scan (keyword selection))
                                                            (end-position (:data xrf-scan))
                                                            (apply max (map second (element-counts xrf-scan (keyword selection))))
                                                            crop-right
                                                            )})])})


(defmethod event-handler ::auto-crop
  [event]
  (print "auto-slide!!")
  (swap! *state assoc :crop-right 0))

(defmethod event-handler ::adjust-crop
  [event]
  (print "slide!!")
  (swap! *state assoc :crop-right (- 1 (:fx/event event)))
  (event-handler {:event/type ::update-display-image}))

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

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [width
           display-width
           options-width
           height
           directory
           working-directory
           optical
           xrf-scan
           selection
           crop-right]}]
  {:fx/type :stage
   :showing true
   :on-width-changed {:event/type ::width-changed}
   :on-height-changed {:event/type ::height-changed}
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children[{:fx/type workspace-settings-display
                             :options-width options-width
                             :working-directory working-directory}
                            {:fx/type optical-image-display
                             :options-width options-width
                             :display-width display-width
                             :display-image (:display-image optical)}
                           {:fx/type crop-slider
                             :options-width options-width
                             :display-width display-width
                             :crop-right crop-right}
                            {:fx/type xrf-scan-display
                             :height (if (:optical-image optical)
                                       (- height
                                          (.getHeight (:optical-image optical))
                                          50 ;; workspace setting fixed size
                                          )
                                       height)
                             :options-width options-width
                             :display-width display-width
                             :xrf-scan xrf-scan
                             :selection selection
                             :crop-right crop-right}
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

;; (-> "/media/geokon/USB/Projects/sausage/resources/AO96-12pc/AO96-12-PC1-0-134cm-Mo/optical.tif"
;;     (UtilImageIO/loadImage)
;;     (ConvertBufferedImage/convertFrom true (ImageType/single Color3_F32))
;;     (ImageMiscOps/flipHorizontal)
;;     (ConvertBufferedImage/convertTo_F32 nil true)
;;     (SwingFXUtils/toFXImage nil))
