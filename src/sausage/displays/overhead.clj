(ns sausage.displays.overhead
  (:require
   [sausage.state :as state]
   [cljfx.api :as fx])
  (:import boofcv.io.image.ConvertBufferedImage
           boofcv.alg.misc.ImageMiscOps
           javafx.embed.swing.SwingFXUtils))

(def fixed-height 133.0)

(defn create
  []
  {:type :overhead
   :height fixed-height
   :scan-line? true})

(defn- to-fx-image
  [boofcv-image]
  (-> boofcv-image
      (ConvertBufferedImage/convertTo nil true)
      (SwingFXUtils/toFXImage nil)))

(defn- flip-image
  [boofcv-image]
  (let [to-flip (.clone boofcv-image)]
    (-> to-flip
        (.getBand 0)
        ImageMiscOps/flipHorizontal)
    (-> to-flip
        (.getBand 1)
        ImageMiscOps/flipHorizontal)
    (-> to-flip
        (.getBand 2)
        ImageMiscOps/flipHorizontal)
    to-flip))

(defn- display-image
  [context
   core-number]
  (-> (fx/sub context
              state/optical-image
              core-number)
      flip-image
      to-fx-image))

(defn- scan-line?
  [context
   display-number]
  (:scan-line? (fx/sub context
                       state/get-display
                       display-number)))

(defn view
  "display and options for the optical image"
  [{:keys [fx/context
           core-number
           display-number
           width]}] ;; TODO Make width calculated here
  (let [height (fx/sub context
                       state/display-height
                       display-number)]
    (if (nil? (fx/sub context
                      state/optical-scan
                      core-number))
      {:fx/type :v-box
       :children [{:fx/type :button
                   :pref-width width
                   :pref-height height
                   :on-action {:core-number core-number
                               :effect sausage.optical/load-data}
                   :text "Load image"}]}
      (let [zoom-factor (/ width
                           (fx/sub context
                                   state/optical-scan-length-pix
                                   core-number))
            crop-left-pix (* zoom-factor
                             (fx/sub context
                                     state/crop-left-pix
                                     core-number))
            crop-right-pix (* zoom-factor
                              (fx/sub context
                                      state/crop-right-pix
                                      core-number))
            unscanned-left-pix (* zoom-factor
                                  (fx/sub context
                                          state/unscanned-left-pix
                                          core-number))
            unscanned-right-pix (* zoom-factor
                                   (fx/sub context
                                           state/unscanned-right-pix
                                           core-number))]
        {:fx/type :group
         :children [{:fx/type :pane
                     :children (filter identity
                                       [{:fx/type :image-view
                                         :fit-width width
                                         :fit-height height
                                         :image (fx/sub context
                                                        display-image
                                                        core-number)}
                                        ;; Center Line
                                        (if (true? (fx/sub context
                                                           scan-line?
                                                           display-number))
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
                                        (if (pos? unscanned-left-pix)
                                          {:fx/type :line
                                           :start-x unscanned-left-pix
                                           :start-y 0
                                           :end-x unscanned-left-pix
                                           :end-y (dec height) ;; this is inclusive!
                                           :stroke "brown"})
                                        (if (pos? unscanned-left-pix)
                                          {:fx/type :rectangle
                                           :x 0
                                           :y 0
                                           :height height
                                           :width unscanned-left-pix
                                           :opacity 0.10
                                           :fill "yellow"})
                                        ;; Right Unscanned Area
                                        (if (pos? unscanned-right-pix)
                                          {:fx/type :line
                                           :start-x (- width
                                                       unscanned-right-pix);;right-width
                                           :start-y 0
                                           :end-x (- width
                                                     unscanned-right-pix);;right-width
                                           :end-y (dec height) ;; this is inclusive!
                                           :stroke "brown"})
                                        (if (pos? unscanned-right-pix)
                                          {:fx/type :rectangle
                                           :x (- width
                                                 unscanned-right-pix)
                                           :y 0
                                           :height height
                                           :width  unscanned-right-pix
                                           :opacity 0.10
                                           :fill "yellow"})
                                        ])}]}))))


(defn scan-line?
  [context
   display-number]
  (:scan-line? (fx/sub context
                       state/get-display
                       display-number)))

(defn options
  ""
  [{:keys [fx/context
           display-number]}]
  {:fx/type :v-box
   :children [{:fx/type :check-box
               :text "Scan Line"
               :selected (fx/sub context
                                 scan-line?
                                 display-number)
               :on-selected-changed
               {:display-number display-number
                :effect (fn [snapshot
                             event]
                          (-> snapshot
                              (fx/swap-context assoc-in
                                               [:displays
                                                (:display-number event)
                                                :scan-line?]
                                               (:fx/event event))))}}]})
