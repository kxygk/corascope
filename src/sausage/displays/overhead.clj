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
   :scan-line? true
   :hightlight-unscanned? true})

(defn scan-line?
  [context
   display-number]
  (:scan-line? (fx/sub context
                       state/get-display
                       display-number)))

(defn hightlight-unscanned?
  [context
   display-number]
  (:hightlight-unscanned? (fx/sub context
                                  state/get-display
                                  display-number)))

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

(defn view
  "display and options for the optical image"
  [{:keys [fx/context
           core-number
           display-number
           width]}] ;; TODO Make width calculated here
  {:fx/type :pane
   :on-drag-over {:core-number core-number
                  :effect (fn [snapshot
                               event]
                            ;; This sorta enables transfering of files
                            ;; If this isn't set, then the `:on-drag-dropped` event can't trigger
                            (.acceptTransferModes  (:fx/event event)
                                                   javafx.scene.input.TransferMode/ANY)
                            snapshot)}
   :on-drag-dropped {:core-number core-number
                     :effect (fn [snapshot
                                  event]
                               (sausage.optical/load-data snapshot
                                                          {:core-number core-number
                                                           :file (-> event
                                                                     :fx/event
                                                                     .getDragboard
                                                                     .getFiles
                                                                     first)}))}
   :children [(let [height (fx/sub context
                                   state/display-height
                                   display-number)]
                (if (nil? (fx/sub context
                                  state/optical-image
                                  core-number))
                  {:fx/type :v-box
                   :children [{:fx/type :button
                               :pref-width width
                               :pref-height height
                               :alignment :center-left
                               :on-action {:core-number core-number
                                           :effect sausage.optical/load-dialogue}
                               :text "Load image"}]}
                  (let [zoom-factor (/ width
                                       (fx/sub context
                                               state/optical-scan-length-pix
                                               core-number))
                        crop-left-pix (* zoom-factor
                                         (fx/sub context
                                                 state/selected-left-pix
                                                 core-number))
                        crop-right-pix (* zoom-factor
                                          (fx/sub context
                                                  state/selected-right-pix
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
                                 :children (cond-> [{:fx/type :image-view
                                                     :fit-width width
                                                     :fit-height height
                                                     :image (fx/sub context
                                                                    display-image
                                                                    core-number)}]
                                             ;; Conditionally add decorations/overlays on image
                                             ;; Center Line
                                             (fx/sub context
                                                     scan-line?
                                                     display-number) (conj {:fx/type :line
                                                                            :start-x 1 ;; pixel offset
                                                                            :start-y (/ height
                                                                                        2.0)
                                                                            :end-x (dec width)
                                                                            :end-y (/ height
                                                                                      2.0)
                                                                            :stroke-dash-array [10 10]
                                                                            :stroke "white"})
                                             ;; Right Crop
                                             (pos? crop-right-pix) (conj {:fx/type :line
                                                                          :start-x (- width
                                                                                      crop-right-pix)
                                                                          :start-y 0
                                                                          :end-x (- width
                                                                                    crop-right-pix)
                                                                          :end-y (dec height) ;; this is inclusive!
                                                                          :stroke "red"}
                                                                         {:fx/type :rectangle
                                                                          :x (- width
                                                                                crop-right-pix)
                                                                          :y 0
                                                                          :height height
                                                                          :width crop-right-pix
                                                                          :opacity 0.10
                                                                          :fill "red"})
                                             ;; Left Crop
                                             (pos? crop-left-pix) (conj {:fx/type :line
                                                                         :start-x crop-left-pix
                                                                         :start-y 0
                                                                         :end-x crop-left-pix
                                                                         :end-y (dec height) ;; this is inclusive!
                                                                         :stroke "red"}
                                                                        {:fx/type :rectangle
                                                                         :x 0
                                                                         :y 0
                                                                         :height height
                                                                         :width crop-left-pix
                                                                         :opacity 0.10
                                                                         :fill "red"})
                                             ;; Left Unscanned Area
                                             (and (fx/sub context
                                                          hightlight-unscanned?
                                                          display-number)
                                                  (pos? unscanned-left-pix)) (conj {:fx/type :line
                                                                                    :start-x unscanned-left-pix
                                                                                    :start-y 0
                                                                                    :end-x unscanned-left-pix
                                                                                    :end-y (dec height) ;; inclusive!
                                                                                    :stroke "brown"}
                                                                                   {:fx/type :rectangle
                                                                                    :x 0
                                                                                    :y 0
                                                                                    :height height
                                                                                    :width unscanned-left-pix
                                                                                    :opacity 0.10
                                                                                    :fill "yellow"})
                                             ;; Right Unscanned Area
                                             (and (fx/sub context
                                                          hightlight-unscanned?
                                                          display-number)
                                                  (pos? unscanned-right-pix)) (conj {:fx/type :line
                                                                                     :start-x (- width
                                                                                                 unscanned-right-pix)
                                                                                     :start-y 0
                                                                                     :end-x (- width
                                                                                               unscanned-right-pix)
                                                                                     :end-y (dec height) ;; inclusive
                                                                                     :stroke "brown"}
                                                                                    {:fx/type :rectangle
                                                                                     :x (- width
                                                                                           unscanned-right-pix)
                                                                                     :y 0
                                                                                     :height height
                                                                                     :width  unscanned-right-pix
                                                                                     :opacity 0.10
                                                                                     :fill "yellow"}))}]})))]})

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
                                               (:fx/event event))))}}
              {:fx/type :check-box
               :text "Highlight unscanned areas"
               :selected (fx/sub context
                                 hightlight-unscanned?
                                 display-number)
               :on-selected-changed
               {:display-number display-number
                :effect (fn [snapshot
                             event]
                          (-> snapshot
                              (fx/swap-context assoc-in
                                               [:displays
                                                (:display-number event)
                                                :hightlight-unscanned?]
                                               (:fx/event event))))}}]})
