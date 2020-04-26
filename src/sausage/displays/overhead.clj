(ns sausage.displays.overhead)

(defn view
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
                             :on-action {:core-number core-number
                                         :effect sausage.optical/load-data}
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

(defn options
  ""
  [{:keys [display-number
           height
           scan-line?]}]
  {:fx/type :v-box
   :children [{:fx/type :check-box
               :text "Scan Line"
               :selected scan-line?
               :on-selected-changed {:display-number display-number
                                     :effect (fn [snapshot
                                                  event]
                                               (-> snapshot
                                                   (assoc-in [:displays
                                                              (:display-number event)
                                                              :scan-line?]
                                                             (:fx/event event))))}}]})
