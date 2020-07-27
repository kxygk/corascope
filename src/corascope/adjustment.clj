(ns corascope.adjustment
  (:require
   [corascope.plot]
   [corascope.common-effects :as effects]
   [corascope.state :as state]
   [corascope.xrf]
   [cljfx.api :as fx]))

(defn plot
  [{:keys [fx/context
           width
           height
           element
           adjustment-core
           overlapped-core
           min-depth
           max-depth]}]
  (let [adjustment-points (->> (fx/sub context
                                      corascope.xrf/element-counts
                                      adjustment-core
                                      element) ;; these are mm from `:start-mm`
                                (filter #(-> %
                                            first
                                            (> (- (fx/sub context
                                                          state/start-mm-after-crop
                                                          adjustment-core)
                                                  (fx/sub context
                                                       state/start-mm
                                                       adjustment-core)))))
                               (filter #(-> %
                                            first
                                            (< (- max-depth
                                                  min-depth)))))
        overlapped-points (->> (fx/sub context
                                      corascope.xrf/element-counts
                                      overlapped-core
                                      element)
                               (filter #(-> %
                                            first
                                            (> min-depth)))
                               (filter #(-> %
                                            first
                                            (< max-depth)))
                               (filter #(-> %
                                            first
                                            (< (fx/sub context
                                                       state/end-mm-after-crop
                                                       overlapped-core))))
                               (map #(-> %
                                         (update-in [0]
                                                    -
                                                    min-depth))))
        plot-svg (corascope.plot/plot-overlapping width
                                                  height
                                                  overlapped-points
                                                  adjustment-points
                                                  (- max-depth
                                                     min-depth)
                                                  (fx/sub context
                                                          corascope.xrf/max-element-count-all-cores
                                                          :Mn))]
    {:fx/type :group
     :children [(corascope.svg/render-with-jfx-shapes plot-svg)]}))

(defn view
  [{:keys [fx/context
           width
           height]}]
  (let [core-to-adjust-index (fx/sub context
                                     state/adjustment-core)
        core-to-adjust-layer (fx/sub context
                                     state/core-row
                                     core-to-adjust-index)
        overlaped-core-index (fx/sub context
                                     state/overlapped-core
                                     core-to-adjust-index)
        overlap-area-start (fx/sub context
                                   state/start-mm
                                   core-to-adjust-index) ;; BEFORE/AFTER CROP?
        overlap-area-end (min (fx/sub context
                                      state/end-mm
                                      core-to-adjust-index) ;; Super short core
                              (fx/sub context
                                      state/end-mm
                                      overlaped-core-index))
        element-to-display (->> (fx/sub context
                                       state/displays)
                                (filter #(= (:type %)
                                            :element-count))
                                first
                                :element)]
    {:fx/type :v-box
     :alignment :center-left
     :children [{:fx/type :h-box
                 :style "-fx-background-color: #d3d3d3;"
                 :alignment :center-left
                 :children [{:fx/type :separator
                             :style "-fx-background-color: #a9a9a9;"
                             :orientation :vertical}
                            {:fx/type :text
                             :text (str "Adjusting Core: "
                                        (fx/sub context
                                                state/adjustment-core))}
                            {:fx/type :pane
                             :h-box/hgrow :always}
                            {:fx/type :button
                             :text "X"
                             :on-action
                             {:effect (fn [snapshot
                                           event]
                                        (-> snapshot
                                            (fx/swap-context assoc
                                                             :adjustment-core
                                                             nil)))}}]}
                {:fx/type plot
                 :width width
                 :height 400
                 :element (if (some? element-to-display)
                            element-to-display
                            (first (fx/sub context
                                           state/xrf-columns)))
                 :adjustment-core core-to-adjust-index
                 :overlapped-core overlaped-core-index
                 :min-depth overlap-area-start
                 :max-depth overlap-area-end
                 :core-number (fx/sub context
                                      state/adjustment-core)}]}))
