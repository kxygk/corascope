(ns corascope.adjustment
  (:require
   [corascope.plot]
   [corascope.common-effects :as effects]
   [corascope.correlation :as correlation]
   [corascope.state :as state]
   [corascope.xrf]
   [cljfx.api :as fx]
   [criterium.core]))

(defn plot-element-counts
  [{:keys [fx/context
           width
           height
           element
           adjustment-core
           overlapped-core
           min-depth
           max-depth
           ]}]
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
                                                  [0
                                                   (- max-depth
                                                        min-depth)]
                                                  [0
                                                   (fx/sub context
                                                           corascope.xrf/max-element-count-all-cores
                                                           :Mn)])]
    {:fx/type :group
     :children [(corascope.svg/render-with-jfx-shapes plot-svg)]}))


(defn plot-correlation
  [{:keys [fx/context
           width
           height
           elements
           adjustment-core
           overlapped-core
           current-start
           ]}]
  (let [correlations (mapv #(correlation/points-cross-correlation (fx/sub context
                                                                          corascope.xrf/element-counts
                                                                          adjustment-core
                                                                          %)
                                                                  (fx/sub context
                                                                          corascope.xrf/element-counts
                                                                          overlapped-core
                                                                          %)
                                                                  (fx/sub context
                                                                          state/mm-per-pixel
                                                                          0))
                           
                           elements)
        num-elements (count elements)
        average-corr (into []
                           (apply (partial map (fn [ first-corr & all-correlations-at-shift]
                                                 [(first first-corr)
                                                  (/ (reduce (fn [sum correlation ]
                                                            (+ (second correlation)
                                                               sum))
                                                          (second first-corr)
                                                          all-correlations-at-shift)
                                                     num-elements)]))
                                  correlations))
        plot-svg (corascope.plot/plot-points width
                                             height
                                             average-corr
                                             [(-> average-corr
                                                  first
                                                  first)
                                              (-> average-corr
                                                  peek
                                                  first)]
                                             [-1.0 1.0]
                                             0
                                             0
                                             [current-start]
                                             true)]
    {:fx/type :group
     :children [(corascope.svg/render-with-jfx-shapes plot-svg)]}))


  (apply (partial map (fn [ first-corr & all-correlations-at-shift]
                                   [(reduce (fn [correlation sum]
                                              (+ (second correlation)
                                                 sum))
                                            first-corr
                                            all-correlations-at-shift)]))
         [[1 2 3 4] [12 23 45 67]])

  (apply (partial map (fn [ first-corr & all-correlations-at-shift]
                (println "first-corr" first-corr)
                (println "rest" all-correlations-at-shift)
                first-corr))
         [[1 2 3 4] [12 23 45 67]])

(map (fn [ first-corr & all-correlations-at-shift]
                (println "first-corr" first-corr)
                (println "rest" all-correlations-at-shift)
                first-corr)
         [1 2 3 4] [12 23 45 67])

(defn view
  [{:keys [fx/context
           width
           height
           header-height]}]
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
        elements-to-display (fx/sub context
                                    state/adjustment-elements)]
    {:fx/type :v-box
     ;;     :alignment :center-left
     :on-mouse-clicked {:effect (fn [snapshot
                                     {:keys [fx/event]}]
                                  (let [first-element (first elements-to-display)
                                        [start
                                         end] (correlation/shift-range-vector (fx/sub snapshot
                                                                                      corascope.xrf/element-counts
                                                                                      overlaped-core-index
                                                                                      first-element)
                                                                              (fx/sub snapshot
                                                                                      corascope.xrf/element-counts
                                                                                      core-to-adjust-index
                                                                                      first-element))
                                        new-start (+ (fx/sub snapshot
                                                             state/start-mm
                                                             overlaped-core-index)
                                                     (+ start
                                                        (* (- end
                                                              start)
                                                           (/ (.getX event)
                                                              width))))]
                                  (effects/update-core-start snapshot
                                                             {:fx/event new-start
                                                              :core-number core-to-adjust-index})))}
     :children [{:fx/type plot-element-counts
                 :width width
                 :height (/ height
                            2)
                 :element   (first elements-to-display)
                 :adjustment-core core-to-adjust-index
                 :overlapped-core overlaped-core-index
                 :min-depth overlap-area-start
                 :max-depth overlap-area-end
                 :core-number (fx/sub context
                                      state/adjustment-core)}
                {:fx/type plot-correlation
                 :width width
                 :height (/ height
                            2)
                 :elements elements-to-display
                 :adjustment-core core-to-adjust-index
                 :overlapped-core overlaped-core-index
                 :min-depth overlap-area-start
                 :max-depth overlap-area-end
                 :core-number (fx/sub context
                                      state/adjustment-core)
                 :current-start (- (+ (fx/sub context
                                           state/start-mm-after-crop
                                           core-to-adjust-index)
                                      (-> (fx/sub context
                                              state/xrf-first-scan-point
                                              core-to-adjust-index)
                                          :depth-mm
                                          read-string))
                                      
                                   (+ (fx/sub context
                                           state/start-mm-after-crop
                                           overlaped-core-index)
                                      (-> (fx/sub context
                                                  state/xrf-first-scan-point
                                                  overlaped-core-index)
                                          :depth-mm
                                          read-string)))
                                        }
                ]}))

;; (time
;; (do 
;; (corascope.correlation4/points-cross-correlation (fx/sub @state/*context
;;                                                          corascope.xrf/element-counts
;;                                                          0
;;                                                          :Zn)
;;                                                  (fx/sub @state/*context
;;                                                          corascope.xrf/element-counts
;;                                                          1
;;                                                          :Zn)
;;                                                  (fx/sub @state/*context
;;                                                          state/mm-per-pixel
;;                                                          0))
;; 0)
;; )




(def periodic-table
  [[:H  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil :He]
   [:Li :Be nil nil nil nil nil nil nil nil nil nil :B  :C  :N  :O  :F  :Ne]
   [:Na :Mg nil nil nil nil nil nil nil nil nil nil :Al :Si :P  :S  :Cl :Ar]
   [:K  :Ca :Sc :Ti :V  :Cr :Mn :Fe :Co :Ni :Cu :Zn :Ga :Ge :As :Se :Br :Kr]
   [:Rb :Sr :Y  :Zr :Nb :Mo :Tc :Ru :Rh :Pd :Ag :Cd :In :Sn :Sb :Te :I  :Xe]
   [:Cs :Ba :La :Hf :Ta :W  :Re :Os :Ir :Pt :Au :Hg :Tl :Pb :Bi :Po :At :Rn]
   [:Fr :Ra :Ac :Rf :Db :Sg :Bh :Hs :Mt :Ds :Rg :Cn :Nh :Fl :Mc :Lv :Ts :Og]
   [:.. :Th :.. :U]])



;; (defn- periodic-buttons
;;   "Periodic table as a grid of buttons
;;   Excess  columns in the xrf-scan file are then appended at the end"
;;   [{:keys [fx/context
;;            display-number]}]
;;   (let [xrf-columns (fx/sub context
;;                             state/xrf-all-columns)
;;         non-elements (clojure.set/difference  (set xrf-columns)
;;                                               (set (flatten periodic-table)))
;;         current-selection (fx/sub context
;;                                   selection
;;                                   display-number)]))

(defn- update-selected-element
  [snapshot
   {:keys [element]}]
  (-> snapshot
      (fx/swap-context update-in
                       [:adjustment
                        :elements]
                       (fn [elements]
                         (if (nil? elements)
                           #{element}
                           (if (contains? elements
                                          element)
                             (if (= (count elements)
                                    1)
                               elements
                               (disj elements
                                   element))
                             (conj elements
                                   element)))))))

(defn is-plottable?
  "Checks if the given element is plottable (is read in as a Long)
  Starts from the given core and checks all subsequent cores
  if not core-number is given then it starts from core 0"
  ([context
    element
    core-number]
   ;; We try to get the element in the first XRF scan point
   (let [value (element (first (filter element (fx/sub context
                                                       state/xrf-element-counts
                                                       core-number))))]
     (if (nil? value) ;; if it doesn't exist
       (if (== core-number ;; we see if there are subsequent cores
               (dec (fx/sub context
                            state/num-cores)))
         false ;; if not, then this element is unplottable
         (recur context ;; else we check the next core
                element
                (inc core-number)))
       (= java.lang.Long ;; if the element exists, we check it's a number
          (try (type (read-string value))
               (catch java.lang.RuntimeException _
                   nil)))))) ;; and not a string or something..
  ([context
    element] ;; Default interface - starts with the first core
   (is-plottable? context
                  element
                  0)))

;; TODO: Should be merged with display/element-stuff's version of periodic table to make something generic
(defn- periodic-buttons
  [{:keys [fx/context]}]
  (let [xrf-columns (fx/sub context
                            state/xrf-all-columns)
        non-elements (clojure.set/difference  (set xrf-columns)
                                              (set (flatten periodic-table)))
        current-selections (fx/sub context
                                   state/adjustment-elements)]
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
                                                           :style (if (contains? current-selections
                                                                                 element)
                                                                    "-fx-background-color: yellow;"
                                                                    "-fx-background-color: #d3d3d3;")
                                                           :grid-pane/column group
                                                           :grid-pane/row valence-electrons
                                                           :disable (not (contains? (set xrf-columns)
                                                                                 element))
                                                           :on-action {;;:display-number display-number
                                                                       :element element
                                                                       :effect update-selected-element}
                                                           :text (name element)}))
                                                      period
                                                      ))
                                       periodic-table)))
           (if (some? non-elements)
             (->> non-elements
                  (filter (partial is-plottable? context))
                  (map-indexed (fn [row-after-table non-element]
                                 (let [columns 6]
                                   {:fx/type :button
                                    :grid-pane/column-span (/ (count (first periodic-table))
                                                              columns)
                                    :style  (if (contains? current-selections
                                                           non-element)
                                             "-fx-background-color: yellow;"
                                             "-fx-background-color: #d3d3d3;")
                                    :max-height 25
                                    :max-width Double/MAX_VALUE
                                    :grid-pane/column (* (int (mod row-after-table columns))
                                                         (/ (count (first periodic-table))
                                                            columns))
                                    :grid-pane/row (+ (count periodic-table)
                                                      (int (/ row-after-table columns)))
                                    :text (name non-element)
                                    :on-action {;;:display-number display-number
                                                :element non-element
                                                :effect update-selected-element}}))))
             [] ))}))


(defn options
  [{:keys [fx/context
           height]}]
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
                                              state/adjustment-core)
                                      "  ")}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :text
                           :text "Cross-correlation window: "}
                          #_{:fx/type :text-field
                             :pref-width 90
                             :text-formatter {:fx/type :text-formatter
                                              :value-converter :integer
                                              :value (int (fx/sub context
                                                                  state/adjustment-window-mm))
                                              :on-value-changed {:effect (fn [snapshot
                                                                              {:keys [fx/event]}]
                                                                           (-> snapshot
                                                                               (fx/swap-context assoc-in [:adjustment
                                                                                                          :window-mm]
                                                                                                event)))}}}
                          {:fx/type :text
                           :text "mm"}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :button
                           :text "X"
                           :on-action
                           {:effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context assoc-in [:adjustment
                                                                     :core]
                                                           nil)))}}

                          ]}
              {:fx/type periodic-buttons}
              {:fx/type :pane
               :v-box/vgrow :always}]})
