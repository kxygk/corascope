(ns sausage.displays.element-count
  (:require
   [sausage.plot]
   [sausage.common-effects :as effects]
   [sausage.state :as state]
   [sausage.xrf]
   [cljfx.api :as fx]))

(def fixed-height 360.0)

(defn create
  []
  {:type :element-count
   :height fixed-height
   :merge-seams? true
   :lines? true
   :element :Mn})

(defn- selection
  [context
   display-number]
  (:element (fx/sub context
                    state/get-display
                    display-number)))

(defn- merge-seams?
  [context
   display-number]
  (:merge-seams? (fx/sub context
                         state/get-display
                         display-number)))

(defn lines?
  [context
   display-number]
  (:lines? (fx/sub context
                   state/get-display
                   display-number)))

(defn view
  "display and options for XRF scan data"
  [{:keys [fx/context
           core-number
           display-number
           width]}] ;; TODO Make width calculated here
  (let [xrf-scan (fx/sub context
                         sausage.state/xrf-scan
                         core-number)
        height (fx/sub context
                       state/display-height
                       display-number)
        selected-element (fx/sub context
                                 selection
                                 display-number)
        ]
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
                                 (sausage.xrf/load-data snapshot
                                                        {:core-number core-number
                                                         :file (-> event
                                                                   :fx/event
                                                                   .getDragboard
                                                                   .getFiles
                                                                   first)}))}
     :children [ (if (nil? xrf-scan)
                   {:fx/type :button
                    :pref-width width
                    :pref-height height
                    :alignment :center-left
                    :on-action {:core-number core-number
                                :effect sausage.xrf/load-dialogue}
                    :text "Load XRF Scan"}
                   (if (nil? (selected-element (set (fx/sub context
                                                            state/xrf-columns
                                                            core-number))))
                     ;;No element counts to display for this selection on this core
                     {:fx/type :label
                      :text-alignment :center
                      :text (str "No ["
                                 (name selected-element)
                                 "] in this core")}
                     {:fx/type :image-view
                      :fit-width width
                      :fit-height height
                      :image (sausage.plot/plot-points width
                                                       height
                                                       (fx/sub context
                                                               sausage.xrf/element-counts
                                                               core-number
                                                               selected-element
                                                               )
                                                       (fx/sub context
                                                               state/length-mm
                                                               core-number)
                                                       (fx/sub context
                                                               sausage.xrf/max-element-count-all-cores
                                                               selected-element)
                                                       (fx/sub context
                                                               state/selected-left-mm
                                                               core-number)
                                                       (fx/sub context
                                                               state/selected-right-mm
                                                               core-number)
                                                       (if (fx/sub context
                                                                   merge-seams?
                                                                   display-number)
                                                         (fx/sub context
                                                                 state/seams
                                                                 core-number)
                                                         [] )
                                                       (fx/sub context
                                                               lines?
                                                               display-number))}))]}))


(def periodic-table
  [[:H  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil :He]
   [:Li :Be nil nil nil nil nil nil nil nil nil nil :B  :C  :N  :O  :F  :Ne]
   [:Na :Mg nil nil nil nil nil nil nil nil nil nil :Al :Si :P  :S  :Cl :Ar]
   [:K  :Ca :Sc :Ti :V  :Cr :Mn :Fe :Co :Ni :Cu :Zn :Ga :Ge :As :Se :Br :Kr]
   [:Rb :Sr :Y  :Zr :Nb :Mo :Tc :Ru :Rh :Pd :Ag :Cd :In :Sn :Sb :Te :I  :Xe]
   [:Cs :Ba :La :Hf :Ta :W  :Re :Os :Ir :Pt :Au :Hg :Tl :Pb :Bi :Po :At :Rn]
   [:Fr :Ra :Ac :Rf :Db :Sg :Bh :Hs :Mt :Ds :Rg :Cn :Nh :Fl :Mc :Lv :Ts :Og]
   [:.. :Th :.. :U]])

(defn- update-selected-element
  [snapshot
   {:keys [display-number
           element]}]
  (-> snapshot
      (fx/swap-context assoc-in
                       [:displays
                        display-number
                        :element]
                       element)))

(defn is-plottable?
  [context
   element]
  (= java.lang.Long
     (type (read-string (element (fx/sub context
                                         state/xrf-first-scan-point
                                         0))))))

(defn- periodic-buttons
  "Periodic table as a grid of buttons
  Excess  columns in the xrf-scan file are then appended at the end"
  [{:keys [fx/context
           display-number]}]
  (let [xrf-columns (fx/sub context
                            state/xrf-all-columns)
        non-elements (clojure.set/difference  (set xrf-columns)
                                              (set (flatten periodic-table)))
        current-selection (fx/sub context
                                  selection
                                  display-number)]
    {:fx/type :grid-pane
     :children
     (into (filter some?
                   (apply concat
                          (map-indexed (fn [valence-electrons period]
                                         (map-indexed (fn [group element]
                                                        (if (some? element)
                                                          {:fx/type :toggle-button
                                                           :padding 2.0
                                                           :min-width 25
                                                           :max-height 25
                                                           :pref-width 25
                                                           :selected (= element
                                                                        current-selection)
                                                           :grid-pane/column group
                                                           :grid-pane/row valence-electrons
                                                           :disable (not (contains? (set xrf-columns)
                                                                                    element))
                                                           :on-action {:display-number display-number
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
                                   {:fx/type :toggle-button
                                    :grid-pane/column-span (/ (count (first periodic-table))
                                                              columns)
                                    :selected (= non-element
                                                 current-selection)
                                    :max-height 25
                                    :max-width Double/MAX_VALUE
                                    :grid-pane/column (* (int (mod row-after-table columns))
                                                         (/ (count (first periodic-table))
                                                            columns))
                                    :grid-pane/row (+ (count periodic-table)
                                                      (int (/ row-after-table columns)))
                                    :text (name non-element)
                                    :on-action {:display-number display-number
                                                :element non-element
                                                :effect update-selected-element}}))))
             [] ))}))

(defn- column-list
  "Periodic table as a grid of buttons
  Excess  columns in the xrf-scan file are then appended at the end"
  [{:keys [fx/context
           display-number]}]
  {:fx/type :choice-box
   :items (->> (fx/sub context
                       state/xrf-all-columns)
               (filterv (partial is-plottable? context))
               (map name))
   :value (name (fx/sub context
                        selection
                        display-number))
   :on-value-changed {:display-number display-number
                      :effect (fn [snapshot
                                   event]
                                (update-selected-element snapshot
                                                         (assoc event
                                                                :element
                                                                (keyword (:fx/event event)))))}})

(defn- toggle-height
  [snapshot
   event]
  (if (:fx/event event)
    (effects/update-display-height snapshot
                                   (assoc event
                                          :display-height
                                          133.0)) ; vertical size of optical
    (effects/update-display-height snapshot
                                   (assoc event
                                          :display-height
                                          fixed-height))))

(defn options
  [{:keys [fx/context
           display-number]}]
  {:fx/type :v-box
   :children [{:fx/type :h-box
               :children [{:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :text
                           :text (str "Displaying: "
                                      (name (fx/sub context
                                                    selection
                                                    display-number)))}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :toggle-button
                           :pref-height 30
                           :pref-width 80
                           :text "Compact"
                           :selected (not= fixed-height
                                           (fx/sub context
                                                   state/display-height
                                                   display-number))
                           :on-selected-changed {:display-number display-number
                                                 :effect toggle-height}}]}
              (if (<= fixed-height
                      (fx/sub context
                              state/display-height
                              display-number))
                {:fx/type periodic-buttons
                 :display-number display-number}
                {:fx/type column-list
                 :display-number display-number})
              {:fx/type :h-box
               :children [{:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :toggle-button
                           :disable (empty? (fx/sub context
                                                    state/seams
                                                    0))
                           :text "Merge Seams"
                           :selected (fx/sub context
                                             merge-seams?
                                             display-number)
                           :on-selected-changed
                           {:display-number display-number
                            :effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context assoc-in [:displays
                                                                     (:display-number event)
                                                                     :merge-seams?]
                                                           (:fx/event event))))}}
                          {:fx/type :button
                           :text (if (fx/sub context
                                             lines?
                                             display-number)
                                   "Lines "
                                   "Points")
                           :on-action
                           {:display-number display-number
                            :effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context update-in [:displays
                                                                      (:display-number event)
                                                                      :lines?]
                                                           not)))}}]}]})
