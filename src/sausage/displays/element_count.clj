(ns sausage.displays.element-count
  (:require
   [sausage.state :as state]
   [sausage.xrf]
   [cljfx.api :as fx]))

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
                      state/seams
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
    {:fx/type :h-box
     :children [{:fx/type :v-box
                 :children (if xrf-scan
                             [{:fx/type :image-view
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
                                                                        state/crop-left-mm
                                                                        core-number)
                                                                (fx/sub context
                                                                        state/crop-right-mm
                                                                        core-number)
                                                                (if (fx/sub context
                                                                            merge-seams?
                                                                            display-number)
                                                                  (fx/sub context
                                                                          state/seams
                                                                          core-number)
                                                                  [] ))}]
                             [{:fx/type :button
                               :pref-width width
                               :pref-height height
                               :on-action {:core-number core-number
                                           :effect sausage.xrf/load-data}
                               :text "Load XRF Scan"}])}]}))


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

(defn- periodic-buttons
  "Periodic table as a grid of buttons
  Excess  columns in the xrf-scan file are then appended at the end"
  [{:keys [columns
           display-number]}]
  (let [non-elements (clojure.set/difference  (set columns)
                                              (set (flatten periodic-table)))]
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
                                                           :grid-pane/column group
                                                           :grid-pane/row valence-electrons
                                                           :disable (not (contains? columns element))
                                                           :on-action {:display-number display-number
                                                                       :element element
                                                                       :effect update-selected-element}
                                                           :text (name element)}))
                                                      period
                                                      ))
                                       periodic-table)))
           (if (some? non-elements)
             (map-indexed (fn [row-after-table non-element]
                            (let [columns 6]
                              {:fx/type :button
                               :grid-pane/column-span (/ (count (first periodic-table))
                                                         columns)
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
                                           :effect update-selected-element}}))
                          non-elements)
             [] ))}))

(defn- merge-seams?
  [context
   display-number]
  (:merge-seams? (fx/sub context
                         state/get-display
                         display-number)))

(defn options
  [{:keys [fx/context
           display-number]}]
  {:fx/type :v-box
   :children [{:fx/type periodic-buttons
               :display-number display-number
               :columns (fx/sub context
                                state/columns)}
              {:fx/type :check-box
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
                                               (:fx/event event))))}}]})
