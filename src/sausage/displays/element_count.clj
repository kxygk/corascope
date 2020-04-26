(ns sausage.displays.element-count)

(defn view
  "display and options for XRF scan data"
  [{:keys [core-number
           width
           height
           xrf-scan
           selection
           max-element-count
           core-length-mm
           crop-left
           crop-right
           merge-seams?
           seams]}]
  {:fx/type :h-box
   :children [{:fx/type :v-box
               :children (if xrf-scan
                           [{:fx/type :image-view
                             :fit-width width
                             :fit-height height
                             :image (sausage.plot/plot-points width
                                                              height
                                                              (sausage.xrf/element-counts xrf-scan
                                                                                          (keyword selection))
                                                              core-length-mm
                                                              max-element-count
                                                              crop-left
                                                              crop-right
                                                              (if merge-seams?
                                                                seams
                                                                []))}]
                           [{:fx/type :button
                             :pref-width width
                             :pref-height height
                             :on-action {:core-number core-number
                                         :effect sausage.xrf/load-data}
                             :text "Load XRF Scan"}])}]})


(def periodic-table
  [[:H  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil :He]
   [:Li :Be nil nil nil nil nil nil nil nil nil nil :B  :C  :N  :O  :F  :Ne]
   [:Na :Mg nil nil nil nil nil nil nil nil nil nil :Al :Si :P  :S  :Cl :Ar]
   [:K  :Ca :Sc :Ti :V  :Cr :Mn :Fe :Co :Ni :Cu :Zn :Ga :Ge :As :Se :Br :Kr]
   [:Rb :Sr :Y  :Zr :Nb :Mo :Tc :Ru :Rh :Pd :Ag :Cd :In :Sn :Sb :Te :I  :Xe]
   [:Cs :Ba :La :Hf :Ta :W  :Re :Os :Ir :Pt :Au :Hg :Tl :Pb :Bi :Po :At :Rn]
   [:Fr :Ra :Ac :Rf :Db :Sg :Bh :Hs :Mt :Ds :Rg :Cn :Nh :Fl :Mc :Lv :Ts :Og]
   [:.. :Th :.. :U]])

(defn update-selected-element
  [snapshot
   event]
  (-> snapshot
      (assoc-in
       [:displays
        (:display-number event)
        :element]
       (:element event))
      sausage.xrf/update-max-element-count))

(defn periodic-buttons
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
                               :on-action {:event/type ::update-selected-element
                                           :display-number display-number
                                           :element non-element}
                               :text (name non-element)}))
                          non-elements)
             [] ))}))

(defn options
  [{:keys [display-number
           height
           columns
           merge-seams?]}]
  {:fx/type :v-box
   :children [{:fx/type periodic-buttons
               :display-number display-number
               :columns columns}
              {:fx/type :check-box
               :text "Merge Seams"
               :selected merge-seams?
               :on-selected-changed {:display-number display-number
                                     :effect (fn [snapshot
                                                  event]
                                               (-> snapshot
                                                   (assoc-in [:displays
                                                              (:display-number event)
                                                              :merge-seams?]
                                                             (:fx/event event))))}}]})
