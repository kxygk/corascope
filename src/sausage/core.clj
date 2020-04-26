(ns sausage.core
  (:require
   [sausage.plot]
   [sausage.optical]
   [sausage.xrf]
   [sausage.displays.element-count]
   [sausage.displays.overhead]
   [sausage.common-effects :as effects]
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view])
  (:import javafx.stage.DirectoryChooser
           javafx.stage.FileChooser
           javafx.stage.Stage)
  (:gen-class :main true))

;; GLOBAL CONSTANTS
;; Can be freely adjusted to tweak the display

;; The cores are displayed/scaled based on their lengths so uninitialized cores
;; need a "size" so that they can take up space to display properly
;; Otherwise nothing shows up and there is no "Load" buttons.
;; (TODO: Think of a less goofy UI solution)
(def fixed-workspace-settings-height 30.0)
(def fixed-margin-width 456) ;; dialed in to fit the periodic table
(def fixed-core-header-height 48)
(def fixed-optical-scan-height 133.0)  ;; needs to be fixed so the core displays line up
(def fixed-element-count-height 300.0)  ;; needs to be fixed so the core displays line up
(def fixed-slider-height 18)
(def fixed-element-selector-width 50)

(def *state
  ""
  (atom {:working-directory ""
         :width 400
         :full-width? false
         :height 400
         :mm-per-pixel 0.5 ;; Common parameter across all cores
         :mm-xrf-step-size 5 ;; Common parameter across all cores
         :cores []
         :displays [{:type :optical
                     :height fixed-optical-scan-height
                     :scan-line? true}
                    {:type :element-count
                     :height fixed-element-count-height
                     :merge-seams? true
                     :element :Mn
                     :max-count 1000}
                    {:type :optical
                     :height fixed-optical-scan-height
                     :scan-line? true}
                    {:type :element-count
                     :height fixed-element-count-height
                     :merge-seams? true
                     :element :Mn
                     :max-count 1000}]}
        ))


;; Crop both :optical and :xrf-data
;; 1 - always crop areas that are optically scanned but have no xrf data
;; 2 - optionally crop more based on the slider positions
;; TODO: Simplify this .. with some destructuring or something
(defn crop
  [snapshot
   {:keys [core-number]}]
  (let [core (-> snapshot
                 :cores
                 (get core-number))
        xrf-scan-element-counts (-> core
                                    :xrf-scan
                                    :element-counts)
        image (-> core
                  :optical
                  :image)
        image-width-pix (-> image
                            .getWidth)
        crop-slider-left (-> core
                             :crop-left)
        crop-slider-right (-> core
                              :crop-right)
        crop-slider-left-pix (int (Math/floor (* crop-slider-left
                                                 image-width-pix)))
        crop-slider-right-pix (int (Math/floor (* crop-slider-right
                                                  image-width-pix)))
        unscanned-left-pix (-> core
                               :optical
                               :unscanned-left-pix)
        unscanned-right-pix (-> core
                                :optical
                                :unscanned-right-pix)
        crop-left-pix (if (> unscanned-left-pix
                             crop-slider-left-pix)
                        unscanned-left-pix
                        crop-slider-left-pix)
        crop-right-pix (if (> unscanned-right-pix
                              crop-slider-right-pix)
                         unscanned-right-pix
                         crop-slider-right-pix)
        mm-per-pixel (-> snapshot
                         :mm-per-pixel)
        crop-left-mm (* crop-left-pix
                        mm-per-pixel)
        crop-right-mm (* crop-right-pix
                         mm-per-pixel)]
    (-> snapshot
        (assoc-in [:cores
                   core-number
                   :optical
                   :image]
                  (sausage.optical/crop image
                                        crop-left-pix
                                        crop-right-pix))
        (assoc-in [:cores
                   core-number
                   :xrf-scan
                   :element-counts]
                  (sausage.xrf/crop xrf-scan-element-counts
                                    (-> core
                                        :length-mm)
                                    crop-left-mm
                                    crop-right-mm))
        (assoc-in [:cores
                   core-number
                   :optical
                   :unscanned-left-pix]
                  0.0)
        (assoc-in [:cores
                   core-number
                   :optical
                   :unscanned-right-pix]
                  0.0)
        (assoc-in [:cores
                   core-number
                   :crop-left]
                  0.0)
        (assoc-in [:cores
                   core-number
                   :crop-right]
                  0.0)
        ;; seams only happen in core=0
        ;; but inserting a conditional in a threading macro is messy
        (update-in
         [:cores
          core-number
          :seams]
         #(map (partial + (- crop-left-mm)) %))
        (effects/update-core-length core-number)
        (sausage.optical/update-display-image core-number))))


(defn merge-cores
  "Given a CORE-A and CORE-B and a MM-PER-PIXEL for their respective images
  RESULT: One core with optical and xrf-scans merged"
  [mm-per-pixel
   core-a
   core-b]
  (let [gap-pmm (- (-> core-b ;; gap between the end of one core and start of next
                       :start-mm)
                   (+ (-> core-a
                          :start-mm)
                      (-> core-a
                          :length-mm)))
        gap-pix (/ gap-pmm
                   mm-per-pixel)
        merged-image (sausage.optical/join-horizontally (-> core-b
                                                            :optical
                                                            :image)
                                                        (-> core-a
                                                            :optical
                                                            :image)
                                                        gap-pix)
        merged-xrf-scan (sausage.xrf/join-horizontally (-> core-a
                                                           :xrf-scan)
                                                       (-> core-b
                                                           :start-mm)
                                                       (-> core-b
                                                           :xrf-scan))]
    (-> core-a
        (update :seams #(into % [(-> core-a :length-mm)]))
        (assoc-in [:optical :image] merged-image)
        (assoc-in [:xrf-scan] merged-xrf-scan)
        (effects/update-core-length-HELPER mm-per-pixel))))


(defn merge-all-cores
  [snapshot
   _]
  (-> snapshot
      (assoc
       :cores
       [(reduce (partial merge-cores (:mm-per-pixel snapshot))
                (-> snapshot
                    :cores
                    (get 0))
                (rest (-> snapshot
                          :cores)))])
      (sausage.optical/update-display-image 0)
      (effects/update-core-length 0)))


(defn add-display
  [snapshot
   {:keys [display-type]}]
  (-> snapshot
      (update
       :displays
       #(conj %
              (case display-type
                :optical
                {:type :optical
                 :height fixed-optical-scan-height
                 :scan-line? true}
                :element-count
                {:type :element-count
                 :height fixed-element-count-height
                 :merge-seams? true
                 :element :Mn
                 :max-count 1000})))
      #_effects/update-max-element-count))

(defn workspace-settings-display
  "Top level settings for the workspace where all data will be stored in"
  [{:keys [working-directory
           height]}]
  {:fx/type :h-box
   :children [{:fx/type :button
               :pref-width fixed-margin-width
               :min-width fixed-margin-width
               :pref-height height
               :min-height height
               :max-height height
               :on-action {:event/type ::set-working-directory}
               :text "Set"}
              {:fx/type :text-field
               :editable false
               :pref-height height ;; TODO: What's with all these pref/min dimensions...?
               :prompt-text "Select a working directory.."
               :pref-column-count 999
               :text working-directory}
              {:fx/type :button
               :pref-height height
               :pref-width height ;; make square button
               :min-width  height
               :on-action {:effect effects/remove-core}
               :text "-"}
              {:fx/type :button
               :pref-height height
               :pref-width height ;; make square button
               :min-width height
               :on-action {:effect effects/add-core}
               :text "+"}]})



(defn crop-slider
  "The sliders that visually help the user cropping the data"
  [{:keys [core-number
           width
           height
           crop-left
           crop-right]}]
  {:fx/type :h-box
   :pref-height height
   :min-height height
   :max-height height
   :pref-width width
   :min-width width
   :max-width width
   :children [{:fx/type :slider
               :max 0.5
               :min 0.0
               :show-tick-labels false
               :show-tick-marks false
               :pref-width (* 0.5
                              width)
               :min-width (* 0.5
                             width)
               :pref-height height
               :min-height height
               :value crop-left
               :on-value-changed {:core-number core-number
                                  :effect (fn [snapshot
                                               event]
                                            (-> snapshot
                                                (assoc-in [:cores
                                                           (:core-number event)
                                                           :crop-left]
                                                          (:fx/event event))))}}
              {:fx/type :slider
               :max 1.0
               :min 0.5
               :show-tick-labels false
               :show-tick-marks false
               :pref-width (* 0.5 width)
               :min-width (* 0.5 width)
               :pref-height height
               :min-height height
               :value (- 1 crop-right)
               :on-value-changed {:core-number core-number
                                  :effect (fn [snapshot
                                               event]
                                            (-> snapshot
                                                (assoc-in [:cores
                                                           (:core-number event)
                                                           :crop-right]
                                                          (- 1
                                                             (:fx/event event)))))}}]})

(defn core-header-display
  "The options bar at the top of every core"
  [{:keys [core-number
           width
           start-mm
           mm-per-pixel
           crop-left
           crop-right]}]
  (let [height (- fixed-core-header-height
                  fixed-slider-height)]
    {:fx/type :v-box
     :children [{:fx/type :h-box
                 :pref-height height
                 :min-height height
                 :max-height height
                 :alignment :center-left
                 :children [{:fx/type :spinner
                             :editable true
                             :value-factory {:fx/type :double-spinner-value-factory
                                             :amount-to-step-by mm-per-pixel
                                             :min 0.0
                                             :max Double/MAX_VALUE
                                             :value start-mm}
                             :on-value-changed {:core-number core-number
                                                :effect effects/update-core-start}
                             }
                            {:fx/type :separator
                             :orientation :horizontal}
                            {:fx/type :text
                             :text "(mm)"}
                            {:fx/type :button
                             :pref-width (* 0.10
                                            width)
                             :pref-height height
                             :on-action {:core-number core-number
                                         :effect crop}
                             :text "Crop"}
                            {:fx/type :pane
                             :h-box/hgrow :always}
                            {:fx/type :button
                             :text "X"
                             :on-action {:core-number core-number
                                         :effect effects/remove-core}}
                            ]}
                {:fx/type crop-slider
                 :core-number core-number
                 :width width
                 :height fixed-slider-height
                 :crop-left  crop-left
                 :crop-right crop-right}]}))

(defn core-display
  "The cummulative core display"
  [{:keys [horizontal-zoom-factor
           mm-per-pixel
           height
           display-row
           core-number
           directory
           core
           displays]}]
  (let [width (* horizontal-zoom-factor
                 (-> core :length-mm))] ;; TODO: Convert to pixels
    {:fx/type :v-box
     :layout-x (* horizontal-zoom-factor
                  (-> core :start-mm))  ;; TODO: Convert to pixels
     :layout-y (* height
                  display-row)  ;; TODO: Convert to pixels
     :children (into [{:fx/type core-header-display
                       :core-number core-number
                       :width width
                       :start-mm (:start-mm core)
                       :mm-per-pixel mm-per-pixel
                       :crop-left (:crop-left core)
                       :crop-right (:crop-right core)}]
                     (map #(case (:type %)
                             :optical {:fx/type sausage.displays.overhead/view
                                       :core-number core-number
                                       :scan-line? (:scan-line? %)
                                       :width width
                                       :height (:height %) ;;fixed-optical-scan-height
                                       :optical (:optical core)
                                       :crop-left  (:crop-left core)
                                       :crop-right (:crop-right core)}
                             :element-count {:fx/type sausage.displays.element-count/view
                                             :core-number core-number
                                             :height (:height %) ;;fixed-optical-scan-height
                                             :width width
                                             :xrf-scan (:xrf-scan core)
                                             :selection (:element %)
                                             :max-element-count (:max-count %)
                                             :core-length-mm (:length-mm core)
                                             :crop-left  (:crop-left core)
                                             :crop-right (:crop-right core)
                                             :merge-seams? (:merge-seams? %)
                                             :seams (:seams core)})
                          displays))}))


(defn core-header-options
  ""
  [{:keys [;;display-number
           width
           can-merge?]}]
  {:fx/type :h-box
   :pref-height fixed-core-header-height
   :min-height fixed-core-header-height
   :max-height fixed-core-header-height
   :alignment :center-left
   :children [{:fx/type :button
               :max-height Double/MAX_VALUE
               :on-action {:effect merge-all-cores}
               :disable true
               :text "Crop"}
              {:fx/type :button
               :max-height Double/MAX_VALUE
               :on-action {:effect merge-all-cores}
               :disable (not can-merge?)
               :text ">> Merge"}
              {:fx/type :check-box
               :text "Full Width"
               :on-selected-changed {:effect (fn [snapshot
                                                  event]
                                               (-> snapshot
                                                   (assoc :full-width? (:fx/event event))))}}]})


(defn display-options-header
  [{:keys [display-number
           display-name]}]
  {:fx/type :v-box
   :alignment :center-left
   :children [{:fx/type :separator
               :orientation :horizontal}
              {:fx/type :h-box
               :children [{:fx/type :text
                           :text display-name}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :button
                           :text "X"
                           :on-action {:display-number display-number
                                       :effect (fn [snapshot
                                                    event]
                                                 (-> snapshot
                                                     (update
                                                      :displays
                                                      #(vec (concat (subvec % 0 display-number)
                                                                    (subvec % (inc display-number)))))))}}]}]})

(defn add-display-options
  [_]
  {:fx/type :h-box
   :children [{:fx/type :button
               :on-action {:display-type :optical
                           :effect add-display}
               :text " + Optical"}
              {:fx/type :button
               :on-action {:display-type :element-count
                           :effect add-display}
               :text " +  Element Count"}]})

(defn margin
  "The right margin with global options/toggles.
  First come static options
  Followed by per-display options"
  [{:keys [width
           height
           columns
           can-merge?
           displays
           ]}]
  {:fx/type :v-box
   :pref-width width
   :min-width width
   :max-width width
   :children [{:fx/type core-header-options
               :can-merge? can-merge?}
              {:fx/type :v-box
               :children (flatten (map-indexed (fn [display-number
                                                    display]
                                                 {:fx/type :v-box
                                                  :pref-height (:height display);; height
                                                  :min-height (:height display) ;;height
                                                  :max-height (:height display) ;;height
                                                  :children [{:fx/type display-options-header
                                                              :display-number display-number
                                                              :display-name (name (:type display))}
                                                             (case (:type display)
                                                               :optical {:fx/type sausage.displays.overhead/options
                                                                         :display-number display-number
                                                                         :height (:height display)
                                                                         :scan-line? (:scan-line? display)}
                                                               :element-count {:fx/type sausage.displays.element-count/options
                                                                               :display-number display-number
                                                                               :height (:height display)
                                                                               :columns columns
                                                                               :merge-seams? (:merge-seams? display)})]})
                                               displays))}
              {:fx/type add-display-options}]})

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [width
           full-width?
           height
           working-directory
           mm-per-pixel
           layout
           columns
           cores
           displays]}]
  (let [horizontal-zoom-factor (if full-width?
                                 1
                                 (/ (- width fixed-margin-width)
                                    (+ (-> cores last :start-mm)
                                       (-> cores last :length-mm))))
        core-display-height (reduce #(+ %1 (:height %2))
                                    (+ fixed-core-header-height
                                       fixed-slider-height)
                                    displays)]
    {:fx/type :stage
     :title "Sausage Scanner"
     :showing true
     :scene {:fx/type :scene
             :on-width-changed {:effect (fn [snapshot
                                             event]
                                          (assoc snapshot :width (:fx/event event)))}
             :on-height-changed {:effect (fn [snapshot
                                              event]
                                           (assoc snapshot :height (:fx/event event)))}
             :root {:fx/type :v-box
                    :children[{:fx/type workspace-settings-display
                               :working-directory working-directory
                               :height fixed-workspace-settings-height}
                              {:fx/type :h-box
                               :children [{:fx/type :scroll-pane
                                           :hbar-policy :never
                                           :vbar-policy :never
                                           :pref-viewport-width (- width fixed-margin-width)
                                           :content {:fx/type :pane
                                                     :children (map-indexed (fn [index core]
                                                                              {:fx/type core-display
                                                                               :core-number index
                                                                               :horizontal-zoom-factor horizontal-zoom-factor
                                                                               :mm-per-pixel mm-per-pixel
                                                                               :height core-display-height
                                                                               :display-row (effects/get-core-row index
                                                                                                                  layout)
                                                                               :core core
                                                                               :displays displays})
                                                                            cores)}}
                                          {:fx/type margin
                                           :width fixed-margin-width
                                           :height core-display-height
                                           :columns columns
                                           :can-merge? (-> layout
                                                           second
                                                           empty?)
                                           :displays displays}
                                          ]}]}}}))




(defn event-handler-wrapper
  [{:keys [snapshot
           effect] :as event}]
  {:updated-state  (effect snapshot
                           (dissoc event
                                   :effect
                                   :snapshot))})
(def event-dispatcher
  (-> event-handler-wrapper
      ;; adds the current state to every processed event
      ;; the event handler can then operate on the current state
      ;; and doesn't need to do it own dereferencing
      (fx/wrap-co-effects {:snapshot #(deref *state)})
      ;; wrap-effects will take:
      ;; - a key where it will some data
      ;; - a side-effect function of what to do with the data
      ;; in our case the data will be an updated state
      ;; and it will update the global state with this updated state
      (fx/wrap-effects {:updated-state (fn [our-updated-state _]
                                         (if (some? our-updated-state)
                                           (reset! *state
                                                   our-updated-state)))})))


(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-dispatcher}))


(defn -main [& args]
  ;; Add a first empty core
  ;; The UI paradigm doesn't make much sense with zero cores
  (event-dispatcher {:effect effects/add-core})
  (fx/mount-renderer
   *state
   renderer))

