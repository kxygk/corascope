(ns sausage.core
  (:require
   [sausage.plot]
   [sausage.optical]
   [sausage.state :as state]
   [sausage.xrf]
   [sausage.displays.element-count]
   [sausage.displays.overhead]
   [sausage.common-effects :as effects]
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view])
  (:import javafx.stage.DirectoryChooser
           javafx.stage.FileChooser
           javafx.application.Platform
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
(def fixed-element-count-height 310.0)  ;; needs to be fixed so the core displays line up
(def fixed-slider-height 18)
(def fixed-element-selector-width 50)

(defn workspace-settings-display
  "Top level settings for the workspace where all data will be stored in"
  [{:keys [fx/context
           working-directory
           height]}]
  {:fx/type :h-box
   :children [{:fx/type :h-box
               :pref-width fixed-margin-width
               :alignment :center-left
               :children [{:fx/type :text
                           :text " Working Directory: "}
                          {:fx/type :text-field
                           :editable false
                           :pref-height height
                           :prompt-text "Select a working directory.."
                           :pref-column-count 999
                           :text working-directory}]}
              {:fx/type :h-box
               :pref-width fixed-margin-width
               :alignment :center-right
               :children [{:fx/type :button
                           :max-height Double/MAX_VALUE
                           :on-action {:effect effects/merge-all-cores}
                           :disable true
                           :text "Crop"}
                          {:fx/type :button
                           :max-height Double/MAX_VALUE
                           :on-action {:effect effects/merge-all-cores}
                           :disable (not (fx/sub context
                                                 state/can-merge?))
                           :text ">> Merge"}
                          {:fx/type :button
                           :max-height Double/MAX_VALUE
                           :on-action {:effect effects/remove-core}
                           :text "Remove Last Core"}
                          {:fx/type :button
                           :max-height Double/MAX_VALUE
                           :on-action {:effect effects/add-core}
                           :text "Add Core to End"}
                          {:fx/type :check-box
                           :text "Full Width"
                           :on-selected-changed {:effect (fn [snapshot
                                                              event]
                                                           (-> snapshot
                                                               (fx/swap-context assoc
                                                                                :full-width?
                                                                                (:fx/event event))))}}]}]})

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
                                                (fx/swap-context assoc-in [:cores
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
                                                (fx/swap-context assoc-in [:cores
                                                                           (:core-number event)
                                                                           :crop-right]
                                                                 (- 1
                                                                    (:fx/event event)))))}}]})

(defn core-header
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
                 :style "-fx-background-color: #d3d3d3;"
                 :pref-height height
                 :min-height height
                 :max-height height
                 :alignment :center-left
                 :children [{:fx/type :spinner
                             :editable true
                             :pref-width 100
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
                                         :effect effects/crop-core}
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

(defn core-displays
  "The virtical stack of displays for a core:
  It consists of the header followed up the current displays, one after another"
  [{:keys [fx/context
           horizontal-zoom-factor
           height
           core-number]}]
  (let [width (* horizontal-zoom-factor
                 (fx/sub context
                         state/length-mm
                         core-number))] ;; TODO: Convert to pixels
    {:fx/type :v-box
     :layout-x (* horizontal-zoom-factor
                  (fx/sub context
                          state/start-mm
                          core-number))  ;; TODO: Convert to pixels
     :layout-y (* height
                  (fx/sub context
                          state/core-row
                          core-number))  ;; TODO: Convert to pixels
     :children (into
                [{:fx/type core-header
                  :core-number core-number
                  :width width
                  :start-mm (fx/sub context
                                    state/start-mm
                                    core-number)
                  :mm-per-pixel (fx/sub context
                                        state/mm-per-pixel
                                        core-number)
                  :crop-left (fx/sub context
                                     state/crop-left
                                     core-number)
                  :crop-right (fx/sub context
                                      state/crop-right
                                      core-number)}]

                (map (fn [display-number]
                       (case (fx/sub context
                                     state/display-type
                                     display-number)
                         :overhead {:fx/type sausage.displays.overhead/view
                                    :core-number core-number
                                    :display-number display-number
                                    :width width}
                         :element-count {:fx/type sausage.displays.element-count/view
                                         :core-number core-number
                                         :display-number display-number
                                         :width width}))
                     (range (fx/sub context
                                    state/num-displays))))}))

(defn display-options-header
  "Each Display's header
  With common things like: Name, Button to close the display, etc.
  This is non display-specific"
  [{:keys [display-number
           display-name]}]
  {:fx/type :v-box
   :alignment :center-left
   :style "-fx-background-color: #d3d3d3;"
   :children [#_{:fx/type :separator
               :orientation :horizontal}
              {:fx/type :h-box
               :alignment :center-left
               :children [{:fx/type :separator
                           :style "-fx-background-color: #a9a9a9;"
                           :orientation :vertical}
                          {:fx/type :text
                           :text display-name}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :button
                           :text "X"
                           :on-action
                           {:display-number display-number
                            :effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context update
                                                           :displays
                                                           #(vec (concat (subvec % 0 display-number)
                                                                         (subvec % (inc display-number)))))))}}]}]})
(defn add-display
  "EFFECT: Adds a display of DISPLAY-TYPE to the display list"
  [snapshot
   {:keys [display-type]}]
  (-> snapshot
      (fx/swap-context update
                       :displays
                       #(conj %
                              (case display-type
                                :overhead
                                {:type :overhead
                                 :height fixed-optical-scan-height
                                 :scan-line? true}
                                :element-count
                                {:type :element-count
                                 :height fixed-element-count-height
                                 :merge-seams? true
                                 :element :Mn})))))

(defn add-display-options
  "A small set of buttons for adding additional displays
  By contrast, displays are removed by the `X` button in their headers"
  [_]
  {:fx/type :h-box
   :pref-height fixed-core-header-height
   :min-height fixed-core-header-height
   :max-height fixed-core-header-height
   :children [{:fx/type :button
               :pref-height Double/MAX_VALUE
               :on-action {:display-type :overhead
                           :effect add-display}
               :text " + Optical"}
              {:fx/type :button
               :pref-height Double/MAX_VALUE
               :on-action {:display-type :element-count
                           :effect add-display}
               :text " +  Element Count"}]})

(defn margin
  "The right margin with global and display specific options"
  [{:keys [fx/context
           width
           height
           ]}]
  {:fx/type :v-box
   :pref-width width
   :min-width width
   :max-width width
   :children [{:fx/type add-display-options}
              {:fx/type :v-box
               :children (->> (fx/sub context
                                      state/displays)
                              (map-indexed (fn [display-number
                                                display]
                                             {:fx/type :v-box
                                              :pref-height (:height display);; height
                                              :min-height (:height display) ;;height
                                              :max-height (:height display) ;;height
                                              :children [{:fx/type display-options-header
                                                          :display-number display-number
                                                          :display-name (name (:type display))}
                                                         (case (:type display)
                                                           :overhead {:fx/type sausage.displays.overhead/options
                                                                      :display-number display-number}
                                                           :element-count {:fx/type sausage.displays.element-count/options
                                                                           :display-number display-number})]}))
                              flatten)}
              ]})

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [fx/context]}]
  (let [test "hello"
        width (fx/sub context
                      state/width)
        full-width? (fx/sub context
                            state/full-width?)
        cores (fx/sub context
                      state/cores)
        displays (fx/sub context
                         state/displays)
        horizontal-zoom-factor (if full-width?
                                 1
                                 (/ (- width
                                       fixed-margin-width)
                                    (fx/sub context
                                            state/end-of-all-scans-mm)))
        core-displays-height (+ (fx/sub context
                                       state/displays-total-height)
                               fixed-core-header-height
                               fixed-slider-height)]
    {:fx/type :stage
     :title "Sausage Scanner"
     :showing true
     :scene {:fx/type :scene
             :on-width-changed {:effect (fn [snapshot
                                             event]
                                          (fx/swap-context snapshot assoc :width (:fx/event event)))}
             :on-height-changed {:effect (fn [snapshot
                                              event]
                                           (fx/swap-context snapshot assoc :height (:fx/event event)))}
             :root {:fx/type :v-box
                    :children [{:fx/type workspace-settings-display
                                :working-directory (fx/sub context
                                                           state/working-directory)
                                :height fixed-workspace-settings-height}
                               {:fx/type :h-box
                                :children [
                                           {:fx/type margin
                                            :width fixed-margin-width
                                            :height core-displays-height}
                                           {:fx/type :scroll-pane
                                            :hbar-policy :never
                                            :vbar-policy :never
                                            :pref-viewport-width (- width fixed-margin-width)
                                            :content {:fx/type :pane
                                                      :children (map-indexed (fn [index core]
                                                                               {:fx/type core-displays
                                                                                :core-number index
                                                                                :horizontal-zoom-factor horizontal-zoom-factor
                                                                                :height core-displays-height})
                                                                             cores)}}
                                           ]}]}}}))

(defn event-handler-wrapper
  [{:keys [snapshot
           effect] :as event}]
  {:updated-context (effect snapshot
                            (dissoc event
                                    :effect
                                    :snapshot))})
(def event-dispatcher
  (-> event-handler-wrapper
      ;; adds the current state to every processed event
      ;; the event handler can then operate on the current state
      ;; and doesn't need to do it own dereferencing
      (fx/wrap-co-effects {:snapshot #(deref sausage.state/*context)})
      ;; wrap-effects will take:
      ;; - a key where it will some data
      ;; - a side-effect function of what to do with the data
      ;; in our case the data will be an updated state
      ;; and it will update the global state with this updated state
      (fx/wrap-effects {:updated-context (fn [our-updated-context _]
                                           (reset! sausage.state/*context ;; feel this should be a `reset-context`
                                                   our-updated-context))})))

(def renderer
  (fx/create-renderer
   :middleware (comp
                ;; passes the state context to all lifecycles
                fx/wrap-context-desc
                (fx/wrap-map-desc (fn [_] {:fx/type root})))
   :opts {:fx.opt/map-event-handler event-dispatcher
          :fx.opt/type->lifecycle #(or (fx/keyword->lifecycle %)
                                       ;; For functions in `:fx/type` values, pass
                                       ;; context from option map to these functions
                                       (fx/fn->lifecycle-with-context %))}))

(defn -main [& args]
  ;; Make the application exit when you close all the windows
  (Platform/setImplicitExit true)
  ;; Add a first empty core
  ;; The UI paradigm doesn't make much sense with zero cores
  (if (zero? (fx/sub @sausage.state/*context
                     state/num-cores))
    (event-dispatcher {:effect effects/add-core}))
  (fx/mount-renderer
   sausage.state/*context
   renderer)
  ;; Add 2 displays so that there is something to look at
  (if (zero? (fx/sub @sausage.state/*context
                     state/num-displays))
    (do (event-dispatcher {:effect add-display
                           :display-type :overhead})
        (event-dispatcher {:effect add-display
                           :display-type :element-count}))))
