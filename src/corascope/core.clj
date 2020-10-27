(ns corascope.core
  (:require
   [clojure.java.io :as io]
   [corascope.adjustment]
   [corascope.plot]
   [corascope.optical]
   [corascope.state :as state]
   [corascope.xrf]
   [corascope.displays.element-count]
   [corascope.displays.overhead]
   [corascope.common-effects :as effects]
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view])
  (:import javafx.scene.image.Image
           javafx.stage.DirectoryChooser
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
(def fixed-adjustment-area-height 400)
(def fixed-core-header-height 76)
(def fixed-slider-height 18)
(def fixed-element-selector-width 50)

#_(def app-icons [(-> (io/resource "128.png")
                    .toString
                    javafx.scene.image.Image.)
                (-> (io/resource "48.png")
                    .toString
                    javafx.scene.image.Image.)
                (-> (io/resource "32.png")
                    .toString
                    javafx.scene.image.Image.)
                (-> (io/resource "24.png")
                    .toString
                    javafx.scene.image.Image.)
                (-> (io/resource "16.png")
                    .toString
                    javafx.scene.image.Image.)])

(defn workspace-settings-display
  "Top level settings for the workspace where all data will be stored in"
  [{:keys [fx/context
           working-directory
           height]}]
  {:fx/type :h-box
   :children [{:fx/type :label
               :pref-width fixed-margin-width
               :text ""}
              {:fx/type :button
               :text " |←→| "
               :max-height Double/MAX_VALUE
               :on-action
               {:effect effects/fit-to-screen}}
              {:fx/type :pane
               :h-box/hgrow :always}
              {:fx/type :button
               :max-height Double/MAX_VALUE
               :on-action {:effect effects/remove-core}
               :text "Remove Last Core"}
              {:fx/type :button
               :max-height Double/MAX_VALUE
               :on-action {:effect effects/add-core}
               :text "Add Core to End"}]})

(defn sliders
  "The sliders that visually help the user cropping the data"
  [{:keys [fx/context
           core-number
           width
           height]}]
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
               :value (fx/sub context
                              state/slider-left
                              core-number)
               :on-value-changed {:core-number core-number
                                  :effect (fn [snapshot
                                               event]
                                            (-> snapshot
                                                (fx/swap-context assoc-in [:cores
                                                                           (:core-number event)
                                                                           :slider-left]
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
               :value (- 1 (fx/sub context
                                   state/slider-right
                                   core-number))
               :on-value-changed {:core-number core-number
                                  :effect (fn [snapshot
                                               event]
                                            (-> snapshot
                                                (fx/swap-context assoc-in [:cores
                                                                           (:core-number event)
                                                                           :slider-right]
                                                                 (- 1
                                                                    (:fx/event event)))))}}]})

(defn core-menu
  [{:keys [fx/context
           core-number]}]
  {:fx/type :menu-button
   :items [{:fx/type :menu-item
            :on-action {:core-number core-number
                        :effect corascope.optical/load-dialogue}
            :text "(Re)Load Optical Image"}
           {:fx/type :menu-item
            :on-action {:core-number core-number
                        :effect corascope.xrf/load-dialogue}
            :text "(Re)Load XRF Scan"}
           {:fx/type :menu-item
            :text "Auto-select areas with no XRF data"
            :on-action {:core-number core-number
                        :effect effects/set-sliders-to-crop-unscanned}}
           {:fx/type :menu-item
            :text "Save Optical Image"
            :disable (nil? (fx/sub context
                                   state/optical-image
                                   core-number))
            :on-action  {:core-number core-number
                         :effect corascope.optical/save-data}}
           {:fx/type :menu-item
            :text "Save XRF Scan"
            :disable (nil? (fx/sub context
                                   state/xrf-scan
                                   core-number))
            :on-action  {:core-number core-number
                         :effect corascope.xrf/save-data}}]})

(defn core-header
  "The options bar at the top of every core"
  [{:keys [fx/context
           core-number
           width]}]
  (let [height (/ (- fixed-core-header-height
                     fixed-slider-height)
                  2)]
    {:fx/type :v-box
     :style "-fx-background-color: #d3d3d3;"
     :max-width width
     :children [{:fx/type :h-box
                 :pref-height height
                 :min-height height
                 :max-height height
                 :alignment :center-left
                 :children [{:fx/type core-menu
                             :core-number core-number}
                            {:fx/type :label
                             :style "-fx-text-fill: red;"
                             :ellipsis-string "ERROR"
                             :text (if (and (some? (fx/sub context
                                                           state/xrf-scan
                                                           core-number))
                                            (some? (fx/sub context
                                                           state/optical-image
                                                           core-number)))
                                     (if (> (fx/sub context
                                                    state/xrf-scan-length-mm
                                                    core-number)
                                            (fx/sub context
                                                    state/optical-scan-length-mm
                                                    core-number))
                                       "ERROR: XRF scan data is longer than the optical data"
                                       "")
                                     "")}
                            {:fx/type :pane
                             :h-box/hgrow :always}
                            {:fx/type :label
                             :ellipsis-string ".."
                             :text (fx/sub context
                                           state/core-name
                                           core-number)}
                            {:fx/type :pane
                             :h-box/hgrow :always}
                            {:fx/type :button
                             :text "X"
                             :on-action {:core-number core-number
                                         :effect effects/remove-core}}]}
                {:fx/type :h-box
                 :pref-height height
                 :min-height height
                 :max-height height
                 :alignment :center-left
                 :children (concat
                            [{:fx/type :text-field
                             :disable (not= :left
                                            (fx/sub context
                                                    state/fixed-side
                                                    core-number))
                             :pref-width 90
                             :style (if (= (fx/sub context
                                                   state/start-mm
                                                   core-number)
                                           (fx/sub context
                                                   state/start-mm-after-crop
                                                   core-number))
                                      "-fx-text-fill: black;"
                                      "-fx-text-fill: red;")
                             :text-formatter {:fx/type :text-formatter
                                              :value-converter :double
                                              :value (fx/sub context
                                                             state/start-mm-after-crop
                                                             core-number)
                                              :on-value-changed {:core-number core-number
                                                                 :effect effects/update-core-start}}}
                            {:fx/type :check-box ;; Pin core to left side
                             :selected (= :left
                                          (fx/sub context
                                                  state/fixed-side
                                                  core-number))
                             :on-selected-changed
                             {:core-number core-number
                              :effect (fn [snapshot
                                           event]
                                        (fx/swap-context snapshot
                                                         assoc-in [:cores
                                                                   (:core-number event)
                                                                   :fixed-side]
                                                         (if (:fx/event event)
                                                           :left
                                                           nil)))}}]
                            (when (> (fx/sub context
                                           state/core-row
                                           core-number)
                                     0)
                              [{:fx/type :toggle-button
                               :text "Adjust"
                               :pref-width 80
                               :pref-height height
                               :selected (= (fx/sub context
                                                    state/adjustment-core)
                                            core-number)
                                :on-selected-changed
                                {:core-number core-number
                                 :effect (fn [snapshot
                                              event]
                                           (if (= (fx/sub context
                                                          state/adjustment-core)
                                                  core-number)
                                             (-> snapshot
                                                 (fx/swap-context assoc-in [:adjustment
                                                                            :core]
                                                                  nil))
                                             (-> snapshot
                                                 (fx/swap-context assoc-in [:adjustment
                                                                            :core]
                                                                  core-number)
                                                 (fx/swap-context update-in [:adjustment
                                                                             :elements]
                                                                  (fn [elements]
                                                                    (if (some? elements)
                                                                      elements
                                                                      ;;search for candidate
                                                                      ;;else get fist thing..
                                                                      (or (->> (fx/sub context
                                                                                       state/displays)
                                                                               (filterv #(= (:type %)
                                                                                            :element-count))
                                                                               (#(if (empty? %)
                                                                                   nil
                                                                                   (set (mapv :element %)))))
                                                                          #{(first (state/xrf-all-columns))})))))))}}])
                            [{:fx/type :pane
                             :h-box/hgrow :always}
                            {:fx/type :button
                             :text "Crop"
                             :pref-width 80
                             :pref-height height
                             :context-menu {:fx/type :context-menu
                                            :items [{:fx/type :menu-item
                                                     :text "Auto-select areas with no XRF data"
                                                     :on-action {:core-number core-number
                                                                 :effect effects/set-sliders-to-crop-unscanned}}]}
                             :on-action {:core-number core-number
                                         :effect effects/crop-selected}}
                            {:fx/type :pane
                             :h-box/hgrow :always}
                            {:fx/type :check-box ;; Pin core to right side
                             :selected (= :right
                                          (fx/sub context
                                                  state/fixed-side
                                                  core-number))
                             :on-selected-changed
                             {:core-number core-number
                              :effect (fn [snapshot
                                           event]
                                        (fx/swap-context snapshot
                                                         assoc-in [:cores
                                                                   (:core-number event)
                                                                   :fixed-side]
                                                         (if (:fx/event event)
                                                           :right
                                                           nil)))}}
                            {:fx/type :text-field
                             :alignment :center-right
                             :disable (not= :right
                                            (fx/sub context
                                                    state/fixed-side
                                                    core-number))
                             :pref-width 90
                             :style (if (= (fx/sub context
                                                   state/end-mm
                                                   core-number)
                                           (fx/sub context
                                                   state/end-mm-after-crop
                                                   core-number))
                                      "-fx-text-fill: black;"
                                      "-fx-text-fill: red;")
                             :text-formatter {:fx/type :text-formatter
                                              :value-converter :double
                                              :value (fx/sub context
                                                             state/end-mm-after-crop
                                                             core-number)
                                              :on-value-changed {:core-number core-number
                                                                 :effect effects/update-core-end}}}])
                            }
                {:fx/type sliders
                 :core-number core-number
                 :width width
                 :height fixed-slider-height}]}))

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
                         core-number))]
    {:fx/type :v-box
     :on-scroll {:fx/sync true
                 :effect (fn [snapshot
                              {:keys [fx/event]}]
                           (let [delta-y (.getDeltaY event)]
                             (if (zero? (.getDeltaY event))
                               snapshot
                               (cond-> snapshot
                                 true (fx/swap-context assoc-in
                                                       [:zoom
                                                        :depth-mm]
                                                      (+ (fx/sub snapshot
                                                                  state/start-mm
                                                                  core-number)
                                                          (* (fx/sub snapshot
                                                                     state/length-mm
                                                                     core-number)
                                                             (/ (.getX event)
                                                                width))))
                                 (pos? delta-y) (fx/swap-context update-in
                                                                 [:zoom
                                                                  :factor]
                                                                 #( * %
                                                                   (+ 1
                                                                      (/ (Math/log delta-y)
                                                                         40))))
                                 (neg? delta-y) (fx/swap-context update-in
                                                                 [:zoom
                                                                  :factor]
                                                                 #( / %
                                                                   (+ 1
                                                                      (/ (Math/log (Math/abs delta-y))
                                                                         40))))))))}
     :layout-x (- (* horizontal-zoom-factor
                     (fx/sub context
                             state/start-mm
                             core-number))
                  (fx/sub context
                          state/display-left-pix)
                  (fx/sub context
                          state/horizontal-drag-pix))
     :layout-y (* height
                  (fx/sub context
                          state/core-row
                          core-number))
     :children (concat
                [{:fx/type core-header
                  :core-number core-number
                  :width width}]
                (mapv (fn [display-number]
                       (case (fx/sub context
                                     state/display-type
                                     display-number)
                         :overhead {:fx/type corascope.displays.overhead/view
                                    :core-number core-number
                                    :display-number display-number
                                    :width width}
                         :element-count {:fx/type corascope.displays.element-count/view
                                         :core-number core-number
                                         :display-number display-number
                                         :width width}))
                     (range (fx/sub context
                                    state/num-displays))))}))

(defn layout-area
  [{:keys [fx/context]}]
  {:fx/type :scroll-pane
   :fit-to-height true
   :hbar-policy :never
   :vbar-policy :never
   :content {:fx/type :pane
             :pref-height 99999
             :on-mouse-dragged {:effect (fn [snapshot
                                             {:keys [fx/event]}]
                                          (-> snapshot
                                              (fx/swap-context assoc-in
                                                               [:mouse
                                                                :drag-x]
                                                               (.getX event))
                                              (fx/swap-context assoc-in
                                                               [:mouse
                                                                :drag-y]
                                                               (.getY event))))}
             :on-mouse-pressed {:effect (fn [snapshot
                                             {:keys [fx/event]}]
                                          (-> snapshot
                                              (fx/swap-context assoc-in
                                                               [:mouse
                                                                :pressed]
                                                               true)
                                              (fx/swap-context assoc-in
                                                               [:mouse
                                                                :x]
                                                               (.getX event))
                                              (fx/swap-context assoc-in
                                                               [:mouse
                                                                :y]
                                                               (.getY event))
                                              (fx/swap-context assoc-in
                                                               [:mouse
                                                                :drag-x]
                                                               (.getX event))
                                              (fx/swap-context assoc-in
                                                               [:mouse
                                                                :drag-y]
                                                               (.getY event))))}
             :on-mouse-released {:effect (fn [snapshot
                                              event]
                                           (-> snapshot
                                               (fx/swap-context update-in
                                                                [:zoom
                                                                 :mouse-horizontal-pix]
                                                                #(- %
                                                                    (fx/sub snapshot
                                                                            state/horizontal-drag-pix)))
                                               (fx/swap-context assoc-in
                                                                [:mouse
                                                                 :pressed]
                                                                false)
                                               (fx/swap-context assoc-in
                                                                [:mouse
                                                                 :drag-x]
                                                                0)
                                               (fx/swap-context assoc-in
                                                                [:mouse
                                                                 :drag-y]
                                                                0)))}
             :on-scroll {:effect (fn [snapshot
                                      event]
                                   (if (zero? (.getDeltaY (:fx/event event)))
                                     snapshot
                                     (-> snapshot
                                         (fx/swap-context assoc-in
                                                          [:zoom
                                                           :mouse-horizontal-pix]
                                                          (.getX (:fx/event event))))))}
             :children (->> (fx/sub context
                                    state/num-cores)
                            range
                            (map (fn [core-index]
                                   {:fx/type core-displays
                                    :fx/key (fx/sub context
                                                    state/core-creation-time
                                                    core-index)
                                    :core-number core-index
                                    :horizontal-zoom-factor (fx/sub context
                                                                    state/zoom-factor)
                                    :height (+ (fx/sub context
                                                       state/displays-total-height)
                                               fixed-core-header-height)}))
                            (into []))}})

(defn overlap-adjustment-area
  [{:keys [fx/context]}]
  #_{:fx/type :text
   :text "ehllo"}
  {:fx/type corascope.adjustment/view
   :width (- (fx/sub context
                     state/width)
             fixed-margin-width)
   :height fixed-adjustment-area-height})

(defn workspace-area
  [{:keys [fx/context]}]
  (cond-> {:fx/type :border-pane
           :center {:fx/type layout-area
                    :key :the-layout-area}}
    (some? (fx/sub context
                   state/adjustment-core)) (assoc :bottom
                                                  {:fx/type overlap-adjustment-area})))

(defn display-options-header
  "Each Display's header
  With common things like: Name, Button to close the display, etc.
  This is non display-specific"
  [{:keys [fx/context
           display-number]}]
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
                           :text (name (fx/sub context
                                               state/display-type
                                               display-number))}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :button
                           :max-height Double/MAX_VALUE
                           :disable (zero? display-number)
                           :text "↑"
                           :on-action
                           {:display-number display-number
                            :effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context update
                                                           :displays
                                                           #(assoc %
                                                                   (dec display-number)
                                                                   (% display-number)
                                                                   display-number
                                                                   (% (dec display-number))))))}}
                          {:fx/type :button
                           :max-height Double/MAX_VALUE
                           :disable (= display-number
                                       (dec (fx/sub context
                                                    state/num-displays)))
                           :text "↓"
                           :on-action
                           {:display-number display-number
                            :effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context update
                                                           :displays
                                                           #(assoc %
                                                                   (inc display-number)
                                                                   (% display-number)
                                                                   display-number
                                                                   (% (inc display-number))))))}}
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
                              (-> (case display-type
                                    :overhead
                                    (corascope.displays.overhead/create)
                                    :element-count
                                    (corascope.displays.element-count/create))
                                  (assoc :creation-time (System/currentTimeMillis)))))))

(defn add-display-options
  "A small set of buttons for adding additional displays
  By contrast, displays are removed by the `X` button in their headers"
  [{:keys [fx/context]}]
  {:fx/type :v-box
   :children [{:fx/type :h-box
               :pref-height (/ fixed-core-header-height 2)
               :min-height (/ fixed-core-header-height 2)
               :max-height (/ fixed-core-header-height 2)
               :children [{:fx/type :pane
                           :h-box/hgrow :always}
                          {:fx/type :button
                           :max-height Double/MAX_VALUE
                           :on-action {:effect effects/merge-all-cores}
                           :disable (not (fx/sub context
                                                 state/can-merge?))
                           :text "Merge <<"}]}
              {:fx/type :h-box
               :pref-height (/ fixed-core-header-height 2)
               :min-height (/ fixed-core-header-height 2)
               :max-height (/ fixed-core-header-height 2)
               :children [{:fx/type :button
                           :pref-height Double/MAX_VALUE
                           :on-action {:display-type :overhead
                                       :effect add-display}
                           :text " ↓ Optical"}
                          {:fx/type :button
                           :pref-height Double/MAX_VALUE
                           :on-action {:display-type :element-count
                                       :effect add-display}
                           :text " ↓ Element Count"}]}]})

(defn margin
  "The right margin with global and display specific options"
  [{:keys [fx/context
           width]}]
  {:fx/type :v-box
   :min-width width
   :max-width width
   :children [{:fx/type add-display-options}
              {:fx/type :v-box
               :children (->> (fx/sub context
                                      state/num-displays)
                              range
                              (map (fn [display-number]
                                     (let [display-height (fx/sub context
                                                                  state/display-height
                                                                  display-number)]
                                       {:fx/type :v-box
                                        :fx/key (fx/sub context
                                                        state/display-creation-time
                                                        display-number)
                                        :pref-height display-height
                                        :min-height display-height
                                        :max-height display-height
                                        :children [{:fx/type display-options-header
                                                    :display-number display-number}
                                                   (case (fx/sub context
                                                                 state/display-type
                                                                 display-number)
                                                     :overhead {:fx/type corascope.displays.overhead/options
                                                                :display-number display-number}
                                                     :element-count {:fx/type corascope.displays.element-count/options
                                                                     :display-number display-number})]})))
                              flatten)}
              {:fx/type :pane
               :v-box/vgrow :always}
              (if (nil? (fx/sub context
                                state/adjustment-core))
                {:fx/type :pane
                 :v-box/vgrow :never}
                {:fx/type :v-box
                 :pref-height fixed-adjustment-area-height
                 :min-height fixed-adjustment-area-height
                 :max-height fixed-adjustment-area-height
                 :children [{:fx/type corascope.adjustment/options
                             :height fixed-adjustment-area-height}]})
              ]})

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [fx/context]}]
  {:fx/type :stage
   :icons [(-> (io/resource "128.png")
                    .toString
                    javafx.scene.image.Image.)]
   :title "Corascope"
   :showing true
   :min-height 400
   :min-width (+ fixed-margin-width
                 100)
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

                             {:fx/type :border-pane
                              :left {:fx/type margin
                                     :width fixed-margin-width}
                              :center {:fx/type workspace-area}
                              }]}}})

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
      (fx/wrap-co-effects {:snapshot #(deref corascope.state/*context)})
      ;; wrap-effects will take:
      ;; - a key where it will some data
      ;; - a side-effect function of what to do with the data
      ;; in our case the data will be an updated state
      ;; and it will update the global state with this updated state
      (fx/wrap-effects {:updated-context (fn [our-updated-context _]
                                           (reset! corascope.state/*context ;; feel this should be a `reset-context`
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
  (if (zero? (fx/sub @corascope.state/*context
                     state/num-cores))
    (event-dispatcher {:effect effects/add-core}))
  (fx/mount-renderer
   corascope.state/*context
   renderer)
  ;; Add 2 displays so that there is something to look at
  (if (zero? (fx/sub @corascope.state/*context
                     state/num-displays))
    (do (event-dispatcher {:effect add-display
                           :display-type :overhead})
        (event-dispatcher {:effect add-display
                           :display-type :element-count}))))
