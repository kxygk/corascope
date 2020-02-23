(ns sausage.gui
  (:require [cljfx.api :as fx])
  (:import [javafx.scene.input KeyCode KeyEvent]))

(def *state
  ""
  (atom {:width 400
         :height 400
         :points []; [[0 0] [250 8] [500 15] [750 19] [1000 20]]
         :degree 1}))

(defmulti event-handler
  "CLJFX -  Event Handlers

  When defining CLJFX event like `on-value-changed` you have two options
  - 1 You can point to *Function* (or lambda) to run when the event happens
  ex:
  ```
  {:fx/type :check-box
               :selected done
               :on-selected-changed #(swap! *state assoc-in [:by-id id :done] %)
  ```
  - 2 You can point to a custom *Map Events*
  ex:
  ```
  {:fx/type :check-box
               :selected done
               :on-selected-changed {:event/type ::set-done :id id}}
  ```
  You will then need to create an event handler function
  ex:
  ```
  (defn map-event-handler [event]
  (case (:event/type event)
    ::set-done (swap! *state assoc-in
                             [:by-id (:id event) :done]
                             (:fx/event event))))
  ```
  And this function will then be registered with the renderer
  ex:
  ```
  (fx/mount-renderer
  *state
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)
    :opts {:fx.opt/map-event-handler map-event-handler}))
  ```
  The map you paired to the event will be passed to the registered event handler
  It will needs to designate its type with the `event/type` key
  And the actual even will be passed in under the `fx/event` key
  Any additional keys you place in the map will also get passed along
  lik `:id` in the example

  In the simple example we registered a function,
  however here I register a multimethod
  Then we simply switch on the `event/type`"
  :event/type)

(defmethod event-handler ::width-changed
  [event]
  (swap! *state assoc :width (:fx/event event)))

(defmethod event-handler ::height-changed
  [event]
  (swap! *state assoc :height (:fx/event event)))

(defmethod event-handler ::mouse-clicked
  [event]
  ;; the magic fudge factors make the tick appear under the mouse correctly
  ;; This is due to auto adjustments thing/geom and svg do to fit labels and stuff
  (swap! *state update :points #(conj % [(- (.getX (:fx/event event))
                                            1) ;; magic adjustment numbers ...
                                         (- (.getY (:fx/event event))
                                            38)]))) ;; magic adjustment numbers ...

(defmethod event-handler ::degree-change
  [event]
  ;; the magic fudge factors make the tick appear under the mouse correctly
  ;; This is due to auto adjustments thing/geom and svg do to fit labels and stuff
  (swap! *state assoc :degree (:fx/event event)))

(defn root
  "Takes the state atom (which is a map) and then get the mixers out of it and builds a windows with the mixers"
  [{:keys [width
           height
           points
           degree]}]

  {:fx/type :stage
   :showing true
   :on-width-changed {:event/type ::width-changed}
   :on-height-changed {:event/type ::height-changed}
   :scene {:fx/type :scene
           :on-mouse-clicked {:event/type ::mouse-clicked}
           :root {:fx/type :v-box
                  :children [{:fx/type :slider
                              :max-height 35
                              :min-height 35
                              :major-tick-unit 1
                              :minor-tick-count 0
                              :block-increment 1
                              :show-tick-labels true
                              :show-tick-marks true
                              :snap-to-ticks true
                              :min 1
                              :max (count points)
                              :value degree
                              :on-value-changed {:event/type ::degree-change}}
                             ]}}})
                             
(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc assoc :fx/type root)
   :opts {:fx.opt/map-event-handler event-handler}))

(fx/mount-renderer
 *state
 renderer)


;; Technically shouldn't be necessary
;; But I get weird artifacts unless i rereun this here
(renderer)
