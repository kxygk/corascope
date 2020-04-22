(ns sausage.plot
  (:require [sausage.svg]
            [thi.ng.geom.core :as g] ;; The graphing libraires
            [thi.ng.math.core :as m]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svgthing]))



(defn- nearest-power-of-ten
  [number]
  (/ (Math/pow 10
               (Math/floor (Math/log10 number)))
     2))

(defn- nice-max-count
  [max-count]
  (let [tick-mark-size (nearest-power-of-ten max-count)
        num-tick-marks (Math/ceil (/ max-count
                                     tick-mark-size))]
    (* num-tick-marks
       tick-mark-size)))



(defn- grid-spec
  "Given a size (WIDTH HEIGHT) the output *spec* describes how the plot looks.
  More detail are in **geom-viz**.
  The data has been left initialized"
  [width
   height
   max-position
   max-count]
  {:x-axis (viz/linear-axis
            {:domain [0 max-position]
             :range  [0.0 width]
             :pos    0.0
             :visible true
             :label-style {:stroke "none"
                           :fill "dimgray"
                           :font-family nil}
             :label-dist  (- height 1)
;;             :major-size 0
             :major 500
             :attribs {:stroke "none"} ;; axis line attributes
             })
             
   :y-axis (viz/linear-axis
            {:domain      [-0.01 max-count]
             :range       [height 0]
             ;; puts the axis out of view (can't show the grid with no axis)
             :pos         0 ;; major-size default
             :visible true
             :major (nearest-power-of-ten max-count)
             :label-dist -25
             :label-y 10
             :major-size 5
             :minor-size 5
             :label-style {:stroke "none"
                           :fill "dimgray"
                           :font-family nil}
            ;; :label-style {:fill "red" :text-anchor "start"}
             })
             
   ;; :grid   {;:attribs {:stroke "#caa"}
   ;;          ; :minor-x true
   ;;          ; :major-x true
   ;;          ; :major-y true
   ;;          :minor-y true
   ;;          }
   })


(defn- add-points
  "Add the points (as little triangles) to the graph"
  [spec
   points]
  (if (empty? points)
    spec ;; do nothing.. else:
    (assoc spec
            :data
            [{:values  points
              :attribs {:fill "none" :stroke "black" :stroke-width 1.25}
              ;; :shape   (viz/svg-triangle-down 6)
              :layout  viz/svg-line-plot}
              ])))

(defn- add-red-overlay
  "Will draw red overlays at the selected points
  ie. the points selected to be cropped"
  [spec
   points]
  (if (empty? points)
    spec
    (update spec
           :data
           #(into %
                  [{:values  points
                    :attribs {:fill "pink" :stroke "red" :stroke-width 1.25}
;;                    :bar-width 100
                    :interleave 1
                    :layout  viz/svg-bar-plot}]))))

(defn- add-seam-marker
  "Will draw red overlays at the selected points
  ie. the points selected to be cropped"
  [spec
   seams
   plot-height]
  (if (empty? seams)
    spec
    (update spec
           :data
           #(into %
                  [{:values (into [] (map (fn [pos] (vector pos
                                                            plot-height))
                                          seams))
                    :attribs {:fill "#bfbfbf" :stroke "dimgray" :stroke-dasharray "1 5"}
;;                    :bar-width 100
                    :interleave 1
                    :layout  viz/svg-bar-plot}]))))

(defn plot-points
  ""
  [width
   height
   points
   max-position
   max-count
   crop-left
   crop-right
   seams]
  (let [right-crop-distance (* (- 1 crop-right)
                               max-position)
        right-crop-points (filter #(> (first %)
                                      right-crop-distance)
                                  points)
        left-crop-distance (* crop-left
                              max-position)
        left-crop-points (filter #(< (first %)
                                     left-crop-distance)
                                 points)
        crop-points (concat right-crop-points
                            left-crop-points)
        graph-height (nice-max-count max-count)]
    (sausage.svg/render-as-jfx-image (-> (grid-spec width
                                                    height
                                                    max-position
                                                    graph-height)
                                         (add-points points)
                                         (add-red-overlay crop-points)
                                         (add-seam-marker seams
                                                          graph-height)
                                         (viz/svg-plot2d-cartesian)
                                         (#(svgthing/svg {:width width
                                                          :height height}
                                                         %))
                                         (svgthing/serialize))
                                     width
                                     height)))
