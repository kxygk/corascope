(ns sausage.plot
  (:require [sausage.svg2jfx]
            [thi.ng.geom.core :as g] ;; The graphing libraires
            [thi.ng.math.core :as m]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svgthing]))


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
             :visible false
             :label-dist  (- height 10)
             :major-size 0
             :major 100
             })
             
   :y-axis (viz/linear-axis
            {:domain      [0 max-count]
             :range       [height 0]
             ;; puts the axis out of view (can't show the grid with no axis)
             :pos         0 ;; major-size default
             :visible true
             :major (/ (Math/pow 10 (Math/floor (Math/log10 max-count))) 2)
             :label-dist -5
             :label-y 10
             :major-size 0
             :minor-size -5
            ;; :label-style {:fill "red" :text-anchor "start"}
             })
             
   :grid   {;:attribs {:stroke "#caa"}
            ; :minor-x true
            ; :major-x true
            ; :major-y true
            :minor-y true
            }})

(defn- add-points
  "Add the points (as little triangles) to the graph"
  [spec
   points
   crop-points]
  (if (empty? points)
    spec ;; do nothing.. else:
    (assoc spec
            :data
            (filter identity [(if (not (empty? crop-points))
                                #_{:values  crop-points
                                 :attribs {:fill "pink" :stroke "red" :stroke-width 1.25}
                                 ;; :shape   (viz/svg-square 5)
                                 :layout  viz/svg-scatter-plot}
                                {:values  crop-points
                                 :attribs {:fill "pink" :stroke "red" :stroke-width 1.25}
                                 :bar-width 100
                                 :interleave 1
                                 :layout  viz/svg-bar-plot})
                              {:values  points
                               :attribs {:fill "none" :stroke "black" :stroke-width 2.25}
                               ;; :shape   (viz/svg-triangle-down 6)
                               :layout  viz/svg-line-plot}
                              ]))))

(defn plot-points
  ""
  [width
   height
   points
   max-position
   max-count
   crop-left
   crop-right]
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
                            left-crop-points)]
  (sausage.svg2jfx/svg-to-javafx-group (-> (grid-spec width
                                                      height
                                                      max-position
                                                      max-count)
                                           (add-points points
                                                       [])
                                           (viz/svg-plot2d-cartesian)
                                           (#(svgthing/svg {:width width
                                                            :height height}
                                                           %))
                                           (svgthing/serialize)))))
