(ns corascope.plot
  (:require [corascope.svg]
            [thi.ng.geom.core :as g] ;; The graphing libraires
            [thi.ng.math.core :as m]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]))

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
      (* (+ num-tick-marks
            0.5)
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
             :label-style {:stroke "darkgray"
                           :stroke-width 0.2
                           :fill "darkgray"
                           :font-family nil}
             :label-dist  (- height 1)
             ;;             :major-size 0
             :major 500
             :attribs {:stroke "darkgray"} ;; axis line attributes
             })
   :y-axis (viz/linear-axis
            {:domain      (if (== 1.0
                                  max-count)
                            [-0.5 1.5] ;; better for 0-1/binary validity plots
                            [-0.01 max-count])
             :range       [height 0]
             ;; puts the axis out of view (can't show the grid with no axis)
             :pos         0 ;; major-size default
             :visible true
             :major (nearest-power-of-ten max-count)
             :label-dist 0
             :label-y 0
             :major-size 5
             :minor-size 5
             :label-style {:stroke "darkgray"
                           :stroke-width 0.2
                           :fill "darkgray"
                           :font-family nil}
             :attribs {:stroke "transparent"} ;; axis line attributes
             ;; :label-style {:fill "red" :text-anchor "start"}
             })
   :data []
   ;; :grid   {;:attribs {:stroke "#caa"}
   ;;          ; :minor-x true
   ;;          ; :major-x true
   ;;          ; :major-y true
   ;;          :minor-y true
   ;;          }
   })


(defn- add-lines
  "Add the lines to the graph"
  ([spec
    points]
   (add-lines spec
              points
              "black"))
  ([spec
    points
    color]
    (-> spec ;; do nothing.. else:
         (update :data
                 #(conj % {:values  points
                           :attribs {:fill "none" :stroke color :stroke-width 1.25}
                           :layout  viz/svg-line-plot})))))

(defn- add-points
  "Add the points (as little triangles) to the graph
  Also has an overload to supply your own point-shape function
  See `add-words` for an example"
  ([spec
    points]
   (add-points spec
               points
               (viz/svg-triangle-down 6)))
  ([spec
    points
    point-shape]
   (if (empty? points)
     spec ;; do nothing.. else:
     (assoc spec
            :data
            [{:values  points
              :attribs {:fill "none" :stroke "black"}
              :shape point-shape
              :layout  viz/svg-scatter-plot}
             ]))))

(defn- add-words
  "Overload for `add-points` to directly display a word/letter at plot point
  See `geom-viz` for details, but the shape supplied is an svg function"
  [spec
   points
   word]
  (add-points spec
              points
              (fn [[[x y]]] (svg/text [x y] word))))

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
                     :attribs {:fill "pink" :stroke "red" :stroke-width 1.00}
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
   seams
   lines?]
  (let [right-crop-points (filter #(> (first %)
                                      (- max-position
                                         crop-right))
                                  points)
        left-crop-points (filter #(< (first %)
                                     crop-left)
                                 points)
        crop-points (concat right-crop-points
                            left-crop-points)
        graph-height (nice-max-count max-count)]
    (cond-> (grid-spec width
                       height
                       max-position
                       graph-height)
      lines? (add-lines points)
      (not lines?)  (add-points points)
      true (add-red-overlay crop-points)
      true (add-seam-marker seams
                            graph-height)
      true (viz/svg-plot2d-cartesian))))

 (-> (grid-spec width
                height
                max-position
                graph-height)
     (add-plot points lines?)
     (add-red-overlay crop-points)
     (add-seam-marker seams
                      graph-height)
     (viz/svg-plot2d-cartesian))))
