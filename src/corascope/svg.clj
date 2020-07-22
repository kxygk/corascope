(ns corascope.svg
  (:import  com.kitfox.svg.SVGCache
            java.awt.image.BufferedImage
            java.awt.RenderingHints
            javafx.embed.swing.SwingFXUtils))

(defn render-as-buffered-image
  "Given an SVG string, return a native Java BufferedImage
  ie. java.awt.image.BufferedImage"
  [svg-xml-string
   width
   height]
  (let [universe (com.kitfox.svg.SVGCache/getSVGUniverse)
        uri (.loadSVG universe
                      (java.io.StringReader. svg-xml-string)
                      "unused-placeholder")
        diagram (.getDiagram universe uri)
        buffered-image (java.awt.image.BufferedImage.
                        width
                        height
                        java.awt.image.BufferedImage/TYPE_4BYTE_ABGR)
        graphics-2d (.createGraphics buffered-image)]
    (.setRenderingHint graphics-2d
                       java.awt.RenderingHints/KEY_ANTIALIASING
                       java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
    (.render diagram graphics-2d)
    (.removeDocument universe
                     uri)
    buffered-image))


(defn svg-to-jfx-attributes
  "The attributes in `thi-ng/geom`'s SVG hiccup
  are very similar to the CLJFX JavaFX ones.
  There are minor differences and defaults

  This helper function massages the SVG ones into a JFX friendly variety"
  [svg-attributes]
  (-> svg-attributes
      (update :stroke #(case %
                         nil :black
                         "none" :black
                         (if (= \# %)
                           %
                           (keyword %))))
      (update :fill #(case %
                       nil "transparent"
                       "none" "transparent"
                       (if (= \# %)
                         %
                         (keyword %))))
      (update :points #(case %
                         nil []
                         [] []
                         (map read-string (-> %
                                              (clojure.string/split #"[ ,]")))))
      (update :stroke-width #(case %
                               nil 1.0
                               %))
      (update :stroke-dasharray #(case %
                                   nil []
                                   (map read-string (-> %
                                                        (clojure.string/split #"[ ,]")))))
      (clojure.set/rename-keys {:stroke-dasharray :stroke-dash-array})
      (update :font-size #(case %
                            nil 10
                            "none" 10
                            %))))

(defn render-with-jfx-shapes
  ""
  [parent-attributes svg]
  (if (coll? (first svg))
    (if (empty? (rest svg))
      (render-with-jfx-shapes parent-attributes
                              (first svg))
      (concat (render-with-jfx-shapes parent-attributes
                                      (first svg))
              (render-with-jfx-shapes parent-attributes
                                      (rest svg))))
    (let [new-attributes (second svg)
          merged-attributes (svg-to-jfx-attributes (merge parent-attributes
                                                          new-attributes))
          shape-attributes (select-keys merged-attributes
                                        [:stroke :fill :stroke-width :stroke-dash-array])]
      (case (first svg)
        (:g :svg) [{:fx/type :group
                   :children
                   (->> (rest (rest svg))
                        (map #(render-with-jfx-shapes merged-attributes %))
                        (apply concat)
                        (filter some?))}]
        ;; Unimplemented
        ;; SVG          JFX
        ;; :path        javafx.scene.shape.Path
        ;; :circle      javafx.scene.shape.Circle
        ;; :rect        javafx.scene.shape.Rectangle
        ;; :use         a link?
        :line [(merge shape-attributes
                     {:fx/type :line
                      :start-x (-> merged-attributes
                                   :x1
                                   read-string)
                      :start-y (-> merged-attributes
                                   :y1
                                   read-string)
                      :end-x (-> merged-attributes
                                 :x2
                                 read-string)
                      :end-y (-> merged-attributes
                                 :y2
                                 read-string)})]
        :text [(merge shape-attributes
                     {:fx/type :text
                      :font (-> merged-attributes
                                :font-size)
                      :x (-> merged-attributes
                             :x
                             read-string)
                      :y (-> merged-attributes
                             :y
                             read-string)
                      :text (nth svg 2)})]
        :polyline [(merge shape-attributes
                          {:fx/type :polyline
                           :points (-> merged-attributes
                                       :points)})]
        :polygon [(merge shape-attributes
                         {:fx/type :polygon
                          :points (-> merged-attributes
                                      :points)})]
        nil nil
        (println "Unexpected SVG element: "(first svg))))))

(defn render-as-jfx-image
  "Given an SVG string, return a JFX Image"
  [svg-xml-string
   width
   height]
  (javafx.embed.swing.SwingFXUtils/toFXImage (render-as-buffered-image svg-xml-string
                                                                       width
                                                                       height)
                                             nil))
