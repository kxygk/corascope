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
    (let [attributes (merge parent-attributes
                            (second svg))
          stroke (case (:stroke attributes)
                   nil :black
                   "none" :black
                   (if (= '#' (:stroke attributes))
                     (:stroke attributes)
                     (keyword (:stroke attributes))))
          fill (case  (:fill attributes)
                 nil :black
                 "none" :black
                 (if (= '#' (:fill attributes))
                   (:fill attributes)
                   (keyword (:fill attributes))))
          points (case (:points attributes)
                   nil []
                   (map read-string (-> (:points attributes)
                                        (clojure.string/split #"[ ,]"))))
          stroke-dash-array (case (:stroke-dasharray attributes)
                              nil []
                              (map read-string (-> (:stroke-dasharray attributes)
                                                   (clojure.string/split #"[ ,]"))))
          stroke-width (case (:stroke-width attributes)
                         nil 1.0
                         (:stroke-width attributes))
          font (case (:font-size attributes)
                 nil 10
                 "none" 10
                 (:font-size attributes))]
      (case (first svg)
        (:g :svg) [{:fx/type :group
                    :children
                    (->> (rest (rest svg))
                         (map #(render-with-jfx-shapes attributes %))
                         (apply concat)
                         (filter some?))}]
        :line [{:fx/type :line
                :stroke stroke
                :fill fill
                :stroke-dash-array stroke-dash-array
                :stroke-width stroke-width
                :start-x (-> attributes
                             :x1
                             read-string)
                :start-y (-> attributes
                             :y1
                             read-string)
                :end-x (-> attributes
                           :x2
                           read-string)
                :end-y (-> attributes
                           :y2
                           read-string)}]
        :text [{:fx/type :text
                :stroke stroke
                :fill fill
                :stroke-dash-array stroke-dash-array
                :stroke-width stroke-width
                :font font
                :x (-> attributes
                       :x
                       read-string)
                :y (-> attributes
                       :y
                       read-string)
                :text (nth svg 2)}]
        :polyline [{:fx/type :polyline
                    :stroke stroke
                    :points points
                    :stroke-dash-array stroke-dash-array
                    :stroke-width stroke-width}]
        :polygon [{:fx/type :polygon
                   :stroke stroke
                   :fill fill
                   :points points
                   :stroke-dash-array stroke-dash-array
                   :stroke-width stroke-width}]
        nil))))

(defn render-as-jfx-image
  "Given an SVG string, return a JFX Image"
  [svg-xml-string
   width
   height]
  (javafx.embed.swing.SwingFXUtils/toFXImage (render-as-buffered-image svg-xml-string
                                                                       width
                                                                       height)
                                             nil))
