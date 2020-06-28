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

(defn render-as-jfx-image
  "Given an SVG string, return a JFX Image"
  [svg-xml-string
   width
   height]
  (javafx.embed.swing.SwingFXUtils/toFXImage (render-as-buffered-image svg-xml-string
                                                                       width
                                                                       height)
                                             nil))
