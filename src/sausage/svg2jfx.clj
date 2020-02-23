(ns sausage.svg2jfx
   (:import  [afester.javafx.svg SvgLoader]))

;; ## Converting SVGs to JavaFX objects
;; Takes a string and turns it into an input stream
(defn string->stream
  ([s] (string->stream s "UTF-8"))
  ([s encoding]
   (-> s
       (.getBytes encoding)
       (java.io.ByteArrayInputStream.))))

;; Use the FranzXaver library to turn a string of XML describing an SVG into a JavaFX compatible Group Node (which shows up as a picture) This is using Batik under the hood somehow	
(defn svg-to-javafx-group
  [svg-xml-string]
  (.loadSvg (SvgLoader.) (string->stream svg-xml-string)))
