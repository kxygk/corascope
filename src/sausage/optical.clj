(ns sausage.optical
  (:import [javafx.scene.input KeyCode KeyEvent]
           boofcv.io.image.ConvertBufferedImage
           boofcv.io.image.UtilImageIO
           boofcv.struct.image.ImageType
           boofcv.core.image.ConvertImage
           boofcv.struct.image.GrayU8
           boofcv.alg.misc.ImageMiscOps
           javafx.embed.swing.SwingFXUtils
           javafx.stage.DirectoryChooser
           javafx.stage.FileChooser
           java.io.File
           java.io.FileWriter
           javafx.stage.Stage
           javax.imageio.ImageIO))

(defn load-image
  [file]
  (println "loading image")
  (println file)
  (-> file
      (.getCanonicalPath)
      (boofcv.io.image.UtilImageIO/loadImage)
      ;;      (ImageMiscOps/flipHorizontal)
      (ConvertBufferedImage/convertFrom true
                                        (ImageType/pl 3
                                                      GrayU8))))

(defn save-fx-image
  ""
  [directory
   fx-image
   name]
  (ImageIO/write (SwingFXUtils/fromFXImage fx-image
                                           nil)
                 "tiff"
                 (File. (str directory
                             "/" name))))

(defn to-fx-image
  [boofcv-image]
  (-> boofcv-image
      (ConvertBufferedImage/convertTo nil true)
      (SwingFXUtils/toFXImage nil)))

(defn flip-image
  [boofcv-image]
  (let [to-flip (.clone boofcv-image)]
    (-> to-flip
        (.getBand 0)
        ImageMiscOps/flipHorizontal)
    (-> to-flip
        (.getBand 1)
        ImageMiscOps/flipHorizontal)
    (-> to-flip
        (.getBand 2)
        ImageMiscOps/flipHorizontal)
    to-flip))

(defn join-horizontally
  "Join two boofCV images horizontally into one.
  ImageA will be on the left
  ImageB will be on the right"
  [imageA
   imageB]
  (cond (nil? imageB) imageA ;; "base" cases
        (nil? imageA) imageB
        :else
        (let [imageA-width (.getWidth imageA)
              imageB-width (.getWidth imageB)
              height (.getHeight imageA)
              width (+ imageA-width
                       imageB-width)
              merged-image (boofcv.struct.image.Planar. boofcv.struct.image.GrayU8
                                                        width
                                                        height
                                                        3)
              subimageA (.subimage merged-image
                                   0
                                   0
                                   imageA-width
                                   height)
              subimageB (.subimage merged-image
                                   imageA-width
                                   0
                                   (+ imageA-width
                                      imageB-width)
                                   height)]
          (.setTo subimageA imageA)
          (.setTo subimageB imageB)
          merged-image)))

(defn crop
  "Crops a BoofCV image by crop-left/crop-right - which are fractional
  NOTE: Don't forget that the image is flipped internally!!
  Hence why the left/right are flipped here"
  [boofcv-image
   crop-left-pix
   crop-right-pix]
  (cond (== 0
            crop-left-pix
            crop-left-pix)
        boofcv-image
        :else
        (let [width (.getWidth boofcv-image)
              height (.getHeight boofcv-image)
              crop-start crop-right-pix
              crop-end (- width
                          crop-left-pix)]
          (.subimage boofcv-image
                     crop-start      ;; inclusive
                     0               ;; inclusive
                     crop-end        ;; exclusive
                     height          ;;exclusive
                     nil))))
