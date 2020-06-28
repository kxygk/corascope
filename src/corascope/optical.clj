(ns corascope.optical
  (:require
   [corascope.state :as state]
   [cljfx.api :as fx])
  (:import [javafx.scene.input KeyCode KeyEvent]
           boofcv.io.image.UtilImageIO
           boofcv.struct.image.ImageType
           boofcv.io.image.ConvertBufferedImage
           boofcv.core.image.ConvertImage
           boofcv.struct.image.GrayU8
           javafx.embed.swing.SwingFXUtils
           javafx.stage.DirectoryChooser
           javafx.stage.FileChooser
           java.io.File
           java.io.FileWriter
           javafx.stage.Stage
           javax.imageio.ImageIO))

(defn pad-front
  "Insert a blank of `start-pix` in front of `image`"
  [image
   start-pix]
  (if (zero? start-pix)
    image
    (let [image-width (.getWidth image)
          final-height (.getHeight image)
          final-width (+ image-width
                         start-pix)
          merged-image (boofcv.struct.image.Planar. boofcv.struct.image.GrayU8
                                                    final-width
                                                    final-height
                                                    3)
          subimage-area (.subimage merged-image
                                   0
                                   0
                                   image-width
                                   final-height)]
      (.setTo subimage-area image)
      merged-image)))

(defn- join-horizontally
  "Join two boofCV images horizontally into one.
  ImageA will be on the left
  ImageB will be on the right"
  [imageA
   imageB
   gap-pix]
  (cond (nil? imageB) imageA ;; "base" cases
        (nil? imageA) imageB
        :else
        (let [imageA-width (.getWidth imageA)
              imageB-width (.getWidth imageB)
              height (.getHeight imageA)
              width (+ imageA-width
                       imageB-width
                       gap-pix)
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
                                   (+ imageA-width
                                      gap-pix)
                                   0
                                   (+ imageA-width
                                      imageB-width
                                      gap-pix)
                                   height)]
          (.setTo subimageA imageA)
          (.setTo subimageB imageB)
          merged-image)))

(defn merge-cores
  "Attach the FROM-CORE-NUMBER optical image horizontally
  to the right of the INTO-CORE-NUMBER optical image
  This updates the INTO-CORE-NUMBER image and leaves
  the FROM-CORE-NUMBER image untouched"
  [snapshot
   {:keys [into-core-number
           from-core-number]}]
  (fx/swap-context snapshot
                   assoc-in
                   [:cores
                    into-core-number
                    :optical
                    :image]
                   ;; The merge order is backwards here ..
                   ;; b/c the core images are stored backwards
                   (join-horizontally (fx/sub snapshot
                                              state/optical-image
                                              from-core-number)
                                      (fx/sub snapshot
                                              state/optical-image
                                              into-core-number)
                                      (fx/sub snapshot
                                              state/distance-between-cores-pix
                                              into-core-number
                                              from-core-number))))

(defn crop
  "Crops a BoofCV image by crop-left/crop-right - which are fractional
  NOTE: Don't forget that the image is flipped internally!!
  Hence why the left/right are flipped here"
  [snapshot
   {:keys [core-number
           crop-left-pix
           crop-right-pix]}]
  (fx/swap-context snapshot
                   update-in
                   [:cores
                    core-number
                    :optical
                    :image]
                   (fn [boofcv-image]
                     (cond (== 0
                               crop-left-pix
                               crop-right-pix)
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
                                        nil))))))

(defn- load-image
  [file]
  (-> file
      (.getCanonicalPath)
      (boofcv.io.image.UtilImageIO/loadImage)
      ;;      (ImageMiscOps/flipHorizontal)
      (ConvertBufferedImage/convertFrom true
                                        (ImageType/pl 3
                                                      GrayU8))))

(defn load-data
  [snapshot
   {:keys [file
           core-number]}]
  (let [optical-image (corascope.optical/load-image file)]
    (-> snapshot
        (fx/swap-context assoc
                         :last-used-path
                         (.getParentFile file))
        (fx/swap-context assoc-in
                         [:cores
                          core-number
                          :optical
                          :image]
                         optical-image)
        #_(fx/swap-context state/snap-core-to-pixel ;; Creates circular dependency!
                         {:core-number core-number}))))

(defn load-dialogue
  [snapshot
   {:keys [core-number]}]
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Load Optical Image")
                   (.setInitialDirectory (fx/sub snapshot
                                                 state/last-used-path)))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (if (nil? file)
      snapshot
      (load-data snapshot
                 {:core-number core-number
                  :file file}))))

(defn save-data
  ""
  [snapshot
   {:keys [core-number]}]
  (let [file (-> (doto (FileChooser.)
                   (.setTitle "Save Optical Image")
                   (.setInitialFileName (str (fx/sub snapshot
                                                     state/autosave-filename
                                                     core-number)
                                             ".tif"))
                   (.setInitialDirectory  (fx/sub snapshot
                                                  state/last-used-path)))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showSaveDialog (Stage.)))]
    ;; Side Effect
    (if (some? file)
      (boofcv.io.image.UtilImageIO/saveImage (fx/sub snapshot
                                                     state/optical-image
                                                     core-number)
                                             (.getCanonicalPath file))))
    ;; return state unchanged
    snapshot)
