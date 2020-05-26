(ns sausage.optical
  (:require
   [sausage.state :as state]
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

;; (defn update-display-image
;;   [snapshot
;;    core-number]
;;   (let [optical (-> snapshot
;;                     :cores
;;                     (get core-number)
;;                     :optical
;;                     :image)]
;;     (assoc-in snapshot [:cores
;;                         core-number
;;                         :optical
;;                         :display]
;;               (-> optical
;;                   flip-image
;;                   to-fx-image))))

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
   event]
  (let [core-number (:core-number event) ;; TODO: Destructure
        file (-> (doto (FileChooser.)
                   (.setTitle "Open a project")
                   (.setInitialDirectory (fx/sub snapshot
                                                 state/last-used-path)))
                 ;; Could also grab primary stage instance to make this dialog blocking
                 (.showOpenDialog (Stage.)))]
    (if (nil? file)
      snapshot
      (let [optical-image (sausage.optical/load-image file)]
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
            #_(fx/swap-context update-display-image
                               core-number))))))
(defn save-data
  ""
  [snapshot
   {:keys [core-number]}]
  ;; Side Effect
  (boofcv.io.image.UtilImageIO/saveImage (fx/sub snapshot
                                                 state/optical-image
                                                 core-number)
                                         (str (str (java.time.LocalDate/now))
                                              "--"
                                              (fx/sub snapshot
                                                      state/core-name
                                                      core-number)
                                              ".tif"))
  ;; return state unchanged
  snapshot)
