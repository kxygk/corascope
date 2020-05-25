(ns sausage.common-effects
  (:require [sausage.xrf]
            [sausage.optical]
            [sausage.state :as state]
            [cljfx.api :as fx]))

;;## Example:
;;Pixel index:  0 1 2 3 4 5 6 7 8
;;             |-|-|-|-|-|-|-|-|-|
;;             0mm               4.5mm
                                        ;                   |~~~~~~~~~|
;;XRF-Scan          |---------|
;;                  1.25mm    3.75mm
;;
;; Optical scan   : mm/pixel = 0.5mm
;;
;; Unscanned Left : 2 Pixels / 1.0mm      ;; Third Pixel overlaps and remains
;;
;; Unscanned Right: 1 Pixel  / 0,5mm      ;; Second Pixel overlaps and remains
;;
;; Note: Cropping needs to be conservative, so unscanned areas need to be
;;       /under-selected/ so that when you merge data doesn't overlap
;;
;;## Degenerate cases:
;;# Scan starts on pixel edge
;; scan-start-mm: 1.0mm
;; scan-end-mm  : 4.0mm
;;
;;Pixel number: 1 2 3 4 5 6 7 8 9
;;             |-|-|-|-|-|-|-|-|-|
;;             0mm               4.5mm
                                        ;                  |~~~~~~~~~~~|
;;XRF-Scan         |-----------|
;;                 1.0mm       4.0mm
;;
;; scan-start-pix 2nd pixel
;; scan-end-pix 8th pixel
;;
;; unscanned-left-pix: 2 pixel
;; unscanned-right-pix:1 pixel
;;
;;# Method annotated from example
;; (defn update-unscanned-areas
;;   [snapshot
;;    core-number]
;;   (let [image-width-pix (-> snapshot
;;                             :cores
;;                             (.get core-number)
;;                             :optical
;;                             :image
;;                             .getWidth) ;; 9 Pixels
;;         mm-per-pixel (:mm-per-pixel snapshot) ;; 0.5 mm/pixel
;;         scan (-> snapshot
;;                  :cores
;;                  (.get core-number)
;;                  :xrf-scan
;;                  :element-counts)
;;         scan-start-mm (-> scan
;;                           first
;;                           :position
;;                           read-string) ;; 1.25mm
;;         scan-end-mm (-> scan
;;                         last
;;                         :position
;;                         read-string)  ;; 3.75mm
;;         ]
;;     (-> snapshot
;;         (assoc-in  [:cores
;;                     core-number
;;                     :optical
;;                     :unscanned-left-pix]
;;                    (int (Math/floor (/ scan-start-mm    ;; (floor 2.5)
;;                                        mm-per-pixel)))) ;; 2 Pixels entirely unscanned
;;         (assoc-in [:cores
;;                    core-number
;;                    :optical
;;                    :unscanned-right-pix]
;;                   (- image-width-pix
;;                      (int (Math/ceil (/ scan-end-mm         ;; 9 - (ceil 7.5)
;;                                         mm-per-pixel)))))))) ;; 9-8 = 1 Pixel entirely unscanned

(defn sort-cores
  [snapshot
   _]
  (-> snapshot
      (fx/swap-context update
                       :cores
                       (fn [cores] ;; can be rewritten with no `fn` ?
                         (->> cores
                              (sort #(< (:start-mm %1)
                                        (:start-mm %2)))
                              (into []))))))

(defn update-core-start
  [snapshot
   {:keys [fx/event
           core-number]}]
  (try
    (let [input-value-mm event
          mm-per-pixel (fx/sub snapshot
                               state/mm-per-pixel
                               core-number)
          rounded-to-pixel (Math/round (/ input-value-mm
                                          mm-per-pixel))
          corrected-start-mm (* rounded-to-pixel
                                mm-per-pixel)]
      (-> snapshot
          (fx/swap-context assoc-in [:cores
                                     core-number
                                     :start-mm]
                           corrected-start-mm)
          (sort-cores nil)))
    (catch Exception ex
      (println "Invalid core-start input")
      snapshot)))

(defn add-core
  [snapshot
   _]
  (let [num-cores (fx/sub snapshot
                          state/num-cores)
        start-mm (fx/sub snapshot
                         state/end-of-all-scans-mm)]
    (-> snapshot
        (fx/swap-context assoc-in
                         [:cores
                          num-cores]
                         {:start-mm start-mm
                          :creation-time (System/currentTimeMillis)
                          :optical nil
                          :xrf-scan nil
                          :crop-slider-left 0
                          :crop-slider-right 0
                          :seams []}))))

(defn remove-core
  "If the EVENT contains a `:core-number` then that core is deleted
  Otherwise the last core is deleted"
  [snapshot
   {:keys [core-number]}]
  (let [
        updated (if (nil? core-number)
                  (-> snapshot
                      (fx/swap-context update
                                       :cores
                                       pop))
                  (-> snapshot
                      (fx/swap-context update
                                       :cores
                                       #(vec (concat (subvec % 0 core-number )
                                                     (subvec % (inc core-number)))))))]
    (if (zero? (fx/sub updated
                       state/num-cores))
      (-> updated
          (add-core nil))
      updated)))

;; Crop both :optical and :xrf-data
;; 1 - always crop areas that are optically scanned but have no xrf data
;; 2 - optionally crop more based on the slider positions
;; TODO: Simplify this .. with some destructuring or something
(defn crop-core
  [snapshot
   {:keys [core-number]}]
  ;; Crop values need to be precalculated
  ;; Once you crop the XRF then the subscriptions in the optical crop will recalculate
  ;; It won't give the desired effect
  (-> snapshot
      (sausage.xrf/crop {:core-number core-number
                         :crop-left-mm (fx/sub snapshot
                                               state/crop-left-mm
                                               core-number)
                         :crop-right-mm  (fx/sub snapshot
                                                 state/crop-right-mm
                                                 core-number)})
      (sausage.optical/crop {:core-number core-number
                             :crop-left-pix (fx/sub snapshot
                                                    state/crop-left-pix
                                                    core-number)
                             :crop-right-pix (fx/sub snapshot
                                                     state/crop-right-pix
                                                     core-number)})
      (fx/swap-context update
                       :seams
                       #(map (partial + (- (fx/sub snapshot
                                                   state/crop-left-mm
                                                   core-number)))
                             %))
      (fx/swap-context assoc-in
                       [:cores
                        core-number
                        :crop-slider-left]
                       0.0)
      (fx/swap-context assoc-in
                       [:cores
                        core-number
                        :crop-slider-right]
                       0.0)))



;; ;; seams only happen in core=0
;;   ;; but inserting a conditional in a threading macro is messy
;;   (update-in
;;    [:cores
;;     core-number
;;     :seams]
;;    #(map (partial + (- crop-left-mm)) %)))))


(defn merge-cores
  "Given a CORE-A and CORE-B and a MM-PER-PIXEL for their respective images
  RESULT: One core with optical and xrf-scans merged"
  [snapshot
   {:keys [into-core-number
           from-core-number] :as core-numbers}]
  (-> snapshot
      (sausage.optical/merge-cores core-numbers)
      (sausage.xrf/merge-cores core-numbers)
      (remove-core {:core-number from-core-number})))

(defn merge-all-cores
  "Merges the first core and the second core
  Then the first core with the third core
  then the fourth.. etc.
  Each merge makes the first core grow!
  Stops when there is only one core left"
  [snapshot
   _]
  (if (= 1
         (fx/sub snapshot
                 state/num-cores))
    snapshot
    (recur (merge-cores snapshot
                        {:into-core-number 0
                         :from-core-number 1})
           nil)))

(defn update-display-height
  [snapshot
   {:keys [fx/event
           display-number
           display-height]}]
  (-> snapshot
      (fx/swap-context assoc-in
                       [:displays
                        display-number
                        :height]
                       display-height)))
