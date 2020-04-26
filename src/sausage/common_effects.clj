(ns sausage.common-effects)

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
(defn update-unscanned-areas
  [snapshot
   core-number]
  (let [image-width-pix (-> snapshot
                            :cores
                            (.get core-number)
                            :optical
                            :image
                            .getWidth) ;; 9 Pixels
        mm-per-pixel (:mm-per-pixel snapshot) ;; 0.5 mm/pixel
        scan (-> snapshot
                 :cores
                 (.get core-number)
                 :xrf-scan
                 :element-counts)
        scan-start-mm (-> scan
                          first
                          :position
                          read-string) ;; 1.25mm
        scan-end-mm (-> scan
                        last
                        :position
                        read-string)  ;; 3.75mm
        ]
    (-> snapshot
        (assoc-in  [:cores
                    core-number
                    :optical
                    :unscanned-left-pix]
                   (int (Math/floor (/ scan-start-mm    ;; (floor 2.5)
                                       mm-per-pixel)))) ;; 2 Pixels entirely unscanned
        (assoc-in [:cores
                   core-number
                   :optical
                   :unscanned-right-pix]
                  (- image-width-pix
                     (int (Math/ceil (/ scan-end-mm         ;; 9 - (ceil 7.5)
                                        mm-per-pixel)))))))) ;; 9-8 = 1 Pixel entirely unscanned



;; Updates the core's 'display image' based on
;; the internally stored 'optical image'
;;
;; NOTE: This is done because we want to avoid
;; operations on the underlying 'optical image'


(defn overlap?
  "Check if CORE-A and CORE-B overlap in any way
  RETURN: TRUE/FALSE"
  [core-a
   core-b]
  (if (>= (-> core-b :start-mm)
          (+ (-> core-a :start-mm)
             (-> core-a :length-mm))) ;; end-mm
    false
    true))

(defn overlapping-cores
  "Given a vector of CORES, it will check which overlap
  and gives you two lists:
  - not-overlapping: a selection of cores that don't overlap (biased towards 0mm)
  - overlapping: the remaining cores that did overlap (and may mutually overlap)
  RETURNS: [[not-overlapping][overlapping]]"
  [cores]
  (if (empty? cores)
    [[][]]
    (let [current-core (first cores)
          later-cores (rest cores)
          [overlapping-current-core
           not-overlapping-current-core] (split-with (partial overlap? current-core)
                                                     later-cores)
          [current-level-cores
           other-level-cores] (overlapping-cores not-overlapping-current-core)]
      [(into [current-core]
             current-level-cores)
       (into (into [] overlapping-current-core)
             other-level-cores)])))

(defn pyramid-stack
  "Stacks CORES into a 'pyramid'.
  Lower rows as tightly as possible - biased to 0mm
  ie. it takes the first core, then chooses the next core that fits next to it
  and then the next.. etc.
  Cores that don't fit on the first row we again try to fit on the next row.
  This happens recursively.
  RETURNS:
  [[core-x core-y core-z] ;; first row
   [core-a core-b]        ;; second row
   [core-c]]               ;; third row ... etc.
  "
  [cores]
  (let [[current-level-cores
         other-cores] (overlapping-cores cores)]
    (if (empty? other-cores)
      [current-level-cores
       other-cores]
      (into [current-level-cores]
            (pyramid-stack other-cores)))))

(defn inject-index
  "Given a VECTOR-OF-MAPS it will add and index key into each map.
  key is named: :tracking-index
  After processing the maps you can use the injected index
  to figure out which one it was"
  [vector-of-maps]
  (map #(assoc %1
               :tracking-index
               %2)
       vector-of-maps
       (range (count vector-of-maps))))

(defn reduce-to-indeces
  "Given a MAP a tracking :tracking-index
  RETURN: index
  Given a VECTOR recursively call this function
  RETURN: vector of vector of vector... of indeces
  EXAMPLE OUTPUT:
  [[0 2 4]
   [1]
   [3 5]]"
  [element]
  (if (map? element)
    (:tracking-index element)
    (map reduce-to-indeces element)))

(defn sort-cores
  "Given a vector of CORES,
  RETURN: a vector sorted by :start-mm
  NOTE: The core-number/index will change"
  [cores]
  (into [] (sort #(< (:start-mm %1)
                     (:start-mm %2))
                 cores)))

;; Adjusts the CORE list so that the cores are in order based on :start-mm
;; Then (re)generates a pyramid/stacked layout so we can then look up which
;; core shows up on which level
(defn update-core-order
  [snapshot]
  (let [with-updated-core-order (-> snapshot
                                    (update :cores
                                            sort-cores))]
    (-> with-updated-core-order
        (assoc :layout
               (-> with-updated-core-order
                   :cores
                   inject-index
                   pyramid-stack
                   reduce-to-indeces)))))



(defn- does-vector-contain-value
  "Local helper - Checks if a VECTOR contains a VALUE
  RETURNS: TRUE/FALSE"
  [value vector]
  (reduce #(or %1 (= %2 value))
          false
          vector))

(defn get-core-row
  "Give a CORE-NUMBER and LAYOUT
  RETURN: Which row the core should be displayed on"
  [core-number layout]
  (let [does-row-have-core? (map (partial does-vector-contain-value
                                          core-number)
                                 layout)
        row-with-core (first (keep-indexed #(if (true? %2) %1)
                                           does-row-have-core?))]
    row-with-core))

(defn update-core-length-HELPER
  "Given a CORE and the optical image's MM-PER-PIXEL
  RETURN: A core with an updated :length-mm
  NOTE: This is either derived from the optical scan size
        or the xrf-scan size
  DEGENERATE: If there is no :optical or :xrf-scan then
              the length is reset to a global default  "
  [core
   mm-per-pixel]
  (if (nil? (-> core :optical))
    (if (nil? (-> core :xrf-scan))
      core ;;(assoc core :length-mm fixed-default-core-length)
      (assoc core :length-mm (* mm-per-pixel
                                (-> core
                                    :xrf-scan
                                    :element-counts
                                    last
                                    :position
                                    read-string))))
    (assoc core :length-mm (* mm-per-pixel
                              (-> core
                                  :optical
                                  :image
                                  .getWidth)))))

;; Makes sure the :length-mm is set properly based on the
;; :optical scan and the :xrf-scan
(defn update-core-length
  [snapshot
   core-number]
  (let [core (-> snapshot
                 :cores
                 (get core-number))]
    (-> snapshot
        (assoc-in [:cores
                   core-number]
                  (update-core-length-HELPER core
                                             (-> snapshot
                                                 :mm-per-pixel)))
        update-core-order)))

(defn update-core-start
  [snapshot
   {:keys [fx/event
           core-number]}]
  (try
    (let [input-value-mm event
          mm-per-pixel (-> snapshot
                           :mm-per-pixel)
          rounded-to-pixel (Math/round (/ input-value-mm
                                          mm-per-pixel))
          corrected-start-mm (* rounded-to-pixel
                                mm-per-pixel)]
      (println "corrected" corrected-start-mm)
      (-> snapshot
          (assoc-in [:cores
                     core-number
                     :start-mm]
                    corrected-start-mm)
          update-core-order))
    (catch Exception ex
      (println "Invalid core-start input")
      snapshot)))

(defn add-core
  [snapshot
   _]
  (let [num-cores (-> snapshot
                      :cores
                      count)]
    (-> snapshot
        (assoc-in [:cores
                   num-cores]
                  {:start-mm (if (nil? (-> snapshot
                                           :cores
                                           last))
                               0.0 ;; if no core to tack on to, then set to zero
                               (+ (-> snapshot
                                      :cores
                                      last
                                      :start-mm)
                                  (-> snapshot
                                      :cores
                                      last
                                      :length-mm)))
                   :optical nil
                   :xrf-scan nil
                   :crop-left 0
                   :crop-right 0
                   :length-mm 300.0 ;; TODO: FIXED VALUE
                   :seams []})
        update-core-order)))

(defn remove-core
  "If the EVENT contains a `:core-number` then that core is deleted
  Otherwise the last core is deleted"
  [snapshot
   {:keys [core-number]}]
  (let [
        updated (if (nil? core-number)
                  (-> snapshot
                      (assoc :cores
                             (pop (:cores snapshot))))
                  (-> snapshot
                      (update
                       :cores
                       #(vec (concat (subvec % 0 core-number )
                                     (subvec % (inc core-number)))))
                      update-core-order))]
    (if (empty? (:cores updated))
      (-> updated
          (add-core nil))
      updated)))


