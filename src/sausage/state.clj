(ns sausage.state
  (:require
   [clojure.core.cache :as cache]
   [cljfx.api :as fx]))

(def *context
  ""
  (atom (fx/create-context {:working-directory ""
                            :width 1000
                            :height 600
                            :zoom 1.0
                            :mm-per-pixel 0.5 ;; Common parameter across all cores
                            :mm-xrf-step-size 5 ;; Common parameter across all cores
                            :cores []
                            :displays []}
                           cache/lru-cache-factory)))

;; BASE SUBSCRIPTIONS
;; Instead of subscribing directly to keys
;; Subscribe through wrappers so that the state can be rearranged later
(defn cores
  [context]
  (fx/sub context
          :cores))

(defn width
  [context]
   (fx/sub context
           :width))

(defn displays
  [context]
  (fx/sub context
          :displays))

(defn working-directory
  [context]
  (fx/sub context
          :working-directory))

(defn last-used-path
  [context]
  (fx/sub context
          :last-used-path))

(defn zoom
  [context]
  (fx/sub context
          :zoom))

;; DEEPER SUBSCRIPTIONS

;; Cores
(defn get-core
  [context
   core-number]
  (get (fx/sub context cores)
       core-number))

(defn start-mm
  [context
   core-number]
  (:start-mm (fx/sub context
                     get-core
                     core-number)))

(defn creation-time
  "The core's creation time (in milliseconds since the epoch)"
  [context
   core-number]
  (:creation-time (fx/sub context
                          get-core
                          core-number)))

(defn fixed-side
  [context
   core-number]
  (:fixed-side (fx/sub context
                       get-core
                       core-number)))

;; Optical

(defn mm-per-pixel
  "Get the mm-per-pixel of the optical scan
  NOTE: Right now this is one global value across all scans
  It may be relatively easy to update this later to be a per-core-value. TBD.."
  [context
   core-number] ;; Unused - currently a global value
  (fx/sub context
          :mm-per-pixel))

(defn optical-scan
  [context
   core-number]
  (:optical (fx/sub context
                    get-core
                    core-number)))

(defn optical-image
  [context
   core-number]
  (:image (fx/sub context
                  optical-scan
                  core-number)))

(defn optical-scan-length-pix
  [context
   core-number]
  (.getWidth (fx/sub context
                     optical-image
                     core-number)))

;; XRF
(defn xrf-scan
  [context
   core-number]
  (:xrf-scan (fx/sub context
                     get-core
                     core-number)))


(defn xrf-file-name
  [context
   core-number]
  (:file-name (fx/sub context
                      xrf-scan
                      core-number)))

(defn xrf-header
  [context
   core-number]
  (:header (fx/sub context
                   xrf-scan
                   core-number)))

(defn xrf-columns
  [context
   core-number]
  (:columns (fx/sub context
                    xrf-scan
                    core-number)))

(defn xrf-element-counts
  [context
   core-number]
  (:element-counts (fx/sub context
                           xrf-scan
                           core-number)))

(defn xrf-last-scan-point
  [context
   core-number]
  (last (fx/sub context
                xrf-element-counts
                core-number)))

(defn xrf-first-scan-point
  [context
   core-number]
  (first (fx/sub context
                xrf-element-counts
                core-number)))

;; Seams
(defn seams
  [context
   core-number]
   (:seams (fx/sub context
                   get-core
                   core-number)))
;; Displays

(defn get-display
  [context
   display-number]
  (get (fx/sub context
               displays)
       display-number))

(defn display-height
  [context
   display-number]
  (:height (fx/sub context
                   get-display
                   display-number)))

(defn display-type
  [context
   display-number]
  (:type (fx/sub context
                 get-display
                 display-number)))

;; DERIVED
;;
;; Other
(defn core-name
  [context
   core-number]
  (let [xrf-file-name (fx/sub context
                              xrf-file-name
                              core-number)]
    (if (nil? xrf-file-name)
      (str "Core Number: " core-number)
      (clojure.string/replace xrf-file-name ".txt" ""))))

(defn num-cores
  [context]
  (count (fx/sub context
                 cores)))

(defn last-core
  [context]
  (last (fx/sub context
                cores)))

(defn num-displays
  [context]
  (count (fx/sub context
                 displays)))

(defn displays-total-height
  "Height of all displays summed up"
  [context]
  (reduce #(+ %1 (:height %2))
          0
          (fx/sub context
                  displays)))

(defn xrf-all-columns
  [context]
  (->> (range (fx/sub context
                      num-cores))
       (map #(fx/sub context
                     xrf-columns
                     %))
       (reduce clojure.set/union)))

;; Lengths
(defn pixels-per-mm
  "Get the mm-per-pixel of the optical scan
  NOTE: Right now this is one global value across all scans
  It may be relatively easy to update this later to be a per-core-value. TBD.."
  [context
   core-number] ;; Unused - currently a global value
  (/ 1
     (fx/sub context
             mm-per-pixel
             core-number)))

(defn optical-scan-length-mm
  [context
   core-number]
  (* (fx/sub context
             mm-per-pixel
             core-number)
     (fx/sub context
             optical-scan-length-pix
             core-number)))

        
(defn xrf-scan-length-mm
  "This value is maybe not what you would expect.
  It's just the position of the last measurements.
  NOT the distance from the first measurement to the last"
  [context
   core-number]
  (- (read-string (:position-mm (fx/sub context
                                     xrf-last-scan-point
                                     core-number)))
     0.0 #_(:position (fx/sub context
                        first-xrf-scan-point
                        core-number))))

(defn length-mm
  [context
   core-number]
  (if (some? (fx/sub context
                     optical-image
                     core-number))
    (fx/sub context
            optical-scan-length-mm
            core-number)
    (if (some? (fx/sub context
                       xrf-scan
                       core-number))
      (fx/sub context
              xrf-scan-length-mm
              core-number)
      300.0)))

(defn end-mm
  [context
   core-number]
  (+ (fx/sub context
             start-mm
             core-number)
     (fx/sub context
             length-mm
             core-number)))
    
(defn end-of-all-scans-mm
  "Last position of the last core"
  [context]
  (let [num-cores (fx/sub context
                          num-cores)]
    (if (zero? num-cores)
    0.0
    (apply max
           (mapv #(fx/sub context
                          end-mm
                          %)
                 (range num-cores ))))))

(defn distance-between-cores-pix
  "Get the distance between cores in rounded to the nearest pixels
  Unlike when cropping, this is rounding and not truncating"
  [context
   from-core-number
   to-core-number]
  (assert (some? (fx/sub context
                         mm-per-pixel
                         from-core-number))
          "Ooops.. The `from-core` doesn't seem to have a `:mm-per-pixel`")
  ;; If the two cores have images with different pixel densities ..
  ;; then it's a bit unclear how to round the distance to the nearest pixel
  (assert (= (fx/sub context
                     mm-per-pixel
                     from-core-number)
             (fx/sub context
                     mm-per-pixel
                     from-core-number))
          "Yikes! The two cores have different pixel densitites")  
  (Math/round (/ (- (fx/sub context
                            start-mm
                            to-core-number)
                    (fx/sub context
                            end-mm
                            from-core-number))
                 (fx/sub context
                         mm-per-pixel
                         from-core-number))))

(defn distance-between-cores-mm
  "Gets the distance between cores in mm.
  If there are optical images, then it will round to the nearest pixel
  Otherwise it just gets the distance from
  - the last point on one core
  -to the `:start-mm` of the other"
  [context
   from-core-number
   to-core-number]
  (if (nil? (fx/sub context
                    optical-scan
                    from-core-number))
    ;;return the straight-forward distance from `:end-mm` to `:start-mm`
    (- (fx/sub context
               start-mm
               to-core-number)
       (fx/sub context
               end-mm
               from-core-number))
    ;;return a distance in mm rounded to the nearest pixel
    (* (fx/sub context
               distance-between-cores-pix
               from-core-number
               to-core-number)
       (fx/sub context
               mm-per-pixel
               from-core-number))))
    
;; Crop

(defn crop-slider-left
  "This returns the crop-percentage.
  But you shouldn't call this subscription directly
  B/c you should be rounding to the nearest pixel"
  [context
   core-number]
  (:crop-slider-left (fx/sub context
                             get-core
                             core-number)))

(defn crop-slider-right
  "This returns the crop-percentage.
  But you shouldn't call this subscription directly
  B/c you should be rounding to the nearest pixel"
  [context
   core-number]
  (:crop-slider-right (fx/sub context
                              get-core
                              core-number)))



(defn unscanned-left-pix
  "See the `docstring` for crop..
  This is also fixed to pixel positions"
  [context
   core-number]
  (if (and (some? (fx/sub context
                          optical-image
                          core-number))
           (some? (fx/sub context
                          xrf-scan
                          core-number)))
    (int (Math/floor (/ (read-string (:position-mm (fx/sub context
                                                        xrf-first-scan-point
                                                        core-number)))
                        (fx/sub context
                                mm-per-pixel
                                core-number))))
    ;; if both `optical` and `xrf` aren't present
    ;; then we say nothing is unscanned
    0))
  
(defn unscanned-left-mm
  "See the `docstring` for crop..
  But this isn't just equal to `start-mm`
  This is also fixed to pixel positions"
  [context
   core-number]
  (* (fx/sub context
             mm-per-pixel
             core-number)
     (fx/sub context
             unscanned-left-pix
             core-number)))

(defn unscanned-right-pix
  "See the `docstring` for crop..
  This is also fixed to pixel positions"
  [context
   core-number]
  (if (and (some? (fx/sub context
                          optical-image
                          core-number))
           (some? (fx/sub context
                          xrf-scan
                          core-number)))
    (int (Math/floor (/ (- (fx/sub context
                                   length-mm
                                   core-number)
                           (read-string (:position-mm (fx/sub context
                                                           xrf-last-scan-point
                                                           core-number))))
                        (fx/sub context
                                mm-per-pixel
                                core-number))))
    ;; if both `optical` and `xrf` aren't present
    ;; then we say nothing is unscanned
    0))
  
(defn unscanned-right-mm
  "See the `docstring` for crop..
  But this isn't just equal to `start-mm`
  This is also fixed to pixel positions"
  [context
   core-number]
  (* (fx/sub context
             mm-per-pixel
             core-number)
     (fx/sub context
             unscanned-right-pix
             core-number)))

(defn crop-left-pix
  "Get the left crop value in Pixels
  It will round down (under-crop) to the next pixel.
  Ex:
  - Given: a 100mm wide image
  - Given: 0.5mm/pixel
  - Your crop slider is set to crop 7.9%
  - This is equivalent to a crop of 7.9mm
  - We round this to 7.5mm and return 16pix
  "
  [context
   core-number]
  (assert (some? (fx/sub context
                         mm-per-pixel
                         core-number))
          "Oh Oh! You can't crop pixels with no `mm-per-pixel`")
  (max (fx/sub context
               unscanned-left-pix
               core-number)
       (int (Math/floor (/ (* (fx/sub context
                                      crop-slider-left
                                      core-number)
                              (fx/sub context
                                      length-mm
                                      core-number))
                           (fx/sub context
                                   mm-per-pixel
                                   core-number))))))

(defn crop-left-mm
  "The crop will be along pixel lines if a `mm-per-pixel` is present
  Otherwise it'll crop at `crop-percent`*`length-m`"
  [context
   core-number]
  (if (some? (fx/sub context
                     mm-per-pixel
                     core-number))
    (max (fx/sub context
                 unscanned-left-mm
                 core-number)
         (* (fx/sub context
                    crop-left-pix
                    core-number)
            (fx/sub context
                    mm-per-pixel
                    core-number)))
    (* (fx/sub context
               crop-slider-left
               core-number)
       (fx/sub context
               length-mm
               core-number))))


(defn crop-right-pix
  "Get the left crop value in Pixels
  It will round down (under-crop) to the next pixel.
  Ex:
  - Given: a 100mm wide image
  - Given: 0.5mm/pixel
  - Your crop slider is set to crop 7.9%
  - This is equivalent to a crop of 7.9mm
  - We round this to 7.5mm and return 16pix
  "
  [context
   core-number]
  (assert (some? (fx/sub context
                         mm-per-pixel
                         core-number))
          "Oh Oh! You can't crop pixels with no `mm-per-pixel`")
  (max (fx/sub context
               unscanned-right-pix
               core-number)
       (int (Math/floor (/ (* (fx/sub context
                                      crop-slider-right
                                      core-number)
                              (fx/sub context
                                      length-mm
                                      core-number))
                           (fx/sub context
                                   mm-per-pixel
                                   core-number))))))

(defn crop-right-mm
    "The crop will be along pixel lines if a `mm-per-pixel` is present
  Otherwise it'll crop at `crop-percent`*`length-m`"
    [context
     core-number]
    (if (some? (fx/sub context
                       mm-per-pixel
                       core-number))
      (max (fx/sub context
                   unscanned-right-mm
                   core-number)
           (* (fx/sub context
                      crop-right-pix
                      core-number)
              (fx/sub context
                      mm-per-pixel
                      core-number)))
      (* (fx/sub context
                 crop-slider-right
                 core-number)
         (fx/sub context
                 length-mm
                 core-number))))

(defn start-mm-after-crop
  [context
   core-number]
  (let [current-start-mm (fx/sub context
                                 start-mm
                                 core-number)]
    (case (fx/sub context
                  fixed-side
                  core-number)
      :left current-start-mm
      nil  (+ current-start-mm
              (fx/sub context
                      crop-left-mm
                      core-number))
      :right (+ current-start-mm
                (fx/sub context
                        crop-left-mm
                        core-number)
                (fx/sub context
                        crop-right-mm
                        core-number)))))

(defn end-mm-after-crop
  [context
   core-number]
  (let [current-end-mm (fx/sub context
                                 end-mm
                                 core-number)]
    (case (fx/sub context
                  fixed-side
                  core-number)
      :right current-end-mm
      nil  (- current-end-mm
              (fx/sub context
                      crop-right-mm
                      core-number))
      :left (- current-end-mm
                (fx/sub context
                        crop-left-mm
                        core-number)
                (fx/sub context
                        crop-right-mm
                        core-number)))))

;; Core Row

(defn- overlap?
  "Check if CORE-A and CORE-B overlap in any way.
  CORE-B comes after CORE-A
  RETURN: TRUE/FALSE"
  [context
   core-a-number
   core-b-number]
  (if (>= (fx/sub context
                  start-mm
                  core-b-number)
          (fx/sub context
                  end-mm
                  core-a-number))
    false
    true))


(defn overlapping-cores
  "Given a vector of CORES, it will check which overlap
  and gives you two lists:
  - not-overlapping: a selection of cores that don't overlap (biased towards 0mm)
  - overlapping: the remaining cores that did overlap (and may mutually overlap)
  RETURNS: [[not-overlapping][overlapping]]"
  ([context
    core-indeces]
   (overlapping-cores context
                      core-indeces
                      []
                      []))
  ([context
    core-indeces
    not-overlapping
    overlapping]
   (let [current-core-num (first core-indeces)
         [overlapping-current-core
          not-overlapping-current-core] (split-with (partial overlap?
                                                             context
                                                             current-core-num)
                                                    (rest core-indeces))]
     (if (empty? not-overlapping-current-core)
       [(conj not-overlapping current-core-num)
        (into overlapping
              overlapping-current-core)]
       (overlapping-cores context
                          not-overlapping-current-core
                          (conj not-overlapping current-core-num)
                          (into overlapping
                                overlapping-current-core))))))

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
  [context
   core-indeces]
  (let [[current-level-cores
         other-cores] (overlapping-cores context
                                         core-indeces)]
    (if (empty? other-cores)
      [current-level-cores
       other-cores]
      (into [current-level-cores]
            (pyramid-stack context
                           other-cores)))))


(defn layout
  [context]
  (fx/sub context
          pyramid-stack
          (range (fx/sub context
                         num-cores))))

(defn can-merge?
  [context]
  (empty? (second (fx/sub context
                  layout))))

(defn- does-vector-contain-value
  "Local helper - Checks if a VECTOR contains a VALUE
  RETURNS: TRUE/FALSE"
  [value vector]
  (reduce #(or %1 (= %2 value))
          false
          vector))
  
(defn core-row
  [context
   core-number]
    (let [does-row-have-core? (map (partial does-vector-contain-value
                                          core-number)
                                   (fx/sub context
                                           layout))
        row-with-core (first (keep-indexed #(if (true? %2) %1)
                                           does-row-have-core?))]
    row-with-core))
