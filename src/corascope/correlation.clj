(ns corascope.correlation)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; Two test vectors
(def vector-lengths 5000.0)

(def a-pos (double-array vector-lengths
                         (range 0.0 vector-lengths)))
(def a-val (double-array vector-lengths
                         (mapv (fn[_](rand 1000)) (range 0 vector-lengths))))
;; TODO: Try having the `val` be `int`s
(def b-pos (double-array vector-lengths
                         (range 0.0 vector-lengths)))
(def b-val (double-array vector-lengths
                         (mapv (fn[_](rand 1000)) (range 0 vector-lengths))))

(defn array-shift!
  "Shifts the array by SHIFT"
  [^doubles array
   ^double shift]
  (loop [index (dec (alength array))]
    (if (< index
           0)
      nil
      (do (aset array
                index
                (+ shift
                   (aget array
                         index)))
          (recur (dec index))))))

(defn zero-array!
  "Shift array so that the first element is zero"
  [^doubles array]
  (array-shift! array
                (- (aget array
                         0))))

(defn shift-range
  "Gets the bounds of how far to shift the TO-SHIFT
  array for the cross correlation"
  [^doubles overlayed-positions
   ^doubles to-shift-positions]
  (let [first-overlayed (aget overlayed-positions
                              0)
        last-overlayed (aget overlayed-positions
                             (aget overlayed-positions
                                   (dec (alength overlayed-positions)))) ;;(alast overlayed-positions))
        first-to-shift (aget to-shift-positions
                             0)
        last-to-shift (aget to-shift-positions
                            (aget to-shift-positions
                                  (dec (alength to-shift-positions))));; (alast to-shift-positions))
        start-shifts (+ (- last-to-shift)
                        first-overlayed)
        end-shifts (- last-overlayed
                      first-to-shift)]
    [start-shifts
     end-shifts]))

(defn overlap-area
  [^doubles array-a
   ^doubles array-b]
  (let [start-a ^double (aget array-a 0)
        end-a ^double   (aget array-a
                              (dec (alength array-a)));; (alast array-a)
        start-b ^double (aget array-b 0)
        end-b ^double   (aget array-b
                              (dec (alength array-b)))] ;;(alast array-b)]
    [(max start-a
          start-b)
     (min end-a
          end-b)]))

(deftype averagingResult [^double average ^long start-index ^long end-index])

(defn count-average-over-range
  [^double start-position
   ^double end-position
   ^doubles positions
   ^doubles counts]
  (loop [index (dec (alength positions))
         total-counts 0.0
         number-of-positions 0.0
         start-index 0
         end-index 0]
    (if (< index ;; loops backwards for efficiency
           0)
      (averagingResult. (/ total-counts
                           number-of-positions)
                        start-index
                        end-index)
      (let [current-position ^double (aget ^doubles positions
                                           index)
            next-index (dec index)]
        (if (< current-position
               start-position)
          (averagingResult. (/ total-counts
                               number-of-positions)
                            (inc index)
                            end-index)
          (if (>= current-position
                  end-position)
            (recur next-index
                   total-counts
                   number-of-positions
                   start-index
                   next-index)
            (recur next-index
                   ^double (+ total-counts
                              ^double (aget ^doubles counts
                                            index))
                   (inc number-of-positions)
                   start-index
                   end-index)))))))


(defn find-next-interpolation-ceiling ^long
  [^long ceiling-index
   ^double ceiling-position
   ^double current-position
   ^doubles shifted-positions]
  (if (< current-position
         ceiling-position)
    ceiling-index
    ;;     current-position]
    (let [next-index (inc ceiling-index)
          new-postion (aget shifted-positions
                            next-index)]
      ;;        (println "finding next index" next-index)
      (recur next-index
             new-postion
             current-position
             shifted-positions))))
;; (find-next-interpolation-ceiling 100 100 120 a-pos)

(defn find-next-interpolation-ceiling ^long
  [^long ceiling-index
   ^double ceiling-position
   ^double current-position
   ^doubles shifted-positions]
  (loop [index ceiling-index
         position ceiling-position]
    (if (< current-position
           position)
      index
      (let [next-index ^long (inc index)
            new-postion ^double (aget shifted-positions
                                      next-index)]
        ;;        (println "finding next index" next-index)
        (recur ^long (inc index)
               ^double (aget shifted-positions
                             next-index))))))

;;(time (dotimes [n 500000]  (test-cross)))


(defn find-next-interpolation-ceiling ^long
  [^long ceiling-index
   ^double ceiling-position
   ^double current-position
   ^doubles shifted-positions]
  (loop [index ceiling-index]
    (if (< current-position
           ^double (aget shifted-positions
                         index))
      index
      (recur ^long (inc index)))))


(defn add-measurement-to-correlation
  [covariances
   overlay-variances
   overlay-average
   shifted-variances
   shifted-average
   overlay-count
   interpolated-count]
  (let [delta-overlay (- ^double overlay-count
                         ^double overlay-average)
        delta-shifted (- ^double interpolated-count
                         ^double shifted-average)]
    [(+ ^double covariances
        (* delta-overlay
           delta-shifted))
     (+ ^double overlay-variances
        (* delta-overlay
           delta-overlay))
     (+ ^double shifted-variances
        (* delta-shifted
           delta-shifted))]))
;; (add-measurement-to-correlation 0.0 0.0 10.0 0.0 10.0 12.0 13.0)

(defn interpolate
  [current-position
   floor-position
   floor-count
   ceiling-position
   ceiling-count]
  ;; basic algebra...
  ;; y = m * x + b
  ;;
  ;; floor-count = m * floor-position + b
  ;; ceiling-count = m * ceiling-position + b
  ;; floor-count - ceiling-count = m * (floor-position - ceiling-position)
  ;; m = (floor-count - ceiling-count)/(floor-position - ceiling-position)
  (let [slope (/ (- ^double floor-count
                    ^double ceiling-count)
                 (- ^double floor-position
                    ^double ceiling-position))
        offset (- ^double floor-count
                  (* slope
                     ^double floor-position))]
    (+ (* slope
          ^double current-position)
       offset)))


;; loop over all overlayed array positions
;; before start-index, do nothing
;; at start-index
;; get overlayed count value
;; interpolate at that position (should be safe at this value)
;; some mechanism to incriment the interpolation range - going to be a bitch!
;;(decompile
(defn array-correlation
  [^doubles overlay-positions
   ^doubles overlay-counts
   overlay-start-index
   overlay-end-index
   shifted-start-index
   shifted-end-index
   ^doubles shifted-positions
   ^doubles shifted-counts
   overlay-average
   shifted-average]
  (loop [index (long overlay-start-index)
         ;;interpolation values
         ceiling-index (long (inc ^long shifted-start-index))
         floor-position (aget shifted-positions
                              ^long shifted-start-index)
         floor-count  (aget shifted-counts
                            ^long shifted-start-index)
         ceiling-position  (aget shifted-positions
                                 (inc ^long shifted-start-index))
         ceiling-count (aget shifted-counts
                             (inc ^long shifted-start-index))
         ;;correlation values
         covariance 0.0
         overlay-variance 0.0
         shifted-variance 0.0]
    (if (= index
           (dec ^long overlay-end-index)) ; this is decremented on each loop
      (/ covariance                                 ; but I can't find a way to improve perf
         (Math/sqrt (* overlay-variance
                       shifted-variance)))
      (let [current-position ^double (aget overlay-positions
                                           index)]
        (if (< current-position
               ceiling-position) ;; can we use previous interpolation bounds?
          (let [overlay-count ^double (aget overlay-counts
                                            index)
                delta-overlay (- overlay-count
                                 ^double overlay-average)
                delta-shifted (- ^double (interpolate current-position
                                                      floor-position
                                                      floor-count
                                                      ceiling-position
                                                      ceiling-count)
                                 ^double shifted-average)]
            (recur (inc index)
                   ceiling-index
                   floor-position
                   floor-count
                   ceiling-position
                   ceiling-count
                   (+ covariance
                      (* delta-overlay
                         delta-shifted))
                   (+ overlay-variance
                      (* delta-overlay
                         delta-overlay))
                   (+ shifted-variance
                      (* delta-shifted
                         delta-shifted))))
          ;; if not, then find new interpolation bounds
          (let [next-ceiling-index ^long (inc ceiling-index)
                new-ceiling-index ^long (find-next-interpolation-ceiling  next-ceiling-index
                                                                          ceiling-position
                                                                          current-position
                                                                          ^doubles shifted-positions)
                new-floor-index ^long (dec new-ceiling-index)
                new-floor-position ^double (aget shifted-positions
                                                 new-floor-index)
                new-floor-count ^double (aget shifted-counts
                                              new-floor-index)
                new-ceiling-position ^double  (aget shifted-positions
                                                    new-ceiling-index) ;; Can be determines in `find-next-interpolation-ceiling`
                new-ceiling-count ^double (aget shifted-counts
                                                new-ceiling-index)
                overlay-count ^double (aget overlay-counts
                                            index)
                delta-overlay (- overlay-count
                                 ^double overlay-average)
                delta-shifted (- ^double (interpolate current-position
                                                      new-floor-position
                                                      new-floor-count
                                                      new-ceiling-position
                                                      new-ceiling-count)
                                 ^double shifted-average)]
            (recur (inc index)
                   new-ceiling-index
                   new-floor-position
                   new-floor-count
                   new-ceiling-position
                   new-ceiling-count
                   (+ covariance
                      (* delta-overlay
                         delta-shifted))
                   (+ overlay-variance
                      (* delta-overlay
                         delta-overlay))
                   (+ shifted-variance
                      (* delta-shifted
                         delta-shifted)))))))))
;;)

(defn shift-range
  [^doubles overlay-positions
   ^doubles shifted-positions]
    (let [first-overlayed (aget overlay-positions
                              0)
          last-overlayed (aget overlay-positions
                               (dec (alength overlay-positions)))
          first-to-shift (aget shifted-positions
                               0)
          last-to-shift (aget shifted-positions
                              (dec (alength shifted-positions)))
          start-shifts (+ (- first-to-shift)
                          first-overlayed)
          end-shifts (- last-overlayed
                        first-to-shift)]
      [start-shifts
       end-shifts]))

(defn shift-range-vector
  "Helped function - not used in this namespace"
  [overlay
   shifted]
  (shift-range (double-array (mapv first overlay))
               (double-array (mapv first shifted))))

(defn array-cross-correlation
  [^doubles overlay-positions
   ^doubles overlay-counts
   ^doubles shifted-positions
   ^doubles shifted-counts
   step-size]
  (let [[^double start-shifts
         ^double end-shifts] (shift-range overlay-positions
                                  shifted-positions)
        shifts (into [] (range (+ start-shifts
                                  ^double step-size)
                               (- end-shifts
                                  ^double step-size)
                               ^double step-size))]
    (array-shift! shifted-positions start-shifts)
    (array-shift! shifted-counts start-shifts)
    (mapv (fn [shift]
            (let [
                  start-overlay ^double (aget overlay-positions 0)
                  end-overlay ^double   (aget overlay-positions
                                              (dec (alength overlay-positions)))
                  start-shifted ^double (aget shifted-positions 0)
                  end-shifted ^double   (aget shifted-positions
                                              (dec (alength shifted-positions)))
                  overlap-start ^double (max start-overlay
                                             start-shifted)
                  overlap-end  ^double (min end-overlay
                                            end-shifted)
                  ^averagingResult overlay-averaging-result (count-average-over-range  overlap-start
                                                                                       overlap-end
                                                                                       overlay-positions
                                                                                       overlay-counts)
                  overlay-average (.average overlay-averaging-result)
                  overlay-start-index (.start-index overlay-averaging-result)
                  overlay-end-index (.end-index overlay-averaging-result)
                  ^averagingResult shifted-averaging-result (count-average-over-range overlap-start
                                                                                      overlap-end
                                                                                      shifted-positions
                                                                                      shifted-counts)
                  shifted-average  (.average shifted-averaging-result)
                  shifted-start-index (.start-index shifted-averaging-result)
                  shifted-end-index (.end-index shifted-averaging-result)]
              (if (or (>= 1
                          (- overlay-end-index
                             overlay-start-index))
                      (>= 1
                          (- shifted-end-index
                             shifted-start-index)))
                (do (array-shift! shifted-positions ^double step-size)
                    (array-shift! shifted-counts ^double step-size)
                    [shift
                     0.0])
                (do (array-shift! shifted-positions ^double step-size)
                    (array-shift! shifted-counts ^double step-size)
                    [shift
                     (array-correlation overlay-positions
                                        overlay-counts
                                        overlay-start-index
                                        overlay-end-index
                                        shifted-start-index
                                        shifted-end-index
                                        shifted-positions
                                        shifted-counts
                                        overlay-average
                                        shifted-average)]))))
          shifts)))

(defn points-cross-correlation
  "Take two vectors of points of the form
  [[position counts]
   [position counts]
   ...
   [position counts]]
  and slide them past each other, calculating a correlation coefficient"
  [overlay-vector
   shifted-vector
   ^double step-size]
  (array-cross-correlation (double-array (map first
                                              overlay-vector))
                           (double-array (map second
                                              overlay-vector))
                           (double-array (map first
                                              shifted-vector))
                           (double-array (map second
                                              shifted-vector))
                           step-size))

(defn test-cross
  []
  (array-cross-correlation ^doubles a-pos
                           ^doubles a-val
                           ^doubles b-pos
                           ^doubles b-val
                           0.5)
  0)
;; (prof/profile (dotimes [_ 10] (test-cross)))
