(ns corascope.correlation)

(defn interpolate-point
  "Given a vector V1 of [X1 Y1] pairs and a vector of X2 values
  this returns an [X2 Y2} values, Where the Y2's are interpolated linearly"
  [x2 v1]
  (let [[preceeding-points
         following-points] (split-with #(<= (first %)
                                           x2)
                                       v1)
        preceeding-point (last preceeding-points)
        following-point (first following-points)]
    (if (or (nil? preceeding-point)
            (nil? following-point))
      nil
      (/ (+ (second preceeding-point)
            (second following-point))
         2.0))))

(defn interpolate-points
  "Given a vector of X2 values and a vector V1 of [X1 Y1] pairs
  this returns an [X2 Y2] values, Where the Y2's are interpolated linearly"
  [x2 v1]
  (->> x2
       (map #(vector %
                     (interpolate-point % v1)))
       #_(filter #(some? (second %)))))

(defn interpolate-overlap
  "Take a 2 point vector PV1 PV2. Crop PV1 to the overlapping segment.
  Interpolate PV2 points to PV1 positions
  Return the cropped PV1 and interpolated PV2 (they will be the same length)"
  [pv1
   pv2]
  (let [first-pv2-position (-> pv2
                              first
                              first)
        last-pv2-position (-> pv2
                             last
                             first)
        trimmed-pv1 (->> pv1
                        ;; drop all preceeding points with no overlap
                        (drop-while #(->> %
                                          first
                                          (>= first-pv2-position)))
                        ;; only take remaining points with overlap
                        (take-while #(->> %
                                          first
                                          (> last-pv2-position))))
        interpolation-positions (map first trimmed-pv1)
        interpolated-pv2 (interpolate-points interpolation-positions
                                             pv2)]
        [trimmed-pv1 interpolated-pv2]))

(defn mean
  [coll]
  (assert (not-empty coll) "calculating the mean of an empty vector")
  (/ (reduce + coll) (count coll)))

(defn- vector-correlation-coefficient
  "Get the correlation-coefficient of two vectors.
  The vectors obviously should be the same length"
  [x
   y]
  (let [x-mean (mean x)
        y-mean (mean y)]
    (/ (reduce +
               (map (fn [xi yi]
                      (* (- xi
                            x-mean)
                         (- yi
                            y-mean)))
                    x
                    y))
       (Math/sqrt (* (reduce +
                             (map (fn [xi]
                                    (* (Math/pow (- xi
                                                    x-mean)
                                                 2.0)))
                                  x))
                     (reduce +
                             (map (fn [yi]
                                    (Math/pow (- yi
                                                 y-mean)
                                              2.0))
                                  y)))))))

(defn points-correlation-coefficient
  "Take TWO element count vectors of the form
  [[position value]
   [position value]
   [position value]
   ...]
  and calculate their correlation.
  Takes an options V2-SHIFT values to adjust v2 postions"
  ([v1
    v2
    v2-shift]
   (let [shifted-v2 (map #(vector (-> %
                                      first
                                      (+ v2-shift))
                                  (-> %
                                      second))
                         v2)]
     #_(print " "
            v2-shift)
     (points-correlation-coefficient v1
                                     shifted-v2)))
  ([v1
    v2]
   (let [[trimmed-v1 trimmed-v2] (interpolate-overlap v1 v2)]
     (vector-correlation-coefficient (map second trimmed-v1)
                                     (map second trimmed-v2)))))

(defn points-cross-correlation
  "Take TWO element count vectors of the form
  [[position value]
   [position value]
   [position value]
   ...]"
  [v1
   v2
   step-size]
  (let [shifts (-> (range (+ (- (first (last v2)))
                             (first (first v1)))
                          (- (first (last v1))
                             (first (first v2)))
                          step-size)
                   rest
                   butlast)
        correlations (map #(points-correlation-coefficient v1
                                                           v2
                                                           %)
                          shifts)]
    (zipmap shifts
            correlations)))
