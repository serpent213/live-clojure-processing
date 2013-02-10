;; PROCESSINGJS.COM HEADER ANIMATION
;; MIT License - F1lT3R/Hyper-Metrix
;; Native Processing Compatible

;; ported from Processing.js to quil by Steffen Beyer

(ns lcp1.animation
  (:use [clojure.set]
        [lcp1.sound]
        [quil.core]))

;; constants

;; number of circles
(def circle-count 50)
;; maximum and minimum circle size
(def max-size 200)
(def min-size 40)
;; size of dot in circle center
(def center-dot 2)

;; state

;; vector to store circle properties
(def circles (atom []))
;; set drag switch to false
(def dragging (atom false))
;; integers showing which circle (the first index in circles) that's locked, and its position in relation to the mouse
(def locked-circle (atom nil))
(def locked-offset-x (atom nil))
(def locked-offset-y (atom nil))
;; set of interconnecting lines, containing sorted tuples like (4 5) (9 13)
(def lines-last (atom #{}))
(def lines-current (atom #{}))

(defn random-circle []
  {:x (rand (width))
   :y (rand (height))
   :diam (+ min-size (rand (- max-size min-size)))
   :x-speed (- (rand 0.24) 0.12)
   :y-speed (- (rand 0.24) 0.12)})

(defn move-circle [c]
  (let [r (/ (:diam c) 2)
        new-x (+ (:x c) (:x-speed c))
        new-y (+ (:y c) (:y-speed c))
        wrapped-x (cond (< new-x (- r)) (+ (width) r)
                        (> new-x (+ (width) r)) (- r)
                        :else new-x)
        wrapped-y (cond (< new-y (- r)) (+ (height) r)
                        (> new-y (+ (height) r)) (- r)
                        :else new-y)]
    (assoc c :x wrapped-x, :y wrapped-y)))

(defn mouse-pressed []
  ;; mouse within circle?
  (when-first [cidx (keep-indexed (fn [idx c] (if (< (dist (:x c) (:y c) (mouse-x) (mouse-y)) (/ (:diam c) 2)) idx)) @circles)]
    (let [c (nth @circles cidx)]
      (reset! dragging true)
      (reset! locked-circle cidx)
      (reset! locked-offset-x (- (mouse-x) (:x c)))
      (reset! locked-offset-y (- (mouse-y) (:y c))))))

(defn mouse-released []
  (reset! dragging false))

(defn transition-meta [cidx1 cidx2]
  (let [c1 (nth @circles cidx1)
        c2 (nth @circles cidx2)]
    {:length (/ (dist (:x c1) (:y c1) (:x c2) (:y c2)) max-size) ;; between 0 .. 1
     :pos (- (* (/ (+ (:x c1) (/ (- (:x c2) (:x c1)) 2)) (width)) 2) 1)})) ;; between -1 .. +1, 0 is center

(defn process-transitions []
  (when (> (frame-count) 2)
    (let [added (difference @lines-current @lines-last)
          removed (difference @lines-last @lines-current)]
      (doseq [t added]
        (let [m (apply transition-meta t)]
          (line-created (:length m) (:pos m))))
      (doseq [t removed]
        (let [m (apply transition-meta t)]
          (line-dropped (:length m) (:pos m)))))))

(defn setup []
  ;; turn on anti-aliasing
  (smooth)
  (frame-rate 60)
  (stroke-weight 1)
  ;; initiate array with random values for circles
  (reset! circles (repeatedly circle-count random-circle)))

(defn draw []
  (background 0)
  (process-transitions)
  (reset! lines-last @lines-current)
  (reset! lines-current #{})
  ;; draw circles
  (doseq [[c idx] (map list @circles (range))]
    (no-stroke)
    (let [d (:diam c)
          r (/ d 2)]
      ;; mouse within circle?
      (if (< (dist (:x c) (:y c) (mouse-x) (mouse-y)) r)
        (fill 64 187 128 100)
        (fill 64 128 187 100))
      (ellipse (:x c) (:y c) d d)
      ;; dot and line colours
      (if (and @dragging (identical? (nth @circles @locked-circle) c))
        (do
          (fill 255 255 255 255)
          (stroke 128 255 0 100))
        (do
          (fill 0 0 0 255)
          (stroke 64 128 128 255)))
      ;; interconnecting lines
      (doseq [[c2 idx2] (map list @circles (range))]
        (when (< (dist (:x c) (:y c) (:x c2) (:y c2)) d)
          (line (:x c) (:y c) (:x c2) (:y c2))
          (swap! lines-current conj (sort (list idx idx2)))))
      ;; dot in the center
      (no-stroke)
      (rect (- (:x c) center-dot) (- (:y c) center-dot) (* center-dot 2) (* center-dot 2))))
  ;; update circle positions
  (swap! circles #(vec (map move-circle %)))
  (if @dragging
    (swap! circles
           update-in [@locked-circle] assoc
           :x (- (mouse-x) @locked-offset-x)
           :y (- (mouse-y) @locked-offset-y))))

(defsketch lcp
  :title "Live Clojure Processing"
  :size [1280 720]
  :setup setup
  :draw draw
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released)

(defn -main [])
