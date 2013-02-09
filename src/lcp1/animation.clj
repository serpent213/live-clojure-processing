;; PROCESSINGJS.COM HEADER ANIMATION
;; MIT License - F1lT3R/Hyper-Metrix
;; Native Processing Compatible

;; ported from Processing.js to quil by Steffen Beyer

(ns lcp1.animation
  (:use quil.core))

;; constants

;; number of circles
(def circle-count 60)
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

(defn random-circle []
  {:x (rand (width))
   :y (rand (height))
   :diam (+ min-size (rand (- max-size min-size)))
   :x-speed (- (rand 0.24) 0.12)
   :y-speed (- (rand 0.24) 0.12)})

(defn move-circle [c]
  ;; moved by user?
  ; (when @dragging
  ;   (println "locked-circle" @locked-circle)
  ;   (println "circles" @circles))
  ; (if (and @dragging (identical? (nth @circles @locked-circle) c))
  ;   (assoc c
  ;          :x (- (mouse-x) @locked-offset-x)
  ;          :y (- (mouse-y) @locked-offset-y))
    ;; natural movement
    (let [r (/ (:diam c) 2)
          new-x (+ (:x c) (:x-speed c))
          new-y (+ (:y c) (:y-speed c))
          wrapped-x (cond (< new-x (- r)) (+ (width) r)
                          (> new-x (+ (width) r)) (- r)
                          :else new-x)
          wrapped-y (cond (< new-y (- r)) (+ (height) r)
                          (> new-y (+ (height) r)) (- r)
                          :else new-y)]
      (assoc c :x wrapped-x, :y wrapped-y))) ; )

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

(defn setup []
  ;; turn on anti-aliasing
  (smooth)
  (frame-rate 60)
  (stroke-weight 1)
  ;; initiate array with random values for circles
  (reset! circles (repeatedly circle-count random-circle)))

(defn draw []
  (background 0)
  ;; draw circles
  (doseq [c @circles]
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
      (doseq [c2 @circles]
        (if (< (dist (:x c) (:y c) (:x c2) (:y c2)) d)
          (line (:x c) (:y c) (:x c2) (:y c2))))
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
