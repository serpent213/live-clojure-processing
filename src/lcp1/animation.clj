;; PROCESSINGJS.COM HEADER ANIMATION
;; MIT License - F1lT3R/Hyper-Metrix
;; Native Processing Compatible

;; ported from Processing.js to quil by Steffen Beyer

(ns lcp1.animation
  (:use quil.core))

;; constants

;; number of circles
(def circle-count 20)
;; maximum and minimum circle size
(def max-size 100)
(def min-size 20)
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
      (if (< (+ (sq (- (:x c) (mouse-x))) (sq (- (:y c) (mouse-y))))
             (sq r))
        (fill 64 187 128 100)
        (fill 64 128 187 100))
      (ellipse (:x c) (:y c) d d)
      ;; interconnecting lines
      (fill 0 0 0 255)
      (stroke 64 128 128 255)
      (doseq [c2 @circles]
        (if (< (+ (sq (- (:x c) (:x c2))) (sq (- (:y c) (:y c2))))
               (sq r))
          (line (:x c) (:y c) (:x c2) (:y c2))))))
  (no-stroke)
  ;; update circle positions
  (swap! circles (partial map (fn [c] (assoc c
                                             :x (+ (:x c) (:x-speed c))
                                             :y (+ (:y c) (:y-speed c)))))))

(defsketch lcp
  :title "Live Clojure Processing"
  :setup setup
  :draw draw
  :size [1280 720])

(defn -main [])
