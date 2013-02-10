(ns lcp1.sound
  (:use [overtone.midi]))

(defonce liveout (midi-out))

(defn line-created [length pos]
  (println "line-created" length pos)
  (let [note 60
        velocity (int (* length 127))
        duration 500
        channel (Math/round (* (+ pos 1) 2))] ; MIDI channels 1 .. 5
    (midi-note-on liveout note velocity channel)
    #_(midi-play liveout [note] [velocity] [duration] channel)))

(defn line-dropped [length pos]
  (println "line-dropped" length pos)
  (let [note 40
        velocity (int (* length 127))
        duration 250
        channel (+ 8 (Math/round (* (+ pos 1) 2)))] ; MIDI channels 9 .. 13
    (midi-note-on liveout note velocity channel)
    #_(midi-play liveout [note] [velocity] [duration] channel)))
