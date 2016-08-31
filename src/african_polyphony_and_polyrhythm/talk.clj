(ns african-polyphony-and-polyrhythm.talk
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon]]
            [leipzig.scale :refer [A B major minor pentatonic]]
            [leipzig.temperament :as temperament]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))

(defn forever [riff]
  (concat riff (lazy-seq (->> riff forever (after (duration riff))))))

(defn clapping-music []
  (let [african-bell-pattern (rhythm [1/8 1/8 1/4 1/8 1/4 1/4 1/8 1/4])]
    (->> african-bell-pattern forever (all :part :clap1)
         (canon #(->> % (take 64) (then (rhythm [1/8])) forever (all :part :clap2))))))

(comment
  (live/play (clapping-music))
  )

(defmethod live/play-note :clap1 [_]
  ((sample "samples/click2.wav")))

(defmethod live/play-note :clap2 [_]
  ((sample "samples/select-click.wav")))

(defn split [n fraction]
  (fn [notes]
    (let [note (nth notes n)
          first-note (-> note (update-in [:duration] * fraction))
          second-note (-> note
                          (update-in [:duration] * (- 1 fraction))
                          (update-in [:time] + (:duration first-note)))]
      (concat
        (take n notes)
        [first-note second-note]
        (drop (inc n) notes)))))

(definst akadinda [freq 440 vol 0.5]
  (-> (sin-osc freq)
      (+ (* 1/3 (sin-osc (* 2.01 freq))))
      (+ (* 1/2 (sin-osc (* 3.001 freq))))
      (+ (* 1/8 (sin-osc (* 5.001 freq))))
      (clip2 0.8)
      (lpf (* 5 440))
      (* (env-gen (adsr 0.01 0.05 0.15 0.2) (line:kr 1 0 0.1) :action FREE))
      (+ (* (env-gen (perc 0.02 0.03)) (* 1/3 (sin-osc (* 0.5 freq)))))
      (* vol)))

(defmethod live/play-note :default [{:keys [pitch]}]
  (when pitch (akadinda pitch)))

(defn rand-variations [variations]
  (concat
    (rand-nth variations)
    (lazy-seq (after 4 (rand-variations variations)))))

(defn part [model & variations]
  ((apply juxt identity variations) model))

(def tete
  (part
    (phrase [2 1/2 3/2] (repeat 0))
    (split 0 1/8)
    (comp (split 1 1/7) (split 0 1/8))
    (comp (split 2 1/6) (split 1 1/7) (split 0 1/8))))

(def ta
  (part
    (phrase [1 5/4 3/4 1] (cons nil (repeat 1)))
    (split 2 1/3)
    (split 3 1/2)))

(def ha
  (part
    (phrase [1/2 3 1/2] (cons nil (repeat 2)))
    (comp (split 4 1/2) (split 2 1/6) (split 0 1/4))))

(def tulule
  (part
    (phrase [2 2] (repeat 3))
    (comp (split 2 1/2) (split 0 1/2))))

(def bongo
  (part
    (phrase [3/2 3/4 7/4] (cons nil (repeat 4)))
    (split 2 5/7)
    (comp (split 1 1/3) (split 2 5/7))
    (comp (split 4 1/2) (split 1 1/3) (split 2 5/7))))

(defn up [notes]
  (->> notes
       (map (partial where :pitch #(+ % 5)))))

(def balendoro
  (->> [tete ta ha tulule bongo
        (up tete) (up ta) (up ha) (up tulule) (up bongo)
        (up (up tete)) (up (up ta)) (up (up ha)) (up (up tulule)) (up (up bongo))
        (up (up (up tete))) (up (up (up ta))) (up (up (up ha)))]
       (map rand-variations)
       (reduce with)))

(def inverse-pentatonic (comp (partial + 12) pentatonic -))

(comment
  (fx-reverb)
  (map fx-distortion [0 1])
  (->>
    balendoro
    (where :pitch (comp temperament/equal A inverse-pentatonic))
    (tempo (bpm 120))
    (live/play)
    )
  )
