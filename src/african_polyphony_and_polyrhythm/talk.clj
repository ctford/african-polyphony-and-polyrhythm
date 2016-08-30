(ns african-polyphony-and-polyrhythm.talk
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon]]
            [leipzig.scale :refer [A B major minor]]
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

(defn split [n fraction notes]
  (let [note (nth notes n)
        first-note (-> note (update-in [:duration] * fraction))
        second-note (-> note
                        (update-in [:duration] * (- 1 fraction))
                        (update-in [:time] + (:duration first-note)))]
    (concat
      (take n notes)
      [first-note second-note]
      (drop (inc n) notes))))

(comment
  (->>
    (rhythm [1/2 1/2 1/3 1/3])
    (split 1 1/2)
    )
  )
