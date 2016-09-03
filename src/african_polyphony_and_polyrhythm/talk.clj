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

(defn vary [t f]
  (fn [notes]
    (let [before? #(-> % :time (< t))
          before (->> notes (take-while before?))
          [note & after] (->> notes (drop-while before?))]
      (concat before (f note) after))))

(defn split [t fraction]
  (letfn [(f [note]
            [(-> note
                 (assoc :duration fraction))
             (-> note
                 (update-in [:duration] - fraction)
                 (update-in [:time] + fraction))])]
    (vary t f)))

(defn accent [t]
  (letfn [(f [note] [(-> note (update-in [:pitch] dec))])]
    (vary t f)))

(defn omit [t]
  (vary t (constantly [])))

(definst horn [freq 440 vol 0.5 pan 0]
  (-> (sin-osc freq)
      (+ (* 1/2 (sin-osc 3) (sin-osc (* 3.01 freq))))
      (+ (* 1/5 (sin-osc 3) (sin-osc (* 2 freq))))
      (+ (* 1/8 (sin-osc 9) (sin-osc (* 4.99 freq))))
      (+ (* 1/8 (sin-osc (* 7.01 freq))))
      (clip2 0.8)
      (rlpf (line:kr freq (* 7 freq) 0.4) 1/3)
      (* (env-gen (adsr 0.2 0.4 0.15 0.2) (line:kr 1 0 0.1) :action FREE))
      (pan2 pan)
      (* vol)))

(defmethod live/play-note :default [{:keys [pitch pan]}]
  (when pitch (horn :freq pitch :pan pan)))

(defn rand-variations [variations]
  (concat
    (rand-nth variations)
    (lazy-seq (after 4 (rand-variations variations)))))

(defn part [model & variations]
  ((apply juxt identity variations) model))

(def tete
  (let [model (phrase [8/4 3/4 5/4] (repeat 0))
        a (split 0 1/4)
        b (comp (split 1/4 1/4) a)
        c (comp (accent 2/4) (split 2/4 1/4) b)
        d (comp (omit 0/4) c)]
    (part model a b c d)))

(def ta
  (part
    (phrase [4/4 5/4 3/4 4/4] (cons nil (repeat 1)))
    (split 9/4 1/4)
    (comp (split 10/4 1/4) (split 9/4 1/4))
    (comp (split 11/4 1/8) (split 10/4 1/4) (split 9/4 1/4))
    (split 4/4 1/4)))

(def ha
  (part
    (phrase [2/4 12/4 2/4] (cons nil (repeat 2)))
    (comp (split 2/4 1/4) (split 0 1/4))
    (comp (split 6/4 1/4) (split 0 1/4))
    (comp (split 6/4 1/4))
    (comp (split 6/4 1/4) (split 2/4 1/4) (split 0 1/4))))

(def tulule
  (part
    (phrase [8/4 8/4] (repeat 3))
    (comp (split 8/4 2/4) (split 0 2/4)) ; 28
    (comp (split 1/4 0) (split 2/4 1/4) (split 8/4 2/4) (split 0 2/4)) ; 6
    (comp (split 3/4 1/8) (split 1/4 0) (split 2/4 1/4) (split 8/4 2/4) (split 0 2/4)))) ; 5

(def bongo
  (part
    (phrase [6/4 3/4 7/4] (cons nil (repeat 4))) ; 5
    (split 9/4 5/4) ; 6
    (comp (split 6/4 1/4) (split 9/4 5/4)) ; 9
    (comp (split 14/4 1/4) (split 3/2 1/4) (split 9/4 5/4)))) ; 8

(defn big [notes]
  (->> notes
       (map (partial where :pitch #(+ % 5)))))

(defn pan [{:keys [pitch] :as note}]
  (if pitch
    (-> note (assoc :pan (-> pitch (/ 9) dec)))
    note))

(def balendoro
  (->> [tete ta ha tulule bongo
        (big tete) (big ta) (big ha) (big tulule) (big bongo)
        (big (big tete)) (big (big ta)) (big (big ha)) (big (big tulule)) (big (big bongo))
        (big (big (big tete))) (big (big (big ta))) (big (big (big ha)))]
       (map rand-variations)
       (map after (range 0 (* 8 18) 8))
       ;(map first)
       (reduce with)
       (map pan)))

(def inverse-pentatonic (comp pentatonic -))

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
