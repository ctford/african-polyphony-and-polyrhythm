(ns african-polyphony-and-polyrhythm.talk
  (:require [overtone.live :refer :all :exclude [stop scale]]
            [leipzig.melody :refer :all]
            [leipzig.canon :refer [canon]]
            [leipzig.scale :refer [scale A B high low]]
            [leipzig.temperament :as temperament]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))

(defn vary
  "Apply f to the note at time t."
  [t f]
  (fn [notes]
    (let [before? (fn [note] (-> note :time (< t)))
          before (->> notes (take-while before?))
          [note & after] (->> notes (drop-while before?))]
      (with before (f note) after))))

(defn split
  "Split a new note off the note at time t."
  [t duration]
  (letfn [(f [note]
            [(-> note (assoc :duration duration))
             (-> note (update :duration - duration) (update :time + duration))])]
    (vary t f)))

(defn accent
  "Accent the pitch of the note at time t."
  [t]
  (letfn [(f [note] [(-> note (update :pitch dec))])]
    (vary t f)))

(defn skip
  "Skip the note at time t."
  [t]
  (vary t (constantly [])))

(defn rand-variations
  "Assemble an infinite random concatenation of variations."
  [variations]
  (let [variation (rand-nth variations)]
    (concat
      variation
      (lazy-seq
        (->> (rand-variations variations)
             (after (duration variation) ))))))

(defn part
  "Generate version of the model using the variations fns."
  [model & variations]
  ((apply juxt identity variations) model))

(def first-drum
  (let [model (->> (rhythm [2/5 3/5 1/5 1/5 3/5]) (all :part :clap1))
        a (split 6/5 1/10)
        b (split 5/5 1/10)
        c (comp (split 17/15 2/15) (split 5/5 2/15) (skip 6/5))
        d (comp (skip 7/5) (skip 6/5))]
    (part model a b c d)))

(def second-drum
  (let [model (->> (rhythm [2/5 2/5 4/5 1/5 1/5]) (all :part :clap2))
        a (skip 9/5)
        b (skip 0)
        c (comp a b)
        d #(then % (->> (rhythm (repeat 5 2/5)) (all :part :clap2)))]
    (part model a b c d)))

(def third-drum
  (let [model (->> (rhythm [2/5 1/5 2/5]) (all :part :clap3))
        a (skip 2/5)
        b (skip 3/5)
        c (comp (split 0 1/5) a)
        d (split 3/5 1/5)
        e (comp (split 3/5 1/10) d)
        f (split 0 1/5)
        g (comp (split 0 1/10) f)]
    (part model a b c d e f g)))

(defn introduce-after [t parts]
  (->> parts
       (map after (range 0 (* (count parts) t) t))))

(def aga-terumo ; p299
  (let [drums [first-drum second-drum third-drum]]
    (->> drums
         ;(map (comp list first))
         (map rand-variations)
         (introduce-after 4)
         (reduce with))))

(comment
  (->>
    aga-terumo
    (tempo (bpm 90))
    (live/play)))

; Clapping music
(defn forever [riff]
  (concat riff (lazy-seq (->> riff forever (after (duration riff))))))

(defn clapping-music []
  (let [african-bell-pattern (rhythm [1/8 1/8 1/4 1/8 1/4 1/4 1/8 1/4])]
    (->> african-bell-pattern forever (all :part :clap1)
         (canon #(->> % (take 32) (then (rhythm [1/8])) forever (all :part :clap2))))))

(comment
  (->> (clapping-music)
       live/play))

; Natural numbers
(comment
  (range 0 8))

; Raw frequencies
(comment
  (->> (phrase (repeat 1/4) [440 494 554 622 659 740 831 880])
       live/play))

; Midi
(comment
  (->> (phrase (repeat 1/4) [69 71 73 74 76 78 80 81])
       (where :pitch temperament/equal)
       live/play))

; Scales
(def major (scale [2 2 1 2 2 2 1]))
(def minor (scale [2 1 2 2 1 2 2]))
(def pentatonic (scale [2 3 2 2 3]))
(def car (comp high pentatonic -))

(comment
  (->> (phrase (repeat 1/4) (range 0 8))
       (where :pitch (comp temperament/equal A major))
       live/play))

(def tete
  (let [model (phrase [8/4 3/4 5/4] (repeat 0))
        a (split 0 1/4)
        b (comp (split 1/4 1/4) a)
        c (comp (accent 2/4) (split 2/4 1/4) b)
        d (comp (skip 0/4) c)]
    (part model a b c d)))

(def ta
  (let [model (->> (phrase [5/4 3/4 4/4] (repeat 1)) (after 4/4))
        a (split 9/4 1/4)
        b (comp (split 10/4 1/4) a)
        c (comp (accent 11/4) (split 11/4 1/8) b) ; 17
        d (split 4/4 1/4)
        e (comp (skip 10/4) c)] ; 8
    (part model a b c d e)))

(def ha
  (let [model (->> (phrase [12/4 2/4] (repeat 2)) (after 2/4))
        a (comp (split 2/4 1/4) (split 0 1/4))
        b (comp (split 6/4 1/4))
        c (comp (split 0 1/4) b)
        d (comp a b)]
    (part model a b c d)))

(def tulule
  (let [model (phrase [8/4 8/4] (repeat 3))
        a (comp (split 8/4 2/4) (split 0 2/4)) ; 28
        b (comp (split 0 1/4) (split 2/4 1/4) a) ; 6
        c (comp (accent 7/8) (split 3/4 1/8) b)] ; 5
    (part model a b c)))

(def bongo
  (let [model (->> (phrase [3/4 7/4] (repeat 4)) (after 6/4)) ; 5
        a (split 9/4 5/4) ; 6
        b (comp (split 6/4 1/4) a) ; 9
        c (comp (split 14/4 1/4) b) ; 8
        d (comp (accent 10/4) (split 9/4 1/4) b)] ; 19
    (part model a b c d)))

(defn big [notes]
  (->> notes
       (map (partial where :pitch #(+ % 5)))))

(defn pan [{:keys [pitch] :as note}]
  (if pitch
    (let [p (-> note :pitch (/ 18) dec)]
      (if (odd? pitch)
        (assoc note :pan (- p))
        (assoc note :pan p)))
    note))

(def ndereje-balendoro ; p 343
  (->> [tete ta ha tulule bongo
        (big tete) (big ta) (big ha) (big tulule) (big bongo)
        (big (big tete)) (big (big ta)) (big (big ha)) (big (big tulule)) (big (big bongo))
        (big (big (big tete))) (big (big (big ta))) (big (big (big ha)))]
       ;(map (comp list first))
       (map rand-variations)
       (introduce-after 4)
       (reduce with)
       (map pan)))

(comment
  (->>
    ndereje-balendoro
    (where :pitch (comp temperament/equal A car))
    (tempo (bpm 120))
    (live/play)))

; Instruments
(definst drum [freq 440 vol 0.5 pan 0]
  (-> (* 2/3 (brown-noise))
      (+ (* 1/2 (sin-osc (* 3 freq))))
      (+ (* 1/5 (sin-osc (* 5 freq))))
      (clip2 0.8)
      (rlpf (line:kr freq (* 7 freq) 0.02))
      (* (env-gen (adsr 0.02 0.4 0.15 0.2) (line:kr 1 0 0.1) :action FREE))
      (pan2 pan)
      (* vol)))

(defmethod live/play-note :clap1 [_]
  (drum 225 :pan 0.75))

(defmethod live/play-note :clap2 [_]
  (drum 150 :pan -0.75))

(defmethod live/play-note :clap3 [_]
  (drum 75 :pan 0))

(definst horn [freq 440 vol 0.5 pan 0]
  (-> (sin-osc freq)
      (+ (* 1/2 (sin-osc 3) (sin-osc (* 3.01 freq))))
      (+ (* 1/5 (sin-osc (* 2 freq))))
      (+ (* 1/8 (sin-osc (* 4.99 freq))))
      (+ (* 1/8 (sin-osc (* 7.01 freq))))
      (clip2 0.8)
      (rlpf (line:kr (* 2 freq) (* 7 freq) 0.8))
      (* (env-gen (adsr 0.2 0.4 0.15 0.2) (line:kr 1 0 0.1) :action FREE))
      (pan2 pan)
      (* vol)))

(defmethod live/play-note :default [{:keys [pitch pan]}]
  (when pitch (horn :freq pitch :pan (or pan 0))))
