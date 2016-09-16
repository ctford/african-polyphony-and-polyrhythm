(ns african-polyphony-and-polyrhythm.talk
  (:require [overtone.live :refer :all :exclude [stop scale]]
            [leipzig.melody :refer :all]
            [leipzig.scale :refer [scale high low from]]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Ostinato with variations ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn target
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
    (target t f)))

(defn accent
  "Accent the pitch of the note at time t."
  [t]
  (letfn [(f [note] [(-> note (update :pitch dec))])]
    (target t f)))

(defn skip
  "Skip the note at time t."
  [t]
  (target t (constantly [])))

(defn rand-variations
  "Assemble an infinite random concatenation of variations."
  [variations]
  (let [variation (rand-nth variations)]
    (concat
      variation
      (lazy-seq
        (->> (rand-variations variations)
             (after (duration variation)))))))

(defn part
  "Generate version of the model using the variations fns."
  [instrument model & variations]
  (let [vary (apply juxt variations)]
    (->> model
         vary
         (map #(all :part instrument %)))))

(defn introduce-successively
  "Gradually introduce each part after t beats."
  [t parts]
  (->> parts
       (map after (range 0 (* (count parts) t) t))
       (reduce with)))

(defn big
  "Make a set of variations one pentatonic octave bigger."
  [notes]
  (->> notes
       (map #(where :pitch (from 5) %))))

(defn pan
  "Pan out an 18 piece orchestra to make individual parts more distinct."
  [{:keys [pitch] :as note}]
  (let [position (if (even? pitch)
                   (-> pitch (/ 18) dec)
                   (-> pitch (/ 18) dec -))]
    (assoc note :pan position)))



;;;;;;;;;;;;;;
; Aga Terumo ;
;;;;;;;;;;;;;;

(def child "First drum"
  (let [model (rhythm [2/5 3/5 5/5])
        a (comp (split 6/5 1/5) (split 5/5 1/5))
        b (comp (split 6/5 1/10) a)
        c (comp (split 17/15 2/15) (split 5/5 2/15) (split 5/5 2/5))
        d (comp (split 5/5 1/10) a)
        e identity]
    (part :child model a b c d e)))

(def mother "Second drum"
  (let [model (rhythm [2/5 2/5 4/5 2/5])
        b (skip 0)
        c (split 8/5 1/5)
        a (comp b c)
        d identity
        e (comp c #(->> (rhythm (repeat 5 2/5)) (then %)))]
    (part :mother model a b c d e)))

(def father "Third drum"
  (let [model (rhythm [2/5 1/5 2/5])
        a identity
        d (split 3/5 1/5)
        c (comp (split 3/5 1/10) d)
        e (split 2/5 3/10)
        f (skip 2/5)
        b (comp (split 0 1/5) f)
        g (comp b d)
        h (comp (split 1/10 1/10) e)
        i (split 0 1/5)]
    (part :father model a b c d e f g h i)))

(defn aga-terumo
  "Banda-Linda ritual music - page 299"
  []
  (let [drums [child mother father]]
    (->> drums
         (map rand-variations)
         (introduce-successively 4)
         (tempo (bpm 90)))))

(comment
  (live/play (aga-terumo)))












;;;;;;;;;;;;;;;;;;
; Clapping Music ;
;;;;;;;;;;;;;;;;;;

(defn clapping-music
  "Steve Reich's 'Clapping Music'"
  []
  (let [african-bell-pattern (rhythm [1/8 1/8 1/4 1/8 1/4 1/4 1/8 1/4])
        ostinato #(rand-variations [%])]
    (with
      (->> african-bell-pattern
           ostinato
           (all :part :mother))
      (->> african-bell-pattern
           (times 4)
           (then (rhythm [1/8]))
           ostinato
           (all :part :child)))))

(comment
  (live/play (clapping-music)))






;;;;;;;;;;;;;;;;;;
; Natural scales ;
;;;;;;;;;;;;;;;;;;

; Natural numbers
(def natural-numbers (range 0 8))

; Raw frequencies
(comment
  (->> (phrase (repeat 1/4) [440.0 493.9 554.4 587.3 659.3 740.0 830.6 880.0])
       live/play))

; Midi
(comment

  (midi->hz 69) ; 440.0
  (midi->hz 71) ; 493.9

  (->> (phrase (repeat 1/4) [69 71 73 74 76 78 80 81])
       (where :pitch midi->hz)
       live/play))


; Keys
(defn A
  "Convert a relative midi code into an absolute one in the key of A."
  [relative-midi]
  (+ relative-midi 69))

(defn B
  "Convert a relative midi code into an absolute one in the key of B."
  [relative-midi]
  (+ relative-midi 71))

(defn C
  "Convert a relative midi code into an absolute one in the key of C."
  [relative-midi]
  (+ relative-midi 72))

(comment
  (->> (phrase (repeat 1/4) [0 2 4 5 7 9 11 12])
       (where :pitch (comp midi->hz A))
       live/play))

; Scales
(def major "Happy seven-note scale" (scale [2 2 1 2 2 2 1]))
(def minor "Sad seven-note scale" (scale [2 1 2 2 1 2 2]))
(def pentatonic "Five-note scale" (scale [2 3 2 2 3]))

(comment
  (->> (phrase (repeat 1/4) (range 0 8))
       (where :pitch (comp midi->hz A major))
       live/play))














; Little-endian
(def little-endian-pentatonic
  "In Central African music, we play scales from small sounds to big ones."
  (comp pentatonic -))

(comment
  (map (comp A little-endian-pentatonic) (range 0 6))

  (->> (phrase (repeat 1/4) (range 0 6))
       (where :pitch (comp midi->hz A little-endian-pentatonic))
       live/play))












;;;;;;;;;;;;;;;;;;;;;
; Ndereje Balendoro ;
;;;;;;;;;;;;;;;;;;;;;

(def tete
  (let [model (phrase [8/4 3/4 5/4] (repeat 0))
        a (split 0 1/4)
        b (comp (split 1/4 1/4) a)
        c (comp (accent 2/4) (split 2/4 1/4) b)
        d (comp (skip 0/4) c)]
    (part :horn model a b c d)))

(def ta
  (let [model (->> (phrase [5/4 3/4 4/4] (repeat 1)) (after 4/4))
        a (split 9/4 1/4)
        b (comp (split 10/4 1/4) a)
        c (comp (accent 11/4) (split 11/4 1/8) b)
        d (split 4/4 1/4)
        e (comp (skip 10/4) c)]
    (part :horn model a b c d e)))

(def ha
  (let [model (->> (phrase [12/4 2/4] (repeat 2)) (after 2/4))
        a (comp (split 2/4 1/4) (split 0 1/4))
        b (comp (split 6/4 1/4))
        c (comp (split 0 1/4) b)
        d (comp a b)]
    (part :horn model a b c d)))

(def tulule
  (let [model (phrase [8/4 8/4] (repeat 3))
        a (comp (split 8/4 2/4) (split 0 2/4))
        b (comp (split 0 1/4) (split 2/4 1/4) a)
        c (comp (accent 7/8) (split 3/4 1/8) b)]
    (part :horn model a b c)))

(def bongo
  (let [model (->> (phrase [3/4 7/4] (repeat 4)) (after 6/4))
        a (split 9/4 5/4)
        b (comp (split 6/4 1/4) a)
        c (comp (split 14/4 1/4) b)
        d (comp (accent 10/4) (split 9/4 1/4) b)]
    (part :horn model a b c d)))

(defn ndereje-balendoro
  "Linda horn music - page 316"
  []
  (->> [tete ;ta ha tulule bongo
        ;(big tete) (big ta) (big ha) (big tulule) (big bongo)
        ;(big (big tete)) (big (big ta)) (big (big ha)) (big (big tulule)) (big (big bongo))
        ;(big (big (big tete))) (big (big (big ta))) (big (big (big ha)))
        ]
       (map rand-variations)
       (introduce-successively 4)
       (map pan)
       (where :pitch (comp midi->hz high A little-endian-pentatonic))
       (tempo (bpm 120))))

(comment
  (live/play (ndereje-balendoro)))







;;;;;;;;;;;;;;;
; Instruments ;
;;;;;;;;;;;;;;;

(definst drum [freq 110 vol 0.5 pan 0]
  (-> (* 2/3 (brown-noise))
      (+ (* 1/2 (sin-osc (* 3 freq))))
      (+ (* 1/5 (sin-osc (* 5 freq))))
      (clip2 0.8)
      (rlpf (line:kr freq (* 7 freq) 0.02))
      (* (env-gen (adsr 0.02 0.4 0.15 0.2) (line:kr 1 0 0.1) :action FREE))
      (pan2 pan)
      (* vol)))

(defmethod live/play-note :child [_]
  (drum 225 :pan 0.75))

(defmethod live/play-note :mother [_]
  (drum 150 :pan -0.75))

(defmethod live/play-note :father [_]
  (drum 75 :pan 0 :vol 1.0))

(definst whistle [freq 880 vol 0.5 pan 0 dur 0.2]
  (-> (sin-osc freq)
      (+ (* 1/2 (sin-osc 3) (sin-osc (* 3.01 freq))))
      (+ (* 1/5 (sin-osc (* 2 freq))))
      (+ (* 1/8 (sin-osc (* 4.99 freq))))
      (+ (* 1/8 (sin-osc (* 7.01 freq))))
      (clip2 0.8)
      (rlpf (line:kr (* 2 freq) (* 7 freq) 0.8))
      (* (env-gen (adsr 0.1 0.1 0.35 0.05) (line:kr 1 0 dur) :action FREE))
      (pan2 pan)
      (* vol)))

(defmethod live/play-note :horn [{:keys [pitch pan duration]}]
  (whistle :freq pitch :pan (or pan 0) :dur (min duration 0.2))
  ;(drum :freq (* 1/2 pitch) :pan (or pan 0) :dur (min duration 0.2))
  )

(defmethod live/play-note :default [{:keys [pitch duration]}]
  (whistle :freq pitch :dur duration))
