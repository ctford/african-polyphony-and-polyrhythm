# African polyphony and polyrhythm

## Abstract
Ethnomusicologists face a dilemma: either shoehorn African music into European notation, or create custom DSLs that can only be understood by a select band of European ethnomusicologists. Algomusicologists can solve this problem, because we have principled ways of modelling specific idioms in terms of general computation. What's more, our models can be executed to generate actual music.

Simha Arom is a French-Israeli ethnomusicologist. In the book from which the title of this talk is borrowed, he describes the principles underlying the musical system of traditional central African polyphony and polyrhythm. Arom invented ingenious recording techniques for deconstructing and systematising musical cultures that had no previous tradition of musical theory. He tested his models by using them to recreate music and inviting central African musicians to critique the results.

This talk will argue that music-as-code is an ideal way to represent Arom's insights. I will show the full truth in the Lévi-Strauss quote with which Arom prefaces his book: "The proof of the analysis is in the synthesis." Programming has played its part in the homogenisation of global culture, but the universality of the lambda calculus also affords a golden opportunity for code to become a point of interchange between formerly incompatible musical systems.

## Talk

### Arom's method
* Identify relevant features
* Realisation -> model -> realisation p436
* Western music originally notated pitch only
* Etic vs emic
* Create model
* Recreate original to validate model
* Recording system
* Show examples of his notation e.g. p297 and p305.

### Tuning systems
* Tuning and temperament
* Bartok's flats
* Pentatonic equal temperament in Uganda

### Polyrhythms
* Different bar lengths and stress
* Clapping music
* Hocket is awkward to represent, as Western notation would require separating different parts.
* Infinite songs

### Variations
* Identifying the most basic form of a song
* Variations as extensions to internal DSL
* Timbre as a variation

### Moving the bias out one level
* Even the enterprise of analysing music this way is culturally determined
* The subjectivity of algorithms
* Falsehoods that programmers believe about music e.g. music and dance are separate
* Social functions of music

### Performance
* ???

## Quotes
* p172 - "to avoid collecting data so full of detail as to make it impossible to distinguish the essential from the extrinsic."
* p173 - "our conventional signs are still quite capable of accurately representing these durations."
* p175 - "The material in its modelised form will generally appear at the top of the table; below will be a vertical column showing a number of possible realisations of each part, all of which are interchangeable, provided their position in the periodic structure remains the same."
* p176 - "If any of them were performed, the members of the community which provided the model for it would immediately identify it."

## Twitter
[Thread](https://twitter.com/ctford/status/752176620899893248)
* Open world DSLs have a representation that is one level lower than its notation, which enables new notational concepts to be defined.
* This is still a half-formed thought, so please bear with me.
* I'm thinking Leipzig is a good example - notation is phrases, chords and scales, representation is a time series of notes.
* Puppet might be an example of a closed world DSL - no representation layer beneath the notation to extend.
* Internal DSLs tend to be open world, external ones closed world. But I think that's a posteriori.
* Java is open world, because the representation level (JVM bytecode) can be produced by new notations e.g. Scala or Clojure.
* Macros bring this to lisp itself - though this is an example rather than the only way to do it.
* Avoiding side effects, global state, unhygeinic/reader macros and other forms of notational interference is important.
* Clojure avoids reader macros because they lead to incompatible dialects.
* By separating notation and representation, we can achieve both extension and mutual intelligability.
* Western music notation can't be extended without out-of-band agreement - because its notation and representation are conjoined.
* That's great for supporting performance by western musicians and terrible for supporting other musical cultures.

* Kovas: agree. Macros maybe the canonical example. Also a fn of simplicity. Dsl that just hides nasty impl not composed on principled prmtvs
* Yeah, the quality and productivity of the representation is crucial to the system being viable.
* Scala macros are an example of a difficult system to extend.

* Tommy: best quote I saw was via @garethr. You can programitically generate declarite config if you wanat/need to.
* [Borg](http://queue.acm.org/detail.cfm?id=2898444): The language to represent the data should be a simple, data-only format such as JSON or YAML, and programmatic modification of this data should be done in a real programming language, where there are well-understood semantics, as well as good tooling.

## References
* [African Polyphony and Polyrhythm](https://www.amazon.co.uk/African-Polyphony-Polyrhythm-Structure-Methodology/dp/0521616018?tag=duc08-21) by Simha Arom.
* [Borg, Omega, and Kubernetes](http://queue.acm.org/detail.cfm?id=2898444) by Brendan Burns, Brian Grant, David Oppenheimer, Eric Brewer, and John Wilkes.
* [Clojure 1.4's extensible reader](https://www.infoq.com/interviews/hickey-clojure-reader) by Rich Hickey.
* [Escaping DSL Hell By Having Parenthesis All The Way Down](https://vimeo.com/100425264) by Tom Hall.
* [The Topos of Music](https://www.amazon.co.uk/Topos-Music-Geometric-Concepts-Performance/dp/3034894546) by Guerino Mazzola.
* [Principles of Design](https://www.w3.org/DesignIssues/Principles.html) by Tim Berners-Lee.
