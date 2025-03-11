#lang racket
(require "./statistics.rkt")

;; International  version

(run-statistics-1 10000)

;; run-statistics-1:
;; ==== Count Invocation of Spirits, the expected number of pulls to obtain a 5-Stars Hero for the first time (sample-size: 10000). ====
;; average: 119.5749
;; median: 135.0
;; max: 194.0
;; min: 1.0
;; stddev: 66.2100490106298
;; 22.61% of players within [1, 50] pulls obtain a 5-Stars Hero for the first time.
;; 17.75% of players within [51, 100] pulls obtain a 5-Stars Hero for the first time.
;; 13.67% of players within [101, 150] pulls obtain a 5-Stars Hero for the first time.
;; 45.97% of players within [151, 200] pulls obtain a 5-Stars Hero for the first time.
;; 0.0% of players through the hard pity of 200 pulls obtain a 5-Stars Hero for the first time.

;; ==== Count Crazy Invocation of Spirits, the expected number of pulls to obtain a 5-Stars Hero for the first time (sample-size: 10000). ====
;; average: 84.5784
;; median: 69.0
;; max: 195.0
;; min: 1.0
;; stddev: 63.02527471927433
;; 39.53% of players within [1, 50] pulls obtain a 5-Stars Hero for the first time.
;; 23.56% of players within [51, 100] pulls obtain a 5-Stars Hero for the first time.
;; 14.37% of players within [101, 150] pulls obtain a 5-Stars Hero for the first time.
;; 22.54% of players within [151, 200] pulls obtain a 5-Stars Hero for the first time.
;; 0.0% of players through the hard pity of 200 pulls obtain a 5-Stars Hero for the first time.

;; ==== Count Normal Divine Summoning, the expected number of pulls to obtain a 5-Stars Hero for the first time (sample-size: 10000). ====
;; average: 10.8187
;; median: 12.0
;; max: 20.0
;; min: 1.0
;; stddev: 5.9689220391960225
;; 26.13% of players within [1, 5] pulls obtain a 5-Stars Hero for the first time.
;; 19.68% of players within [6, 10] pulls obtain a 5-Stars Hero for the first time.
;; 26.09% of players within [11, 15] pulls obtain a 5-Stars Hero for the first time.
;; 28.1% of players within [16, 20] pulls obtain a 5-Stars Hero for the first time.
;; 5.47% of players through the hard pity of 20 pulls obtain a 5-Stars Hero for the first time.

;; ==== Count Crazy Divine Summoning, the expected number of pulls to obtain a 5-Stars Hero for the first time (sample-size: 10000). ====
;; average: 7.3246
;; median: 6.0
;; max: 20.0
;; min: 1.0
;; stddev: 5.339066851051783
;; 47.12% of players within [1, 5] pulls obtain a 5-Stars Hero for the first time.
;; 25.17% of players within [6, 10] pulls obtain a 5-Stars Hero for the first time.
;; 17.36% of players within [11, 15] pulls obtain a 5-Stars Hero for the first time.
;; 10.35% of players within [16, 20] pulls obtain a 5-Stars Hero for the first time.
;; 1.39% of players through the hard pity of 20 pulls obtain a 5-Stars Hero for the first time.
