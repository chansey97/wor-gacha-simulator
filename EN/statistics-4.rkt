#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-4: Count the expected number of pulls to obtain a 5-Star Hero for the first time in
;; Normal Ancient Summoning.
;; NOTE:
;; 1. Since the ancient pool does not reset the pity after pulling a 5-Star Normal Hero, the sample here
;; may not necessarily represent the number of pulls for the first time a 5-Star Hero is obtained.
;; 2. The parameter 'first-only' forces the statistics to focus on the first time only.
(define (run-statistics-4 sample-size [first-only #f])
  (printf "run-statistics-4:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))

  (define (pull-until-get-5-star)
    (when first-only
      (send pool reset))
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-star rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)])
                    (pull-until-get-5-star)))
         (average (exact->inexact (mean samples)))
         (median (exact->inexact (median < samples)))
         (max (exact->inexact (apply max samples)))
         (min (exact->inexact (apply min samples)))
         (stddev (exact->inexact (stddev samples)))
         (bins (bin-samples (split-into-4-segments (add1 hard-pity-threshold)) <= samples))
         (percentages (for/list ([bin bins])
                        (exact->inexact (* (/ (sample-bin-total bin) (length samples)) 100))))
         (hard-pity-percentage (exact->inexact (* (/ (count (λ (x) (= x (add1 hard-pity-threshold))) samples)
                                                     (length samples)) 100))))
    (if first-only
        (printf "==== Count ~a, the expected number of pulls to obtain a ~a for the first time (sample-size: ~a). ====\n"
                pool-name "5-Star Hero" sample-size)
        (printf "==== Count ~a, the expected number of pulls to obtain a ~a for (sample-size: ~a). ====\n"
                pool-name "5-Star Hero" sample-size))
    (printf "average: ~a\n" average)
    (printf "median: ~a\n" median)
    (printf "max: ~a\n" max)
    (printf "min: ~a\n" min)
    (printf "stddev: ~a\n" stddev)
    (for ([bin bins]
          [percentage percentages])
      (if first-only
          (printf "~a% of players within [~a, ~a] pulls obtain a ~a for the first time.\n"
                  percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Star Hero")
          (printf "~a% of players within [~a, ~a] pulls obtain a ~a.\n"
                  percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Star Hero")))
    (if first-only
        (printf "~a% of players through the hard pity of ~a pulls obtain a ~a for the first time.\n"
                hard-pity-percentage (add1 hard-pity-threshold) "5-Star Hero")
        (printf "~a% of players through the hard pity of ~a pulls obtain a ~a.\n"
                hard-pity-percentage (add1 hard-pity-threshold) "5-Star Hero"))
    (printf "\n")
    ))

(run-statistics-4 10000)

(run-statistics-4 10000 #t)

;; run-statistics-4:
;; ==== Count Normal Ancient Summoning, the expected number of pulls to obtain a 5-Star Hero for (sample-size: 10000). ====
;; average: 49.2505
;; median: 35.0
;; max: 194.0
;; min: 1.0
;; stddev: 44.78417968155719
;; 63.26% of players within [1, 50] pulls obtain a 5-Star Hero.
;; 23.03% of players within [51, 100] pulls obtain a 5-Star Hero.
;; 9.05% of players within [101, 150] pulls obtain a 5-Star Hero.
;; 4.66% of players within [151, 200] pulls obtain a 5-Star Hero.
;; 0.0% of players through the hard pity of 200 pulls obtain a 5-Star Hero.

;; run-statistics-4:
;; ==== Count Normal Ancient Summoning, the expected number of pulls to obtain a 5-Star Hero for the first time (sample-size: 10000). ====
;; average: 53.5911
;; median: 39.0
;; max: 195.0
;; min: 1.0
;; stddev: 48.43470141117833
;; 59.78% of players within [1, 50] pulls obtain a 5-Star Hero for the first time.
;; 24.39% of players within [51, 100] pulls obtain a 5-Star Hero for the first time.
;; 9.1% of players within [101, 150] pulls obtain a 5-Star Hero for the first time.
;; 6.73% of players within [151, 200] pulls obtain a 5-Star Hero for the first time.
;; 0.0% of players through the hard pity of 200 pulls obtain a 5-Star Hero for the first time.
