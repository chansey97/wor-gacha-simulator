#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-6: Count the number of pulls per 5-Stars Hero when pulling a limited character in
;; Limited Invocation of Spirits.
(define (run-statistics-6 sample-size)
  (printf "run-statistics-6:\n")
  (define pool limited-spirits-pool)
  (define pool-name (get-field name pool))
  (define up-hero (get-field up-hero pool))
  (define rarities (get-field base-rarities pool))
  
  (define (pull-until-get-limited)
    (send pool reset)
    (let loop ((pull-count 0)
               (5-stars-count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (hero (card-hero card))
             (rarity (card-rarity card)))
        (if (= (rarity-stars rarity) 5)
            (if (string=? hero up-hero)
                (list (add1 pull-count) (add1 5-stars-count))
                (loop (add1 pull-count) (add1 5-stars-count)))
            (loop (add1 pull-count) 5-stars-count)))))

  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-limited)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-stars-count (map second samples))
         (global-pulls-per-5-stars (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-stars-count))))
         (individual-pulls-per-5-stars-samples (map / lst-of-pull-count lst-of-5-stars-count))
         (individual-pulls-per-5-stars-average (exact->inexact (mean individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-median (exact->inexact (median < individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-max (exact->inexact (apply max individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-min (exact->inexact (apply min individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-stddev (exact->inexact (stddev individual-pulls-per-5-stars-samples))))
    (printf "==== Count ~a the number of pulls per ~a when a Limited 5-Stars Hero is obtained (sample-size: ~a). ====\n"
            pool-name "5-Stars Hero" sample-size)
    (printf "average(Macro indicator): ~a\n" global-pulls-per-5-stars)
    (printf "average: ~a\n" individual-pulls-per-5-stars-average)
    (printf "median: ~a\n" individual-pulls-per-5-stars-median)
    (printf "max: ~a\n" individual-pulls-per-5-stars-max)
    (printf "min: ~a\n" individual-pulls-per-5-stars-min)
    (printf "stddev: ~a\n" individual-pulls-per-5-stars-stddev)
    ;; (printf "lst-of-pull-count: ~a\n" (remove-duplicates lst-of-pull-count))
    ;; (printf "lst-of-5-stars-count ~a\n" (remove-duplicates lst-of-5-stars-count))
    (printf "\n")
    ))

(run-statistics-6 10000)

;; run-statistics-6:
;; ==== Count Limited Invocation of Spirits the number of pulls per 5-Stars Hero when a Limited 5-Stars Hero is obtained (sample-size: 10000). ====
;; average(Macro indicator): 86.82110663598006
;; average: 93.98688666666666
;; median: 100.0
;; max: 194.0
;; min: 1.0
;; stddev: 35.88219003421614
