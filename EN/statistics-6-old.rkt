#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version (old version)

;; run-statistics-6: Count the number of pulls per 5-Star Hero when pulling a limited character in
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
               (5-star-count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (hero (card-hero card))
             (rarity (card-rarity card)))
        (if (= (rarity-star rarity) 5)
            (if (string=? hero up-hero)
                (list (add1 pull-count) (add1 5-star-count))
                (loop (add1 pull-count) (add1 5-star-count)))
            (loop (add1 pull-count) 5-star-count)))))

  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-limited)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-star-count (map second samples))
         (global-pulls-per-5-star (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-star-count))))
         (individual-pulls-per-5-star-samples (map / lst-of-pull-count lst-of-5-star-count))
         (individual-pulls-per-5-star-average (exact->inexact (mean individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-median (exact->inexact (median < individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-max (exact->inexact (apply max individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-min (exact->inexact (apply min individual-pulls-per-5-star-samples)))
         (individual-pulls-per-5-star-stddev (exact->inexact (stddev individual-pulls-per-5-star-samples))))
    (printf "==== Count ~a the number of pulls per ~a when a Limited 5-Star Hero is obtained (sample-size: ~a). ====\n"
            pool-name "5-Star Hero" sample-size)
    (printf "average(macro indicator): ~a\n" global-pulls-per-5-star)
    (printf "average: ~a\n" individual-pulls-per-5-star-average)
    (printf "median: ~a\n" individual-pulls-per-5-star-median)
    (printf "max: ~a\n" individual-pulls-per-5-star-max)
    (printf "min: ~a\n" individual-pulls-per-5-star-min)
    (printf "stddev: ~a\n" individual-pulls-per-5-star-stddev)
    ;; (printf "lst-of-pull-count: ~a\n" (remove-duplicates lst-of-pull-count))
    ;; (printf "lst-of-5-star-count ~a\n" (remove-duplicates lst-of-5-star-count))
    (printf "\n")
    ))

;; In old international version, Limited Pool's own-pity-threshold is 249 instead of 199
(let ((backup-own-pity-threshold (get-field own-pity-threshold limited-spirits-pool)))
  (set-field! own-pity-threshold limited-spirits-pool 249)
  (run-statistics-6 10000)
  (set-field! own-pity-threshold limited-spirits-pool backup-own-pity-threshold))


;; run-statistics-6:
;; ==== Count Limited Invocation of Spirits the number of pulls per 5-Star Hero when a Limited 5-Star Hero is obtained (sample-size: 10000). ====
;; average(macro indicator): 94.93040945840215
;; average: 102.73705261904762
;; median: 107.0
;; max: 193.0
;; min: 1.0
;; stddev: 37.99421506282296
