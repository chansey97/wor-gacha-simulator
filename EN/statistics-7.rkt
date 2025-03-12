#lang racket
(require math/statistics)
(require "../structs.rkt")
(require "./statistics.rkt")

;; International version

;; run-statistics-7: Count the number of pulls per 5-Stars Hero when using the dual-lego strategy in
;; Limited Invocation of Spirits and Normal Invocation of Spirits
(define (run-statistics-7 sample-size init-shared-pity start-in-limited-pulls [adjoint-is-crazy #f])
  (printf "run-statistics-7:\n")
  (define limited-pool limited-spirits-pool)
  (define limited-pool-name (get-field name limited-pool))
  (define limited-pool-up-hero (get-field up-hero limited-pool))
  (define adjoint-pool (if adjoint-is-crazy crazy-spirits-pool normal-spirits-pool))
  (define adjoint-pool-name (get-field name adjoint-pool))
  (define shared-pity-system (get-field pity-system limited-pool))

  (define (dual-lego-optimization-strategy)
    (send limited-pool reset)
    (send adjoint-pool reset)
    (set-field! current-pity shared-pity-system init-shared-pity)
    
    (let loop ((pull-count 0)
               (5-stars-count 0)
               (status 'start-in-limited)
               (status-chains '((start-in-limited 0))))
      (match status
        ['start-in-limited
         (if (< pull-count start-in-limited-pulls)
             (let* ((cards (send limited-pool pull))
                    (card (first cards))
                    (hero (card-hero card))
                    (rarity (card-rarity card)))
               (if (= (rarity-stars rarity) 5)
                   (if (string=? hero limited-pool-up-hero)
                       (list (add1 pull-count) (add1 5-stars-count)
                             (append status-chains (list (list status (add1 pull-count)))))
                       (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited
                             (append status-chains (list (list 'continue-in-limited (add1 pull-count))))))
                   (loop (add1 pull-count) 5-stars-count status status-chains)))
             (loop pull-count 5-stars-count 'pull-in-adjoint
                   (append status-chains (list (list 'pull-in-adjoint pull-count)))))]
        ['continue-in-limited
         (let* ((cards (send limited-pool pull))
                (card (first cards))
                (hero (card-hero card))
                (rarity (card-rarity card)))
           (if (= (rarity-stars rarity) 5)
               (if (string=? hero limited-pool-up-hero)
                   (list (add1 pull-count) (add1 5-stars-count)
                         (append status-chains (list (list status (add1 pull-count)))))
                   (loop (add1 pull-count) (add1 5-stars-count) status status-chains))
               (loop (add1 pull-count) 5-stars-count status status-chains)))]
        ['pull-in-adjoint
         (let* ((cards (send adjoint-pool pull))
                (card (first cards))
                (rarity (card-rarity card)))
           (if (= (rarity-stars rarity) 5)
               (loop (add1 pull-count) (add1 5-stars-count) 'continue-in-limited
                     (append status-chains (list (list 'continue-in-limited (add1 pull-count)))))
               (loop (add1 pull-count) 5-stars-count status status-chains)))]
        )))
  (let* ((samples (for/list ([i (in-range sample-size)]) (dual-lego-optimization-strategy)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-stars-count (map second samples))
         (lst-of-status-chains (map third samples))
         (global-pulls-per-5-stars (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-stars-count))))
         (individual-pulls-per-5-stars-samples (map / lst-of-pull-count lst-of-5-stars-count))
         (individual-pulls-per-5-stars-average (exact->inexact (mean individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-median (exact->inexact (median < individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-max (exact->inexact (apply max individual-pulls-per-5-stars-samples)))
         ;; (individual-pulls-per-5-stars-max-detail
         ;;  (let ((max-index (index-of individual-pulls-per-5-stars-samples (exact-round individual-pulls-per-5-stars-max))))
         ;;    (list (list-ref lst-of-pull-count max-index)
         ;;          (list-ref lst-of-5-stars-count max-index)
         ;;          (list-ref lst-of-status-chains max-index))))
         (individual-pulls-per-5-stars-min (exact->inexact (apply min individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-stddev (exact->inexact (stddev individual-pulls-per-5-stars-samples)))
         ;; (debug-detail
         ;;  (let ((median-index (index-of individual-pulls-per-5-stars-samples 100)))
         ;;    (if median-index
         ;;        (list (list-ref lst-of-pull-count median-index)
         ;;              (list-ref lst-of-5-stars-count median-index)
         ;;              (list-ref lst-of-status-chains median-index))
         ;;        #f)))
         )

    (printf "==== Count ~a + ~a the number of pulls per ~a when using the dual-lego strategy (sample-size: ~a) ====\n"
            limited-pool-name adjoint-pool-name "5-Stars Hero" sample-size)
    (printf "-- Firstly, pulling in ~a (init-shared-pity:~a)\n"
            limited-pool-name init-shared-pity)
    (printf "If ~a pulls do not yield a 5-Stars Hero, switch to ~a until a 5-Stars Hero is pulled, then return to ~a and continue pulling until a Limited 5-Stars Hero is obtained.\n"
            start-in-limited-pulls adjoint-pool-name limited-pool-name)
    (printf "If ~a pulls yield a 5-Stars Hero, if it is a Limited 5-Stars Hero, then stop; otherwise, continue pulling until a Limited 5-Stars Hero is obtained.\n"
            start-in-limited-pulls)
    (printf "average(Macro indicator): ~a\n" global-pulls-per-5-stars)
    (printf "average: ~a\n" individual-pulls-per-5-stars-average)
    (printf "median: ~a\n" individual-pulls-per-5-stars-median)
    (printf "max: ~a\n" individual-pulls-per-5-stars-max)
    ;; (printf "max detail: ~a\n" individual-pulls-per-5-stars-max-detail)
    (printf "min: ~a\n" individual-pulls-per-5-stars-min)
    (printf "stddev: ~a\n" individual-pulls-per-5-stars-stddev)
    ;; (printf "debug-detail: ~a\n" debug-detail)
    (printf "\n")
    ))

(for ([i (in-range 10 200 10)])
  (run-statistics-7 10000 0 i))

(for ([i (in-range 10 200 10)])
  (run-statistics-7 10000 0 i #t))
