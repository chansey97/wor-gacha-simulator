#lang racket
(require racket/class)
(require threading)
(require math/statistics)
(require "../structs.rkt")
(require "../utils.rkt")
(require "../pity-system.rkt")
(require "../spirits-pool.rkt")
(require "../limited-spirits-pool.rkt")
(require "../surprise-spirits-pool.rkt")
(require "../ancient-pool.rkt")
(require "../divine-pool.rkt")
(require "./rarities.rkt")
(provide (all-defined-out))

;; Spirits shared pity system
(define spirits-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 180]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #t]))

;; Ancient shared pity system
(define ancient-pity-system
  (new pity-system%
       [hard-pity-threshold 199]
       [soft-pity-threshold 185]
       [soft-pity-boost #e0.08]
       [is-soft-pity-on #t]))

;; Divine shared pity system
(define divine-pity-system
  (new pity-system%
       [hard-pity-threshold 19]
       [soft-pity-threshold 12]
       [soft-pity-boost #e0.05]
       [is-soft-pity-on #t])) 

;; Normal Invocation of Spirits
(define normal-spirits-rarities spirits-rarities)
(define normal-spirits-pool-name "Invocation of Spirits")
(define normal-spirits-pool
  (new spirits-pool%
       [name normal-spirits-pool-name]
       [base-rarities normal-spirits-rarities]
       [pity-system spirits-pity-system]))

;; Special Invocation of Spirits
(define special-spirits-up-heroes-5-stars-lord '())
(define special-spirits-up-heroes-5-stars '("Constance" "Calypso"))
(define special-spirits-up-heroes-4-stars '("Esme" "Cyrene" "Osiren"))
(define special-spirits-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) spirits-rarities special-spirits-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-spirits-up-heroes-5-stars)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-spirits-up-heroes-4-stars)))
(define special-spirits-pool-name "Special Invocation of Spirits")
(define special-spirits-pool
  (new spirits-pool%
       [name special-spirits-pool-name]
       [base-rarities special-spirits-rarities]
       [pity-system spirits-pity-system]))

;; Limited Invocation of Spirits
(define limited-spirits-up-hero-5-stars "Diaochan")
(define limited-spirits-rarities
  (add-hero spirits-rarities limited-spirits-up-hero-5-stars 5 #f 15))
(define limited-spirits-pool-name "Limited Invocation of Spirits")
(define limited-spirits-pool
  (new limited-spirits-pool%
       [name limited-spirits-pool-name]
       [base-rarities limited-spirits-rarities]
       [pity-system spirits-pity-system]
       [up-hero limited-spirits-up-hero-5-stars]))

;; Crazy Invocation of Spirits
(define crazy-spirits-rarities
  (make-spirits-crazy spirits-rarities))
(define crazy-spirits-pool-name "Crazy Invocation of Spirits")
(define crazy-spirits-pool
  (new spirits-pool%
       [name crazy-spirits-pool-name]
       [base-rarities crazy-spirits-rarities]
       [pity-system spirits-pity-system]))

;; Surprise Invocation of Spirits
(define surprise-spirits-rarities spirits-rarities)
(define surprise-spirits-pool-name "Surprise Invocation of Spirits")
(define surprise-spirits-pool
  (new surprise-spirits-pool%
       [name surprise-spirits-pool-name]
       [base-rarities surprise-spirits-rarities]
       [pity-system spirits-pity-system]))

;; Normal Ancient Summoning
(define normal-ancient-rarities ancient-rarities)
(define normal-ancient-pool-name "Normal Ancient Summoning")
(define normal-ancient-pool
  (new ancient-pool%
       [name normal-ancient-pool-name]
       [base-rarities normal-ancient-rarities]
       [pity-system ancient-pity-system]))

;; Special Ancient Summoning
(define special-ancient-up-heroes-5-stars-lord '("Cyrus"))
(define special-ancient-up-heroes-5-stars '("Lu Bu"))
(define special-ancient-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) ancient-rarities special-ancient-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _                special-ancient-up-heroes-5-stars)))
(define special-ancient-pool-name "Special Ancient Summoning")
(define special-ancient-pool
  (new ancient-pool%
       [name special-ancient-pool-name]
       [base-rarities special-ancient-rarities]
       [pity-system ancient-pity-system]))

;; Normal Divine Summoning
(define normal-divine-rarities divine-rarities)
(define normal-divine-pool-name "Normal Divine Summoning")
(define normal-divine-pool
  (new divine-pool%
       [name normal-divine-pool-name]
       [base-rarities normal-divine-rarities]
       [pity-system divine-pity-system]))

;; Special Divine Summoning
(define special-divine-up-heroes-5-stars-lord '())
(define special-divine-up-heroes-5-stars '("Constance" "Calypso"))
(define special-divine-up-heroes-4-stars '("Esme" "Cyrene" "Osiren"))
(define special-divine-rarities
  (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) divine-rarities special-divine-up-heroes-5-stars-lord)
      (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-divine-up-heroes-5-stars)
      (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-divine-up-heroes-4-stars)))
(define special-divine-pool-name "Special Divine Summoning")
(define special-divine-pool
  (new divine-pool%
       [name special-divine-pool-name]
       [base-rarities special-divine-rarities]
       [pity-system divine-pity-system]))

;; Crazy Divine Summoning
(define crazy-divine-rarities
  (make-divine-crazy divine-rarities))
(define crazy-divine-pool-name "Crazy Divine Summoning")
(define crazy-divine-pool
  (new divine-pool%
       [name crazy-divine-pool-name]
       [base-rarities crazy-divine-rarities]
       [pity-system divine-pity-system]))

(define (split-into-4-segments n)
  (for/list ((i (in-range 5))) (* i (/ n 4))))

;; run-statistics-1: Count the expected number of pulls to obtain a 5-Stars Hero for the first time in
;; Normal Invocation of Spirits, Crazy Invocation of Spirits, Normal Divine Summoning and Crazy Divine Summoning.
(define (run-statistics-1 sample-size)
  (printf "run-statistics-1:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-stars pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-stars rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-stars pool)))
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
      (printf "==== Count ~a, the expected number of pulls to obtain a ~a for the first time (sample-size: ~a). ====\n"
              pool-name "5-Stars Hero" sample-size)
      (printf "average: ~a\n" average)
      (printf "median: ~a\n" median)
      (printf "max: ~a\n" max)
      (printf "min: ~a\n" min)
      (printf "stddev: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% of players within [~a, ~a] pulls obtain a ~a for the first time.\n"
                percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Stars Hero"))
      (printf "~a% of players through the hard pity of ~a pulls obtain a ~a for the first time.\n"
              hard-pity-percentage (add1 hard-pity-threshold) "5-Stars Hero")
      (printf "\n")
      )))

;; run-statistics-2: Count the expected number of pulls to obtain a 5-Stars Lord Hero for the first time in
;; Normal Ancient Summoning.
(define (run-statistics-2 sample-size)
  (printf "run-statistics-2:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define rarities (get-field base-rarities pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))
  
  (define (pull-until-get-5-stars-lord)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-stars rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-5-stars-lord)))
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
    (printf "==== Count ~a, the expected number of pulls to obtain a ~a for the first time (sample-size: ~a). ====\n"
            pool-name "5-Stars Lord Hero" sample-size)
    (printf "average: ~a\n" average)
    (printf "median: ~a\n" median)
    (printf "max: ~a\n" max)
    (printf "min: ~a\n" min)
    (printf "stddev: ~a\n" stddev)
    (for ([bin bins]
          [percentage percentages])
      (printf "~a% of players within [~a, ~a] pulls obtain a ~a for the first time.\n"
              percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Stars Lord Hero"))
    (printf "~a% of players through the hard pity of ~a pulls obtain a ~a for the first time.\n"
            hard-pity-percentage (add1 hard-pity-threshold) "5-Stars Lord Hero")
    (printf "\n")
    ))

;; run-statistics-3: Count the expected number of pulls to obtain a 5-Stars Lord Hero for the first time in
;; Normal Invocation of Spirits, Crazy Invocation of Spirits, Normal Divine Summoning and Crazy Divine Summoning.
(define (run-statistics-3 sample-size)
  (printf "run-statistics-3:\n")
  (define pools (list normal-spirits-pool
                      crazy-spirits-pool
                      normal-divine-pool
                      crazy-divine-pool))
  
  (define (pull-until-get-5-stars-lord pool)
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (and (= (rarity-stars rarity) 5)
                 (rarity-is-lord rarity))
            (add1 count)
            (loop (add1 count))))))
  
  (for ([pool pools])
    (send pool reset)
    (let* ((pity-system (get-field pity-system pool))
           (pool-name (get-field name pool))
           (hard-pity-threshold (get-field hard-pity-threshold pity-system))
           (samples (for/list ([i (in-range sample-size)])
                      (pull-until-get-5-stars-lord pool)))
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
      (printf "==== Count ~a, the expected number of pulls to obtain a ~a for the first time (sample-size: ~a). ====\n"
              pool-name "5-Stars Lord Hero" sample-size)
      (printf "average: ~a\n" average)
      (printf "median: ~a\n" median)
      (printf "max: ~a\n" max)
      (printf "min: ~a\n" min)
      (printf "stddev: ~a\n" stddev)
      (for ([bin bins]
            [percentage percentages])
        (printf "~a% of players within [~a, ~a] pulls obtain a ~a for the first time.\n"
                percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Stars Lord Hero"))
      (printf "\n")
      )))

;; run-statistics-4: Count the expected number of pulls to obtain a 5-Stars Hero for the first time in
;; Normal Ancient Summoning.
;; NOTE:
;; 1. Since the ancient pool does not reset the pity after pulling a 5-Stars Normal Hero, the sample here
;; may not necessarily represent the number of pulls for the first time a 5-Stars Hero is obtained.
;; 2. The parameter 'first-only' forces the statistics to focus on the first time only.
(define (run-statistics-4 sample-size [first-only #f])
  (printf "run-statistics-4:\n")
  (define pool normal-ancient-pool)
  (define pool-name (get-field name pool))
  (define pity-system (get-field pity-system pool))
  (define hard-pity-threshold (get-field hard-pity-threshold pity-system))

  (define (pull-until-get-5-stars)
    (when first-only
      (send pool reset))
    (let loop ((count 0))
      (let* ((cards (send pool pull))
             (card (first cards))
             (rarity (card-rarity card)))
        (if (= (rarity-stars rarity) 5)
            (add1 count)
            (loop (add1 count))))))

  (send pool reset)
  (let* ((samples (for/list ([i (in-range sample-size)])
                    (pull-until-get-5-stars)))
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
                pool-name "5-Stars Hero" sample-size)
        (printf "==== Count ~a, the expected number of pulls to obtain a ~a for (sample-size: ~a). ====\n"
                pool-name "5-Stars Hero" sample-size))
    (printf "average: ~a\n" average)
    (printf "median: ~a\n" median)
    (printf "max: ~a\n" max)
    (printf "min: ~a\n" min)
    (printf "stddev: ~a\n" stddev)
    (for ([bin bins]
          [percentage percentages])
      (if first-only
          (printf "~a% of players within [~a, ~a] pulls obtain a ~a for the first time.\n"
                  percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Stars Hero")
          (printf "~a% of players within [~a, ~a] pulls obtain a ~a.\n"
                  percentage (add1 (sample-bin-min bin)) (sample-bin-max bin) "5-Stars Hero")))
    (if first-only
        (printf "~a% of players through the hard pity of ~a pulls obtain a ~a for the first time.\n"
                hard-pity-percentage (add1 hard-pity-threshold) "5-Stars Hero")
        (printf "~a% of players through the hard pity of ~a pulls obtain a ~a.\n"
                hard-pity-percentage (add1 hard-pity-threshold) "5-Stars Hero"))
    (printf "\n")
    ))

;; run-statistics-5: Count the number of pulls per 5-Stars Hero when obtaining a surprise reward in
;; Surprise Invocation of Spirits.
(define (run-statistics-5 sample-size)
  (printf "run-statistics-5:\n")
  (define pool surprise-spirits-pool)
  (define pool-name (get-field name pool))
  
  (define (pull-until-get-surprise-bouns)
    (send pool reset)
    (let loop ((pull-count 0))
      (let* ((heroes (send pool pull)))
        (let ((heroes-count (length heroes)))
          (if (= heroes-count 2) ; in surprise pool, when return 2 heroes, MUST BE 5-stars
              (list (add1 pull-count) heroes-count)
              (loop (add1 pull-count)))))))
  
  (let* ((samples (for/list ([i (in-range sample-size)]) (pull-until-get-surprise-bouns)))
         (lst-of-pull-count (map first samples))
         (lst-of-5-stars-count (map second samples))
         (global-pulls-per-5-stars (exact->inexact (/ (apply + lst-of-pull-count) (apply + lst-of-5-stars-count))))
         (individual-pulls-per-5-stars-samples (map / lst-of-pull-count lst-of-5-stars-count))
         (individual-pulls-per-5-stars-average (exact->inexact (mean individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-median (exact->inexact (median < individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-max (exact->inexact (apply max individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-min (exact->inexact (apply min individual-pulls-per-5-stars-samples)))
         (individual-pulls-per-5-stars-stddev (exact->inexact (stddev individual-pulls-per-5-stars-samples))))
    (printf "==== Count ~a, the number of pulls per ~a when a surprise reward is obtained (sample size: ~a). ====\n"
            pool-name "5-Stars Hero" sample-size)
    (printf "average(Macro indicator): ~a\n" global-pulls-per-5-stars)
    (printf "average: ~a\n" individual-pulls-per-5-stars-average)
    (printf "median: ~a\n" individual-pulls-per-5-stars-median)
    (printf "max: ~a\n" individual-pulls-per-5-stars-max)
    (printf "min: ~a\n" individual-pulls-per-5-stars-min)
    (printf "stddev: ~a\n" individual-pulls-per-5-stars-stddev)
    (printf "\n")))

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
    (printf "==== Count the number of pulls per ~a when a Limited 5-Stars Hero is obtained (sample-size: ~a). ====\n"
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

    (printf "==== Count the number of pulls per 5-stars (~a + ~a) when using the dual-lego strategy (sample-size: ~a) ====\n"
            limited-pool-name adjoint-pool-name "5-Stars Hero" sample-size)
    (printf "-- Firstly pulling in ~a (init-shared-pity:~a)\n"
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

