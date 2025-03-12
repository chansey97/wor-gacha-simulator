#lang racket
(require threading)
(require racket/class)
(require "../structs.rkt")
(require "../utils.rkt")
(require "../pity-system.rkt")
(require "../spirits-pool.rkt")
(require "../limited-spirits-pool.rkt")
(require "../surprise-spirits-pool.rkt")
(require "../ancient-pool.rkt")
(require "../divine-pool.rkt")
(require "./rarities.rkt")

;; International server has soft-pity
(define is-soft-pity-on #t)

(define (main)
  ;; Spirits shared pity system
  (define spirits-pity-system
    (new pity-system%
         [hard-pity-threshold 199]
         [soft-pity-threshold 180]
         [soft-pity-boost #e0.05]
         [is-soft-pity-on is-soft-pity-on]))
  
  ;; Ancient shared pity system
  (define ancient-pity-system
    (new pity-system%
         [hard-pity-threshold 199]
         [soft-pity-threshold 185]
         [soft-pity-boost #e0.08]
         [is-soft-pity-on is-soft-pity-on]))
  
  ;; Divine shared pity system
  (define divine-pity-system
    (new pity-system%
         [hard-pity-threshold 19]
         [soft-pity-threshold 12]
         [soft-pity-boost #e0.05]
         [is-soft-pity-on is-soft-pity-on])) 

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
  
  (let loop ()
    (printf "Welcome to WOR gacha simulator!\n")
    (printf "1. ~a\n" normal-spirits-pool-name)
    (printf "2. ~a -" special-spirits-pool-name)
    (for ([h special-spirits-up-heroes-5-stars-lord])
      (printf " ~a" h))
    (for ([h special-spirits-up-heroes-5-stars])
      (printf " ~a" h))
    (for ([h special-spirits-up-heroes-4-stars])
      (printf " ~a" h))
    (printf "\n")
    (printf "3. ~a -" limited-spirits-pool-name)
    (printf " ~a\n" limited-spirits-up-hero-5-stars)
    (printf "4. ~a\n" crazy-spirits-pool-name)
    (printf "5. ~a\n" surprise-spirits-pool-name)
    (printf "6. ~a\n" normal-ancient-pool-name)
    (printf "7. ~a -" special-ancient-pool-name)
    (for ([h special-ancient-up-heroes-5-stars-lord])
      (printf " ~a" h))
    (for ([h special-ancient-up-heroes-5-stars])
      (printf " ~a" h))
    (printf "\n")
    (printf "8. ~a\n" normal-divine-pool-name)
    (printf "9. ~a -" special-divine-pool-name)
    (for ([h special-divine-up-heroes-5-stars-lord])
      (printf " ~a" h))
    (for ([h special-divine-up-heroes-5-stars])
      (printf " ~a" h))
    (for ([h special-divine-up-heroes-4-stars])
      (printf " ~a" h))
    (printf "\n")
    (printf "10. ~a\n" crazy-divine-pool-name)
    (display "Please select a card pool from 1-10, enter 'i' to check the card pool status, 'r' to reset the card pool, or 'q' to quit:\n")
    (flush-output)
    (match (read-line)
      ["q"
       (printf "Goodbye!\n")]
      ["r"
       (define pools (list normal-spirits-pool
                           special-spirits-pool
                           limited-spirits-pool
                           crazy-spirits-pool
                           surprise-spirits-pool
                           normal-ancient-pool
                           special-ancient-pool
                           normal-divine-pool
                           special-divine-pool
                           crazy-divine-pool))
       (for ([pool pools])
         (send pool reset))
       (printf "The card pool has been reset:\n")
       (printf "Spirits shared pity: ~a/~a\n"
               (get-field current-pity spirits-pity-system)
               (add1 (get-field hard-pity-threshold spirits-pity-system)))
       (printf "Ancient shared pity: ~a/~a\n"
               (get-field current-pity ancient-pity-system)
               (add1 (get-field hard-pity-threshold ancient-pity-system)))
       (printf "Divine shared pity: ~a/~a\n"
               (get-field current-pity divine-pity-system)
               (add1 (get-field hard-pity-threshold divine-pity-system)))
       (let ((limited-spirits-own-pity (get-field own-pity limited-spirits-pool))
             (limited-spirits-own-pity-threshold+1 (add1 (get-field own-pity-threshold limited-spirits-pool)))
             (limited-spirits-up-hero (get-field up-hero limited-spirits-pool)))
         (printf "Limited Invocation of Spirits - ~a, pity:~a/~a\n"
                 limited-spirits-up-hero
                 limited-spirits-own-pity
                 limited-spirits-own-pity-threshold+1))
       (printf "Press any key to return to the main menu:\n")
       (flush-output)
       (read-line)
       (loop)]
      ["i"
       (printf "Card pool status:\n")
       (printf "Spirits shared pity: ~a/~a\n"
               (get-field current-pity spirits-pity-system)
               (add1 (get-field hard-pity-threshold spirits-pity-system)))
       (printf "Ancient shared pity: ~a/~a\n"
               (get-field current-pity ancient-pity-system)
               (add1 (get-field hard-pity-threshold ancient-pity-system)))
       (printf "Divine shared pity: ~a/~a\n"
               (get-field current-pity divine-pity-system)
               (add1 (get-field hard-pity-threshold divine-pity-system)))
       (let ((limited-spirits-own-pity (get-field own-pity limited-spirits-pool))
             (limited-spirits-own-pity-threshold+1 (add1 (get-field own-pity-threshold limited-spirits-pool)))
             (limited-spirits-up-hero (get-field up-hero limited-spirits-pool)))
         (printf "Limited Invocation of Spirits - ~a, pity:~a/~a\n"
                 limited-spirits-up-hero
                 limited-spirits-own-pity
                 limited-spirits-own-pity-threshold+1))
       (printf "Press any key to return to the main menu:\n")
       (flush-output)
       (read-line)
       (loop)]
      ["1"
       (pull-interface normal-spirits-pool)
       (loop)]
      ["2"
       (pull-interface special-spirits-pool)
       (loop)]
      ["3"
       (pull-interface limited-spirits-pool)
       (loop)]
      ["4"
       (pull-interface crazy-spirits-pool)
       (loop)]
      ["5"
       (pull-interface surprise-spirits-pool)
       (loop)]
      ["6"
       (pull-interface normal-ancient-pool)
       (loop)]
      ["7"
       (pull-interface special-ancient-pool)
       (loop)]
      ["8"
       (pull-interface normal-divine-pool)
       (loop)]
      ["9"
       (pull-interface special-divine-pool)
       (loop)]
      ["10"
       (pull-interface crazy-divine-pool)
       (loop)]
      [_
       (printf "Invalid input\n")
       (loop)])
    ))

(define (pull-interface pool)
  (define pool-name (get-field name pool))
  (define pool-pity-system (get-field pity-system pool))
  (printf "~a\n" pool-name)
  (printf "Enter 's' for a single pull, 'm' for 10-pulls, or 'b' to return to the main menu:\n")
  (let loop ()
    (flush-output)
    (match (read-line)
      ["b"
       (printf "return to the main menu\n")]
      ["s"
       (for ([card (send pool pull)]
             [i (in-naturals)])
         (let ((hero (card-hero card))
               (rarity (card-rarity card)))
           (if (= i 0)
               (printf "~a | ~a\n" (rarity-name rarity) hero)
               (printf "~a | ~a (*)\n" (rarity-name rarity) hero))))
       (if (is-a? pool limited-spirits-pool%)
           (printf "shared pity:~a/~a, limited pity:~a/~a\n"
                   (get-field current-pity pool-pity-system)
                   (add1 (get-field hard-pity-threshold pool-pity-system))
                   (get-field own-pity pool)
                   (add1 (get-field own-pity-threshold pool)))  
           (printf "shared pity:~a/~a\n"
                   (get-field current-pity pool-pity-system)
                   (add1 (get-field hard-pity-threshold pool-pity-system))))
       (loop)]
      ["m"
       (for ([i (in-range 10)])
         (for ([card (send pool pull)]
               [i (in-naturals)])
           (let ((hero (card-hero card))
                 (rarity (card-rarity card)))
             (if (= i 0)
                 (printf "~a | ~a\n" (rarity-name rarity) hero)
                 (printf "~a | ~a (*)\n" (rarity-name rarity) hero)))))
       (if (is-a? pool limited-spirits-pool%)
           (printf "shared pity:~a/~a, limited pity:~a/~a\n"
                   (get-field current-pity pool-pity-system)
                   (add1 (get-field hard-pity-threshold pool-pity-system))
                   (get-field own-pity pool)
                   (add1 (get-field own-pity-threshold pool)))
           (printf "shared pity:~a/~a\n"
                   (get-field current-pity pool-pity-system)
                   (add1 (get-field hard-pity-threshold pool-pity-system))))
       (loop)]
      [_
       (printf "Invalid input\n")
       (loop)])))

(main)
