#lang racket
(require threading)
(require racket/class)
(require "./structs.rkt")
(require "./utils.rkt")
(require "./spirits-pool.rkt")
(require "./limited-spirits-pool.rkt")
(require "./surprise-spirits-pool.rkt")
(require "./ancient-pool.rkt")
(require "./divine-pool.rkt")
(require "./rarities.rkt")

(define (create-pools is-soft-pity-on)
  (define spirits-shared-pity (box 0))  ; 英灵系共享保底
  (define ancient-shared-pity (box 0)) ; 远古系共享保底
  (define divine-shared-pity (box 0))  ; 神圣系共享保底

  ;; 普通英灵召唤
  (define normal-spirits-rarities spirits-rarities)
  (define normal-spirits-pool-name "普通英灵召唤")
  (define normal-spirits-pool
    (new spirits-pool%
         [name normal-spirits-pool-name]
         [shared-pity spirits-shared-pity]
         [rarities normal-spirits-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 特定英灵召唤
  (define special-spirits-up-heroes-5-stars-lord '())
  (define special-spirits-up-heroes-5-stars '("康斯坦丝" "卡利普索"))
  (define special-spirits-up-heroes-4-stars '("艾斯米" "塞蕾妮" "奥西伦"))
  (define special-spirits-rarities
    (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) spirits-rarities special-spirits-up-heroes-5-stars-lord)
        (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-spirits-up-heroes-5-stars)
        (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-spirits-up-heroes-4-stars)))
  (define special-spirits-pool-name "特定英灵召唤")
  (define special-spirits-pool
    (new spirits-pool%
         [name special-spirits-pool-name]
         [shared-pity spirits-shared-pity]
         [rarities special-spirits-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 限定英灵召唤
  (define limited-spirits-up-hero-5-stars "貂蝉")
  (define limited-spirits-rarities
    (add-hero spirits-rarities limited-spirits-up-hero-5-stars 5 #f 15))
  (define limited-spirits-pool-name "限定英灵召唤")
  (define limited-spirits-pool
    (new limited-spirits-pool%
         [name limited-spirits-pool-name]
         [shared-pity spirits-shared-pity]
         [rarities limited-spirits-rarities]
         [is-soft-pity-on is-soft-pity-on]
         [up-hero limited-spirits-up-hero-5-stars]))

  ;; 狂欢英灵召唤
  (define crazy-spirits-rarities
    (make-spirits-crazy spirits-rarities))
  (define crazy-spirits-pool-name "狂欢英灵召唤")
  (define crazy-spirits-pool
    (new spirits-pool%
         [name crazy-spirits-pool-name]
         [shared-pity spirits-shared-pity]
         [rarities crazy-spirits-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 惊喜英灵召唤
  (define surprise-spirits-rarities spirits-rarities)
  (define surprise-spirits-pool-name "惊喜英灵召唤")
  (define surprise-spirits-pool
    (new surprise-spirits-pool%
         [name surprise-spirits-pool-name]
         [shared-pity spirits-shared-pity]
         [rarities surprise-spirits-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 普通远古召唤
  (define normal-ancient-rarities ancient-rarities)
  (define normal-ancient-pool-name "普通远古召唤")
  (define normal-ancient-pool
    (new ancient-pool%
         [name normal-ancient-pool-name]
         [shared-pity ancient-shared-pity]
         [rarities normal-ancient-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 特定远古召唤
  (define special-ancient-up-heroes-5-stars-lord '("居鲁士"))
  (define special-ancient-up-heroes-5-stars '("吕布"))
  (define special-ancient-rarities
    (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) ancient-rarities special-ancient-up-heroes-5-stars-lord)
        (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _                special-ancient-up-heroes-5-stars)))
  (define special-ancient-pool-name "特定远古召唤")
  (define special-ancient-pool
    (new ancient-pool%
         [name special-ancient-pool-name]
         [shared-pity ancient-shared-pity]
         [rarities special-ancient-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 普通神圣召唤
  (define normal-divine-rarities divine-rarities)
  (define normal-divine-pool-name "普通神圣召唤")
  (define normal-divine-pool
    (new divine-pool%
         [name normal-divine-pool-name]
         [shared-pity divine-shared-pity]
         [rarities normal-divine-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 特定神圣召唤
  (define special-divine-up-heroes-5-stars-lord '())
  (define special-divine-up-heroes-5-stars '("康斯坦丝" "卡利普索"))
  (define special-divine-up-heroes-4-stars '("艾斯米" "塞蕾妮" "奥西伦"))
  (define special-divine-rarities
    (~> (foldl (λ (hero rs) (add-hero rs hero 5 #t 14)) divine-rarities special-divine-up-heroes-5-stars-lord)
        (foldl (λ (hero rs) (add-hero rs hero 5 #f 14)) _ special-divine-up-heroes-5-stars)
        (foldl (λ (hero rs) (add-hero rs hero 4 #f 14)) _ special-divine-up-heroes-4-stars)))
  (define special-divine-pool-name "特定神圣召唤")
  (define special-divine-pool
    (new divine-pool%
         [name special-divine-pool-name]
         [shared-pity divine-shared-pity]
         [rarities special-divine-rarities]
         [is-soft-pity-on is-soft-pity-on]))

  ;; 狂欢神圣召唤
  (define crazy-divine-rarities
    (make-divine-crazy divine-rarities))
  (define crazy-divine-pool-name "狂欢神圣召唤")
  (define crazy-divine-pool
    (new divine-pool%
         [name crazy-divine-pool-name]
         [shared-pity divine-shared-pity]
         [rarities crazy-divine-rarities]
         [is-soft-pity-on is-soft-pity-on]))

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
  pools)

(define (test-hard-pity-exact)
  (printf "test-hard-pity\n")
  (define pools (create-pools #f))
  (printf "口口口口召唤 (5星领主 5星普通 4星领主 4星普通 3星领主 3星普通 2星普通)\n")
  (printf "probability\n")
  (for ([pool pools])
    (let ((probs (for/list ([rarity (get-field rarities pool)])
                   (rarity-probability rarity))))
      (printf "~a ~a\n" (get-field name pool) probs)))
  (printf "probability exact?\n")
  (for ([pool pools])
    (let ((probs (for/list ([rarity (get-field rarities pool)])
                   (rarity-probability rarity))))
      (printf "~a ~a\n" (get-field name pool) (map exact? probs))))
  
  (printf "call update-rarities-for-hard-pity\n")
  (printf "probability\n")
  (for ([pool pools])
    (send pool update-rarities-for-hard-pity)
    (let ((probs (for/list ([rarity (get-field rarities pool)])
                   (rarity-probability rarity))))
      (printf "~a ~a\n" (get-field name pool) probs)))
  (printf "probability exact?\n")
  (for ([pool pools])
    (let ((probs (for/list ([rarity (get-field rarities pool)])
                   (rarity-probability rarity))))
      (printf "~a ~a\n" (get-field name pool) (map exact? probs))))
  (flush-output))

(define (test-soft-pity-exact)
  (printf "test-soft-pity\n")
  (define pools (create-pools #t))
  (printf "口口口口召唤 (5星领主 5星普通 4星领主 4星普通 3星领主 3星普通 2星普通)\n")
  (printf "probability\n")
  (for ([pool pools])
    (let ((probs (for/list ([rarity (get-field rarities pool)])
                   (rarity-probability rarity))))
      (printf "~a ~a\n" (get-field name pool) probs)))
  (printf "probability exact?\n")
  (for ([pool pools])
    (let ((probs (for/list ([rarity (get-field rarities pool)])
                   (rarity-probability rarity))))
      (printf "~a ~a\n" (get-field name pool) (map exact? probs))))

  (for ([i (in-range 1 3)])    
    (printf "call update-rarities-for-soft-pity ~a\n" i)
    (printf "probability\n")
    (for ([pool pools])
      (send pool update-rarities-for-soft-pity i)
      (let ((probs (for/list ([rarity (get-field rarities pool)])
                     (rarity-probability rarity))))
        (printf "~a ~a\n" (get-field name pool) probs)))
    (printf "probability exact?\n")
    (for ([pool pools])
      (let ((probs (for/list ([rarity (get-field rarities pool)])
                     (rarity-probability rarity))))
        (printf "~a ~a\n" (get-field name pool) (map exact? probs)))))
  (flush-output))


(define (test-soft-pity-update-rarities)
  (printf "test-soft-pity\n")
  (define pools (create-pools #t))
  (printf "口口口口召唤 (5星领主 5星普通 4星领主 4星普通 3星领主 3星普通 2星普通)\n")
  
  (printf "probability\n")
  (for ([pool pools])
    (let ((probs (for/list ([rarity (get-field rarities pool)])
                   (rarity-probability rarity))))
      (printf "~a ~a\n" (get-field name pool) (map exact->inexact probs))))
  
  (printf "cumulative\n")
  (for ([pool pools])
    (let* ((rarities (get-field rarities pool))
           (cumulative (send pool build-cumulative rarities)))
      (printf "~a ~a\n" (get-field name pool) (map exact->inexact cumulative))
      ))
  ;; 软保底先于硬保底达到 100%
  ;; 比如：远古池是从 185 抽开始，每抽5星领主概率提升8%
  ;; 从 186 到 200 共有 15 次机会，每次增加 8%。
  ;; 在第 198 抽时（触发第 13 次软保底时），5星领主英雄的概率已经超过 100%，其他品质英雄的概率都为 0
  ;; （此时，5星领主英雄的概率 1.048=(13*0.08)+0.008），因此我们有理由在 [0, 1.048) 中选取，而非 [0, 1)。
  ;; 更极端的情况，比如：神圣池（已普通神圣池为例），是从 12 抽开始，每抽5星英雄概率提升5%
  ;; 从 13 到 20 共有 8 次机会，每次增加 5%。
  ;; 显然，第 20 抽时，还不足以让 5星英雄的概率达到 100%，但为了更好的容错，我们处理这种情况。
  ;; 假设没有硬保底，在第 31 抽时（触发第 19 次软保底时），5星英雄的概率超过了 100%，其他品质英雄的概率都为 0
  ;; （此时，5星英雄的概率 1.01=(19*0.05)+0.004+0.056），但5星领主和5星普通英雄的概率仍然要维持原来的比例
  ;; （事实上，此时 5星领主的概率是 0.06733333333333333，5星普通的概率是 0.9426666666666667）。
  (for ([i (in-range 1 20)]) ; 注意 i = 13 和 i=19
    (printf "call update-rarities-for-soft-pity ~a\n" i)
    (printf "probability\n")
    (for ([pool pools])
      (send pool update-rarities-for-soft-pity i)
      (let ((probs (for/list ([rarity (get-field rarities pool)])
                     (rarity-probability rarity))))
        (printf "~a ~a\n" (get-field name pool) (map exact->inexact probs))))
    (printf "cumulative\n")
    (for ([pool pools])
      (let* ((rarities (get-field rarities pool))
             (cumulative (send pool build-cumulative rarities)))
        (printf "~a ~a\n" (get-field name pool) (map exact->inexact cumulative))
        )))
  
  (flush-output))
