#lang racket
(require racket/class)
(require threading)
(require math/statistics)
(require "./structs.rkt")
(require "./utils.rkt")
(require "./spirits-pool.rkt")
(require "./limited-spirits-pool.rkt")
(require "./surprise-spirits-pool.rkt")
(require "./ancient-pool.rkt")
(require "./divine-pool.rkt")
(require "./rarities.rkt")

(define is-soft-pity-on #f)
;; (define is-soft-pity-on #t)

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

(define (simulate pool predicate)
  ;; (send pool reset)
  (let loop ([count 1])
    (define heroes (send pool pull))
    (define hero (first heroes))
    (if (predicate hero)
        count
        (loop (add1 count)))))

(define (run-simulation name pool predicate sample-size)
  (define results
    (for/list ([i (in-range sample-size)])
      (simulate pool predicate)))
  
  ;; TODO: 200 not HARD CODE!
  (define hard-pity-count 
    (count (λ (x) (= x 200)) results))

  (define average (mean results))
  (define median- (median < results))
  (define max-pulls (apply max results))
  (define min-pulls (apply min results))
  (define std-dev (stddev results))

  (printf "==== ~a 模拟结果（样本量：~a） ====\n" name sample-size)
  (printf "平均抽数: ~a\n" (exact-round average))
  (printf "中位数: ~a\n" median-)
  (printf "最大值: ~a（硬保底触发次数：~a）\n" max-pulls hard-pity-count)
  (printf "最小值: ~a\n" min-pulls)
  (printf "标准差: ~a\n" (real->decimal-string std-dev 1))
  )

(let* ((pool normal-spirits-pool)
       (5-star? (λ (hero)
                  (let* ((rs (get-field rarities pool))
                         (r (find-rarity rs hero)))
                    (= (rarity-stars r) 5)))))
  (run-simulation "5星英雄" pool 5-star? 10000))
;; ==== 5星英雄 模拟结果（样本量：10000） ====
;; 平均抽数: 126
;; 中位数: 139
;; 最大值: 200（硬保底触发次数：3691）
;; 最小值: 1
;; 标准差: 71.8

(let* ((pool crazy-spirits-pool)
       (5-star? (λ (hero)
                  (let* ((rs (get-field rarities pool))
                         (r (find-rarity rs hero)))
                    (= (rarity-stars r) 5)))))
  (run-simulation "5星英雄" pool 5-star? 10000))
;; ==== 5星英雄 模拟结果（样本量：10000） ====
;; 平均抽数: 86
;; 中位数: 70
;; 最大值: 200（硬保底触发次数：1295）
;; 最小值: 1
;; 标准差: 65.9




