#lang racket
(require threading)
(require racket/class)
(require "./structs.rkt")
(require "./utils.rkt")
(require "./pity-system.rkt")
(require "./spirits-pool.rkt")
(require "./limited-spirits-pool.rkt")
(require "./surprise-spirits-pool.rkt")
(require "./ancient-pool.rkt")
(require "./divine-pool.rkt")
(require "./rarities.rkt")

;; 软保底（国际服有，国服没有）
(define is-soft-pity-on #f)

(define (main)
  ;; 英灵系共享保底
  (define spirits-pity-system
    (new pity-system%
         [hard-pity-threshold 199]
         [soft-pity-threshold 180]
         [soft-pity-boost #e0.05]
         [is-soft-pity-on is-soft-pity-on]))
  
  ;; 远古系共享保底
  (define ancient-pity-system
    (new pity-system%
         [hard-pity-threshold 199]
         [soft-pity-threshold 185]
         [soft-pity-boost #e0.08]
         [is-soft-pity-on is-soft-pity-on]))
  
  ;; 神圣系共享保底
  (define divine-pity-system
    (new pity-system%
         [hard-pity-threshold 19]
         [soft-pity-threshold 12]
         [soft-pity-boost #e0.05]
         [is-soft-pity-on is-soft-pity-on])) 

  ;; 普通英灵召唤
  (define normal-spirits-rarities spirits-rarities)
  (define normal-spirits-pool-name "普通英灵召唤")
  (define normal-spirits-pool
    (new spirits-pool%
         [name normal-spirits-pool-name]
         [base-rarities normal-spirits-rarities]
         [pity-system spirits-pity-system]))

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
         [base-rarities special-spirits-rarities]
         [pity-system spirits-pity-system]))

  ;; 限定英灵召唤
  (define limited-spirits-up-hero-5-stars "貂蝉")
  (define limited-spirits-rarities
    (add-hero spirits-rarities limited-spirits-up-hero-5-stars 5 #f 15))
  (define limited-spirits-pool-name "限定英灵召唤")
  (define limited-spirits-pool
    (new limited-spirits-pool%
         [name limited-spirits-pool-name]
         [base-rarities limited-spirits-rarities]
         [pity-system spirits-pity-system]
         [up-hero limited-spirits-up-hero-5-stars]))

  ;; 狂欢英灵召唤
  (define crazy-spirits-rarities
    (make-spirits-crazy spirits-rarities))
  (define crazy-spirits-pool-name "狂欢英灵召唤")
  (define crazy-spirits-pool
    (new spirits-pool%
         [name crazy-spirits-pool-name]
         [base-rarities crazy-spirits-rarities]
         [pity-system spirits-pity-system]))
  
  ;; 惊喜英灵召唤
  (define surprise-spirits-rarities spirits-rarities)
  (define surprise-spirits-pool-name "惊喜英灵召唤")
  (define surprise-spirits-pool
    (new surprise-spirits-pool%
         [name surprise-spirits-pool-name]
         [base-rarities surprise-spirits-rarities]
         [pity-system spirits-pity-system]))
  
  ;; 普通远古召唤
  (define normal-ancient-rarities ancient-rarities)
  (define normal-ancient-pool-name "普通远古召唤")
  (define normal-ancient-pool
    (new ancient-pool%
         [name normal-ancient-pool-name]
         [base-rarities normal-ancient-rarities]
         [pity-system ancient-pity-system]))

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
         [base-rarities special-ancient-rarities]
         [pity-system ancient-pity-system]))

  ;; 普通神圣召唤
  (define normal-divine-rarities divine-rarities)
  (define normal-divine-pool-name "普通神圣召唤")
  (define normal-divine-pool
    (new divine-pool%
         [name normal-divine-pool-name]
         [base-rarities normal-divine-rarities]
         [pity-system divine-pity-system]))

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
         [base-rarities special-divine-rarities]
         [pity-system divine-pity-system]))

  ;; 狂欢神圣召唤
  (define crazy-divine-rarities
    (make-divine-crazy divine-rarities))
  (define crazy-divine-pool-name "狂欢神圣召唤")
  (define crazy-divine-pool
    (new divine-pool%
         [name crazy-divine-pool-name]
         [base-rarities crazy-divine-rarities]
         [pity-system divine-pity-system]))
  
  (let loop ()
    (printf "欢迎来到 WOR 抽卡模拟器！\n")
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
    (display "请选择卡池 1-10，输入 'i' 查询卡池状态，'r' 重置卡池, 'q' 退出：\n")
    (flush-output)
    (match (read-line)
      ["q"
       (printf "再见！\n")]
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
       (printf "卡池已重置：\n")
       (printf "英灵召唤 共享保底：~a\n" (get-field current-pity spirits-pity-system))
       (printf "远古召唤 共享保底：~a\n" (get-field current-pity ancient-pity-system))
       (printf "神圣召唤 共享保底：~a\n" (get-field current-pity divine-pity-system))
       (let ((limited-spirits-own-pity (get-field own-pity limited-spirits-pool))
             (limited-spirits-up-hero (get-field up-hero limited-spirits-pool)))
         (printf "限定英灵召唤 - ~a 保底：~a\n" limited-spirits-up-hero limited-spirits-own-pity))
       (printf "按任意键，返回主菜单：\n")
       (flush-output)
       (read-line)
       (loop)]
      ["i"
       (printf "卡池状态：\n")
       (printf "英灵召唤 共享保底：~a\n" (get-field current-pity spirits-pity-system))
       (printf "远古召唤 共享保底：~a\n" (get-field current-pity ancient-pity-system))
       (printf "神圣召唤 共享保底：~a\n" (get-field current-pity divine-pity-system))
       (let ((limited-spirits-own-pity (get-field own-pity limited-spirits-pool))
             (limited-spirits-up-hero (get-field up-hero limited-spirits-pool)))
         (printf "限定英灵召唤 - ~a 保底：~a\n" limited-spirits-up-hero limited-spirits-own-pity))
       (printf "按任意键，返回主菜单：\n")
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
       (printf "无效输入\n")
       (loop)])
    ))

(define (pull-interface pool)
  (define pool-name (get-field name pool))
  (define pool-rarities (get-field base-rarities pool))
  (define pool-pity-system (get-field pity-system pool))
  (printf "~a\n" pool-name)
  (printf "输入 's' 单抽，'m' 十连，'b' 返回主菜单：\n")
  (let loop ()
    (flush-output)
    (match (read-line)
      ["b"
       (printf "返回主菜单\n")]
      ["s"
       (for ([card (send pool pull)]
             [i (in-naturals)])
         (let ((hero (card-hero card))
               (rarity (card-rarity card)))
           (if (= i 0)
               (printf "~a ~a\n" (rarity-name rarity) hero)
               (printf "+ ~a ~a\n" (rarity-name rarity) hero))))
       (if (is-a? pool limited-spirits-pool%)
           (printf "共享保底：~a，限定保底：~a\n" (get-field current-pity pool-pity-system) (get-field own-pity pool))  
           (printf "共享保底：~a\n" (get-field current-pity pool-pity-system)))
       (loop)]
      ["m"
       (for ([i (in-range 10)])
         (for ([card (send pool pull)]
               [i (in-naturals)])
           (let ((hero (card-hero card))
                 (rarity (card-rarity card)))
             (if (= i 0)
                 (printf "~a ~a\n" (rarity-name rarity) hero)
                 (printf "~a ~a (*)\n" (rarity-name rarity) hero)))))
       (if (is-a? pool limited-spirits-pool%)
           (printf "共享保底：~a，限定保底：~a\n" (get-field current-pity pool-pity-system) (get-field own-pity pool))
           (printf "共享保底：~a\n" (get-field current-pity pool-pity-system)))
       (loop)]
      [_
       (printf "无效操作\n")
       (loop)])))

(main)
