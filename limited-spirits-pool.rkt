#lang racket
(require "./spirits-pool.rkt")
(provide (all-defined-out))

;; 限定英灵召唤（带独立保底）
(define limited-spirits-pool%
  (class spirits-pool%
    (super-new)
    (init-field up-hero)       ; UP 的英雄名称
    (init-field [own-pity 0])  ; 独立保底计数器

    (define/override (pull)
      (cond
        ;; 限定保底关闭前
        [(>= own-pity 0)
         (cond
           ;; 200 抽必出限定, 关闭限定保底
           [(>= own-pity 199)
            (super reset)
            (set! own-pity -1)
            (list up-hero)]
           [else
            ;; 200 抽之前的状态
            (match-let ([(list hero) (super pull)])
              (cond
                ;; 提前抽出限定, 关闭限定保底
                [(string=? hero up-hero)
                 (super reset)
                 (set! own-pity -1)  
                 (list hero)]
                ;; 没有抽出限定
                [else
                 (set! own-pity (+ own-pity 1))
                 (list hero)]))])]
        ;; 限定保底关闭后
        [else
         (super pull)]))

    (define/override (reset)
      (super reset)
      (set! own-pity 0))
    ))
