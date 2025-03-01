#lang racket
(require threading)
(require "./utils.rkt")
(require "./rarities.rkt")

;; 特定英灵召唤
;; UP 哈苏和喀美特，相当于放入 14 只 "哈苏" 和 14 只 "喀美特"
(define special-spirits-rarities
  (~> (add-hero spirits-rarities "哈苏" 5 #f 14)
      (add-hero _ "喀美特" 5 #f 14)))

;; 狂欢英灵召唤
(define crazy-spirits-rarities (make-spirits-crazy spirits-rarities))

;; 狂欢神圣召唤
(define crazy-divine-rarities (make-divine-crazy divine-rarities))


