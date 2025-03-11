#lang racket
(require "./statistics.rkt")

;; International version

(for ([i (in-range 10 200 10)])
  (run-statistics-7 10000 0 i))

(for ([i (in-range 10 200 10)])
  (run-statistics-7 10000 0 i #t))
