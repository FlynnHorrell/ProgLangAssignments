#lang racket
;; Programming Languages, Assignment 9 tests
;;
;; This file uses a basic testing mechanism similar to what we did with OCAML.
;; To test, simply run this file in DrRacket.

(require "assignment9sub.rkt")

;; add-nums
(equal? (add-nums (list)) 0) ;;empty list
(equal? (add-nums (list 1 2 'a 3)) 6) ;; non-number
(equal? (add-nums (list -1 2 3 4 5)) 13) 

;; length
(equal? (length (list)) 0) ;; empty list
(equal? (length (list 1 2 3 4)) 4)
(equal? (length (list 1 'a 2 )) 3)



;; get-nth
(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "negative index"))])
    (get-nth null -2))   ;;negative index

(with-handlers ([exn:fail? (lambda (exn) (equal? (exn-message exn)
                                                 "list is too short"))])
    (get-nth (list 1 2 3) 5))   ;;List is too short
(equal? (get-nth (list 1 4 3 'a 3) 2) 3)    
(equal? (get-nth (list 1 4 3 'a 3) 0) 1)  

;; every-other
(equal? (every-other (list 1 2 3 4)) (list 1 3)) ;; even length
(equal? (every-other (list 1 2 3)) (list 1 3))   ;; odd length
(equal? (every-other (list)) (list))   ;; null list
(equal? (every-other (list 1)) (list 1))   
(equal? (every-other (list 1 2)) (list 1))   

;; map
(equal? (map (lambda (x) (* x x)) (list 1 2 3))
     (list 1 4 9))       ;; squaring

;; map2
(equal? (map2 (lambda (x y) (* x y)) (list 1 2 3) (list 2 3 4))
     (list 2 6 12))      ;; multiply

;; filter
(equal? (filter (lambda (x) (= (modulo x 2) 1))
               (list 1 2 3 4))
     (list 1 3))      ;; odd

;; call-all
(equal? (call-all (list (lambda () 2)))
     (list 2))        ;; one-element
