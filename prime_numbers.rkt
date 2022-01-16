;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname _primenumbers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Constants

(define WIDTH 500)
(define HEIGHT 100)

(define TEXT-COLOR "black")
(define TEXT-SIZE 12)

(define TEXT-X (/ WIDTH 2))
(define TEXT-Y (/ HEIGHT 2))

(define MTS (rectangle WIDTH HEIGHT "solid" "white"))


;; Uses a recursive definition of Natural, but with add1 instead of sub1
;; This is because there are infinite primes, so we can always find the next

;; Functions

;; Calculates next prime number after n. Press space to get next prime.
(define (main n)
  (big-bang n
    (to-draw render-text)
    (on-key next-prime)))

;; Runs until it finds next prime number after n
(define (calculate-primes n)
  (cond [(prime? n) n]
        [else (calculate-primes (add1 n))]))

;; Check if number is prime. 0 and 1 are not prime.
(define (prime? n)
  (if (<= n 1) false
      (divisible? n 2)))

;; Checks if n is divisible by any d -> sqrt n.
(define (divisible? n d)
  (cond [(> d (sqrt n)) true]
        [else (if (= (remainder n d) 0)
                  false
                  (divisible? n (add1 d)))]))

;; Renders prime numbers as text
(define (render-text n)
  (place-image (text (number->string n) TEXT-SIZE TEXT-COLOR)
               TEXT-X
               TEXT-Y
               MTS))

(define (next-prime n ke)
  (cond [(key=? ke " ") (calculate-primes (add1 n))]
        [else n]))
