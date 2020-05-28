(load "mk.scm")
(load "numbers.scm")

(define (mult a b)
  (cond
    ((zero? a) 0)
    ((zero? b) 0)
    (else (+ a (mult a (- b 1))))))

(define (multo a b c)
  (conde
    ((zeroo a) (== c 0))
    ((zeroo b) (== c 0))
    ((== '(1) b) (== c a))
    ((fresh (res b-1 d)
       (minuso b (build-num 1) b-1)
       (multo a b-1 res)
       (pluso a res c)))))

(define (divo a b c)
  (multo b c a))

(define (squareo a o)
  (multo a a o))

(run 1 (q) (== q 5) (== q 6))

     
