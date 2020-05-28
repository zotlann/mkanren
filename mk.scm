;;representation of variables is a vector with the variable index
(define (var c) (vector c))
(define (var? c) (vector? c))
(define (var=? x y) (eqv? (vector-ref x 0) (vector-ref y 0)))

;;the state(list of variable bindings) is an assoc list of (var . val) pairs.
(define (ext-s var val s)
  `((,var . ,val) . ,s))
(define empty-state '(() . 0))
(define (walk u s)
  (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
   (if pr (walk (cdr pr) s) u)))

;;== returns a goal that succeeds if the terms passed to it unify
(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
     (if s (unit `(,s . ,(cdr s/c))) mzero))))
(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
   (cond
     ((and (var? u) (var? v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
       (and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s)))))

;;call/fresh
(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
     ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

;;disj/conj
(define (disj g1 g2)
  (lambda (s/c) 
    (mplus (g1 s/c) (g2 s/c))))

(define (conj g1 g2)
  (lambda (s/c)
    (bind (g1 s/c) g2)))

;;mplus/bind
(define (mplus $1 $2)
  (cond
    ((null? $1) $2)
    ((procedure? $1) (lambda () (mplus $2 ($1))))
    (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
    ((null? $) mzero)
    ((procedure? $) (lambda () (bind ($) g)))
    (else (mplus (g (car $)) (bind (cdr $) g)))))

(define (fives x)
  (disj (== x 5) (lambda (s/c) (lambda () ((fives x) s/c)))))

(define (sixes x)
  (disj (== x 6) (lambda (s/c) (lambda () ((sixes x) s/c)))))

(define fives-and-sixes
  (call/fresh (lambda (x) (disj (fives x) (sixes x)))))

;;some user-level macros for usability
;;inverse-n-delay macro
(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

;;conj+ and disj+
(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

;;conde and fresh
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))


(define (pull $) (if (procedure? $) (pull ($)) $))

(define (take-all $)
  (let (($ (pull $)))
   (if (null? $) '() (cons (car $) (take-all (cdr $))))))


(define (take n $)
  (if (zero? n) '()
      (let (($ (pull $)))
       (cond
         ((null? $) '())
         (else (cons (car $) (take (- n 1) (cdr $))))))))


;;reification
(define (mK-reify n s/c*)
  (if (<= n 0)
      (map (lambda (s/c) (reify-state/nth-var 0 s/c)) s/c*)
      (reify-n-variables n s/c*)))

(define (reify-n-variables n s/c*)
  (reify-n-variables-helper 0 n s/c*))

(define (reify-n-variables-helper c n s/c*)
  (cond
    [(= c n) (map (lambda (s/c) (reify-state/nth-var c s/c)) s/c*)]
    (else (map list (map (lambda (s/c) (reify-state/nth-var c s/c)) s/c*) (reify-n-variables-helper (+ c 1) n s/c*)))))


(define (reify-state/nth-var n s/c)
  (let ((v (walk* (var n) (car s/c))))
   (walk* v (reify-s v '()))))

(define (reify-s v s)
  (let ((v (walk v s)))
   (cond
     ((var? v)
      (let ((n (reify-name (length s))))
       (cons `(,v . ,n) s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(define (reify-name n)
  (string->symbol (string-append "_" "." (number->string n))))

(define (walk* v s)
  (let ((v (walk v s)))
   (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
     (else v))))


(define (call/empty-state g) (g empty-state))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (mK-reify (- (length (quote (x ...))) 1) (take n (call/empty-state
                         (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (mK-reify (- (length (quote (x ...))) 1) (take-all (call/empty-state
                           (fresh (x ...) g0 g ...)))))))

