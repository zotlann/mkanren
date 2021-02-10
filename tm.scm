(load "mk.scm")

;symbol which represents blank symbols on the tape
(define *blank-symbol* #\#)

;returns a pair with the car being the list with the last 
;element removed and the cdr being the removed element
(define (remove-lasto lst out)
  (define (helper lst acc out)
    (fresh (head tail new-acc)
      (== `(,head . ,tail) lst)
      (conde
        ((== tail '())(== out `(,acc . ,head)))
        ((appendo acc `(,head) new-acc)(helper tail new-acc out)))))
  (helper lst '() out))

(define (appendo l1 l2 out)
  (conde
    ((== l1 '()) (== l2 out))
    ((fresh (head tail res)
      (== l1 `(,head . ,tail))
      (appendo tail l2 res)
      (== `(,head . ,res) out)))))

(run 1 (q) (remove-lasto '(1 2 3) q))
      
(define make-tape
  (lambda (str)
    (define left (list *blank-symbol*))
    (define head (car (string->list str)))
    (define right (append (cdr (string->list str)) (list *blank-symbol*)))
    (lambda (method . args)
      (cond
       [(eq? method 'read) head]
       [(eq? method 'write) (set! head (car args))]
       [(eq? method 'move-right) (begin (set! left (append left (list head)))
                                        (set! head (car right))
                                        (if (null? (cdr right))
                                            (set! right (list *blank-symbol*))
                                            (set! right (cdr right))))]
       [(eq? method 'move-left) (let* ((x (remove-last left))
                                       (new-left (car x))
                                       (new-head (cdr x))
                                       (new-right (append (list head) right)))
                                  (set! head new-head)
                                  (set! right new-right)
                                  (if (null? new-left)
                                      (set! left (list *blank-symbol*))
                                      (set! left new-left)))]
       [(eq? method 'get-string)
	(if *remove-blank-symbol?*
	    (string-append (list->string (remove *blank-symbol* left))
			   (list->string (remove *blank-symbol* (list head))) 
			   (list->string (remove *blank-symbol* right)))
	    (string-append (list->string left) 
			   (list->string (list head)) 
			   (list->string right)))]))))

;Bindings for tape methods
(define read-tape
  (lambda (tape)
    (tape 'read)))

(define write-tape
  (lambda (tape char)
    (tape 'write char)))

(define move-left
  (lambda (tape)
    (tape 'move-left)))

(define move-right
  (lambda (tape)
    (tape 'move-right)))

(define get-tape-string
  (lambda (tape)
    (tape 'get-string)))

;constructs a tm with initial state q0, final-state qf
;and with a list of transitions tr
;q0 and qf should be quoted symbols that match the initial and final states
;respectively
;tr is a list of transitions in the form
;(current-state read-symbol write-symbol direction resulting-state)
;direction is either 'L for left or 'R for right
;eg: (q0 #\A #\B R q0)
;for a state that replaces A with B and moves to the right
;What is returned is a function that takes a string as input, and 
;outputs the result for applying the tm to the string
;Because the output is a function that takes a string, turing machines
;can be combined through the composite function to feed a string through
;multiple turing machines
(define make-tm
  (lambda (q0 qf tr)
    (define initial-state q0)
    (define final-state qf)
    (define transitions (format-transitions tr))
    (define get-transition
      (lambda (tape current-state)
	(let* ((read-symbol (read-tape tape))
	       (write-dir-resulting (assoc (cons current-state read-symbol)
					   transitions)))
	  (if write-dir-resulting
	      (set! write-dir-resulting (cdr write-dir-resulting)))
	  (if write-dir-resulting
	      (lambda (tape)
		(write-tape tape (get-write-symbol write-dir-resulting))
		(if (eq? 'L (get-direction write-dir-resulting))
		    (move-left tape)
		    (move-right tape))
		(get-resulting-state write-dir-resulting))
	      #f))))
    (lambda (str)
      (define helper
	(lambda (tape current-state)
	  (cond
	    [(eq? final-state current-state) (get-tape-string tape)]
	    (else (let ((transition (get-transition tape current-state)))
		    (if transition
			(helper tape (transition tape))
			#f))))))
      (helper (make-tape str) initial-state))))

;given the triplet (write-symbol direction resulting-state
;return the write-symbol
(define get-write-symbol
  (lambda (triplet)
    (car triplet)))

;given the triplet (write-symbol direction resulting-state) 
;return the direction
(define get-direction
  (lambda (triplet)
    (cadr triplet)))

;given the triplet (write-symbol direction resulting state)
;return the resulting state
(define get-resulting-state
  (lambda (triplet)
    (caddr triplet)))

;flattens a list, used when printing transitions for a tm
(define flatten
  (lambda (lst)
    (cond 
      [(null? lst) '()]
      [(pair? lst) (append (flatten (car lst)) (flatten (cdr lst)))]
      (else (list lst)))))

;returns a list equal to lst with all instances of tar removed
(define remove
  (lambda (tar lst)
    (cond
      [(null? lst) '()]
      [(equal? (car lst) tar)(remove tar (cdr lst))]
      (else (cons (car lst) (remove tar (cdr lst)))))))

;takes a transition function in the form:
;'(current state read-symbol write-symbol direciton resulting-state)
;and returns a transition function in the form:
;'(current-state . read-symbol (write-symbol direction resulting-state))
;used to create correctly formated transition functions to pass into make-tm
(define format-transition
  (lambda (lst)
    (cons (cons (car lst) (cadr lst)) 
	  (list (car (cddr lst))
		(car (cdddr lst))
		(car (cddddr lst))))))

;takes a list of transitions and formats them as alists for use in a tm
(define format-transitions
  (lambda (trans-lst)
    (map format-transition trans-lst)))


;returns the composite of two functions, used to combine tms
(define composite
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

|#
