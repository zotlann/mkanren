(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))])))

;;Our representation of the tape will be a triplet
;;The car is the left tape, the cadr is the character on the r/w head
;;and the cddr is the right tape
(define (make-tape left current right) (cons left (cons current right)))
(define (make-tapeo left current right tape) (== tape `(,left ,current ,right)))
(define (left tape) (car tape))
(define (lefto tape out) (fresh (left current right) (== tape `(,left ,current ,right)) (== left out)))
(define (current tape) (cadr tape))
(define (currento tape out) (fresh (left current right) (== tape `(,left ,current ,right)) (== current out)))
(define (right tape) (cddr tape))
(define (righto tape out) (fresh (left current right) (== tape `(,left ,current ,right)) (== right out)))


(define remove-last
  (lambda (lst)
    (define helper
      (lambda (lst so-far)
        (cond
         [(null? (cdr lst))(cons so-far (car lst))]
         (else (helper (cdr lst) (append so-far (list (car lst))))))))
    (helper lst '())))

(define (remove-lasto lst out)
  (define (helpero lst so-far out)
    (fresh (head tail recur new-so-far)
      (== lst `(,head . ,tail))
      (conde
        ((== tail '()) (== out `(,so-far . ,head)))
        ((appendo so-far `(,head) new-so-far)
         (helpero tail new-so-far out)))))
  (helpero lst '() out))

(define (move-left tape)
  (let* ((s (remove-last (left tape)))
         (new-left (car s))
         (new-current (cdr s)))
    (if (null? new-left)
        (make-tape (list #\#) new-current (append (list (current tape)) (right tape)))
        (make-tape new-left new-current (append (list (current tape)) (right tape))))))

(define (move-lefto tape out)
  (fresh (lefto-val righto-val new-left new-current current-val new-right remove-lasto-result head tail)
    (lefto tape lefto-val)
    (remove-lasto lefto-val remove-lasto-result)
    (== remove-lasto-result `(,new-left . ,new-current))
    (righto tape righto-val)
    (currento tape current-val)
    (appendo `(,current-val) righto-val new-right)
    (conde
      ((== new-left '()) (make-tapeo (list #\#) new-current new-right out))
      ((== new-left `(,head . ,tail)) (make-tapeo new-left new-current new-right out)))))

(define (move-right tape)
  (let ((new-left (append (left tape) (list (current tape))))
        (new-current (car (right tape)))
        (new-right (cdr (right tape))))
    (if (null? new-right)
        (make-tape new-left new-current (list #\#))
        (make-tape new-left new-current new-right))))

(define (move-righto tape out)
  (fresh (lefto-val righto-val currento-val new-left new-right new-current newest-righto head tail)
    (lefto tape lefto-val)
    (righto tape righto-val)
    (currento tape currento-val)
    (appendo lefto-val `(,currento-val) new-left)
    (== `(,new-current . ,new-right) righto-val)
    (conde
      ((== new-right '()) (make-tapeo new-left new-current (list #\#) out))
      ((== new-right `(,head . ,tail)) (make-tapeo new-left new-current new-right out)))))

(define (read tape) (current tape))
(define (reado tape out) (currento tape out))

(define (write tape char) (make-tape (left tape) char (right tape)))
(define (writeo tape char out)
  (fresh (left-val right-val)
    (lefto tape left-val)
    (righto tape right-val)
    (make-tapeo left-val char right-val out)))

;;Our turing machine is represented of a quad
;;The car is the initial state, the cadr is the terminating state
;;the caddr is the list of state transitions
;;the cdddr is the current state
;;A transition is a list in the form
;;'(current-state read-symbol write-symbol direction resulting-state)
;;eg; (q0 #\A #\B R q0)
;;is a transition function that when in state q0, and the read-write head is on #\A, it 
;;will write #\B and move to the right, and then move to state q0
(define (make-tm initial final transitions current) (cons initial (cons final transitions current)))
(define (make-tmo initial final transitions current out) (== out `(,initial ,final ,transitions ,current)))

(define (initial-state tm) (car tm))
(define (initial-stateo tm out) (fresh (initial final transitions current) (== tm `(,initial ,final ,transitions ,current)) (== initial out)))

(define (final-state tm) (cadr tm))
(define (final-stateo tm out) (fresh (initial final transitions current) (== tm `(,initial ,final ,transitions ,current)) (== final out)))

(define (transitions tm) (cddr tm))
(define (transitionso tm out) (fresh (initial final transitions current) (== tm `(,initial ,final ,transitions ,current)) (== transitions out)))

(define (current-state tm) (cdddr tm))
(define (current-stateo tm out)
(fresh (initial final transitions current) (== tm `(,initial ,final ,transitions ,current)) (== current out)))

(define (make-transition q0 read write direction resulting-state)
  (list q0 read write direction resulting-state))
(define (make-transitiono q0 read write direction resulting-state out)
  (== out `(,q0 ,read ,write ,direction ,resulting-state)))

(define (tr-initial-state transition) (car transition))
(define (tr-read transition) (cadr transition))
(define (tr-write transition) (caddr transition))
(define (tr-direction transition) (cadddr transition))
(define (tr-resulting transition) (car (cddddr transition)))

(define (tr-initial-stateo transition out)
  (fresh (initial read write direction resulting)
    (== transition `(,initial ,read ,write ,direction ,resulting))
    (== out initial)))
(define (tr-reado transition out)
  (fresh (initial read write direction resulting)
    (== transition `(,initial ,read ,write ,direction ,resulting))
    (== out read)))
(define (tr-writeo transition out)
  (fresh (initial read write direction resulting)
    (== transition `(,initial ,read ,write ,direction ,resulting))
    (== out write)))
(define (tr-directiono transition out)
  (fresh (initial read write direction resulting)
    (== transition `(,initial ,read ,write ,direction ,resulting))
    (== out direction)))
(define (tr-resultingo transition out)
  (fresh (initial read write direction resulting)
    (== transition `(,initial ,read ,write ,direction ,resulting))
    (== out resulting)))

(define (apply-transition tm transition tape)
  (let* ((resulting-state (tr-resulting transition))
        (write-char (tr-write transition))
        (resulting-tm (make-tm (initial-state tm) (final-state tm) (transitions tm) resulting-state))
        (direction (tr-direction transition)))
    (if (eq? 'L direction)
        (cons resulting-tm (move-left (write tape write-char)))
        (cons resulting-tm (move-right (write tape write-char))))))

(define (apply-transitiono tm transition tape out)
  (fresh (resulting-state write-char direction resulting-tm tm-initial tm-final tm-transitions written-tape moved-tape)
    (tr-resultingo transition resulting-state)
    (tr-writeo transition write-char)
    (initial-stateo tm tm-initial)
    (final-stateo tm tm-final)
    (transitionso tm tm-transitions)
    (make-tmo tm-initial tm-final tm-transitions resulting-state resulting-tm)
    (tr-directiono transition direction)
    (writeo tape write-char written-tape)
    (conde
      ((== 'L direction) (move-lefto written-tape moved-tape) (== out `(,resulting-tm . ,moved-tape)))
      ((== 'R direction) (move-righto written-tape moved-tape) (== out `(,resulting-tm . ,moved-tape))))))

(define (run-tm tm tape)
  (let ((current-st (current-state tm)) (current-char (current tape)) (tm-transitions (transitions tm)))
    (if (eq? current-st (final-state tm))
        tape
        (let* ((transition (find-transition current-st current-char tm-transitions))
               (result (apply-transition tm transition tape))
               (resulting-tm (car result))
               (resulting-tape (cdr result)))
           (run-tm resulting-tm resulting-tape)))))

(define (run-tmo tm tape out)
  (fresh (current-st current-char tm-transitions tm-final good-transition)
    (current-stateo tm current-st)
    (currento tape current-char)
    (transitionso tm tm-transitions)
    (final-stateo tm tm-final)
    (conde
      ((== current-st tm-final) (== out tape))
      ((fresh (result resulting-tm resulting-tape recur)
         (find-transitiono current-st current-char tm-transitions good-transition)
         (apply-transitiono tm good-transition tape result)
         (== result `(,resulting-tm . ,resulting-tape))
         (run-tmo resulting-tm resulting-tape out))))))

(define (find-transition x y z) #f)

(define (find-transitiono state char trs out)
  (conde
    ((== trs '()) (== #f #t))
    ((fresh (tr rest tr-initial tr-char)
     (== trs `(,tr . ,rest))
     (tr-initial-stateo tr tr-initial)
     (tr-reado tr tr-char)
     (conde
       ((== tr-char char) (== tr-initial state) (== out tr))
       ((find-transitiono state char rest out)))))))

(define (flatten-tapeo tape out)
  (fresh (left right current left-current)
    (lefto tape left)
    (righto tape right)
    (currento tape current)
    (appendo left (list current) left-current)
    (appendo left-current right out)))



(define inc-trans
 '(
   #|go to rightmost|#
   (q0 #\0 #\0 R q0)
   (q0 #\1 #\1 R q0)
   (q0 #\# #\# L q1)
   #|add 1 |#
   (q1 #\0 #\1 L qf)
   (q1 #\1 #\0 L q1)
   (q1 #\# #\1 R qf)))

(define test-trans
  '(
     (q0 #\1 #\0 R q1)
     (q2 #\0 #\1 L q2)
     (q1 #\0 #\# L q2)
     (q2 #\# #\0 R qf)))
     
(run 1 (q r) (fresh (tape)
               (make-tapeo '(#\# #\0) #\1 '(#\0 #\#) tape)
               (lefto tape q)
               (h '(0 1 2))))

