(define cddr (lambda (ls) (cdr (cdr ls))))
(define caar (lambda (ls) (car (car ls))))
(define caddr (lambda (ls) (car (cdr (cdr ls)))))
(define cadr (lambda (ls) (car (cdr ls))))
(define cadar (lambda (ls) (car (cdr (car ls)))))

(define reverse2
  (lambda (ls ret)
    (if (null? ls)
        ret
        (reverse2 (cdr ls) (cons (car ls) ret)))))
         
(define reverse
  (lambda (ls)
    (reverse2 ls '())))

(define filter
  (lambda (fn ls)
    (if (null? ls)
        '()
        (let ((v (car ls)))
          (if (fn v)
              (cons v (filter fn (cdr ls)))
              (filter fn (cdr ls)))))))

(define map
  (lambda (fn ls)
    (if (null? ls)
        '()
        (cons 
         (fn (car ls)) 
         (map fn (cdr ls))))))

(define memq
  (lambda (s ls)
    (cond
      ((null? ls) #f)
      ((eq? (car ls) s) ls)
      (else
       (memq s (cdr ls))))))

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (length (cdr ls))))))


(define list
  (lambda (a . b)
    (cons a b)))

(define myappend1
  (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (myappend1 (cdr a) b)))))

(define myappend2
  (lambda (a b)
    (cond
      ((null? b) a)
      ((null? (cdr b)) 
       (myappend1 a (car b)))
      (else
       (myappend1 a (myappend2 (car b) (cdr b)))))))

(define append
  (lambda (a . b)
    (myappend2 a b)))

(define atom?
  (lambda (v)
    (not (pair? v))))

#|
(define assq
  (lambda (s ls)
    (cond
      ((null? ls) #f)
      ((eq? (caar ls) s) ls)
      (else
       (assq s (cdr ls))))))
|#

