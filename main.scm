(use srfi-4)
;;;--------------------initialize--------------------------
(define r.init '())
(define sg.current (make-vector 100))
(define sg.init (make-vector 100))
(define g.current '())
(define g.init '())
(define desc.init '())

(define (primitive? x)
  (member x '(cons car cdr pair? symbol? atom? eq? '+ '- '* '/ '= '< '> '>= '<= 'null? 'not 'make-vector 'vector-set! 'vector-ref)))

(load "lib.scm")

(define description-extend! 
  (lambda (name description)
    (set! desc.init 
          (cons (cons name description) desc.init))))

(define g.init-extend! 
  (lambda (n)
    (let ((level (length g.init)))
      (set! g.init
            (cons (cons n (cons 'predefined level)) g.init))
      level )))

(define defprimitive
  (lambda (name value num)
    (begin
      (g.init-extend! name)
      (description-extend! name (list 'function name num)))))
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Describe a predefined value.
;;; The description language only represents primitives with their arity:
;;;          (FUNCTION address . variables-list)
;;; with variables-list := () | (a) | (a b) | (a b c)
;;; Only the structure of the VARIABLES-LIST is interesting (not the
;;; names of the variables). ADDRESS is the address of the primitive
;;; to use when inlining an invokation to it. This address is
;;; represented by a Scheme procedure.
(defprimitive 'cons cons 2)
(defprimitive 'car car 1)
(defprimitive 'cdr cdr 1)
(defprimitive 'pair? pair? 1)
(defprimitive 'symbol? symbol? 1)
(defprimitive 'eq? eq? 2)
;;(defprimitive set-car! set-car! 2)
;;(defprimitive set-cdr! set-cdr! 2)
(defprimitive '+ + 2)
(defprimitive '- - 2)
(defprimitive '= = 2)
(defprimitive '< < 2)
(defprimitive '> > 2)
(defprimitive '* * 2)
(defprimitive '<= <= 2)
(defprimitive '>= >= 2)
(defprimitive 'remainder remainder 2)
(defprimitive 'display display 1)
(defprimitive 'read read 0)
(defprimitive 'primitive? primitive? 1)
(defprimitive 'continuation? continuation? 1)
(defprimitive 'null? null? 1)
(defprimitive 'newline newline 0)
(defprimitive 'eof-object? eof-object? 1)
(defprimitive 'make-vector make-vector 1)
(defprimitive 'vector-set! vector-set! 3)
(defprimitive 'vector-ref vector-ref 2)
(defprimitive 'not not 1)
(defprimitive 'atom? atom? 1)

(load "desugar.scm")
(load "meaning.scm")

(define cc
  (lambda (e)
    (set! *defined* '())
    (set! g.current '())
    (let* ((e1 (desugar e))
	   (result (meaning e1 '() #t)))
      (if (null? *defined*)
	  (list->u8vector (append result (FINISH)))
	  (static-wrong "undefined" *defined*)))))

(define compile
  (lambda (e)
    (let ((code (cc e)))
      (if (u8vector? code)
	  (cons code 
		(map (lambda (x)
		       (cons (car x) (cddr x))) g.current))
	  code))))
