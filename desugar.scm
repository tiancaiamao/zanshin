(define my-macros '())

(define (desugar exp)
  (cond
   ((number? exp) exp)
   ((symbol? exp) exp)
   ((string? exp) exp)
   ((boolean? exp) exp)
   (else
    (if (symbol? (car exp))
	(case (car exp)
	  [(if) `(if ,(desugar (cadr exp))
		     ,(desugar (caddr exp))
		     ,(desugar (cadddr exp)))]
	  [(set!) `(set! ,(cadr exp) ,(desugar (caddr exp)))]
	  [(begin) `(begin ,@(map desugar (cdr exp)))]
	  [(lambda) `(lambda ,(cadr exp) ,@(map desugar (cddr exp)))]
	  (else
	   (let ([a (assq (car exp) my-macros)])
	     (if a
		 ((cdr a) exp desugar)
		 exp))))
	(map desugar exp)))))

(define (desugar-letrec exp desugar)
  (let ([vars (map car (cadr exp))]
	[vals (map desugar (map cadr (cadr exp)))]
	[body (map desugar (cddr exp))])
    (let ([sets (map (lambda (k v) `(set! ,k ,v)) vars vals)])
      `(let ,(map (lambda (x) (cons x '((##sandbox#void)))) vars)
	 ,@(append sets body)))))

(define (desugar-cond exp desugar)
  (let ([tests (cdr exp)])
    (if (null? tests)
	'(##sandbox#void)
	(let ([first (car tests)]
	      [rem (cdr tests)])
	  (if (eq? (car first) 'else)
	      (desugar (cadr first))	      	      
	      (let ([rem (cons 'cond rem)]
		    [test (desugar (car first))]
		    [succ (desugar (cadr first))])
		`(if ,test
		     ,succ
		     ,(desugar-cond rem desugar))))))))

(define (desugar-define exp desugar)
  (let ([head (cadr exp)]
	[body (cddr exp)])
    (if (pair? head)
	(let* ([body1 `(lambda ,(cdr head) ,@body)])
	  `(define ,(car head) ,(desugar body1)) )
	`(define ,head ,(desugar (car body))))))

(define (desugar-and exp desugar)
  (if (null? (cdr exp))
      #t
      (let ([rbody (cddr exp)]
	    [hbody (cadr exp)])
	(if (eq? rbody '())
	    (desugar hbody)
	    `(if ,(desugar hbody)
		 ,(desugar-and `(and ,@rbody) desugar)
		 #f)))))

(define (desugar-or exp desugar)
  (if (null? (cdr exp))
      #f
      (let ([hbody (cadr exp)]
	    [rbody (cddr exp)])
	(if (eq? rbody '())
	    (desugar hbody)
	    (let ((tmp (gensym)))
	      `(let ([,tmp ,(desugar hbody)])
		 (if ,tmp
		     ,tmp
		     ,(desugar-or `(or ,@rbody) desugar))))))))

(define (desugar-let* exp desugar)
  (let ([bindings (cadr exp)]
	[body (cddr exp)])
    (let expand ((bs bindings))
      (if (eq? bs '())
	  (cons 'begin (map desugar body))
	  `(let (,(car bs))
	     ,(expand (cdr bs)))))))  

(set! my-macros (cons (cons 'let* desugar-let*) my-macros))
(set! my-macros (cons (cons 'letrec desugar-letrec) my-macros))
(set! my-macros (cons (cons 'cond desugar-cond) my-macros))
(set! my-macros (cons (cons 'define desugar-define) my-macros))
(set! my-macros (cons (cons 'and desugar-and) my-macros))
(set! my-macros (cons (cons 'or desugar-or) my-macros))



#|
(define rewrite-cond
  (lambda (e*)
    (cond ((null? e*) (static-wrong "bad syntax in" "cond"))
          ((null? (cdr e*))
           (if (eq? (caar e*) 'else)
               (cadar e*)
               (list 'if (caar e*) (cadar e*) #f)))
          (else 
           (list 'if (caar e*) (cadar e*) (rewrite-cond (cdr e*)))))))

(define rewrite-let
  (lambda (e)
    (if (pair? (car e))
        (rewrite-let-normal (car e) (cadr e))
        (rewrite-let-loop (car e) (cadr e) (caddr e)))))

(define rewrite-let-normal
  (lambda (bind body)
    (let ((n* (map car bind))
          (e* (map cadr bind)))
      (cons (list 'lambda n*
                  body) e*))))

(define rewrite-let-loop
  (lambda (name bind body)
    (let ((n* (map car bind))
          (e* (map cadr bind)))
      (cons (list (cond
                    ((eq? (length n*) 1) 'Y1)
                    ((eq? (length n*) 2) 'Y2)
                    ((eq? (length n*) 3) 'Y3))
                  (list 'lambda (list name)
                        (list 'lambda n*
                              body)))
            e*))))

(define rewrite-let*
  (lambda (rbind body)
    (if (null? rbind)
        body
        (rewrite-let* (cdr rbind)
                      (list (list 'lambda (list (caar rbind)) body) (cadar rbind))))))
      
(define Y1
    (lambda (F)
      ((lambda (u) (u u))
       (lambda (x) (F (lambda (v) ((x x) v)))))))

(define Y2
    (lambda (F)
      ((lambda (u) (u u))
       (lambda (x) (F (lambda (v1 v2) ((x x) v1 v2)))))))

(define Y3
    (lambda (F)
      ((lambda (u) (u u))
       (lambda (x) (F (lambda (v1 v2 v3) ((x x) v1 v2 v3)))))))
|#
