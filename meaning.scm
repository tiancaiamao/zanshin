;;-----------------------compiler---------------------------
(define meaning
  (lambda (e r tail?)
    (if (atom? e)
	(if (symbol? e)
	    (meaning-reference e r tail?)
	    (meaning-quotation e r tail?))
	(case (car e)
	  ('quote (meaning-quotation (cadr e) r tail?))
	  ('if (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
	  ('begin (meaning-sequence (cdr e) r tail?))
	  ('set! (meaning-assignment (cadr e) (caddr e) r tail?))
	  ('lambda (meaning-abstraction (cadr e) (cddr e) r tail?))
	  ('define (meaning-define (cadr e) (caddr e) r tail?))
	  (else (meaning-application (car e) (cdr e) r tail?))))))
(define meaning-reference
  (lambda (n r tail?)
    (let ((kind (compute-kind r n)))
      (if kind
	  (cond ((eq? (car kind) 'local)
		 (let ((i (cadr kind)) (j (cddr kind)))
		   (if (= i 0)
		       ((SHALLOW-ARGUMENT-REF) j)
		       ((DEEP-ARGUMENT-REF) i j))))
		((eq? (car kind) 'global)
		 (let ((i (cdr kind))) ((CHECKED-GLOBAL-REF) i)))
		((eq? (car kind) 'predefined)
		 (let ((i (cdr kind))) ((PREDEFINED) i))))
	  ((CHECKED-GLOBAL-REF) (adjoint n))))))
(define meaning-quotation (lambda (v r tail?) ((CONSTANT) v)))
(define meaning-alternative
  (lambda (e1 e2 e3 r tail?)
    (let ((m1 (meaning e1 r #f))
	  (m2 (meaning e2 r tail?))
	  (m3 (meaning e3 r tail?)))
      (ALTERNATIVE m1 m2 m3))))
(define meaning-assignment
  (lambda (n e r tail?)
    (let ((m (meaning e r #f)) (kind (compute-kind r n)))
      (if kind
	  (cond ((eq? (car kind) 'local)
		 (let ((i (cadr kind)) (j (cddr kind)))
		   (if (= i 0)
		       (SHALLOW-ARGUMENT-SET! j m)
		       (DEEP-ARGUMENT-SET! i j m))))
		((eq? (car kind) 'global)
		 (let ((i (cdr kind))) (GLOBAL-SET! i m)))
		((eq? (car kind) 'predefined)
		 (static-wrong "Immutable predefined variable" n)))
	  (static-wrong "No such variable" n)))))
(define meaning-define
  (lambda (n e r tail?)
    (if (global-variable? g.current n)
	(if (memq n *defined*)
	    (begin
	      (set! *defined* (filter (lambda (v) (not (eq? v n))) *defined*))
	      (meaning-assignment n e r tail?))
	    (static-wrong "Cannot redefine variable" n))
	(begin (g.current-extend! n) (meaning-assignment n e r tail?)))))
(define meaning-sequence
  (lambda (e+ r tail?)
    (if (pair? e+)
	(if (pair? (cdr e+))
	    (meaning*-multiple-sequence (car e+) (cdr e+) r tail?)
	    (meaning*-single-sequence (car e+) r tail?))
	(static-wrong "Illegal syntax: (begin)"))))
(define meaning*-single-sequence (lambda (e r tail?) (meaning e r tail?)))
(define meaning*-multiple-sequence
  (lambda (e e+ r tail?)
    ((lambda (m1 m+) (SEQUENCE m1 m+))
     (meaning e r #f)
     (meaning-sequence e+ r tail?))))
(define meaning-abstraction
  (lambda (nn* e+ r tail?)
    (let parse ((n* nn*) (regular '()))
      (cond ((pair? n*) (parse (cdr n*) (cons (car n*) regular)))
	    ((null? n*) (meaning-fix-abstraction nn* e+ r tail?))
	    (else
	     (meaning-dotted-abstraction (reverse regular) n* e+ r tail?))))))
(define meaning-fix-abstraction
  (lambda (n* e+ r tail?)
    (let* ((arity (length n*))
	   (r2 (r-extend* r n*))
	   (m+ (meaning-sequence e+ r2 #t)))
      (FIX-CLOSURE m+ arity))))
(define meaning-dotted-abstraction
  (lambda (n* n e+ r tail?)
    (let* ((arity (length n*))
	   (r2 (r-extend* r (append n* (list n))))
	   (m+ (meaning-sequence e+ r2 #t)))
      (NARY-CLOSURE m+ arity))))
(define meaning-application
  (lambda (e e* r tail?)
    (cond ((and (symbol? e)
		(let ((kind (compute-kind r e)))
		  (and (pair? kind)
		       (eq? 'predefined (car kind))
		       (let ((desc (get-description e)))
			 (and desc
			      (eq? 'function (car desc))
			      (or (= (caddr desc) (length e*))
				  (static-wrong "Incorrect arity for primitive" e)))))))
	   (meaning-primitive-application e e* r tail?))
	  ((and (pair? e) (eq? 'lambda (car e)))
	   (meaning-closed-application e e* r tail?))
	  (else (meaning-regular-application e e* r tail?)))))
(define meaning-closed-application
  (lambda (e ee* r tail?)
    (let ((nn* (cadr e)))
      (let parse ((n* nn*) (e* ee*) (regular '()))
	(cond ((pair? n*)
	       (if (pair? e*)
		   (parse (cdr n*) (cdr e*) (cons (car n*) regular))
		   (static-wrong "Too less arguments" e)))
	      ((null? n*)
	       (if (null? e*)
		   (meaning-fix-closed-application nn* (cddr e) ee* r tail?)
		   (static-wrong "Too much arguments" e ee*)))
	      (else
	       (meaning-dotted-closed-application
		(reverse regular)
		n*
		(cddr e)
		ee*
		r
		tail?)))))))
(define meaning-fix-closed-application
  (lambda (n* body e* r tail?)
    (let* ((m* (meaning* e* r (length e*) #f))
	   (r2 (r-extend* r n*))
	   (m+ (meaning-sequence body r2 tail?)))
      (if tail? (TR-FIX-LET m* m+) (FIX-LET m* m+)))))
(define meaning-dotted-closed-application
  (lambda (n* n body e* r tail?)
    (let* ((m* (meaning-dotted* e* r (length e*) (length n*) #f))
	   (r2 (r-extend* r (append n* (list n))))
	   (m+ (meaning-sequence body r2 tail?)))
      (if tail? (TR-FIX-LET m* m+) (FIX-LET m* m+)))))
(define meaning-primitive-application
  (lambda (e e* r tail?)
    (let* ((desc (get-description e))
	   (address (cadr desc))
	   (size (length e*)))
      (cond ((eq? size 0) ((CALL0) address))
	    ((eq? size 1)
	     (let ((m1 (meaning (car e*) r #f))) (CALL1 address m1)))
	    ((eq? size 2)
	     (let ((m1 (meaning (car e*) r #f)) (m2 (meaning (cadr e*) r #f)))
	       (CALL2 address m1 m2)))
	    ((eq? size 3)
	     (let ((m1 (meaning (car e*) r #f))
		   (m2 (meaning (cadr e*) r #f))
		   (m3 (meaning (caddr e*) r #f)))
	       (CALL3 address m1 m2 m3)))
	    (else (meaning-regular-application e e* r tail?))))))
(define meaning-regular-application
  (lambda (e e* r tail?)
    (let* ((m (meaning e r #f)) (m* (meaning* e* r (length e*) #f)))
      (if tail? (TR-REGULAR-CALL m m*) (REGULAR-CALL m m*)))))
(define meaning*
  (lambda (e* r size tail?)
    (if (pair? e*)
	(meaning-some-arguments (car e*) (cdr e*) r size tail?)
	(meaning-no-argument r size tail?))))
(define meaning-dotted*
  (lambda (e* r size arity tail?)
    (if (pair? e*)
	(meaning-some-dotted-arguments (car e*) (cdr e*) r size arity tail?)
	(meaning-no-dotted-argument r size arity tail?))))
(define meaning-some-arguments
  (lambda (e e* r size tail?)
    (let ((m (meaning e r #f))
	  (m* (meaning* e* r size tail?))
	  (rank (- size (+ (length e*) 1))))
      (STORE-ARGUMENT m m* rank))))
(define meaning-some-dotted-arguments
  (lambda (e e* r size arity tail?)
    (let ((m (meaning e r #f))
	  (m* (meaning-dotted* e* r size arity tail?))
	  (rank (- size (+ (length e*) 1))))
      (if (< rank arity)
	  (STORE-ARGUMENT m m* rank)
	  (CONS-ARGUMENT m m* arity)))))
(define meaning-no-argument (lambda (r size tail?) ((ALLOCATE-FRAME) size)))
(define meaning-no-dotted-argument
  (lambda (r size arity tail?) ((ALLOCATE-DOTTED-FRAME) arity)))


;;;------------------------utility-----------------------------------
(define static-wrong (lambda (msg v) (display msg) (display v) (newline)))
(define compute-kind
  (lambda (r n)
    (or (local-variable? r 0 n)
	(global-variable? g.current n)
	(global-variable? g.init n))))
(define *defined* '())
(define adjoint
  (lambda (n) (set! *defined* (cons n *defined*)) (g.current-extend! n)))
(define r-extend* (lambda (r n*) (cons n* r)))
(define local-variable?
  (lambda (r i n)
    (and (pair? r)
	 (let scan ((names (car r)) (j 0))
	   (cond ((pair? names)
		  (if (eq? n (car names))
		      (cons 'local (cons i j))
		      (scan (cdr names) (+ 1 j))))
		 ((null? names) (local-variable? (cdr r) (+ i 1) n))
		 ((eq? n names) (cons 'local (cons i j))))))))
(define g.current-extend!
  (lambda (n)
    (let ((level (length g.current)))
      (set! g.current (cons (cons n (cons 'global level)) g.current))
      level)))
(define global-variable?
  (lambda (g n) (let ((var (assq n g))) (and (pair? var) (cdr var)))))
(define global-fetch (lambda (i) (vector-ref sg.current i)))
(define global-update! (lambda (i v) (vector-set! sg.current i v)))
(define get-description
  (lambda (name) (let ((p (assq name desc.init))) (and (pair? p) (cdr p)))))
(define ALTERNATIVE
  (lambda (m1 m2 m3)
    (let ((mm2 (append m2 ((GOTO) (length m3)))))
      (append m1 ((JUMP-FALSE) (length mm2)) mm2 m3))))
(define SHALLOW-ARGUMENT-SET!
  (lambda (j m) (append m ((SET-SHALLOW-ARGUMENT!) j))))
(define DEEP-ARGUMENT-SET!
  (lambda (i j m) (append m ((SET-DEEP-ARGUMENT!) i j))))
(define GLOBAL-SET! (lambda (i m) (append m ((SET-GLOBAL!) i))))
(define SEQUENCE (lambda (m m+) (append m m+)))
(define FIX-CLOSURE
  (lambda (m+ arity)
    (let* ((the-function
	    (append ((ARITY=?) arity) ((EXTEND-ENV)) m+ ((RETURN))))
	   (the-goto ((GOTO) (length the-function))))
      (append ((CREATE-CLOSURE) (length the-goto)) the-goto the-function))))
(define TR-FIX-LET (lambda (m* m+) (append m* ((EXTEND-ENV)) m+)))
(define FIX-LET (lambda (m* m+) (append m* ((EXTEND-ENV)) m+ ((UNLINK-ENV)))))
(define CALL1 (lambda (address m1) (append m1 ((INVOKE1) address))))
(define CALL2
  (lambda (address m1 m2)
    (append m1 ((PUSH-VALUE)) m2 ((POP-ARG1)) ((INVOKE2) address))))
(define CALL3
  (lambda (address m1 m2 m3)
    (append
     m1
     ((PUSH-VALUE))
     m2
     ((PUSH-VALUE))
     m3
     ((POP-ARG2))
     ((POP-ARG1))
     ((INVOKE3) address))))
(define TR-REGULAR-CALL
  (lambda (m m*)
    (append m ((PUSH-VALUE)) m* ((POP-FUNCTION)) ((FUNCTION-INVOKE)))))
(define REGULAR-CALL
  (lambda (m m*)
    (append
     m
     ((PUSH-VALUE))
     m*
     ((POP-FUNCTION))
     ((PRESERVE-ENV))
     ((FUNCTION-INVOKE))
     ((RESTORE-ENV)))))
(define NARY-CLOSURE
  (lambda (m+ arity)
    (let* ((the-function
	    (append
	     ((ARITY>=?) (+ arity 1))
	     ((PACK-FRAME!) arity)
	     ((EXTEND-ENV))
	     m+
	     ((RETURN))))
	   (the-goto ((GOTO) (length the-function))))
      (append ((CREATE-CLOSURE) (length the-goto)) the-goto the-function))))
(define STORE-ARGUMENT
  (lambda (m m* rank) (append m ((PUSH-VALUE)) m* ((POP-FRAME!) rank))))
(define CONS-ARGUMENT
  (lambda (m m* arity)
    (append m ((PUSH-VALUE)) m* ((POP-CONS-FRAME!) arity))))

;;;---------------------interface---------------------
(define SHALLOW-ARGUMENT-REF (make-parameter (lambda (j) (list 'SHALLOW-ARGUMENT-REF j))))
(define DEEP-ARGUMENT-REF (make-parameter (lambda (i j) (list 'DEEP-ARGUMENT-REF i j))))
(define SET-DEEP-ARGUMENT! (make-parameter (lambda (i j) (list 'SET-DEEP-ARGUMENT! i j))))
(define CHECKED-GLOBAL-REF (make-parameter (lambda (i) (list 'CHECKED-GLOBAL-REF i))))
(define CONSTANT (make-parameter (lambda (v) (list 'CONSTANT v))))
(define GOTO (make-parameter (lambda (offset) (list 'GOTO offset))))
(define UNLINK-ENV (make-parameter (lambda () (list 'UNLINK-ENV))))
(define INVOKE1 (make-parameter (lambda (address) (list 'INVOKE1 address))))
(define POP-ARG1 (make-parameter (lambda () (list 'POP-ARG1))))
(define POP-ARG2 (make-parameter (lambda () (list 'POP-ARG2))))
(define CREATE-CLOSURE (make-parameter (lambda (offset) (list 'CREATE-CLOSURE offset))))
(define RETURN (make-parameter (lambda () (list 'RETURN))))
(define ARITY>=? (make-parameter (lambda (arity+1) (list 'ARITY>=? arity+1))))
(define FUNCTION-INVOKE (make-parameter (lambda () (list 'FUNCTION-INVOKE))))
(define RESTORE-ENV (make-parameter (lambda () (list 'RESTORE-ENV))))
(define POP-CONS-FRAME! (make-parameter (lambda (arity) (list 'POP-CONS-FRAME! arity))))
(define ALLOCATE-DOTTED-FRAME (make-parameter (lambda (arity) (list 'ALLOCATE-DOTTED-FRAME arity))))
(define PREDEFINED (make-parameter (lambda (i) (list 'PREDEFINED i))))
(define SET-SHALLOW-ARGUMENT! (make-parameter (lambda (j) (list 'SET-SHALLOW-ARGUMENT! j))))
(define GLOBAL-REF (make-parameter (lambda (i) (list 'GLOBAL-REF i))))
(define SET-GLOBAL! (make-parameter (lambda (i) (list 'SET-GLOBAL! i))))
(define JUMP-FALSE (make-parameter (lambda (offset) (list 'JUMP-FALSE offset))))
(define EXTEND-ENV (make-parameter (lambda () (list 'EXTEND-ENV))))
(define CALL0 (make-parameter (lambda (address) (list 'CALL0 address))))
(define PUSH-VALUE (make-parameter (lambda () (list 'PUSH-VALUE))))
(define INVOKE2 (make-parameter (lambda (address) (list 'INVOKE2 address))))
(define INVOKE3 (make-parameter (lambda (address) (list 'INVOKE3 address))))
(define ARITY=? (make-parameter (lambda (arity+1) (list 'ARITY=? arity+1))))
(define PACK-FRAME! (make-parameter (lambda (arity) (list 'PACK-FRAME! arity))))
(define POP-FUNCTION (make-parameter (lambda () (list 'POP-FUNCTION))))
(define PRESERVE-ENV (make-parameter (lambda () (list 'PRESERVE-ENV))))
(define POP-FRAME! (make-parameter (lambda (rank) (list 'POP-FRAME! rank))))
(define ALLOCATE-FRAME (make-parameter (lambda (size) (list 'ALLOCATE-FRAME size))))
(define FINISH (make-parameter (lambda () (list 'FINISH))))
(define EXPLICIT-CONSTANT 'wait)
