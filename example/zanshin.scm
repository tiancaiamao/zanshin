(use tcp)

(define-values (i o) (tcp-connect "localhost" 8293))

(define (loop)
  (let* ((input (read))
	 (res (compile input)))
    (if (pair? res)
	(let* ((bytecode (car res))
	       (len (u8vector-length bytecode))
	       (blob (u32vector->blob/shared (u32vector len)))
	       (head (blob->u8vector blob)))
	  (write-u8vector head o)
	  (write-u8vector bytecode o))
	(begin
	  (write "compile error: ")
	  (write res)
	  (newline)))
    (loop)))
