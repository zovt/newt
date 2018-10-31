(use utils)

(define-record proc fn)

; bindings are an alist of symbol names to a binding
; a binding is one of: (Proc)
(define-record context bindings)

(define builtins 
  `((println . ,(make-proc (lambda (thing) (display thing) (newline))))))

(define (default-context)
  (make-context builtins))

(define (eval-form form context)
  (let* ([form-name (car form)]
	 [form-rest (cdr form)]
	 [form-name-binding (cdr (assoc form-name (context-bindings context)))]
	 [form-rest-eval (map (lambda (item) (evaluate item context)) form-rest)])
    (cond [(null? form-name-binding) (error (concat form-name-binding " is not found in the context"))]
	  [(proc? form-name-binding)
	   (let ([fn (proc-fn form-name-binding)])
	     (if (eq? (length form-rest-eval)
		      (length (cdr (procedure-information fn))))
		 (apply fn form-rest-eval)
		 (error "Function args not equal")))])))

(define (eval-binding form context)
  'binding)

(define (evaluate item context)
  (cond [(list? item) (eval-form item context)]
	[(symbol? item) (eval-binding item context)]
	[(string? item) item]
	[else (error "Item is not list, symbol, or string")]))

(define (run file-path)
  (let [(context (default-context))
	(forms (read-file file-path))]
    (display (map (lambda (form)
		    (evaluate form context)) forms))))

(if (null? (command-line-arguments))
    (display "Missing file to run\n")
    (run (first (command-line-arguments))))
