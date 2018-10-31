(use utils)

(define-record proc fn)
(define-record type type)
(define-record lang handler)
(define-record const value)

; bindings are an alist of symbol names to a binding
; a binding is one of: (Lang Proc Type Const)
(define-record context bindings)

(define builtins 
  `(;; functions
    (println . ,(make-proc (lambda (thing) (display thing) (newline))))
    (+ . ,(make-proc (lambda (a b) (+ a b))))
    ;; types
    (int . ,(make-type 'int))
    ;; language features
    (def . ,(make-lang
	      (lambda (name val context)
		(make-context (append (context-bindings context) '((,name ,val)))))))
    (sig . ,(make-lang (lambda (name val context) context)))))

(define (default-context)
  (make-context builtins))

(define (eval-form form context)
  (display "eval-form")
  (newline)
  (display form)
  (newline)
  (let* ([form-name (car form)]
	 [form-rest (cdr form)]
	 [form-name-binding (assoc form-name (context-bindings context))]
	 [form-rest-eval (map (lambda (item) (car (evaluate item context))) form-rest)])
    (if (or (null? form-name-binding) (eq? #f form-name-binding))
	(error (string-append (symbol->string form-name) " is not found in the context"))
	(cond [(proc? form-name-binding)
	       (let ([fn (proc-fn form-name-binding)])
		 (if (eq? (length form-rest-eval)
			  (length (cdr (procedure-information fn))))
		     `(,(apply fn form-rest-eval) ,context)
		     (error "Missing required args")))]))))

(define (eval-binding item context)
  (display "Called eval-binding")
  (newline)
  (display item)
  (newline)
  `(,item ,context))

(define (evaluate item context)
  (display "evaluate")
  (newline)
  (display item)
  (newline)
  (cond [(list? item) (eval-form item context)]
	[(symbol? item) (eval-binding item context)]
	[(string? item) '(,item ,context)]
	[(number? item) '(,item ,context)]
	[else (error (string-append (format #f "~a" item) " is not list, symbol, or string"))]))

(define (run file-path)
  (let [(context (default-context))
	(forms (read-file file-path))]
    (foldl (lambda (context form)
	     (let ([out (evaluate form context)])
	       (display (car out))
	       (newline)
	       (cdr out)))
	   context
	   forms)))

(if (null? (command-line-arguments))
    (display "Missing file to run\n")
    (run (first (command-line-arguments))))
