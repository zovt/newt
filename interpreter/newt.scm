(use utils)

(define-record proc fn)
(define-record type type)
(define-record lang handler)
(define-record const value)

; bindings are an alist of symbol names to a binding
; a binding is one of: (Lang proc Type Const)
(define-record context bindings)
(define-record-printer (context con out)
  (fprintf out "#.(context ~a)" (context-bindings con)))

(define (handle-def name val context)
  (make-context (append (context-bindings context) `((,name . ,(make-const val))))))

(define builtins 
  `(;; functions
    (println . ,(make-proc (lambda (thing) (display thing) (newline))))
    (+ . ,(make-proc +))
    ;; types
    (int . ,(make-type 'int))
    ;; language features
    (def . ,(make-lang handle-def))
    (sig . ,(make-lang (lambda (name val context) context)))))

(define (default-context)
  (make-context builtins))

(define (eval-lang form-name-binding form-rest context)
  (apply (lang-handler form-name-binding) (append form-rest (list context))))

(define (eval-proc form-name-binding form-rest context)
  (display 'eval-proc)
  (newline)
  (display form-rest)
  (newline)
  (apply (proc-fn form-name-binding) form-rest))

(define (eval-form form context)
  (let* ([form-name (car form)]
         [form-rest (cdr form)]
         [form-name-binding-entry (assoc form-name (context-bindings context))])
    (if (or (null? form-name-binding-entry) (eq? #f form-name-binding-entry))
        (error (string-append (symbol->string form-name) " is not found in the context"))
        (let ([form-name-binding (cdr form-name-binding-entry)])
          (cond [(lang? form-name-binding)
                 `(() . ,(eval-lang form-name-binding form-rest context))]
                [(proc? form-name-binding)
                 `(,(eval-proc form-name-binding (map (lambda (item) (car (evaluate item context))) form-rest) context) . ,context)]
                [(const? form-name-binding)
                 `(,(const-value form-name-binding) . ,context)]
                )))))

(define (eval-binding item context)
  (let ([binding (assoc item (context-bindings context))])
    (cond [(or (null? binding) (eq? #f binding))
           (error (format #f "The symbol ~a is not defined" item))]
          [(not (const? (cdr binding))) (error (format #f "The symbol ~a is not a const" item))]
          [else `(,(const-value (cdr binding)) . ,context)])))

(define (evaluate item context)
  (cond [(list? item) (eval-form item context)]
        [(symbol? item) (eval-binding item context)]
        [(string? item) `(,item . ,context)]
        [(number? item) `(,item . ,context)]
        [else (error (string-append (format #f "~a" item) " is not list, symbol, or string"))]))
(define (run file-path)
  (let [(context (default-context))
        (forms (read-file file-path))]
    (foldl (lambda (context form)
       (display form)
       (newline)
       (let ([out (evaluate form context)])
         (display (car out))
         (newline)
         (cdr out)))
     context
     forms)))

(if (null? (command-line-arguments))
    (display "Missing file to run\n")
    (run (first (command-line-arguments))))
