(use utils)

(define-record proc fn)
(define-record type type)
(define-record lang handler)
(define-record const value)
(define-record-printer (const c out)
  (fprintf out "#.(const ~a)" (const-value c)))
(define-record func args form)
(define-record-printer (func fn out)
  (fprintf out "#.(func ~a ~a)" (func-args fn) (func-form fn)))

; bindings are an alist of symbol names to a binding
; a binding is one of: (Lang Proc Type Const Func)
(define-record context bindings)
(define-record-printer (context con out)
  (fprintf out "#.(context ~a)" (context-bindings con)))

(define (handle-def form val context)
  (if (list? form)
    (make-context (append (context-bindings context) `((,(car form) . ,(make-func (cdr form) val)))))
    (make-context (append (context-bindings context) `((,form . ,(make-const val)))))))

(define (handle-sig form ret context)
  (make-context (append (context-bindings context) '())))

(define builtins 
  `(;; functions
    (println . ,(make-proc (lambda (thing) (display thing) (newline))))
    (+ . ,(make-proc +))
    (- . ,(make-proc -))
    (/ . ,(make-proc /))
    (* . ,(make-proc *))
    ;; types
    (int8 . ,(make-type 'builtin))
    (uint8 . ,(make-type 'builtin))
    (int16 . ,(make-type 'builtin))
    (uint16 . ,(make-type 'builtin))
    (int32 . ,(make-type 'builtin))
    (uint32 . ,(make-type 'builtin))
    (int64 . ,(make-type 'builtin))
    (uint64 . ,(make-type 'builtin))
    ;; language features
    (def . ,(make-lang handle-def))
    (: . ,(make-lang handle-sig))))

(define (default-context)
  (make-context builtins))

(define (eval-lang form-name-binding form-rest context)
  (apply (lang-handler form-name-binding) (append form-rest (list context))))

(define (eval-proc form-name-binding form-rest context)
  (apply (proc-fn form-name-binding) form-rest))

(define (eval-func func-name func args context)
  (if (not (eq? (length (func-args func)) (length args)))
    (error (format #f "Not enough arguments applied to ~a" func-name))
    (car (evaluate (func-form func) (foldl (lambda (context arg-pair)
                                      (handle-def (car arg-pair) (cdr arg-pair) context))
                                     context
                                     (map cons (func-args func)
                                           (map (lambda (arg) (car (evaluate arg context))) args)))))))

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
                [(const? form-name-binding) `(,(const-value form-name-binding) . ,context)]
                [(func? form-name-binding) `(,(eval-func form-name form-name-binding (map (lambda (item) (car (evaluate item context))) form-rest) context) . ,context)]
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
