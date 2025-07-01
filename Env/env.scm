(module env_env
   (import (ast_node "Ast/node.scm"))
           ;; section 3.1.6
   (export (class env::object
              (ntype::bint (default 0))
              (type-table (default (create-hashtable eqtest: eq?)))
              (types::vector (default (make-vector 100000)))
              (type-names::vector (default (make-vector 100000)))

              (field-names::vector (default (make-vector 100000)))

              (nlocal::bint (default 0))
              (local-names::pair-nil (default '()))
              (local-types::vector (default (make-vector 0)))

              (nlabel::bint (default 0))
              (label-names::pair-nil (default '()))
              (label-types::vector (default (make-vector 10000)))

              (refs (default (create-hashtable eqtest: eq?)))

              (ndata::bint (default 0))
              (data-table (default (create-hashtable eqtest: eq?)))

              (nglobal::bint (default 0))
              (global-table (default (create-hashtable eqtest: eq?)))
              (global-types::vector (default (make-vector 1000000)))
              (global-names::vector (default (make-vector 1000000)))


              (nfunc::bint (default 0))
              (func-table (default (create-hashtable eqtest: eq?)))
              (func-types::vector (default (make-vector 1000000)))
              (func-names::vector (default (make-vector 1000000)))

              (ntag::bint (default 0))
              (tag-table (default (create-hashtable eqtest: eq?)))
              (tag-types::vector (default (make-vector 10000)))
              (tag-names::vector (default (make-vector 10000)))

              (nmem::bint (default 0))
              (mem-table (default (create-hashtable eqtest: eq?)))
              (mem-types::vector (default (make-vector 10000)))
              (mem-names::vector (default (make-vector 10000)))

              (return::pair-nil (default '()))
              (last-type (default #f))
              (error-list::pair-nil (default '()))
              (parent::modulefield (default (instantiate::modulefield))))

           (class local-var::object
              type
              init?::bool)

           (type-get-index::bint env::env x)
           (type-get env::env x)
           (type-get-name env::env x)
           (add-type! env::env nm t)
           (set-type! env::env id t)
           (typeidx::bint env::env x)

           (func-get-index::bint env::env x)
           (func-get-type env::env x::bint)
           (func-add! env::env t)
           (func-add-name! env::env id::symbol)
           (funcidx::bint env::env x)

           (global-get-index::bint env::env x)
           (global-get-type env::env x::bint)
           (global-add! env::env t)
           (global-add-name! env::env id::symbol)
           (globalidx::bint env::env x)

           (tag-get-index::bint env::env x)
           (tag-get-type env::env x::bint)
           (tag-add! env::env t)
           (tag-add-name! env::env id::symbol)
           (tagidx::bint env::env x)

           (mem-get-index::bint env::env x)
           (mem-get-type env::env x::bint)
           (mem-add! env::env t)
           (mem-add-name! env::env id::symbol)
           (memidx::bint env::env x)

           (data-get-index::bint env::env x)
           (dataidx::bint env::env x)

           (field-get-index::bint env::env t::bint f)
           (field-get-name env::env x::bint y::bint)
           (fieldidx::bint env::env x)

           (local-get-index env::env l)
           (local-init! env::env l::bint)
           (local-init?::bool env::env l)
           (local-get-type env::env l)
           (localidx::bint env::env x)

           (label-get-index::bint env::env x)
           (label-get-type env::env x::bint)
           (push-label! env::env nm t::pair-nil)
           (pop-label! env::env)
           (labelidx::bint env::env x)))

(define (ident? x)
   (and (symbol? x) (equal? #\$ (string-ref (symbol->string x) 0))))

(define (get-index::bint table range::bint x ex-oor ex-unkwn ex-exp)
   (cond ((number? x)
          (if (<fx x range)
              x
              (raise (list ex-oor range x))))
         ((ident? x)
          (if (hashtable-contains? table x)
              (hashtable-get table x)
              (raise (list ex-unkwn x))))
         (else (raise (list ex-exp x)))))

(define (index::bint lst::pair-nil x i::bint e)
      (cond ((null? lst) (raise (list e x)))
            ((equal? (car lst) x) i)
            (else (index (cdr lst) x (+fx 1 i) e))))

(define (type-get-index::bint env::env x)
   (get-index (-> env type-table) (-> env ntype) x '(idx-out-of-range type)
              '(unknown type) '(expected-idx type)))

(define (type-get env::env x)
   (vector-ref (-> env types) (type-get-index env x)))

(define (type-get-name env::env x)
   (let ((nm (vector-ref (-> env type-names) (type-get-index env x))))
      (if nm nm x)))

(define (add-type! env::env nm t)
   (let ((x (-> env ntype)))
      (set! (-> env ntype) (+fx 1 x))
      (when nm
         (hashtable-put! (-> env type-table) nm x))
      (vector-set! (-> env type-names) x nm)
      (vector-set! (-> env types) x t)))

(define (set-type! env::env id t)
   (vector-set! (-> env types) (type-get-index env id) t))

(define (typeidx::bint env::env i)
   (set! (-> env last-type) (type-get-index env i))
   (-> env last-type))

(define-macro (table-boilerplate x)
   `(begin
       (define (,(symbol-append x '-get-index::bint) env::env x)
          (get-index (-> env ,(symbol-append x '-table))
                     (-> env ,(symbol-append 'n x)) x
                     '(idx-out-of-range ,x) '(unknown ,x) '(expected-idx ,x)))

       (define (,(symbol-append x '-get-type) env::env x::bint)
          (vector-ref (-> env ,(symbol-append x '-types)) x))

       (define (,(symbol-append x '-add!) env::env t)
          (let ((x (-> env ,(symbol-append 'n x))))
             (vector-set! (-> env ,(symbol-append x '-types)) x t)
             (set! (-> env ,(symbol-append 'n x)) (+fx 1 x))))

       (define (,(symbol-append x '-add-name!) env::env id::symbol)
          (when (hashtable-contains? (-> env ,(symbol-append x '-table)) id)
             (raise `(name-already-used ,id)))
          (hashtable-put! (-> env ,(symbol-append x '-table)) id
                          (-> env ,(symbol-append 'n x)))
          (vector-set! (-> env ,(symbol-append x '-names))
                       (-> env ,(symbol-append 'n x)) id))

       (define (,(symbol-append x 'idx::bint) env::env x)
          (,(symbol-append x '-get-index) env x))))

(table-boilerplate func)
(table-boilerplate global)
(table-boilerplate tag)
(table-boilerplate mem)

(define (data-get-index::bint env::env x)
   (get-index (-> env data-table) (-> env ndata) x '(idx-out-range data)
              '(unknown data) '(expected-idx data)))

(define (dataidx::bint env::env x)
   (data-get-index env x))

(define (field-get-index::bint env::env t::bint f)
   (let ((v (vector-ref (-> env field-names) t)))
      (cond
       ((number? f)
        (if (<fx f (length v))
         t
         (raise `((idx-out-of-range field) ,t (length v) ,f))))
    ((ident? f)
     (index v f 0 'unknown-field))
    (#t (raise `(expected-fieldidx ,t))))))

(define (field-get-name env::env x::bint y::bint)
   (list-ref (vector-ref (-> env field-names) x) y))

(define (local-get-index env::env l)
   (cond
    ((number? l)
     (if (< l (vector-length (-> env local-types)))
         l
         (raise `(localidx-out-of-range ,l))))
    ((ident? l)
     (index (-> env local-names) l 0 'unknown-local))
    (#t `(expected-local ,l))))

(define (local-init! env::env l::bint)
   (with-access::local-var (vector-ref (-> env local-types) l) ((init? init?))
      (set! init? #t)))

(define (local-init?::bool env::env l)
   (with-access::local-var
      (vector-ref (-> env local-types) (local-get-index env l))
      ((init? init?))
      init?))

(define (local-get-type env::env l)
   (with-access::local-var
      (vector-ref (-> env local-types) (local-get-index env l))
      ((type type))
      type))

(define (label-get-index::bint env::env x)
  (cond
   ((number? x)
    (if (<fx x (-> env nlabel))
        x
        (raise `(labelidx-out-of-range ,x))))
   ((ident? x)
    (index (-> env label-names) x 0 'unknown-label))
   (else `((expected-idx label) ,x))))

(define (label-get-type env::env x::bint)
   (vector-ref (-> env label-types) (- (-> env nlabel) x 1)))

(define (push-label! env::env nm t::pair-nil)
   (let ((x (-> env nlabel)))
      (set! (-> env nlabel) (+fx x 1))
      (set! (-> env label-names) (cons nm (-> env label-names)))
      (vector-set! (-> env label-types) x t)))

(define (pop-label! env::env)
   (set! (-> env nlabel) (-fx (-> env nlabel) 1))
   (set! (-> env label-names) (cdr (-> env label-names))))

;; to work, needs to be called after typeidx
(define (fieldidx::bint env::env x)
   (unless (vector-ref (-> env field-names) (-> env last-type))
      (raise `(expected-struct ,(-> env last-type)
               ,(expand (type-get env (-> env last-type))))))
   (field-get-index env (-> env last-type) x))

(define (localidx::bint env::env x)
   (local-get-index env x))

(define (labelidx::bint env::env x)
   (label-get-index env x))
