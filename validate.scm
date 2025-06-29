(module validate
   (library srfi1 pthread)
   (include "read-table.sch")
   (main main))

;;; we force everywhere the number of type indices after sub final? to be one;
;;; even though forms with more than one type are syntactically correct, they
;;; are never valid

(define keep-going #f)
(define error-encountered? #f)
(define silent #f)
(define nthreads 1)

(define-macro (with-default-value env def dec . body)
   `(with-handler
       (lambda (e)
          (if (isa? e &error)
              (raise e))
          (if keep-going
              (begin
                 (env-error-list-set! ,env (cons (append ,dec (list e))
                                                 (env-error-list ,env)))
                 ,def)
              (raise (append ,dec (list e)))))
       ,@body))

(define *reported-missing-idents* (make-hashtable))

(read-table *numtypes* "numtypes.sch")
(define (numtype?::bool t)
   (hashtable-contains? *numtypes* t))
(read-table *vectypes* "vectypes.sch")
(define (vectype?::bool t)
   (hashtable-contains? *vectypes* t))
(read-table *packedtypes* "packedtypes.sch")
(define (packedtype?::bool t)
   (hashtable-contains? *packedtypes* t))
(read-table *absheaptype* "absheaptypes.sch")
(define (absheaptype?::bool t)
   (hashtable-contains? *absheaptype* t))
(read-table *reftypes-abbreviations* "type-abbreviations.sch")
(define (reftype-abv?::bool t)
   (hashtable-contains? *reftypes-abbreviations* t))
(define (reftype?::bool t)
   (or (reftype-abv? t) (and (pair? t) (eq? 'ref (car t)))))
(define (valtype?::bool t)
   (or (reftype? t) (numtype? t) (vectype? t)))
(define (addrtype?::bool t)
   (or (eq? t 'i32) (eq? t 'i64)))

;; section 3.2.20
(define (defaultable? t)
   (or (vectype? t)
       (numtype? t)
       (and (reftype? t) (nullable? t))))

;; section 3.4.13
;; only use on desuggared instructions
(read-table *const-instrs* "constant-instructions.sch")
(define (non-constant-instr? env::struct i::pair)
   (if (or (hashtable-contains? *const-instrs* (car i))
           (eq? (car i) 'error)
           (and (eq? (car i) 'global.get)
                (not (car (global-get-type env (cadr i))))))
       #f
       i))

(define (non-constant-expr? env::struct e::pair-nil)
   (find (lambda (i) (non-constant-instr? env i)) e))

;; section 2.3.8
(define (unpack t)
   (if (packedtype? t) 'i32 t))

(define (unpack-ft t)
   (unpack (cadr t)))

;; section 3.1.1 (convention)
(define (type-diff t1 t2)
   (if (nullable? t2)
       `(ref ,(reftype->heaptype t1))
       t1))

(define (ht-upperbound::symbol env::struct t)
   (match-case t
      ((or any none eq i31 struct array) 'any)
      ((or func nofunc) 'func)
      ((or extern noextern) 'extern)
      ((or exn noexn) 'exn)
      ((? idx?) (ht-upperbound env (get-type env t)))
      ((? deftype?) (ht-upperbound env (deftype-head t)))))

(define (rt-upperbound::pair env::struct t)
   `(ref null ,(ht-upperbound env (reftype->heaptype t))))

(define-macro (replace-exception e e' . body)
   `(with-handler
       (lambda (exn)
          (if (and (pair? exn) (eq? (car exn) ,e))
              (raise (cons ,e' (cdr exn)))
              (raise exn)))
       ,@body))

(define-macro (map-env f env . l)
   `(map (lambda (x) (,f ,env x)) ,@l))

;; like every but returns #f if the lists are not of the same length
(define (every' f . lists)
   (if (any null? lists)
       (every null? lists)
       (and (apply f (map car lists)) (apply every' f (map cdr lists)))))

;; evaluation order is not unspecified in the standard, we enforce one here
(define (map-seq f . lists)
   (if (any null? lists)
       '()
       (cons (apply f (map car lists)) (apply map-seq f (map cdr lists)))))

(define (length>=?::bool l::pair-nil i::bint)
   (if (null? l)
       (>=fx 0 i)
       (or (>=fx 1 i) (length>=? (cdr l) (-fx i 1)))))

(define (stack-take::pair-nil st::pair-nil i::bint)
   (cond ((=fx i 0) '())
         ((null? st) (raise 'empty-stack))
         ((eq? (car st) 'poly) (make-list i 'bot))
         (#t (cons (car st) (stack-take (cdr st) (-fx i 1))))))

(define (stack-drop::pair-nil st::pair-nil i::bint)
   (cond ((=fx i 0) '())
         ((null? st) (raise 'empty-stack))
         ((eq? (car st) 'poly) '(poly))
         (#t (stack-drop (cdr st) (-fx i 1)))))

(define (index::bint lst::pair-nil x i::bint e::symbol)
      (cond ((null? lst) (raise (list e x)))
            ((equal? (car lst) x) i)
            (#t (index (cdr lst) x (+fx 1 i) e))))

(define (type-size::llong t)
   (match-case t
      (i8 #l8)
      (i16 #l16)
      (i32 #l32)
      (i64 #l64)))

(define (ident? x)
   (and (symbol? x) (equal? #\$ (string-ref (symbol->string x) 0))))

(define (idx? x)
   (or (ident? x)
       (number? x)))

(define-struct local-var
   (init? #f)
   (type #t))

;; section 3.1.6
(define-struct env
   (ntypes 0)
   ; to access with the names, contains indices
   (types-table (create-hashtable eqtest: eq?))
   ; to access with the index, contains lists (type name), the name is stored
   ; for error reporting
   (types-vector (make-vector 100000))
   (fields-names (make-vector 100000))

   (nlocals 0)
   (locals-names '())
   ; relevant information can only be accessed by index ; find the index of a
   ; name first to access by name
   (locals-types (make-vector 0))

   (nlabels 0)
   (labels-names '())
   (labels-types (make-vector 10000))

   (nfunc 0)
   (func-table (create-hashtable eqtest: eq?))
   (func-types (make-vector 1000000))
   (func-names (make-vector 1000000)) ; for error reporting

   (refs (make-hashtable))

   (ndata 0)
   (data-table (create-hashtable eqtest: eq?))

   (nglobal 0)
   (global-table (create-hashtable eqtest: eq?))
   (global-types (make-vector 100000))
   (global-names (make-vector 100000)) ; for error reporting

   (ntag 0)
   (tag-table (create-hashtable eqtest: eq?))
   (tag-types (make-vector 100000))
   (tag-names (make-vector 100000)) ; for error reporting

   (nmem 0)
   (mem-table (create-hashtable eqtest: eq?))
   (mem-types (make-vector 100000))
   (mem-names (make-vector 100000)) ; for error reporting

   (return #f)

   (last-type #f)
   (error-list '()))

(define-inline (get-index::bint table range::bint x ex-oor ex-unkwn ex-exp)
   (cond
    ((number? x)
     (if (<fx x range)
         x
         (raise (list ex-oor range x))))
    ((ident? x)
     (if (hashtable-contains? table x)
         (hashtable-get table x)
         (raise (list ex-unkwn x))))
    (#t (raise (list ex-exp x)))))

(define (get-type-index::bint env::struct t)
   (get-index (env-types-table env) (env-ntypes env) t 'typeidx-out-of-range
              'unknown-type 'expected-typeidx))

(define (get-type env::struct t)
   (car (vector-ref (env-types-vector env) (get-type-index env t))))

(define (add-type! env::struct nm t)
   (let ((x (env-ntypes env)))
      (env-ntypes-set! env (+fx 1 x))
      (when nm
         (hashtable-put! (env-types-table env) nm x))
      (vector-set! (env-types-vector env) x (list t nm))))

(define (set-type! env::struct id t)
   (let ((v (env-types-vector env))
         (idx (get-type-index env id)))
     (vector-set! v idx (cons t (cdr (vector-ref v idx))))))

(define-macro (table-boilerplate x)
   `(begin
       (define (,(symbol-append x '-get-index::bint) env::struct x)
          (get-index (,(symbol-append 'env- x '-table) env)
                     (,(symbol-append 'env-n x) env) x
                     '(idx-out-of-range ,x) '(unknown ,x) '(expected ,x 'idx)))

       (define (,(symbol-append x '-get-type::pair) env::struct x::bint)
          (vector-ref (,(symbol-append 'env- x '-types) env) x))

       (define (,(symbol-append x '-add!) env::struct t::pair)
          (let ((x (,(symbol-append 'env-n x) env)))
             (vector-set! (,(symbol-append 'env- x '-types) env) x t)
             (,(symbol-append 'env-n x '-set!) env (+fx 1 x))))

       (define (,(symbol-append x '-add-name!) env::struct id::symbol)
          (hashtable-put! (,(symbol-append 'env- x '-table) env) id
                          (,(symbol-append 'env-n x) env))
          (vector-set! (,(symbol-append 'env- x '-names) env)
                       (,(symbol-append 'env-n x) env) id))

       (define (,(symbol-append x 'idx::bint) env::struct x)
          (,(symbol-append x '-get-index) env x))))

(table-boilerplate func)
(table-boilerplate global)
(table-boilerplate tag)
(table-boilerplate mem)

(define (valid-func-ref?::bool env::struct x::bint)
   (let ((h (env-refs env)))
      (or (hashtable-contains? h x)
          (hashtable-contains? h (vector-ref (env-func-names env) x)))))

(define (field-get-index::bint env::struct t::bint f)
   (let ((v (vector-ref (env-fields-names env) t)))
      (cond
       ((number? f)
        (if (<fx f (length v))
         t
         (raise `(fieldidx-out-of-range ,t (length v) ,f))))
    ((ident? f)
     (index v f 0 'unknown-field))
    (#t (raise `(expected-fieldidx ,t))))))

(define (data-get-index::bint env::struct x)
   (get-index (env-data-table env) (env-ndata env) x 'dataidx-out-range
              'unknown-data 'expected-dataidx))

(define (local-get-index env::struct l)
   (cond
    ((number? l)
     (if (< l (vector-length (env-locals-types env)))
         l
         (raise `(localidx-out-of-range ,l))))
    ((ident? l)
     (index (env-locals-names env) l 0 'unknown-local))
    (#t `(expected-local ,l))))

(define (local-init! env::struct l::bint)
   (local-var-init?-set! (vector-ref (env-locals-types env) l) #t))

(define (local-init?::bool env::struct l)
   (local-var-init? (vector-ref (env-locals-types env)
                                (local-get-index env l))))

(define (local-get-type env::struct l)
   (local-var-type (vector-ref (env-locals-types env) (local-get-index env l))))

(define (get-label-index::bint env::struct x)
  (cond
   ((number? x)
    (if (<fx x (env-nlabels env))
        x
        (raise `(labelidx-out-of-range ,x))))
   ((ident? x)
    (index (env-labels-names env) x 0 'unknown-label))
   (#t `(expected-label ,x))))

; only used on arguments returned by the previous function
(define (get-label-type env::struct x::bint)
   (vector-ref (env-labels-types env) (- (env-nlabels env) x 1)))

(define (push-label! env::struct nm t::pair-nil)
   (let ((x (env-nlabels env)))
      (env-nlabels-set! env (+fx x 1))
      (env-labels-names-set! env (cons nm (env-labels-names env)))
      (vector-set! (env-labels-types env) x t)))

(define (pop-label! env::struct)
   (env-nlabels-set! env (- (env-nlabels env) 1))
   (env-labels-names-set! env (cdr (env-labels-names env))))

;; deftypes (rec subtypes*).i are represented as (deftype subtypes* i))
;; (rec i) are represented as (rec i)
(define (deftype?::bool t)
   (and (pair? t) (eq? 'deftype (car t))))

(define (rectype?::bool t)
   (and (pair? t) (eq? 'rec (car t))))

;; avoid full expansion when it is not needed
(define (deftype-head t)
   (match-case (cdr (list-ref (cadr t) (caddr t))) ; remove the "sub" keyword
      ((or (final ?- (?hd . ?-))
           (final (?hd . ?-))
           (?- (?hd . ?-))
           ((?hd . ?-)))
       hd)))

;; we could do the equality check respecting all the structure but we can use
;; our representations slopiness to do things shorter
(define (eq-clos-ct? env::struct t1 t2)
   #f)
(define (eq-clos-ht? env::struct t1 t2)
   #f)

(define (structural-eq-clos-st? env::struct t1 t2)
   (match-case (cons t1 t2)
      ((or ((sub final ?ht1 ?cts1) . (sub final ?ht2 ?cts2))
           ((sub (and (not final) ?ht1) ?cts1) .
            (sub (and (not final) ?ht2) ?cts2)))
       (and (eq-clos-ht? env ht1 ht2)
            (every' (lambda (ct1 ct2) eq-clos-ct? env ct1 ct2) cts1 cts2)))
      ((or ((sub final ?cts1) . (sub final ?cts2)) ((sub ?cts1) . (sub ?cts2)))
       (every' (lambda (ct1 ct2) eq-clos-ct? env ct1 ct2) cts1 cts2))
      (else #f)))

(define (eq-clos-st?::bool env::struct t1 t2)
   (cond
      ((symbol? t1) (eq? t1 t2))
      ((idx? t1) (eq-clos-st? env (get-type env t1) t2))
      ((idx? t2) (eq-clos-st? env t1 (get-type env t2)))
      ; we suppose defined types are already closed, i.e. we have to put closed
      ; types in the context, otherwise, we may have to close the whole context
      ; each time we want to close a type
      ((or (deftype? t1) (rectype? t1) (deftype? t2) (rectype? t2))
       (equal? (cdr t1) (cdr t2)))
      ((and (pair? t1) (pair? t2))
       (every' (lambda (t1 t2) eq-clos-st? env t1 t2) t1 t2))
      ((and (null? t1) (null? t2)) #t)
      (#t #f)))

(define (eq-clos-dt?::bool env::struct t1::pair t2::pair)
   (and (=fx (caddr t1) (caddr t2))
        (every' (lambda (st1 st2) eq-clos-st? env st1 st2)
                (cadr t1) (cadr t2))))

;; section 3.1.3

;; we use the same slopiness as in eq-clos-st?
(define (unroll-st sts st)
   (cond ((rectype? st) `(deftype ,sts ,(cadr st)))
         ((pair? st) (map (lambda (st) (unroll-st sts st)) st))
         (else st)))

;; expects well formed arguments
(define (unroll-dt::pair t::pair)
   (unroll-st (cadr t) (list-ref (cadr t) (caddr t))))

(define (roll-st env::struct st x::bint n::bint)
   (cond
    ((idx? st)
     (let ((id (get-type-index env st)))
        (if (and (<=fx x id) (<fx id n))
            `(rec ,(-fx id x))
            st)))
    ((pair? st) (map (lambda (st) (roll-st env st x n)) st))
    (else st)))

(define (roll-rect::pair env::struct rect::pair x::bint)
   (let ((n (+fx x (length (cdr rect)))))
      (map (lambda (st) (roll-st env st x n)) (cdr rect))))

(define (roll* env::struct rect::pair x::bint)
   (let ((rolled-rect (roll-rect env rect x)))
      (list-tabulate (length (cdr rect))
                     (lambda (i) `(deftype ,rolled-rect ,i)))))

(define (expand t)
   (match-case (cdr (unroll-dt t)) ; remove the sub keyword
      ((or (final ?- ?ct)
           (final ?ct)
           (?- ?ct)
           (?ct))
       ct)))

;; subtyping relation return only a boolean, which is good if it is true, but in
;; case of failure we might want a reason to explain it (kind of a constructive
;; proof that not (t1 < t2), could thus be solved by writing a function not-<
;; that returns #f in case t1 < t2 and a reason for why it is not the case
;; otherwise)
;;
;; we could probably speed up the subtyping comparaisons by inserting eq?
;; shortcuts at the write place, the official validation tool does it for
;; defined types; we probably need benchmarks for that

;; section 3.3.3
(define (<ht=::bool env::struct t1 t2)
   (cond ((equal? t1 t2) #t)
         ((eq? t1 'bot) #t)
         ((idx? t1) (<ht= env (get-type env t1) t2))
         ((idx? t2) (<ht= env t1 (get-type env t2)))
         ((eq? 'none t1) (<ht= env t2 'any))
         ((eq? 'nofunc t1) (<ht= env t2 'func))
         ((eq? 'noextern t1) (<ht= env t2 'extern))
         ((eq? 'noexn t1) (<ht= env t2 'exn))
         ((eq? t2 'any) (<ht= env t1 'eq))
         ((eq? t2 'eq)
          (or (eq? t1 'i31)
              (eq? t1 'struct)
              (eq? t1 'array)
              (and (deftype? t1) (<ht= env (deftype-head t1) 'eq))))
         ((and (deftype? t1) (symbol? t2))
          (eq? (deftype-head t1) t2))
         ((and (deftype? t1) (deftype? t2))
          (<dt= env t1 t2))
         (#t #f)))

(define (nullable?::bool rt::pair)
   (eq? (cadr rt) 'null))

(define (reftype->heaptype rt::pair)
   (if (nullable? rt)
       (caddr rt)
       (cadr rt)))

;; actually does redundant checks, could be expanded to avoid them
;; section 3.3.4
(define (<rt=::bool env::struct t1::pair t2::pair)
   (and (or (nullable? t2) (not (nullable? t1)))
        (<ht= env (reftype->heaptype t1) (reftype->heaptype t2))))

;; section 3.3.5
(define (<vt=::bool env::struct t1 t2)
   (or (eq? t1 'bot)
       ;(and (numtype? t1) (numtype? t2) (eq? t1 t2))
       ;(and (vectype? t1) (vectype? t2) (eq? t1 t2))
       (eq? t1 t2)
       (and (reftype? t1) (reftype? t2) (<rt= env t1 t2))
       (eq? t2 'top)))

;; section 3.3.6
(define (<res=::bool env l1 l2)
   (every' (lambda (t1 t2) (<vt= env t1 t2)) l1 l2))

;; section 3.3.8
(define (<funct=::bool env::struct t1::pair t2::pair)
   (let ((t11 (car t1))
         (t12 (cadr t1))
         (t21 (car t2))
         (t22 (cadr t2)))
      (and
       (<res= env t21 t11)
       (<res= env t12 t22))))

;; section 3.3.10
(define (<st=::bool env t1 t2)
  (or ; (and (packedtype? t1) (eq? t1 t2))
      (eq? t1 t2)
      (and (valtype? t1) (valtype? t2) (<vt= env t1 t2))))

(define (<fldt=::bool env::struct t1::pair t2::pair)
   (match-case (cons t1 t2)
      (((#f ?st1) . (#f ?st2))
       (<st= env st1 st2))
      (((#t ?st1) . (#t ?st2))
       (and (<st= env st1 st2) (<st= env st2 st1)))
      (else #f)))

;; section 3.3.9
(define (<ct=::bool env::struct t1::pair t2::pair)
   (match-case (cons t1 t2)
      (((array ?fldt1) . (array ?fldt2))
       (<fldt= env fldt1 fldt2))
      (((func . ?funct1) . (func . ?funct2))
       (<funct= env funct1 funct2))
      (((struct . ?fldts1) . (struct . ?fldts2))
       (let ((n1 (length fldts1))
             (n2 (length fldts2)))
          (and (<= n2 n1)
               (every (lambda (fldt1 fldt2) (<fldt= env fldt1 fldt2))
                      fldts1 fldts2))))
      (else #f)))

;; section 3.3.11
(define (get-sub-heaptype st)
   (match-case (cdr st)
      ((or (final ?ht ?-)
           ((and (not final) ?ht) ?-))
       ht)
      (else #f)))

(define (<dt=::bool env::struct t1 t2)
   (if (eq-clos-dt? env t1 t2)
       #t
       (let ((ht (get-sub-heaptype (unroll-dt t1))))
          (and ht (<ht= env ht t2)))))

;; section 3.3.12

;; limits are represented as (min . max) when there is a max and (min . #f)
;; otherwise
(define (<lim=::bool l1::pair l2::pair)
   (and (<= (car l2) (car l1))
        (if (cdr l2)
            (and (cdr l1) (<= (cdr l1) (cdr l2)))
            #t)))

;; section 3.3.13
(define (<tt=::bool env::struct t1::pair t2::pair)
   (match-case (cons t1 t2)
      (((?at ?l1 ?rt1) . (?at ?l2 ?rt2))
       (and (<lim= l1 l2)
            (<rt= env l1 l2)
            (<rt= env l2 l1)))
      (else #f)))

;; section 3.3.14
(define (<mt=::bool t1 t2)
   (match-case (cons t1 t2)
      (((?at ?l1) . (?at ?l2))
       (<lim= l1 l2))))

;; section 3.3.15
(define (<gt=::bool env::struct t1 t2)
   ; when the underlying types are valtypes matching for fieldtypes and global
   ; types are the same
   (<fldt= env t1 t2))

;;; validation functions take types in the text-format and return the internal
;;; representation if there wasn't any problem

;; section 3.2.3
(define (valid-ht env::struct t)
   (if (or (absheaptype? t) (rectype? t))
       t
       ;; ensures that type that get validated are closed
       (replace-exception 'expected-typeidx 'expected-heaptype
          (get-type env t))))

;; section 3.2.4
(define (valid-rt::pair env::struct t)
   (replace-exception 'expected-heaptype 'expected-reftype
      (match-case t
         ((ref ?ht) `(ref ,(valid-ht env ht)))
         ((ref null ?ht) `(ref null ,(valid-ht env ht)))
         ((? reftype-abv?) (hashtable-get *reftypes-abbreviations* t))
         (else (raise `(expected-reftype ,t))))))

;; sections 3.2.1, 3.2.2, and 3.2.5
(define (valid-vt env::struct t)
   (replace-exception 'expected-reftype 'expected-valtype
      (if (or (vectype? t) (numtype? t))
          t
          (valid-rt env t))))

;; section 3.2.9 and 6.4.6
(define (valid-names/param/result/get-tl env::struct l::pair-nil)
   (define (valid-vt-at pos::epair t)
      (with-default-value env 'error `(at ,pos)
         (valid-vt env t)))

   (match-case l
      (((param (and (? ident?) ?id) ?vt) . ?tl)
       (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env tl)
          (values (cons id n) (cons (valid-vt-at (car l) vt) p) r tl)))
      (((param . ?vts) . ?tl)
       (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env tl)
          (values (append (make-list (length vts) #f))
                  (append (map-env valid-vt-at (car l) vts) p) r tl)))
      (((result . ?-) . ?-)
       (define (get-results/tl l)
          (match-case l
             (((result . ?vts) . ?tl)
              (multiple-value-bind (r tl) (get-results/tl tl)
                 (values (append (map-env valid-vt-at (car l) vts) r) tl)))
             (else (values '() l))))
       (multiple-value-bind (r tl) (get-results/tl l)
          (values '() '() r tl)))
      (else (values '() '() '() l))))

(define (valid-param/result env::struct l::pair-nil)
   (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env l)
      (unless (null? tl)
         (raise `(expected-functiontype ,tl)))
      (values p r)))

(define (valid-tu/get-tl env::struct l::pair-nil)
   (multiple-value-bind (n p r tl) (valid-names/param/result/get-tl env l)
      (values n (list p r) tl)))

(define (valid-blocktype/get-tl env::struct l::pair-nil)
   (multiple-value-bind (args f tl) (valid-tu/get-tl env l)
      (unless (every not args)
         (raise `(named-param-blocktype ,args)))
      (values f tl)))

;; section 3.2.11 and 6.4.7
(define (valid-fldt env::struct t)
   (define (valid-st t)
      (replace-exception 'expected-valtype 'expected-storagetype
         (if (packedtype? t)
             t
             (valid-vt env t))))
   (with-default-value env '(#f error) '()
      (match-case t
         ((mut ?st) `(#t ,(valid-st st)))
         (?st (replace-exception 'expected-storagetype 'expected-fieldtype
                 `(#f ,(valid-st st)))))))

;; section 6.4.7
(define (valid-fields/names env::struct l)
   (match-case l
      (() (values '() '()))
      (((field (and (? ident?) ?name) ?fldt) . ?tl)
       (multiple-value-bind (fields names) (valid-fields/names env tl)
          (values (cons (valid-fldt env fldt) fields) (cons name names))))
      (((field . ?fldts) . ?tl)
       (multiple-value-bind (fields names) (valid-fields/names env tl)
          (values (append (map-env valid-fldt env fldts) fields)
                  (append (map (lambda (-) (gensym "$unnamedfield")) fldts)
                          names))))
      (else (raise `(expected-fields ,l)))))

;; section 3.2.10
(define (valid-ct env::struct t x::bint)
   (match-case t
      ((func . ?p/r)
       (multiple-value-bind (p r) (valid-param/result env p/r)
          `(func ,p ,r)))
      ((array ?fldt) `(array ,(valid-fldt env fldt)))
      ((struct . ?fldts)
       (multiple-value-bind (fields names) (valid-fields/names env fldts)
          (vector-set! (env-fields-names env) x names)
          `(struct ,@fields)))
      (else (raise `(expected-comptype ,t)))))

;; expects input to be of the form (sub final? id? t)*, where t has been
;; validated: a first lifting is done while checking modules.

;; section 3.2.12
(define (valid-rect env::struct l x::bint)
   (define (valid-st t x)
      (match-case t
         ((sub final . ?rst)
          `(sub final ,@(cdr (valid-st `(sub ,@rst) x))))
         ((sub ?y ?ct)
          (if (<= x (get-type-index env y))
              (raise `(forward-subtype ,x ,y)))
          (match-case (cdr (unroll-dt (get-type env y)))
             ((final . ?-) (raise `(supertype-final ,x ,y)))
             ((or (?- ?ct') (?ct'))
              (unless (<ct= env ct ct')
                 (raise `(non-matching-supertype ,x ,y ,ct ,ct'))))
             (else (raise 'internal-error)))
          `(sub ,(get-type-index env y) ,ct))
         ((sub ?-) t)))

   (define (valid-rec-list l x)
      (if (null? l) ; (rec) is valid
          '()
          (cons (valid-st (car l) x) (valid-rec-list (cdr l) (+fx x 1)))))

   `(rec ,(valid-rec-list l x)))

;; section 3.2.14
(define (valid-lim l::pair-nil k::llong)
   (when (null? l)
      (raise `((expected limit) ,l)))
   (let ((n (wnumber->number (car l))))
      (unless (<= n k)
         (raise `(invalid-bound ,n ,k)))
      (if (null? (cdr l))
          (cons n '())
          (let ((m (wnumber->number (cadr l))))
             (unless (<= m k)
                (raise `(invalid-bound ,m ,k)))
             (cons n m)))))

;; section 3.2.16 and 6.4.13
(define (valid-mt env::struct t)
   (with-default-value env '(error (0)) '()
      (match-case t
         (((and ?at (? addrtype?)) . ?l)
          (list at
                (valid-lim l (bit-lshllong #l1 (-llong (type-size at) #l16)))))
         ; section 6.4.10
         (else (valid-mt env `(i32 ,@t))))))

;; section 3.2.18 and 6.4.14
(define (valid-gt::pair env::struct t)
   (with-default-value env '(#f error) '()
         (match-case t
            ((mut ?vt) (list #t (valid-vt env vt)))
            (else (replace-exception 'expected-valtype 'expected-globaltype
                     (list #f (valid-vt env t)))))))

;; section 6.4.9
(define (clean-mod-rectype! env::struct l x::bint)
   ; the `(rec ...) in the environment assures rolling
   (let ((sts (map-seq
                 (match-lambda
                    ((type (and (? ident?) ?id) ?st)
                     (add-type! env id `(rec ,(-fx (env-ntypes env) x))) st)
                    ((type ?st)
                     (add-type! env #f `(rec ,(-fx (env-ntypes env) x))) st)
                    (else (raise `(expected-typedef ,l)))) l)))
      (define (valid-st st x)
         (with-default-value env '(sub final (error)) `(at-subtype ,st)
               (match-case st
                  ((sub final ?ct) `(sub final ,(valid-ct env ct x)))
                  ((sub final (and ?y (? idx?)) ?ct)
                   `(sub final ,(get-type-index env y) ,(valid-ct env ct x)))
                  ((sub (and ?y (? idx?)) ?ct)
                   `(sub ,(get-type-index env y) ,(valid-ct env ct x)))
                  ((sub ?ct)
                   `(sub ,(valid-ct env ct x)))
                  (else
                   (replace-exception 'expected-comptype 'expected-subtype
                      `(sub final ,(valid-ct env st x)))))))

      (let ((rolled-sts (map valid-st sts (iota (length sts) x 1))))
         (for-each (lambda (i t) (set-type! env (+fx x i)
                                            `(deftype ,rolled-sts ,i)))
                   (iota (length sts)) rolled-sts)
         rolled-sts)))

;; section 3.4

(define (wnumber->number n)
   (cond ((number? n) n)
         ((eq? n 'inf) +inf.0)
         ((eq? n '-inf) -inf.0)
         ((eq? n 'nan) +nan.0)
         ((symbol? n)
          (let ((s (symbol->string n)))
             (if (and (>= (string-length s) 2) (substring-at? s "0x" 0))
                 (string->number (substring s 2) 16)
                 (raise `(expected-number ,n)))))
         (#t (raise `(expected-number ,n)))))

(define (i32 env::struct n)
   (let ((n (wnumber->number n)))
      (cond ((not (integer? n)) (raise `(expected-int ,n)))
            ((and (<= n 2147483647) (>= n -2147483648)) n)
            ((and (> n 2147483647) (<= n 4294967295)) (- n (* 2 2147483648)))
            (#t (raise `(out-bounds-i32 ,n))))))

(define (i64 env::struct n)
   (let ((n (wnumber->number n)))
      (cond ((not (integer? n)) (raise `(expected-int ,n)))
            ((and (<= n 9223372036854775807) (>= n -9223372036854775808)) n)
            ((and (> n 9223372036854775807) (<= n 18446744073709551615))
             (- n (* 2 9223372036854775808)))
            (#t (raise `(out-bounds-i64 ,n))))))

(define (f32 env::struct n)
   (wnumber->number n))

(define (f64 env::struct n)
   (wnumber->number n))

(define (u32 env::struct n)
   (let ((n (wnumber->number n)))
      (cond ((not (integer? n)) (raise `(expected-int ,n)))
            ((and (<= n 4294967295) (>= n 0)) n)
            (#t (raise `(out-bounds-u32 ,n))))))

(define (ht env::struct t)
   (valid-ht env t))

(define (rt::pair env::struct t)
   (valid-rt env t))

(define (typeidx::bint env::struct i)
   (env-last-type-set! env (get-type-index env i))
   (env-last-type env))

;; to work, needs to be called after typeidx
(define (fieldidx::bint env::struct x)
   (unless (vector-ref (env-fields-names env) (env-last-type env))
      (raise `(expected-struct ,(env-last-type env) ,(expand (get-type env (env-last-type env))))))
   (field-get-index env (env-last-type env) x))

(define (dataidx::bint env::struct x)
   (data-get-index env x))

(define (localidx::bint env::struct x)
   (local-get-index env x))

(define (labelidx::bint env::struct x)
   (get-label-index env x))

(define (get-struct-fldts env::struct x::bint)
   (match-case (expand (get-type env x))
      ((struct . ?fldts) fldts)
      (?t (raise `(expected-struct ,x ,t)))))

(define (get-array-ft env::struct x::bint)
   (match-case (expand (get-type env x))
      ((array ?ft) ft)
      (?t (raise `(expected-array ,x ,t)))))

(define (get-label-last-rest env::struct l::bint)
   (let ((t'* (get-label-type env l)))
      (when (null? t'*)
         (raise 'expected-non-empty-result))
      (multiple-value-bind (t* tl) (split-at t'* (- (length t'*) 1))
         (values t* (car tl)))))

(read-table *instruction-types* "instruction-types.sch")

; the following function implements subsumption (section 3.4.12) in an
; syntax-directed way
(define (check-stack::pair-nil env::struct st::pair-nil ts::pair-nil)
   (define (aux::pair-nil st::pair-nil ts::pair-nil)
      (cond ((null? st)
             (unless (null? ts) (raise `(empty-stack ,ts)))
             '())
            ((null? ts) st)
            ((equal? st '(poly)) '(poly))
            ((<vt= env (car st) (car ts))
             (aux (cdr st) (cdr ts)))
            (#t (raise `(non-matching-stack ,(car st) ,(car ts))))))
   (aux st (reverse ts)))

(define (check-block::pair-nil env::struct body::pair-nil t::pair-nil
                               bt::pair #!optional (l #f))
   (let ((loc-init (env-locals-types env)))
      (push-label! env l t)
      (multiple-value-bind (i st) (valid-instrs env body (car bt))
         (let ((st-rst (check-stack env st (cadr bt))))
            (unless (or (null? st-rst) (eq? 'poly (car st-rst)))
               (raise `(value-left-stack ,st-rst))))
         (pop-label! env)
         (env-locals-types-set! env loc-init)
         i)))

(define (valid-blockinstr env::struct i::pair st::pair-nil)
   (match-case i
      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.183
      ((block (and (? ident?) ?l) . ?body)
       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (values bt `(block ,bt ,(check-block env tl (cadr bt) bt l)) '())))
      ((block . ?rst)
       (valid-blockinstr env `(block ,(gensym "$unnamed-label") ,@rst) st))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.184
      ((loop (and (? ident?) ?l) . ?body)
       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (values bt `(loop ,bt ,(check-block env tl (car bt) bt l)) '())))
      ((loop . ?rst)
       (valid-blockinstr env `(loop ,(gensym "$unnamed-label") ,@rst) st))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.185
      ((if (and (? ident?) ?l) . ?body)
       (define (get-tl/then/else l::pair-nil)
          (match-case l
             (((then . ?then) ((kwote else) . ?else)) (values '() then else))
             (((then . ?then)) (values '() then '()))
             ((?hd . ?tl)
              (multiple-value-bind (tl then else) (get-tl/then/else tl)
                 (values (cons hd tl) then else)))
             (else (raise `(expected-then/else ,l)))))

       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (multiple-value-bind (tl then else) (get-tl/then/else tl)
             (values `((,@(car bt) i32) ,(cadr bt))
                     `(if ,bt ,(check-block env then (cadr bt) bt l)
                              ,(check-block env else (cadr bt) bt l))
                     tl))))
      ((if . ?rst)
       (valid-blockinstr env `(if ,(gensym "$unnamed-label") ,@rst) st))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.186
      ((try_table (and (? ident?) ?l) . ?body)
       (define (valid-catch/get-body l::pair-nil)
          (match-case l
             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.187
             (((catch ?tag ?lab) . ?tl)
              (let* ((x (tagidx env tag))
                     (l (labelidx env lab))
                     (t* (cadr (expand (tag-get-type env x))))
                     (lt (get-label-type env l)))
                 (unless (<res= env t* lt)
                    (raise `(non-matching-catch ,x ,l ,t* ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons `(catch ,l ,x) c) tl))))

             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.188
             (((catch_ref ?tag ?lab) . ?tl)
              (let* ((x (tagidx env tag))
                     (l (labelidx env lab))
                     (t* (cadr (expand (tag-get-type env x))))
                     (lt (get-label-type env l)))
                 (unless (<res= env (append t* '((ref exn))) lt)
                    (raise `(non-matching-catch-ref ,x ,l ,t* ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons `(catch_ref ,l ,x) c) tl))))

             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.189
             (((catch_all ?lab) . ?tl)
              (let* ((l (labelidx env lab))
                     (lt (get-label-type env l)))
                 (unless (null? lt)
                    (raise `(non-empty-label-catch-all ,l ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons `(catch_all ,l) c) tl))))

             ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.190
             (((catch_all_ref ?lab) . ?tl)
              (let* ((l (labelidx env lab))
                     (lt (get-label-type env l)))
                 (unless (<res= env '((ref exn)) lt)
                    (raise `(non-matching-catch-all-ref ,l ,lt)))
                 (multiple-value-bind (c tl) (valid-catch/get-body tl)
                    (values (cons `(catch_all_ref ,l) c) tl))))
             (?tl (values '() tl))))
       (multiple-value-bind (bt tl) (valid-blocktype/get-tl env body)
          (multiple-value-bind (c tl) (valid-catch/get-body tl)
             (values bt `(try_table ,bt ,c
                          ,(check-block env tl (cadr bt) bt l)) '()))))
      ((try_table . ?rst)
       (valid-blockinstr env `(try_table ,(gensym "$unnamed-label") ,@rst) st))

      (else (raise `(unknown-opcode ,(car i))))))

;; returns the type of the given instruction, the desuggared instruction and
;; the tail in case it is in s-expression format
;;
;; for instance with (i32.add (i32.const 0) (i32.const 0)), it will return:
;; (values (i32.add) ((i32 i32) (i32)) ((i32.const 0) (i32.const 0)))
(define (typeof-instr/instr/tl env::struct i::pair st::pair-nil)
   (if (hashtable-contains? *instruction-types* (car i))
       (let* ((v (hashtable-get *instruction-types* (car i)))
              (exp-args (car v))
              (t (cadr v))
              (k (length exp-args)))
          (when (< (length (cdr i)) k)
             (raise `(not-enough arguments ,i ,exp-args)))
          (multiple-value-bind (giv-args tl) (split-at (cdr i) k)
             (let ((args (map (lambda (f x) (f env x)) exp-args giv-args)))
                (values (if (procedure? t) (apply t env args) t)
                         `(,(car i) ,@args) tl))))
       (valid-blockinstr env i st)))

(define (adhoc-instr?::bool s)
   (or (eq? s 'br_table) (eq? s 'br_on_null) (eq? s 'ref.as_non_null)))

(define (adhoc-instr env::struct i::pair st::pair-nil)
   (match-case i
      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.193

      ; to avoid having to compute a greatest lower bound, we check each label's
      ; type against the stack, which will serve as a lower bound in absence of
      ; failure and if all the label's arity are the same
      ((br_table ?lab . ?rst)
       (let* ((l (labelidx env lab))
              (n (length (get-label-type env l))))

          (define (get-label/tl l::pair-nil)
             (match-case l
                (((and (? idx?) ?lab) . ?tl)
                    (multiple-value-bind (ls tl) (get-label/tl tl)
                       (values (cons (labelidx env lab) ls) tl)))
                (else (values '() l))))

          (multiple-value-bind (ls tl) (get-label/tl (cdr i))
             (multiple-value-bind (i st) (valid-instrs env tl st)
                (let* ((st (check-stack env st '(i32)))
                       (lower-bound (stack-take st n)))
                   (define (valid-label l::bint)
                      (let ((lt (get-label-type env l)))
                         (unless (<res= env lower-bound lt)
                            (raise `(non-matching ,lower-bound ,lt)))))
                   (for-each valid-label ls)
                   (values (append i `((br_table ,ls))) '(poly)))))))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.194
      ((br_on_null ?lab . ?tl)
       (let* ((l (labelidx env lab))
              (t* (get-label-type env l)))
          (multiple-value-bind (i st) (valid-instrs env tl st)
             (when (null? st)
                (raise 'empty-stack-reftype))
             (let ((ht (cond ((reftype? (car st)) (reftype->heaptype (car st)))
                             ((eq? 'poly (car st)) 'bot)
                             (#t (raise `(expected-reftype-stack ,st))))))
               (values (append i `((br_on_null ,l)))
                       (append (cons `(ref ,ht) (reverse t*))
                               (check-stack env (stack-drop st 1) t*)))))))

      ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.100
      ((ref.as_non_null ?lab . ?tl)
       (multiple-value-bind (i st) (valid-instrs env tl st)
          (when (null? st)
             (raise 'empty-stack-reftype))
          (let ((ht (cond ((reftype? (car st)) (reftype->heaptype (car st)))
                          ((eq? 'poly (car st)) 'bot)
                          (#t (raise `(expected-reftype-stack ,st))))))
             (values (append i '((ref.as_non_null)))
                     (cons `(ref ,ht) (stack-drop st 1))))))))

;; returns the desuggared instructions and the new stack state
(define (valid-instrs env::struct l::pair-nil st::pair-nil)
   (define (valid-instr i::pair st::pair-nil)
      (with-default-value env (values '((error)) '(poly)) `(at-instruction ,i)
            (if (adhoc-instr? (car i))
                (adhoc-instr env i st)
                (multiple-value-bind (t i tl) (typeof-instr/instr/tl env i st)
                   (multiple-value-bind (tl st) (valid-instrs env tl st)
                      (for-each (lambda (x) (local-init! env x)) (cddr t))
                      (let ((st (check-stack env st (car t))))
                             ; t : ... -> (poly) ?
                         (if (and (not (null? (cadr t))) (eq? 'poly (caadr t)))
                             ; try to avoid this append (cps by hand ?)
                             (values (append tl (list i)) '(poly))
                             (values (append tl (list i))
                                     (append (reverse (cadr t)) st)))))))))
   (if (null? l)
       (values '() st)
       (if (pair? (car l))
           (multiple-value-bind (is st) (valid-instr (car l) st)
              (multiple-value-bind (tl st) (valid-instrs env (cdr l) st)
                 (values (append is tl) st)))
           (raise `(expected-instruction ,(car l))))))

(define (valid-expr env::struct l::pair-nil)
   (valid-instrs env l '()))

;; section 3.5.3
(define (valid-loct::struct env::struct t)
   (let ((vt (valid-vt env t)))
      (make-local-var (defaultable? vt) vt)))

;; section 6.6.4
(define (valid-names/local/get-tl env::struct l::pair-nil)
   (match-case l
      (((local (and (? ident?) ?id) ?vt) . ?tl)
       (multiple-value-bind (n l tl) (valid-names/local/get-tl env tl)
          (values (cons id n) (cons (valid-loct env vt) l) tl)))
      (((local . ?vts) . ?tl)
       (multiple-value-bind (n l tl) (valid-names/local/get-tl env tl)
          (values (append (make-list (length vts) #f) n)
                  (append (map-env valid-loct env vts) l) tl)))
      (else (values '() '() l))))

;; section 6.6.3 and 3.5.12
(define (valid-importdesc env::struct d)
   (match-case d
      ((func (and (? ident?) ?id) . ?rst)
       (func-add-name! env id)
       (valid-importdesc env `(func ,@rst)))
      ((memory (and (? ident?) ?id) . ?rst)
       (mem-add-name! env id))
      ((global (and (? ident?) ?id) . ?rst)
       (global-add-name! env id)
       (valid-importdesc env `(global ,@rst)))
      ((tag (and (? ident?) ?id) . ?rst)
       (tag-add-name! env id)
       (valid-importdesc env `(tag ,@rst)))

      ((func . ?ft)
       (multiple-value-bind (p r) (valid-param/result env ft)
          (vector-set! *funcs* (env-nfunc env) #f)
          (func-add! env `(deftype ((sub final (func ,p ,r))) ,0))))
      ((global ?gt)
       (vector-set! *globals* (env-nglobal env) #f)
       (global-add! env (valid-gt env gt)))
      ((memory . ?mt)
       (mem-add! env (valid-mt env mt)))
      ((tag . ?tt)
       (multiple-value-bind (p r) (valid-param/result env tt)
          (tag-add! env `(deftype ((sub final (func ,p ,r))) ,0))))
      (else (raise `(expected-importdesc ,d)))))

; section 6.6.9 and
(define (valid-exportdesc env::struct d)
   (match-case d
      ((func ?id) (funcidx env id))
      ((memory ?id) (memidx env id))
      ((global ?id) (globalidx env id))
      ((tag ?id) (tagidx env id))
      (else (raise `(expected-exportdesc ,d)))))

(define (type-pass-mf env::struct m)
   (with-handler
      (match-lambda
         ((in-module ?- ?e) (raise `(in-module ,m ,e)))
         (?e
           (if (isa? e &error)
               (raise e))
          (raise `(in-module ,m ,e))))
      (match-case m
         ; first abreviation of 6.4.9
         ((type . ?-) (type-pass-mf env `(rec ,m)))
         ((rec . ?l)
          (let ((x (env-ntypes env)))
             (valid-rect env (clean-mod-rectype! env l x) x)
             #f))
         (else m))))

(define (decorate::epair m::epair l::pair)
   (econs (car l) (cdr l) (cer m)))

(define-struct function
   type
   formals
   body
   locs
   pos)

(define-struct global
   type
   body
   pos)

(define-struct data
   body)

(define *funcs* (make-vector 1000000))
(define *globals* (make-vector 1000000))
(define *data* (make-vector 1000000))
(define *exports* '())

(define (env-pass-mf env::struct m)
   (with-default-value env #f `(in-module ,m ,e)
      (match-case m
         (#f #f)
         ; section 6.6.4 (abbreviations)
         ((func (export (? string?)) . ?rst)
          (env-pass-mf env (decorate m `(func ,@rst))))
         ((func (import (and (? string?) ?nm1) (and (? string?) ?nm2)) . ?rst)
          (env-pass-mf env (decorate m `(import ,nm1 ,nm2 (func ,@rst)))))
         ; section 6.6.4
         ((func (and (? ident?) ?id) . ?rst)
          (func-add-name! env id)
          (env-pass-mf env (decorate m `(func ,@rst))))
         ((func . ?rst)
          (multiple-value-bind (args f tl) (valid-tu/get-tl env rst)
             (vector-set! *funcs* (env-nfunc env)
                          (make-function f args tl '() (cer m)))
             (func-add! env `(deftype ((sub final (func ,@f))) ,0))))

         ((data (and (? ident?) ?id) (memory ?memidx) (offset . ?expr) . ?-)
          (raise 'todo))
          ; section 6.6.12
         ((data (and (? ident?) ?id) . ?rst)
          (hashtable-put! (env-data-table env) id (env-ndata env))
          (env-pass-mf env (decorate m `(data ,@rst))))
         ((data . ?rst)
          (for-each (lambda (s) (unless (string? s)
                                   (raise `(expected-string ,s)))) rst)
          ; section 3.5.9 - passive data segments are always valid, we currently
          ; only support those
          (vector-set! *data* (env-ndata env) (make-data rst))
          (env-ndata-set! env (+ 1 (env-ndata env))))

          ; section 6.6.7
         ((global (and (? ident?) ?id) . ?rst)
          (global-add-name! env id)
          (env-pass-mf env (decorate m `(global ,@rst))))
         ((global (export (? string?)) . ?rst)
          (env-pass-mf env (decorate m `(global ,@rst))))
         ((global (import (and (? string?) ?nm1) (and (? string?) ?nm2)) . ?rst)
          (env-pass-mf env (decorate m `(import ,nm1 ,nm2 (global ,@rst)))))
         ((global ?gt . ?e)
          (define (add-func-refs! l)
             (cond ((pair? l)
                    (if (eq? 'ref.func (car l))
                        (hashtable-put! (env-refs env) (cadr l) #t)
                        (for-each add-func-refs! l)))))

          (let ((t (valid-gt env gt)))
             (vector-set! *globals* (env-nglobal env) (make-global t e (cer m)))
             (global-add! env t)
             (add-func-refs! e)))

         ; section 6.6.3
         ((import (and (? string?) ?mod) (and (? string?) ?nm) ?d)
          (valid-importdesc env d))

         ; we don't have all the indices yet, so we can't validate an
         ; exportdesc's
         ((export (? string?) ?d) (set! *exports* (cons d *exports*)))

         ; section 6.6.8 and 3.5.7
         ((tag (and (? ident?) ?id) . ?rst)
          (tag-add-name! env id)
          (env-pass-mf env (decorate m `(tag ,@rst))))
         ((tag . ?tu)
          (multiple-value-bind (p r) (valid-param/result env tu)
             (unless (null? r)
                (tag-add! env '(deftype ((sub final (error))) ,0))
                (raise `(non-empty-tag-result ,r)))
             (tag-add! env `(deftype ((sub final (func ,p ,r))) ,0))))

         ; section 6.6.6
         ((memory (and (? ident?) ?id) . ?rst)
          (mem-add-name! env id)
          (env-pass-mf env (decorate m `(memory ,@rst))))
         ((memory (export (? string?)) . ?rst)
          (env-pass-mf env (decorate m `(memory ,@rst))))
         ((memory (import (and (? string?) ?nm1) (and (? string?) ?nm2)) . ?rst)
          (env-pass-mf env (decorate m `(import ,nm1 ,nm2 (memory ,@rst)))))
         ((memory . ?mt)
          (mem-add! env (valid-mt env mt)))

         (else (raise 'expected-modulefield)))))

(define (valid-global env::struct g::struct x::bint)
   (let ((old-nglobal (env-nglobal env)))
      ; global can only refer to the previous ones
     (env-nglobal-set! env x)
     (multiple-value-bind (e t') (valid-expr env (global-body g))
        (when (and (length>=? t' 2) (not (eq? 'poly (cadr t'))))
           (raise `(too-much-value-stack ,t')))
        (when (or (null? t') (eq? 'poly (car t')))
           (raise `(missing-value-stack ,(global-type g))))
        (unless (<vt= env (car t') (cadr (global-type g)))
           (raise `(non-matching-globaltype ,(car t') ,(global-type g))))
        (when (non-constant-expr? env e)
           (raise `(non-constant-global ,(non-constant-expr? env e))))
        (env-nglobal-set! env old-nglobal)
        (global-body-set! (vector-ref *globals* x) e))))

(define (valid-function env::struct f::struct x::bint)
   (multiple-value-bind (n lts body)
      (valid-names/local/get-tl env (function-body f))
      (env-locals-names-set! env (append (function-formals f) n))
      (env-locals-types-set!
       env
       (list->vector (append (map (lambda (vt) (make-local-var #t vt))
                                  (car (function-type f))) lts)))
      (function-locs-set! f (map local-var-type lts))
      (env-return-set! env (cadr (function-type f)))
      (function-body-set! f
       (replace-exception 'empty-stack 'no-return-value-stack
          (check-block env body (cadr (function-type f))
                       `(() ,(cadr (function-type f))))))))

(define (valid-functions env::struct a::bint b::bint)
   (let ((x (env-nfunc env)))
      (do ((i 0 (+fx i 1)))
          ((>=fx (+fx (*fx a i) b) x))
         (let ((f (vector-ref *funcs* (+fx (*fx a i) b))))
            (when f
               (with-handler format-exn
                  (valid-function env f (+fx (*fx a i) b)))
               (unless (null? (env-error-list env))
                  (format-exn `(at-pos ,(function-pos f) ""))
                  (for-each format-exn (env-error-list env))
                  (env-error-list-set! env '())))))))

(define (valid-globals env::struct)
   (let ((x (env-nglobal env)))
      (do ((i 0 (+fx i 1)))
          ((>=fx i x))
         (let ((g (vector-ref *globals* i)))
           (when g
               (with-handler format-exn
                  (valid-global env g i))
               (unless (null? (env-error-list env))
                  (format-exn `(at-pos ,(global-pos g) ""))
                  (for-each format-exn (env-error-list env))
                  (env-error-list-set! env '())))))))

(define (valid-exports env::struct)
   (for-each (lambda (d) (valid-exportdesc env d)) *exports*))

(define (format-exn e)
   (if (isa? e &error)
       (raise e))
   (set! error-encountered? #t)
   (define (rep msg obj)
     (with-handler error-notify
        (when (epair? obj)
           (error/location "val" msg "" (cadr (cer obj)) (caddr (cer obj))))))
   (define (rep/pos msg pos)
     (with-handler error-notify
        (error/location "val" msg "" (cadr pos) (caddr pos))))

   (match-case e
      ((in-module ?m ?e)
       (unless silent (rep "in module" m))
       (format-exn e))
      ((at-pos ?p ?e)
       (unless silent (rep/pos "in module" p))
       (format-exn e))
      ((at-instruction ?i ?e)
       (unless silent (rep "at instruction" i))
       (format-exn e))
      ("" #f)
      (else
       (when (number? keep-going)
          (set! keep-going (-fx keep-going 1))
          (if (=fx 0 keep-going)
              (set! keep-going #f)))
       (display "***ERROR: " (current-error-port))
       (display e (current-error-port))
       (newline (current-error-port))))
   (unless keep-going
        (exit 1)))

(define (copy-env::struct env::struct)
   (make-env
    (env-ntypes env)
    (env-types-table env)
    (env-types-vector env)
    (env-fields-names env)

    0
    '()
   ; relevant information can only be accessed by index ; find the index of a
   ; name first to access by name
   (make-vector 0)

   0
   '()
   (make-vector 10000)

   (env-nfunc env)
   (env-func-table env)
   (env-func-types env)
   (env-func-names env)

   (env-refs env)

   (env-ndata env)
   (env-data-table env)

   (env-nglobal env)
   (env-global-table env)
   (env-global-types env)
   (env-global-names env)

   (env-ntag env)
   (env-tag-table env)
   (env-tag-types env)
   (env-tag-names env)

   (env-nmem env)
   (env-mem-table env)
   (env-mem-types env)
   (env-mem-names env)

   #f
   #f
   '()))

(define (valid-file f::pair-nil)
   (let ((env (make-env)))
      (define (mf-pass/handle-error f)
         (lambda (m)
            (let ((clean-m (with-handler format-exn (f env m))))
               (unless (null? (env-error-list env))
                 (format-exn `(in-module ,m ""))
                 (for-each format-exn (env-error-list env))
                 (env-error-list-set! env '()))
               clean-m)))

      (match-case f
         ((or (module (? ident?) . ?mfs) (module . ?mfs))
          (let* ((type-mfs (map (mf-pass/handle-error type-pass-mf) mfs)))
             (for-each (mf-pass/handle-error env-pass-mf) type-mfs)

             (if (= nthreads 1)
                 (begin
                    (valid-globals env)
                    (valid-exports env)
                    (valid-functions env 1 0))
                 (let ((ts (cons
                            (instantiate::pthread
                             (body
                              (lambda ()
                                 (valid-globals (copy-env env))
                                 (valid-functions (copy-env env) nthreads 0))))
                            (cons
                             (instantiate::pthread
                              (body
                               (lambda ()
                                  (valid-exports (copy-env env))
                                  (valid-functions (copy-env env) nthreads 1))))
                             (list-tabulate
                              (-fx nthreads 2)
                              (lambda (i)
                                 (instantiate::pthread
                                  (body
                                   (lambda ()
                                      (valid-exports (copy-env env))
                                      (valid-functions (copy-env env) nthreads
                                                       (+fx 2 i)))))))))))
                    (map thread-start-joinable! ts)
                    (map thread-join! ts))))))))

(define (main argv)
   (define input-file #f)
   (args-parse (cdr argv)
      ((("--help" "-h") (help "Display this help message"))
       (args-parse-usage #f))
      ((("--keep-going" "-k") (help "Continue when encountering an error"))
       (set! keep-going #t))
      (("--stop-after" ?n (help "Stop after encountering N errors"))
       (set! keep-going (string->number n)))
      (("-s" (help "Display less verbose error messages"))
       (set! silent #t))
      (("-j" ?n (help "Use multiple threads"))
       (set! nthreads (string->number n)))
      (else
       (set! input-file else)))
   (if input-file
       (call-with-input-file input-file
          (lambda (ip)
             (let ((f (valid-file (read ip #t))))
                (if error-encountered?
                    (exit 1)))))))
