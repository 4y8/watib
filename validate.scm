(module validate
   (library srfi1)
   (include "read-table.sch")
   (main main))

;;; we force everywhere the number of type indices after sub final? to be one;
;;; even though forms with more than one type are syntactically correct, they
;;; are never valid

(define keep-going #f)

(define (report f msg obj)
   (if (epair? obj)
       (error/location f msg obj (cadr (cer obj)) (caddr (cer obj)))
       (error f msg obj)))

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
; section 6.4.4 - Abbreviations
(read-table *reftypes-abbreviations* "type-abbreviations.sch")
(define (reftype-abv?::bool t)
   (hashtable-contains? *reftypes-abbreviations* t))
(define (reftype?::bool t)
   (or (reftype-abv? t) (and (pair? t) (equal? 'ref (car t)))))
(define (valtype?::bool t)
   (or (reftype? t) (numtype? t) (vectype? t)))
(define (addrtype?::bool t)
   (or (equal? t 'i32) (equal? t 'i64)))

;; section 3.2.20
(define (defaultable? t)
   (or (vectype? t)
       (numtype? t)
       (and (reftype? t) (nullable? t))))

;; section 2.3.8
(define (unpack t)
   (if (packedtype? t) 'i32 t))

(define (unpack-ft t)
   (unpack (cadr t)))

(define-macro (replace-exception e e' . body)
   `(with-handler
       (match-lambda
         ((,e . ?tl) (raise (cons ,e' tl)))
         (?e (raise e)))
       ,@body))

(define-macro (map-env f env . l)
   `(map (lambda (x) (,f ,env x)) ,@l))

;; like every but returns #f if the lists are not of the same length
(define (every' f . lists)
   (if (any null? lists)
       (every null? lists)
       (and (apply f (map car lists)) (apply every' f (map cdr lists)))))

;; evaluation order is not unspecified in the standard, we enforce one here

;; not tail recursive, but not destined to be used no big inputs (rec vectors
;; arent't large)
(define (map-seq f . lists)
   (if (any null? lists)
       '()
       (cons (apply f (map car lists)) (apply map-seq f (map cdr lists)))))

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

;; section 3.1.6
(define-struct env
   (ntypes 0)
   ; to access with the names, contains indices
   (types-table (make-hashtable))
   ; to access with the index, contains lists (type name), the name is stored
   ; for error reporting
   (types-vector (make-vector 0))
   (fields-names (make-vector 0))

   (locals-names '())
   ; relevant information can only be accessed by index ; find the index of a
   ; name first to access by name
   (locals-init (make-vector 0))
   (locals-types (make-vector 0))

   (labels-names '())
   (labels-types (make-vector 0))

   ; like types
   (nfuncs 0)
   (funcs-table (make-hashtable))
   (funcs-types (make-vector 0))

   (refs (make-hashtable))

   (ndata 0)
   (data-table (make-hashtable))

   (return #f))

(define (get-type-index::bint env::struct t)
   (cond
    ((number? t)
     (if (< t (env-ntypes env))
         t
         (raise `(typeidx-out-of-range ,(env-ntypes env) ,t))))
    ((ident? t)
     (if (hashtable-contains? (env-types-table env) t)
         (hashtable-get (env-types-table env) t)
         (raise `(unknown-type ,t))))
    (#t (raise `(expected-typeidx ,t)))))

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

(define (get-func-index::bint env::struct x)
   (cond
    ((number? x)
     (if (< x (env-nfuncs env))
         x
         (raise `(funcidx-out-of-range ,(env-ntypes env) ,x))))
    ((ident? x)
     (if (hashtable-contains? (env-funcs-table env) x)
         (hashtable-get (env-funcs-table env) x)
         (raise `(unknown-function ,x))))
    (#t (raise `(expected-funcidx ,x)))))

(define (get-func-type::pair env::struct x::bint)
   (vector-ref (env-funcs-types env) x))

(define (get-field-index::bint env::struct t::bint f)
   (define (index::bint lst::pair-nil i::bint)
      (cond ((null? lst) (raise `(unknown-field ,t ,f)))
            ((equal? (car lst) f) i)
            (#t (index (cdr lst) (+fx 1 i)))))
   (let ((v (vector-ref (env-fields-names env) t)))
      (cond
       ((number? f)
        (if (< f (length v))
         t
         (raise `(fieldidx-out-of-range ,t (length v) ,f))))
    ((ident? f)
     (index v 0))
    (#t (raise `(expected-fieldidx ,t))))))

(define (get-data-index::bint env::struct x)
   (cond ((integer? x)
          (if (< x (env-ndata env))
              x
              (raise `(dataidx-out-range ,(env-ndata env) ,x))))
         ((ident? x)
          (if (hashtable-contains? (env-data-table env) x)
              (hashtable-get (env-data-table env) x)
              (raise `(unknown-data ,x))))
         (#t (raise `(expected-dataidx ,x)))))

(define (local-ident-idx env::struct l)
   (define (index::bint lst::pair-nil i::bint)
      (cond ((null? lst) (raise `(unknown-local ,l)))
            ((equal? (car lst) l) i)
            (#t (index (cdr lst) (+fx 1 i)))))
   (index (env-locals-names env) 0))

(define (local-init! env::struct l)
   '())

(define (local-init?::bool env::struct l)
   (cond
    ((number? l)
     (if (< l (vector-length (env-locals-init env)))
         (vector-ref (env-locals-init env) l)
         (raise `(localidx-out-of-range ,(env-locals-init env) ,l))))
    ((ident? l)
     (vector-ref (env-locals-init env) (local-ident-idx env l)))
    (#t `(expected-local ,l))))

;; deftypes (rec subtypes*).i are represented as (deftype subtypes* i))
;; (rec i) are represented as (rec i)
(define (deftype?::bool t)
   (and (pair? t) (equal? 'deftype (car t))))

(define (rectype?::bool t)
   (and (pair? t) (equal? 'rec (car t))))

;; avoid full expansion when it is not needed
(define (deftype-head t)
   (match-case (list-ref (cadr t) (caddr t))
      ((or (sub final ?- (?hd . ?-))
           (sub final (?hd . ?-))
           (sub ?- (?hd . ?-))
           (sub (?hd . ?-)))
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
      ((symbol? t1) (equal? t1 t2))
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

(define (eq-clos-dt?::bool env::struct t1 t2)
   (match-case (cons t1 t2)
      (((deftype ?sts1 ?i) . (deftype ?sts2 ?i))
       (every' (lambda (st1 st2) eq-clos-st? env st1 st2) sts1 sts2))
      (else #f)))

;; section 3.1.3

;; we use the same slopiness as in eq-clos-st?
(define (unroll-st sts st)
   (cond ((or (symbol? st) (idx? st)) st)
         ((and (rectype? st)) `(deftype ,sts ,(cadr st)))
         ((pair? st) (map (lambda (st) (unroll-st sts st)) st))
         ((null? st) st)
         ; we don't report here like elsewhere, because this function doesn't
         ; follow structural rules
         (#t (raise `(internal-unroll-st ,st)))))

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
    ((symbol? st) st)
    ((pair? st) (map (lambda (st) (roll-st env st x n)) st))
    ((null? st) st)
    (#t (raise `(internal-roll-st ,st)))))

(define (roll-rect::pair env::struct rect::pair x::bint)
   (let ((n (+fx x (length (cdr rect)))))
      `(rec ,@(map (lambda (st) (roll-st env st x n)) (cdr rect)))))

(define (roll* env::struct rect::pair x::bint)
   (let ((rolled-rect (roll-rect env rect x)))
      (list-tabulate (length (cdr rect))
                     (lambda (i) `(deftype ,(cdr rect) ,i)))))

(define (expand t)
   (match-case (unroll-dt t)
      ((or (sub final ?- ?ct)
           (sub final ?ct)
           (sub ?- ?ct)
           (sub ?ct))
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
         ((equal? t1 'bot) #t)
         ((idx? t1) (<ht= env (get-type env t1) t2))
         ((idx? t2) (<ht= env t1 (get-type env t2)))
         ((equal? t2 'any) (<ht= env t1 'eq))
         ((equal? t2 'eq)
          (or (equal? t1 'i31)
              (equal? t1 'struct)
              (equal? t1 'array)
              (and (deftype? t1) (<ht= env (deftype-head t1) 'eq))))
         ((and (deftype? t1) (symbol? t2))
          (equal? (deftype-head t1) t2))
         ((equal? 'none t1) (<ht= env t2 'any))
         ((equal? 'nofunc t1) (<ht= env t2 'func))
         ((equal? 'noextern t1) (<ht= env t2 'extern))
         (#t #f)))

(define (nullable?::bool rt::pair)
   (equal? (cadr rt) 'null))

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
   (or (equal? t1 'bot)
       (and (numtype? t1) (numtype? t2) (equal? t1 t2))
       (and (reftype? t1) (reftype? t2) (<rt= env t1 t2))))

;; section 3.3.6
(define (<res=::bool env l1 l2)
   (every' (lambda (t1 t2) (<vt= env t1 t2)) l1 l2))

;; instruction types [t1*] --->x* [t2*] are represented as ((t1*) (t2*) (x*)),
;; where x is the index (as an integer of the variable)

;; we do not need to prefix it with a special symbol (what we did for deftypes),
;; because instruction types are not added as an option for another kind of type
;; (i.e. there is no t ::= ... | instrtype in the spec)

;; section 3.3.7
(define (<it=::bool env::struct t1::pair t2::pair)
   (let* ((t11 (car t1))
          (t12 (cadr t1))
          (x1  (caddr t1))
          (tt21 (car t2))
          (tt22 (cadr t2))
          (x2  (caddr t2))
          (n11 (length t11))
          (n12 (length t12))
          (n21 (length tt21))
          (n22 (length tt22)))
      (and
      ; bind some values and use split to avoid redundant computations ?
       (and (equal? (- n21 n11) (- n22 n12))
            (equal? (take tt21 (- n22 n12)) (take tt22 (- n22 n12))))
       (<res= env (drop tt21 (- n22 n12)) t11)
       (<res= env t12 (drop tt22 (- n22 n11)))
       (every (lambda (x) (or (member x x1) (local-init? env x))) x2))))

;; function types are represented like instruction types but without the locals

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
(define (<fldt=::bool env::struct t1::pair t2::pair)
   (define (<st=::bool t1 t2)
     (or
      (and (packedtype? t1) (equal? t1 t2))
      (and (valtype? t1) (valtype? t2) (<vt= env t1 t2))))
   (match-case (cons t1 t2)
      (((const ?st1) . (const ?st2))
       (<st= st1 st2))
      (((var ?st1) . (var ?st2))
       (and (<st= st1 st2) (<st= st2 st1)))
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

;; section 3.3.16
(define (<tagt=::bool env::struct t1 t2)
   (and (<dt= env t1 t2) (<dt= env t2 t1)))

;; section 3.3.17
(define (<et=::bool env::struct t1 t2)
   (match-case (cons t1 t2)
      (((func ?ft1) . (func ?ft2))
       (<funct= env ft1 ft2))
      (((table ?tt1) . (table ?tt2))
       (<tt= env tt1 tt2))
      (((mem ?mt1) . (mem ?mt2))
       (<mt= mt1 mt2))
      (((global ?gt1) . (global ?gt2))
       (<gt= env gt1 gt2))
      (((tag ?tagt1) . (tag ?tagt2))
       (<tagt= env tagt1 tagt2))
      (else #f)))

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

;; section 3.2.7
(define (valid-res env::struct l)
   (for-each (lambda (t) (valid-vt env t)) l))

;; see where it is used
;; section 3.2.8
(define (valid-it env::struct t)
   (match-case t ; the shape test might be useless
      ((?t1 ?t2 ?x)
       (valid-res env t1)
       (valid-res env t2)
       (if (list? x) ; once again, might be useless
           (for-each (lambda (v) (local-init? env v)) x)
           (raise `(expected-locals ,x))))
      (else (raise `(expected-instructiontype ,t)))))

;; section 3.2.9 and 6.4.6
(define (valid-param/result env::struct l)
   (match-case l
      (() (values '() '()))
      (((param (? ident?) ?vt) . ?tl)
       (multiple-value-bind (p r) (valid-param/result env tl)
          (values (cons (valid-ht env vt) p))))
      (((param . ?vts) . ?tl)
       (multiple-value-bind (p r) (valid-param/result env tl)
          (values (append (map-env valid-vt env vts) p) r)))
      (((result . ?-) . ?-)
       (define (get-results l)
          (match-case l
             (((result . ?vts) . ?tl)
              (append (map-env valid-vt env vts) (get-results tl)))
             (() '())
             (else (raise `(expected-functiontype ,l)))))
       (values '() (get-results l)))
      (else (raise `(expected-functiontype ,l)))))

;; section 3.2.11 and 6.4.7
(define (valid-fldt env::struct t)
   (define (valid-st t)
      (replace-exception 'expected-valtype 'expected-storagetype
         (if (packedtype? t)
             t
             (valid-vt env t))))
   (match-case t
      ((mut ?st) `(var ,(valid-st st)))
      (?st (replace-exception 'expected-storagetype 'expected-fieldtype
              `(const ,(valid-st st))))))

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
                  (append (map (lambda (-) (gensym)) fldts) names))))
      (else (raise `(expected-fields ,l)))))

;; section 3.2.10
(define (valid-ct env::struct t)
   (match-case t
      ((func . ?p/r)
       (multiple-value-bind (p r) (valid-param/result env p/r)
          `(func ,p ,r)))
      ((array ?fldt) `(array ,(valid-fldt env fldt)))
      ((struct . ?fldts)
       (multiple-value-bind (fields names) (valid-fields/names env fldts)
          ; we assume valid-ct is only called just before assigning a new type
          ; index
          (vector-set! (env-fields-names env) (env-ntypes env) names)
          (cons 'struct fields)))
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
          (cons (valid-st (car l) x) (valid-rec-list (cdr l) (+ x 1)))))

   `(rec ,(valid-rec-list l x)))

;; section 3.2.14
(define (valid-lim l::pair k::llong)
   ; no shape check
   (unless (<= (car l) k)
      (raise `(invalid-bound ,(car l) k)))
   (unless (or (null? (cdr l)) (<= (cdr l) k))
      (raise `(invalid-bound ,(cdr l) k)))
   l)

;; section 3.2.15 and 6.4.14
(define (valid-tt env::struct t)
   (match-case t
      ; coherence problem as we don't check niether l's validity here nor in the
      ; previous function ; will have to see this later
      (((and ?at (? addrtype?)) ?l ?rt)
       (list at
             (valid-lim l (bit-lshllong #l1 (-llong (type-size at) #l1)))
             (valid-rt env rt)))
      (else (raise `(expected-tabletype ,t)))))

;; section 3.2.16 and 6.4.13
(define (valid-mt t)
   (match-case t
      (((and ?at (? addrtype?)) ?l)
       (valid-lim l (bit-lshllong #l1 (-llong (type-size at) #l16))))
      (else (raise `(expected-memorytype ,t)))))

;; section 3.2.17
;; rest of the validation for types : todo

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
      (define (valid-st st)
         (match-case st
            ((sub final ?ct) `(sub final ,(valid-ct env ct)))
            ((sub final (and ?y (? idx?)) ?ct)
             `(sub final ,y ,(valid-ct env ct)))
            ((sub (and ?y (? idx?)) ?ct)
             `(sub ,y ,(valid-ct env ct)))
            ((sub ?ct)
             `(sub ,(valid-ct env ct)))
            (else
              (replace-exception 'expected-comptype 'expected-subtype
                 `(sub final ,(valid-ct env st))))))
      (let ((rolled-sts (map valid-st sts)))
         (for-each (lambda (i t) (set-type! env (+ x i)
                                            `(deftype ,rolled-sts ,i)))
                   (iota (length sts)) rolled-sts)
         rolled-sts)))

;; section 3.4

(define (wnumber->number n)
   (cond ((number? n) n)
         ((equal? n 'inf) +inf.0)
         ((equal? n '-inf) -inf.0)
         ((equal? n 'nan) +nan.0)
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
   (valid-ht env ht))

(define (funcidx env::struct f)
   (get-func-index env f))

(define (reftype env::struct t)
   (valid-rt env t))

(define last-type #f)

(define (typeidx env::struct i)
   (set! last-type (get-type-index env i))
   last-type)

;; to work, needs to be called after typeidx
(define (fieldidx env::struct x)
   (unless (vector-ref (env-fields-names env) last-type)
      (raise `(expected-struct ,last-type ,(expand (get-type env last-type)))))
   (get-field-index env last-type x))

(define (dataidx env::struct x)
   (get-data-index env x))

(define (get-struct-fldts env::struct x::bint)
   (match-case (expand (get-type env x))
      ((struct . ?fldts) fldts)
      (?t (raise `(expected-struct ,x ,t)))))

(define (get-array-ft env::struct x::bint)
   (match-case (expand (get-type env x))
      ((array ?ft) ft)
      (?t (raise `(expected-array ,x ,t)))))

(read-table *instruction-types* "instruction-types.sch")

;; returns the type of the given instruction, the desuggared instruction and
;; the tail in case it is in s-expression format
;;
;; for instance with (i32.add (i32.const 0) (i32.const 0)), it will return:
;; (values (i32.add) ((i32 i32) (i32)) ((i32.const 0) (i32.const 0)))
(define (typeof-instr/instr/tl env::struct i::pair)
   (unless (hashtable-contains? *instruction-types* (car i))
      (raise `(unknown-opcode ,i)))
   (with-handler
      (lambda (e) (raise `(at-instruction ,i ,e)))
      (let* ((v (hashtable-get *instruction-types* (car i)))
             (exp-args (car v))
             (t (cadr v))
             (k (length exp-args)))
         (when (< k (length (cdr i)))
            (raise `(not-enough arguments ,i ,exp-args)))
         (multiple-value-bind (giv-args tl) (split-at (cdr i) k)
            (let ((args (map (lambda (f x) (f env x)) exp-args giv-args)))
               (values (if (procedure? t) (apply t env args) t)
                       `(,(car i) ,@args) tl))))))

;; returns the desuggared instructions and the new stack state
(define (valid-instrs env::struct l::pair-nil st::pair-nil)
   ; the following function implements subsumption (section 3.4.12) in an
   ; syntax-directed way
   (define (check-stack st::pair-nil ts::pair-nil)
      (cond ((null? st)
             (unless (null? ts) (raise `(empty-stack ,ts))))
            ((null? ts) st)
            ((equal? st '(poly)) '(poly))
            ((<vt= env (car st) (car ts))
             (check-stack (cdr st) (cdr ts)))
            (#t (raise `(non-matching-stack ,(car st) ,(car ts))))))

   (define (valid-instr i::pair st::pair-nil)
      (multiple-value-bind (t i tl) (typeof-instr/instr/tl env i)
         (multiple-value-bind (tl st) (valid-instrs env tl st)
            (check-stack st (car t))
            (if (equal? '(poly) (cadr t))
                ; try to avoid this append (cps by hand ?)
                (values (append tl (list i)) '(poly))
                (values (append tl (list i)) (append (cadr t) (st)))))))

   (if (null? l)
       (values '() st)
       (if (pair? (car l))
           (multiple-value-bind (is st) (valid-instr (car l) st)
              (multiple-value-bind (tl st) (valid-instrs env (cdr l) st)
                 (values (append is tl) st)))
           (raise `(expected-instruction ,(car l))))))

;; section 6.6.13
(define (valid-modulefield env::struct m)
   (with-handler
     (match-lambda
       ((in-module ?- ?e) (raise `(in-module ,m ,e)))
       (?e (raise `(in-module ,m ,e))))
     (match-case m
        ; first abreviation of 6.4.9
        ((type . ?-) (valid-modulefield env `(rec ,m)))
        ((rec . ?l)
         (let ((x (env-ntypes env)))
            (valid-rect env (clean-mod-rectype! env l x) x)))
        ((data (and (? ident?) ?id) (memory ?memidx) (offset . ?expr) . ?-)
         (raise `todo))
        ((data (and (? ident?) ?id) . ?rst)
         (hashtable-put! (env-data-table env) id (env-ndata env))
         (valid-modulefield env `(data ,@rst)))
        ((data . ?rst)
         ; section 6.6.12
         (for-each (lambda (s) (unless (string? s)
                                 (raise `(expected-string ,s))))
                   rst)
         ; section 3.5.9 - passive data segments are always valid
         (env-ndata-set! env (+ 1 (env-ndata env)))
         m)
        (else (raise `(expected-modulefield))))))

(define (valid-file f::pair-nil)
   (let ((env (make-env)))
      (match-case f
         ((or (module (? ident?) . ?mfs) (module . ?mfs) ?mfs)
          (map-env valid-modulefield env mfs)))))

(define (main argv)
   (define input-file #f)
   (args-parse (cdr argv)
      ((("--help" "-h") (help "Display this help message"))
       (args-parse-usage #f))
      (else
       (set! input-file else)))
   (if input-file
       (call-with-input-file input-file
          (lambda (ip)
             (print (valid-file (read ip #t)))))))
