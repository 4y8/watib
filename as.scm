(module binary
   (library srfi1)
   (main main)
   (include "read-table.sch")
   (import leb128))

;;; we currently miss table and complete elem - they are not used by bigloo

(define typeidxs (make-hashtable))
(define funcidxs (make-hashtable))
(define globalidxs (make-hashtable))
(define tagidxs (make-hashtable))
(define memidxs (make-hashtable))
(define dataidxs (make-hashtable))
(define fieldidxs (make-hashtable))

(define fieldtypes (make-hashtable))
(define arraytypes (make-hashtable))
(define funcrefs '())

(define (fresh-var) (gensym "__bigloo_wasm_dummy_id_"))

(read-table *valtype-symbols* "valtype-symbols.sch")
(read-table *type-abbreviations* "type-abbreviations.sch")

(define wasm-as-compat #t)
(define keep-going #f)
(define error-encountered? #f)
(define output-file "a.out.wasm")

(define (report fun s obj)
   (if keep-going
       (begin
          (set! error-encountered? #t)
          (warning fun s obj))
       (error fun s obj)))

(define (valtype-symbol? s)
   (and (symbol? s) (hashtable-contains? *valtype-symbols* s)))

(define (type-abbreviation? s)
   (and (symbol? s) (hashtable-contains? *type-abbreviations* s)))

(define (unreachable-body-end l)
   (match-case l
      (() #f)
      (((or (if (? symbol?) . ?rst) (if . ?rst) (block (? symbol?) . ?rst)
            (block . ?rst) (loop (? symbol?) . ?rst) (loop . ?rst)
            (try_table (? symbol?) . ?rst)(try_table . ?rst)))
       (multiple-value-bind (- - r -) (get-names/params/results/tl rst)
          (null? r)))
      ((br . ?-) #t)
      ((((or return return_call return_call_ref) . ?-) . ?-) #t)
      ((?- . ?rst) (unreachable-body-end rst))))

(define (wnumber? n)
   (cond ((number? n) #t)
         ((equal? n 'inf) #t)
         ((equal? n '-inf) #t)
         ((equal? n 'nan) #t)
         ((symbol? n)
          (let ((s (symbol->string n)))
             (if (and (>= (string-length s) 2) (substring-at? s "0x" 0))
                 (string->number (substring s 2) 16)
                 #f)))
         (#t #f)))

(define (wnumber->number n)
   (cond ((number? n) n)
         ((equal? n 'inf) +inf.0)
         ((equal? n '-inf) -inf.0)
         ((equal? n 'nan) +nan.0)
         (#t
          (string->number (substring (symbol->string n) 2) 16))))

(define (idx? x)
   (or (and (symbol? x) (equal? #\$ (string-ref (symbol->string x) 0)))
       (number? x)))

(define (get-results l)
   (match-case l
      (((result . ?t) . ?rst)
       (multiple-value-bind (r tl)
          (get-results rst)
          (values (append t r) tl)))
      (else (values '() l))))

; decomposes a typeuse by separating parameters (types and names), results (only
; types) and the rest ; it generates dummy names to complete the list if
; necessary
(define (get-names/params/results/tl l)
   (match-case l
      (((param (and (? idx?) ?name) ?t) . ?rst)
       (multiple-value-bind (n p r tl)
          (get-names/params/results/tl rst)
          (values (cons name n) (cons t p) r tl)))
      (((param . ?t) . ?rst)
       (multiple-value-bind (n p r tl)
          (get-names/params/results/tl rst)
          (values (append (map (lambda (-) (fresh-var)) t) n)
                  (append t p) r tl)))
      (else
       (multiple-value-bind (r tl) (get-results l) (values '() '() r tl)))))

(define (get-typeidxs/comptype l)
  (match-case l
     ((?ct) (values '() ct))
     ((?x . ?rst)
      (multiple-value-bind (idxs ct)
         (get-typeidxs/comptype rst)
         (values (cons x idxs) ct)))))

(define (get-catches/tl l)
   (match-case l
      (((catch ?x ?l) . ?rst)
       (multiple-value-bind (c tl)
          (get-catches/tl rst)
          (values (cons (cons #x00 (cons x l)) c) tl)))
      (((catch_ref ?x ?l) . ?rst)
       (multiple-value-bind (c tl)
          (get-catches/tl rst)
          (values (cons (cons #x01 (cons x l)) c) tl)))
      (((catch_all ?l) . ?rst)
       (multiple-value-bind (c tl)
          (get-catches/tl rst)
          (values (cons (cons #x02 l) c) tl)))
      (((catch_all_ref ?l) . ?rst)
       (multiple-value-bind (c tl)
          (get-catches/tl rst)
          (values (cons (cons #x03 l) c) tl)))
      (else (values '() l))))

(define (get-names/locals/tl l)
   (match-case l
      (((local (and (? idx?) ?name) ?type) . ?rst)
       (multiple-value-bind (n t tl) (get-names/locals/tl rst)
          (values (cons name n) (cons type t) tl)))
      (((local . ?types) . ?rst)
       (multiple-value-bind (n t tl) (get-names/locals/tl rst)
          (values (append (map (lambda (-) (fresh-var)) types) n)
                  (append types t) tl)))
      (else (values '() '() l))))

(define (get-fieldnames l)
   (match-case l
      (((field (and (? idx?) ?name) ?-) . ?rst)
       (cons name (get-fieldnames rst)))
      (((field . ?types) . ?rst)
       (append (map (lambda (-) (fresh-var)) types) (get-fieldnames rst)))
      (() '())))

(define (get-fields l)
   (match-case l
      (((field (and (? symbol?) (not (? valtype-symbol?))) ?t) . ?rst)
       (cons t (get-fields rst)))
      (((field . ?types) . ?rst)
       (append types (get-fields rst)))
      (() '())))

(define (remove-names-comptype t)
   (match-case t
      ((func . ?rst)
       (multiple-value-bind (- p r -)
          (get-names/params/results/tl rst)
          `(func (param ,@p) (result ,@r))))
      (else t)))

(define (remove-names-subtype st)
   (match-case st
      ((sub final . ?rst)
        (multiple-value-bind (idxs ct)
          (get-typeidxs/comptype rst)
          `(sub final ,@idxs ,(remove-names-comptype ct))))
      ((sub . ?rst)
       (multiple-value-bind (idxs ct)
          (get-typeidxs/comptype rst)
          `(sub ,@idxs ,(remove-names-comptype ct))))
      (else (remove-names-comptype st))))

(define (write-string n out-port)
   (leb128-write-unsigned (string-length n) out-port)
   (display n out-port))

(define (write-vec v wr out-port)
   (leb128-write-unsigned (length v) out-port)
   (for-each (lambda (x) (wr x out-port)) v))

;; actually also works on heap types to avoid other boilerplate functions, might
;; change that
(define (write-valtype t out-port)
   (match-case t
      ((? valtype-symbol?)
       (write-byte (hashtable-get *valtype-symbols* t) out-port))
      ((? type-abbreviation?)
       (write-valtype (hashtable-get *type-abbreviations* t) out-port))
      ((? symbol?)
       (if (hashtable-contains? typeidxs t)
           (leb128-write-signed (hashtable-get typeidxs t) out-port)
           (report "write-valtype" "unknown type" t)))
      ((? wnumber?)
       (leb128-write-signed (wnumber->number t) out-port))
      ((ref ?t)
       (write-byte #x64 out-port)
       (write-valtype t out-port))
      ((ref null ?t)
       (write-byte #x63 out-port)
       (write-valtype t out-port))))

;; the following function expect comptypes to be in the form given by
;; remove-names-comptype, i.e. (func (param ...) (result ...)), or (array ...)
(define (write-comptype t out-port)
   (define (write-fieldtype t out-port)
      (match-case t
         ((mut ?t)
          (write-valtype t out-port)
          (write-byte #x01 out-port))
         (else
          (write-valtype t out-port)
          (write-byte #x00 out-port))))
   (match-case t
      ((func (param . ?p) (result . ?r))
       (write-byte #x60 out-port)
       (write-vec p write-valtype out-port)
       (write-vec r write-valtype out-port))
      ((array ?t)
       (write-byte #x5E out-port)
       (write-fieldtype t out-port))
      ((struct . ?fds)
       (write-byte #x5F out-port)
       (write-vec (get-fields fds) write-fieldtype out-port))))

(define (write-subtype st out-port)
   (match-case st
      ((sub final . ?rst)
       (multiple-value-bind (idxs ct)
          (get-typeidxs/comptype rst)
          (unless (null? idxs)
             (write-byte #x4F out-port)
             (write-vec
              idxs
              (lambda (x p) (typeidx '() '() x p)) out-port))
          (write-comptype ct out-port)))
      ((sub . ?rst)
       (multiple-value-bind (idxs ct)
          (get-typeidxs/comptype rst)
          (write-byte #x50 out-port)
          (write-vec idxs (lambda (x p) (typeidx '() '() x p)) out-port)
          (write-comptype ct out-port)))
      (else (write-comptype st out-port))))

(define (write-rectype typeidxs rt out-port)
   (define (write-typedef td out-port)
      (match-case td
         ((type (and (? symbol?) ?name) ?st)
          (write-subtype (remove-names-subtype st) out-port))
         ((type ?st)
          (write-subtype (remove-names-subtype st) out-port))))
   (match-case rt
      ((rec . ?v)
       (write-byte #x4E out-port)
       (write-vec v write-typedef out-port))
      (else (write-typedef rt out-port))))

(define (write-globaltype gt out-port)
   (match-case gt
      ((mut ?vt)
       (write-valtype vt out-port)
       (write-byte #x01 out-port))
      (else (write-valtype gt out-port)
            (write-byte #x00 out-port))))

(define (write-section secid in-port out-port vec-len)
   (let* ((len (call-with-output-string
                  (lambda (p) (leb128-write-unsigned vec-len p))))
          (content (string-append len (close-output-port in-port))))
      (unless (= vec-len 0)
         (write-byte secid out-port)
         (write-string content out-port))))

(define (hack-hash x)
  (get-hashnumber (with-output-to-string (lambda () (display x)))))

(define-macro (define-globalidx name table)
   `(define (,(string->symbol (string-append name "idx"))
             locals labls x out-port)
      (cond ((and (symbol? x) (hashtable-contains? ,table x))
             (leb128-write-unsigned (hashtable-get ,table x) out-port))
           ((number? x) (leb128-write-unsigned x out-port))
           (#t (report ,name ,(string-append "unknown " name) x)))))

(define-globalidx "func" funcidxs)
(define-globalidx "global" globalidxs)
(define-globalidx "mem" memidxs)
(define-globalidx "data" dataidxs)
(define-globalidx "tag" tagidxs)

;;;; kind of hacky, but should be working
(define last-type #f)

(define (typeidx - - t out-port)
   (let ((idx
          (cond ((and (symbol? t) (hashtable-contains? typeidxs t))
                 (hashtable-get typeidxs t))
                ((number? t) t)
                (#t (report "typeidx" "unknown type" t) 0))))
      (leb128-write-unsigned idx out-port)
      (set! last-type idx)))

(define (index x l i)
   (cond ((null? l) (report "index" "unknown identifier" x) 0)
         ((equal? (car l) x) i)
         (#t (index x (cdr l) (+ 1 i)))))

(define (fieldidx locals labls f out-port)
   (if (number? f)
       (leb128-write-unsigned f out-port)
       (leb128-write-unsigned
        (index f (hashtable-get fieldidxs last-type) 0) out-port)))

(define (labelidx locals labls lab out-port)
   (if (number? lab)
       (leb128-write-unsigned lab out-port)
       (leb128-write-unsigned (index lab labls 0) out-port)))

(define (localidx locals labls v out-port)
   (if (number? v)
      (leb128-write-unsigned v out-port)
      (leb128-write-unsigned (index v locals 0) out-port)))

(define (heaptype locals labls ht out-port)
   (write-valtype ht out-port))

(define (i32 locals labls n out-port)
   (let ((n (wnumber->number n)))
     (if (> n 2147483647)
         (begin (print "quick hack")
                (leb128-write-signed (- n (* 2 2147483648)) out-port))
         (leb128-write-signed (wnumber->number n) out-port))))

(define (i64 locals labls n out-port)
   (leb128-write-signed (wnumber->number n) out-port))

(define (f32 locals labls n out-port)
   (let* ((m (wnumber->number n))
          (s (if (fixnum? m)
                 (float->ieee-string (fixnum->flonum m))
                 (float->ieee-string m))))
     (do ((i 3 (- i 1)))
         ((< i 0))
       (write-byte (char->integer (string-ref s i)) out-port))))

(define (f64 locals labls n out-port)
   (let* ((m (wnumber->number n))
          (s (if (fixnum? m)
                 (double->ieee-string (fixnum->flonum m))
                 (double->ieee-string m))))
      (do ((i 7 (- i 1)))
          ((< i 0))
        (write-byte (char->integer (string-ref s i)) out-port))))

(define (u32 locals labls n out-port)
   (leb128-write-unsigned n out-port))

(define (ref.test-heaptype locals labls t out-port)
   (match-case t
      ((ref ?ht)
       (leb128-write-unsigned 20 out-port)
       (write-valtype ht out-port))
      ((ref null ?ht)
       (leb128-write-unsigned 21 out-port)
       (write-valtype ht out-port))))

(define (ref.cast-heaptype locals labls t out-port)
   (match-case t
      ((ref ?ht)
       (leb128-write-unsigned 22 out-port)
       (write-valtype ht out-port))
      ((ref null ?ht)
       (leb128-write-unsigned 23 out-port)
       (write-valtype ht out-port))))

(define (struct.get-typeidx locals labls t out-port)
   (if wasm-as-compat
       (set! last-type
             (cond ((and (symbol? t) (hashtable-contains? typeidxs t))
                    (hashtable-get typeidxs t))
                   ((number? t) t)
                   (#t (report "typeidx" "unknown type" t) 0)))
       (begin
         (leb128-write-unsigned 2 out-port)
          (typeidx locals labls t out-port))))

(define (struct.get-fieldidx locals labls f out-port)
   (if wasm-as-compat
       (let ((idx (if (number? f)
                      f
                      (index f (hashtable-get fieldidxs last-type) 0))))
          (match-case (list-ref (hashtable-get fieldtypes last-type) idx)
             ((or i8 i16) (leb128-write-unsigned 4 out-port))
             (else (leb128-write-unsigned 2 out-port)))
          (leb128-write-unsigned last-type out-port)
          (leb128-write-unsigned idx out-port))
       (fieldidx locals labls f out-port)))

(define (array.get-typeidx locals labls t out-port)
   (let ((idx (cond ((and (symbol? t) (hashtable-contains? typeidxs t))
                    (hashtable-get typeidxs t))
                   ((number? t) t)
                   (#t (report "typeidx" "unknown type" t) 0))))
      (if wasm-as-compat
          (begin
             (match-case (hashtable-get arraytypes idx)
                ((or i8 i16 (mut i8) (mut i16))
                 (leb128-write-unsigned 13 out-port))
                (else (leb128-write-unsigned 11 out-port)))
             (leb128-write-unsigned idx out-port))
          (begin
             (leb128-write-unsigned 11 out-port)
             (typeidx locals labls t out-port)))))

(define (ref.func-funcidx locals labls f out-port)
   (if wasm-as-compat
       (set! funcrefs
             (cons (if (and (symbol? f) (hashtable-contains? funcidxs f))
                       (hashtable-get funcidxs f)
                       f)
                   funcrefs)))
   (funcidx locals labls f out-port))

(read-table *opcodes* "opcodes.sch")

(define (write-module m out-port)
   (let ((typep (open-output-string))
         (othertypes (open-output-string))
         (importp (open-output-string))
         (funcp (open-output-string))
         (exportp (open-output-string))
         (codep (open-output-string))
         (globalp (open-output-string))
         (memp (open-output-string))
         (tagp (open-output-string))
         (datap (open-output-string))
         (defined-types (create-hashtable :eqtest equal? :hash hack-hash))
         (ntypes 0)
         (nrecs 0)
         (nimports 0)
         (ncodes 0)
         (nfuncs 0)
         (nexports 0)
         (nglobals 0)
         (ndefglobals 0)
         (nmems 0)
         (ndata 0)
         (ntags 0))

      (define (typeidx t)
         (cond ((and (symbol? t) (hashtable-contains? typeidxs t))
                (hashtable-get typeidxs t))
               ((number? t) t)
               (#t (report "typeidx" "unknown type" t))))

      (define (get-typeidx! t)
         (let ((h (hashtable-get defined-types t)))
            (if h
                h
                (begin
                   (hashtable-put! defined-types t ntypes)
                   (set! ntypes (+ 1 ntypes))
                   (set! nrecs (+ 1 nrecs))
                   (- ntypes 1)))))

      (define (add-funcref! f)
         (unless (member f funcrefs)
            (set! funcrefs (cons f funcrefs))))

      (define (update-tables! m)
         (match-case m
            ; only support named imports for the moment (didn't find a use-case
            ; yet for unnamed imports)
            ((import ?- ?- (func (and ?id (? symbol?)) . ?-))
             (hashtable-put! funcidxs id nfuncs)
             (set! nfuncs (+ 1 nfuncs))
             (set! nimports (+ 1 nimports))
             (list m))
            ((import ?- ?- (global (and ?id (? symbol?)) . ?-))
             (hashtable-put! globalidxs id nglobals)
             (set! nglobals (+ 1 nglobals))
             (set! nimports (+ 1 nimports))
             (list m))
            ((export . ?-)
             (set! nexports (+ 1 nexports))
             (list m))
            ((type ?name ?st)
             (let* ((t (remove-names-subtype st))
                    (old-ntypes ntypes)
                    (id (get-typeidx! t)))
                (hashtable-put! typeidxs name id)
                (match-case st
                   ((or (sub final ??- (struct . ?fds))
                        (sub ??- (struct . ?fds))
                        (struct . ?fds))
                    (hashtable-put! fieldidxs id (get-fieldnames fds))
                    ;;;;; quick hack
                    (hashtable-put! fieldtypes id (get-fields fds)))
                   ((or (sub final ??- (array ?t))
                        (sub ??- (array ?t))
                        (array ?t))
                    (hashtable-put! arraytypes id t)))
                (if (= old-ntypes ntypes) ; has t already been defined ?
                    '()
                    (list `(type ,name ,t)))))
            ((rec . ?lst)
             (let ((saved-nrecs nrecs)
                   (l (remove null? (map update-tables! lst))))
                (set! nrecs (+ 1 saved-nrecs))
                (if (null? l)
                    (begin
                       (set! nrecs (- nrecs 1))
                       '())
                    (list `(rec ,@(map car l))))))
            ((func (export (and ?nm (? string?))) . ?rst)
             (update-tables! `(func ,(fresh-var) (export ,nm) ,@rst)))
            ((func (and (? symbol?) ?id) (export (and ?nm (? string?))) . ?rst)
             (append (update-tables! `(export ,nm (func ,id)))
                     (update-tables! `(func ,id ,@rst))))
            ((func (and (? symbol?) ?id) (import (and ?nm1 (? string?))
                                                 (and ?nm2 (? string?))) . ?tu)
             (update-tables! `(import ,nm1 ,nm2 (func ,id ,@tu))))
            ((func (and ?id (? symbol?)) . ?-)
             (hashtable-put! funcidxs id nfuncs)
             (set! nfuncs (+ 1 nfuncs))
             (set! ncodes (+ 1 ncodes))
             (list m))
            ((global (and (? symbol?) ?id) (import (and ?nm1 (? string?))
                                                   (and ?nm2 (? string?)) ?gt))
             (update-tables! `(import ,nm1 ,nm2 (global ,id ,gt))))
            ((global (export (and ?nm (? string?))) . ?rst)
             (update-tables! `(global ,(fresh-var) (export ,nm) ,@rst)))
            ((global (and (? symbol?) ?id) (export (and ?nm (? string?))) .
                     ?rst)
             (append (update-tables! `(export ,nm (global ,id)))
                     (update-tables! `(global ,id ,@rst))))
            ((global (and (? symbol?) ?id) . ?-)
             (hashtable-put! globalidxs id nglobals)
             (set! nglobals (+ 1 nglobals))
             (set! ndefglobals (+ 1 ndefglobals))
             (list m))
            ((memory (and (? symbol?) ?id) . ?limits)
             (hashtable-put! memidxs id nmems)
             (set! nmems (+ 1 nmems))
             (list `(memory ,@limits)))
            ((memory . ?-)
             (set! nmems (+ 1 nmems))
             (list m))
            ((tag (and (? symbol?) ?id) . ?tu)
             (hashtable-put! tagidxs id ntags)
             (set! ntags (+ 1 ntags))
             (list `(tag ,@tu)))
            ((tag . ?-)
             (set! ntags (+ 1 ntags))
             (list m))
            ((data (and (? symbol?) ?id) . ?d)
             (hashtable-put! dataidxs id ndata)
             (set! ndata (+ 1 ndata))
             (list `(data ,@d)))
            ((data . ?-)
             (set! ndata (+ 1 ndata))
             (list m))
            (else (report "update-table!" "unknown declaration" m))))

      ;; returns an id for the typeuse, a list of names for the parameters and
      ;; the rest of the program
      (define (write-typeuse! tu out-port)
         (match-case tu
            ; to validate we should check that the eventual params/results are
            ; correct, when no params/results are given, we should generate
            ; dummy names, but the (type x) syntax is not used in bigloo
            (((type ?x) . ?rst)
             (multiple-value-bind (n - - tl)
                (get-names/params/results/tl rst)
                (values (typeidx x) n tl)))
            ; we have params/results
            (else
             (multiple-value-bind (n p r tl) (get-names/params/results/tl tu)
                (let* ((t `(func (param ,@p) (result ,@r)))
                       (old-ntypes ntypes)
                       (id (get-typeidx! t)))
                   (unless (= old-ntypes ntypes) ; has t already been defined ?
                       (write-comptype t out-port))
                   (values id n tl))))))

      (define (write-import-desc! d)
         ; todo : tag memory table - not used in bigloo
         (match-case d
            ((or (func (? symbol?) . ?tu) (func . ?tu))
             (multiple-value-bind (id - -) (write-typeuse! tu othertypes)
                (write-byte #x00 importp)
                (leb128-write-unsigned id importp)))
            ((or (global (? symbol?) ?gt) (global ?gt))
             (begin (write-byte #x03 importp)
                    (write-globaltype gt importp)))))

      (define (write-export-desc d)
         ; todo : tag table - not used in bigloo
         (match-case d
            ((func ?f)
             (write-byte #x00 exportp)
             (funcidx '() '() f exportp))
            ((memory ?m)
             (write-byte #x02 exportp)
             (memidx '() '() m exportp))
            ((global ?x)
             (write-byte #x03 exportp)
             (globalidx '() '()x exportp))))

      ;; todo : add checks (with file position)
      (define (take/drop l k::bint)
         (if (=fx 0 k)
             (values '() l)
             (multiple-value-bind (tk drp) (take/drop (cdr l) (-fx k 1))
                                  (values (cons (car l) tk) drp))))

      (define (new-write-instruction! locals labls i out-port)
         (define (go i)
            (new-write-instruction! locals labls i out-port))

         (define (go-new-labels labls)
            (lambda (i)
              (new-write-instruction! locals labls i out-port)))

         (define (write-if-branch! ret? labls bt b)
            (match-case b
               ((then . ?body)
                (begin (write-byte #x04 out-port)
                       (display bt out-port)
                       (for-each (go-new-labels labls) body)
                       (if (and wasm-as-compat ret? (unreachable-body-end body))
                           (write-byte #x00 out-port))))
               (((kwote else) . ?body)
                (begin (write-byte #x05 out-port)
                       (for-each (go-new-labels labls) body)
                       (if (and wasm-as-compat ret? (unreachable-body-end body))
                           (write-byte #x00 out-port))))
               (else (go b))))

         (define (compile-blocktype! p r)
            (match-case (cons p r)
               ((() . ()) "\x40")
               ((() . (?vt))
                (call-with-output-string
                   (lambda (p) (write-valtype vt p))))
               (else
                (call-with-output-string
                   (lambda (op)
                      (let* ((t `(func (param ,@p) (result ,@r)))
                             (old-ntypes ntypes)
                             (id (get-typeidx! t)))
                         ; has t already been defined ?
                         (unless (= old-ntypes ntypes)
                            (write-comptype t othertypes))
                         (leb128-write-signed id op)))))))

         (if (hashtable-contains? *opcodes* (car i))
             (let* ((t (hashtable-get *opcodes* (car i)))
                    (args (car t))
                    (bytes (cdr t)))
                (multiple-value-bind (vals tl) (take/drop (cdr i) (length args))
                   (for-each go tl)
                   (display bytes out-port)
                   (for-each
                    (lambda (f v)
                       (f locals labls v out-port)) args vals)))
             (match-case i
                ((block (and (? symbol?) ?label) . ?rst)
                 (multiple-value-bind (- p r tl)
                    (get-names/params/results/tl rst)
                    (write-byte #x02 out-port)
                    (display (compile-blocktype! p r) out-port)
                    (for-each (go-new-labels (cons label labls)) tl)
                    (if (and wasm-as-compat (not (null? r))
                             (unreachable-body-end tl))
                        (write-byte #x00 out-port))
                    (write-byte #x0B out-port)))
                ((block . ?rst)
                 (go `(block ,(fresh-var) ,@rst)))
                ((loop (and (? symbol?) ?label) . ?rst)
                 (multiple-value-bind (- p r tl)
                    (get-names/params/results/tl rst)
                    (write-byte #x03 out-port)
                    (display (compile-blocktype! p r) out-port)
                    (for-each (go-new-labels (cons label labls)) tl)
                    (if (and wasm-as-compat (not (null? r))
                             (unreachable-body-end tl))
                        (write-byte #x00 out-port))
                    (write-byte #x0B out-port)))
                ((loop . ?rst)
                 (go `(loop ,(fresh-var) ,@rst)))
                ((if (and (? symbol?) ?label) . ?rst)
                 (multiple-value-bind (- p r tl)
                    (get-names/params/results/tl rst)
                    (let ((bt (compile-blocktype! p r)))
                       (for-each
                        (lambda (b)
                          (write-if-branch! (not (null? r))
                                            (cons label labls) bt b))
                        tl)
                       (write-byte #x0B out-port))))
                ((if . ?rst)
                 (go `(if ,(fresh-var) ,@rst)))
                ((br_table . ?rst)
                 (define (get-labls/default/tl l)
                    (match-case l
                       (((and ?def (? idx?)) (and ?hd (not (? idx?))) . ?tl)
                        (values '() def (cons hd tl)))
                       (((and ?lab (? idx?)) . ?rst)
                        (multiple-value-bind (l d tl) (get-labls/default/tl rst)
                           (values (cons lab l) d tl)))))
                 (multiple-value-bind (lab def tl) (get-labls/default/tl rst)
                    (for-each go tl)
                    (write-byte #x0E out-port)
                    (write-vec lab (lambda (l p) (labelidx locals labls l p))
                               out-port)
                    (labelidx locals labls def out-port)))
                ((try_table (and (? symbol?) ?label) . ?rst)
                 (write-byte #x1F out-port)
                 (multiple-value-bind (- p r tl)
                    (get-names/params/results/tl rst)
                    (display (compile-blocktype! p r) out-port)
                    (define (write-catch c out-port)
                       (write-byte (car c) out-port)
                       (if (pair? (cdr c))
                           (begin
                              (tagidx '() '() (cadr c) out-port)
                              (labelidx locals labls (cddr c) out-port))
                           (labelidx locals labls (cdr c) out-port)))
                    (multiple-value-bind (c tl) (get-catches/tl tl)
                       (write-vec c write-catch out-port)
                       (for-each (go-new-labels (cons label labls)) tl)
                       (if (and wasm-as-compat (not (null? r))
                                (unreachable-body-end tl))
                           (write-byte #x00 out-port))
                       (write-byte #x0B out-port))))
                ((try_table . ?rst)
                 (go `(try_table ,(fresh-var) ,@rst)))
                (else (report "write-instruction" "unknown instruction" i)))))

      (define (write-func rst)
        (multiple-value-bind (id par-nms tl) (write-typeuse! rst othertypes)
           (multiple-value-bind (loc-nms loc-tys body) (get-names/locals/tl tl)
              (leb128-write-unsigned id funcp)
              ; we are wasting space on local declaration, as two i32 variables
              ; will be declared as one i32 local twice (that's what indicates
              ; the write-byte 1), instead of being a single declaration of two
              ; i32 locals
              (let* ((loc-decl
                      (call-with-output-string
                       (lambda (p)
                         (write-vec loc-tys
                                    (lambda (t p) (write-byte #x01 p)
                                            (write-valtype t p)) p))))
                     (code
                      (call-with-output-string
                       (lambda (p)
                         (for-each
                          (lambda (i)
                             (new-write-instruction! (append par-nms loc-nms)
                                                     '() i p))
                          body)
                         ;;;;; quick hack
                         (if wasm-as-compat
                             (multiple-value-bind (- - r -)
                                (get-names/params/results/tl rst)
                                (if (and (unreachable-body-end body)
                                         (not (null? r)))
                                    (write-byte #x00 p))))
                         (write-byte #x0B p))))
                     (cont (string-append loc-decl code)))
                (write-string cont codep)))))

      (define (out-mod m)
         (match-case m
            ((import (and ?mod (? string?)) (and ?nm (? string?)) ?d)
             (write-string mod importp)
             (write-string nm importp)
             (write-import-desc! d))
            ((export (and ?nm (? string?)) ?d)
             (write-string nm exportp)
             (write-export-desc d))
            ((elem declare func . ?lst)
             (set! funcrefs (append lst funcrefs)))
            ((or (type . ?-) (rec . ?-)) (write-rectype typeidxs m typep))
            ((func (? symbol?) . ?rst)
             (write-func rst))
            ((global (? symbol?) ?gt . ?expr)
             (write-globaltype gt globalp)
             (for-each (lambda (i) (new-write-instruction! '() '() i globalp))
                       expr)
             (write-byte #x0B globalp))
            ((memory (and (? number?) ?n))
             (write-byte #x00 memp)
             (leb128-write-unsigned n memp))
            ((memory (and (? number?) ?n) (and (? number?) ?m))
             (write-byte #x01 memp)
             (leb128-write-unsigned n memp)
             (leb128-write-unsigned m memp))
            ((data . ?d)
             (leb128-write-unsigned 1 datap)
             (write-string (apply string-append d) datap))
            ((tag . ?tu)
             (multiple-value-bind (id - -) (write-typeuse! tu othertypes)
                (write-byte #x00 tagp)
                (leb128-write-unsigned id tagp)))))

      (let ((desuggared-mods (map update-tables! (cddr m))))
         (for-each (lambda (l) (for-each out-mod l)) desuggared-mods))

      (unless error-encountered?
         (display "\x00asm\x01\x00\x00\x00" out-port) ; magic and version
         (display (close-output-port othertypes) typep)
         (write-section #x01 typep out-port nrecs)
         (write-section #x02 importp out-port nimports)
         (write-section #x03 funcp out-port ncodes)
         (write-section #x05 memp out-port nmems)
         (write-section #x0D tagp out-port ntags)
         (write-section #x06 globalp out-port ndefglobals)
         (write-section #x07 exportp out-port nexports)
         (unless (null? funcrefs)
           (write-byte #x09 out-port)
           (write-string
            (call-with-output-string
             (lambda (p)
               (leb128-write-unsigned 1 p)
               (leb128-write-unsigned 3 p)
               (write-byte #x00 p)
               (write-vec funcrefs leb128-write-unsigned p))) out-port))
         (unless (= 0 ndata)
           (write-byte #x0C out-port)
           (write-string
            (call-with-output-string
             (lambda (p) (leb128-write-unsigned ndata p)))
            out-port))
         (write-section #x0A codep out-port ncodes)
         (write-section #x0B datap out-port ndata))))

(define (main argv)
   (define input-file #f)
   (args-parse (cdr argv)
      ((("--help" "-h") (help "Display this help message"))
       (args-parse-usage #f))
      (("-o" ?file (help "Write output to file"))
       (set! output-file file))
      (("-all") #f)
      (("-k" (help "Keep going when encoutering errors"))
       (set! keep-going #t))
      (("--wasm-as-compat"
        (help "Assure compatibility with binaryen's wasm-as"))
       (set! wasm-as-compat #t))
      (else
       (set! input-file else)))
   (call-with-input-file input-file
     (lambda (ip)
        (call-with-output-file output-file
           (lambda (op)
              (write-module (read ip) op)))))
   (if error-encountered?
       (exit 1)))
