(module binary
   (library srfi1)
   (main main)
   (import leb128))

(define dummy-integer 0)

(define (fresh-var)
   (set! dummy-integer (+ 1 dummy-integer))
   (string->symbol (string-append "__bigloo_wasm_dummy_id_"
                                  (number->string dummy-integer))))

(define *valtype-symbols* (make-hashtable))

(for-each (match-lambda ((?sym . ?code)
                         (hashtable-put! *valtype-symbols* sym code)))
          '((i32 . #x7F)
            (i64 . #x7E)
            (f32 . #x7D)
            (f64 . #x7C)
            (v128 . #x7B)
            ; these two are not valtypes but we do not validate
            (i8 . #x78)
            (i16 . #x77)
            (nofunc . #x73)
            (noextern . #x72)
            (none . #x71)
            (func . #x70)
            (extern . #x6F)
            (any . #x6E)
            (eq . #x6D)
            (i31 . #x6C)
            (struct . #x6B)
            (array . #x6A)))

(define *simple-opcodes* (make-hashtable))

(for-each (match-lambda ((?sym . ?code)
                         (hashtable-put! *simple-opcodes* sym code)))
          '((unreachable . #x00)
            (nop . #x01)
            (return . #x0F)

            (drop . #x1A)

            (i32.eqz  . #x45)
            (i32.eq   . #x46)
            (i32.ne   . #x47)
            (i32.lt_s . #x48)
            (i32.lt_u . #x49)
            (i32.gt_s . #x4A)
            (i32.gt_u . #x4B)
            (i32.le_s . #x4C)
            (i32.le_u . #x4D)
            (i32.ge_s . #x4E)
            (i32.ge_u . #x4F)

            (i64.eqz  . #x50)
            (i64.eq   . #x51)
            (i64.ne   . #x52)
            (i64.lt_s . #x53)
            (i64.lt_u . #x54)
            (i64.gt_s . #x55)
            (i64.gt_u . #x56)
            (i64.le_s . #x57)
            (i64.le_u . #x58)
            (i64.ge_s . #x59)
            (i64.ge_u . #x5A)

            (f32.eq . #x5B)
            (f32.ne . #x5C)
            (f32.lt . #x5D)
            (f32.gt . #x5E)
            (f32.le . #x5F)
            (f32.ge . #x60)

            (f64.eq . #x61)
            (f64.ne . #x62)
            (f64.lt . #x63)
            (f64.gt . #x64)
            (f64.le . #x65)
            (f64.ge . #x66)

            (i32.clz    . #x67)
            (i32.ctz    . #x68)
            (i32.popcnt . #x69)
            (i32.add    . #x6A)
            (i32.sub    . #x6B)
            (i32.mul    . #x6C)
            (i32.div_s  . #x6D)
            (i32.div_u  . #x6E)
            (i32.rem_s  . #x6F)
            (i32.rem_u  . #x70)
            (i32.and    . #x71)
            (i32.or     . #x72)
            (i32.xor    . #x73)
            (i32.shl    . #x74)
            (i32.shr_s  . #x75)
            (i32.shr_u  . #x76)
            (i32.rotl   . #x77)
            (i32.rotr   . #x78)

            (i64.clz    . #x67)
            (i64.ctz    . #x68)
            (i64.popcnt . #x69)
            (i64.add    . #x6A)
            (i64.sub    . #x6B)
            (i64.mul    . #x6C)
            (i64.div_s  . #x6D)
            (i64.div_u  . #x6E)
            (i64.rem_s  . #x6F)
            (i64.rem_u  . #x70)
            (i64.and    . #x71)
            (i64.or     . #x72)
            (i64.xor    . #x73)
            (i64.shl    . #x74)
            (i64.shr_s  . #x75)
            (i64.shr_u  . #x76)
            (i64.rotl   . #x77)
            (i64.rotr   . #x78)

            (f32.abs      . #x8B)
            (f32.neg      . #x8C)
            (f32.ceil     . #x8D)
            (f32.floor    . #x8E)
            (f32.trunc    . #x8F)
            (f32.nearest  . #x90)
            (f32.sqrt     . #x91)
            (f32.add      . #x92)
            (f32.sub      . #x93)
            (f32.mul      . #x94)
            (f32.div      . #x95)
            (f32.min      . #x96)
            (f32.max      . #x97)
            (f32.copysign . #x98)

            (f64.abs      . #x99)
            (f64.neg      . #x9A)
            (f64.ceil     . #x9B)
            (f64.floor    . #x9C)
            (f64.trunc    . #x9D)
            (f64.nearest  . #x9E)
            (f64.sqrt     . #x9F)
            (f64.add      . #xA0)
            (f64.sub      . #xA1)
            (f64.mul      . #xA2)
            (f64.div      . #xA3)
            (f64.min      . #xA4)
            (f64.max      . #xA5)
            (f64.copysign . #xA6)))

(define (valtype-symbol? s)
   (and (symbol? s) (hashtable-contains? *valtype-symbols* s)))

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
      (((param (and (? symbol?) ?name (not (? valtype-symbol?))) ?t) . ?rst)
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

(define (get-names/locals/tl l)
   (match-case l
      (((local (and (? symbol?) ?name (not (? valtype-symbol?))) ?type) . ?rst)
       (multiple-value-bind (n t tl) (get-names/locals/tl rst)
          (values (cons name n) (cons type t) tl)))
      (((local . ?types) . ?rst)
       (multiple-value-bind (n t tl) (get-names/locals/tl rst)
          (values (append (map (lambda (-) (fresh-var)) types))
                  (append types t) tl)))
      (else (values '() '() l))))

; we do not support anonymous fields yet, they are not used by bigloo like named
; parameters, the only thing missing is a valtype? function which could be
; easily implemented by replacing the match in the write-valtype function by a
; hashtable ; however, we use anonymous fields internally, with the next
; function
(define (get-fields l)
   (map caddr l))

(define (remove-names-comptype t)
   (match-case t
      ((func . ?rst)
       (multiple-value-bind (- p r -)
          (get-names/params/results/tl t)
          `(func (param ,@p) (result ,@r))))
      ((struct . ?fds) `(struct (field ,@(get-fields fds))))
      (else t)))

(define (remove-names-subtype st)
   (match-case st
      ((sub ?ct) `(sub ,(remove-names-comptype ct)))
      ((sub final ?ct) `(sub final ,(remove-names-comptype ct)))
      (else (remove-names-comptype st))))

(define (write-string n out-port)
   (leb128-write-unsigned (string-length n) out-port)
   (display n out-port))

(define (write-vec v wr out-port)
   (leb128-write-unsigned (length v) out-port)
   (for-each (lambda (x) (wr x out-port)) v))

(define (write-valtype typeidxs t out-port)
   (match-case t
      ((? valtype-symbol?)
       (write-byte (hashtable-get *valtype-symbols* t) out-port))
      ((? symbol?) (leb128-write-signed (hashtable-get typeidxs t) out-port))
      ((ref ?t)
       (write-byte #x64 out-port)
       (write-valtype typeidxs t out-port))
      ((ref null ?t)
       (write-byte #x63 out-port)
       (write-valtype typeidxs t out-port))))

;; the following function expect comptypes to be in the form given by
;; remove-names-comptype, i.e. (func (param ...) (result ...)),
;; (struct (field ...)), or (array ...)
(define (write-comptype typeidxs t out-port)
   (define (write-fieldtype t out-port)
      (match-case t
         ((mut ?t)
          (write-valtype typeidxs t out-port)
          (write-byte #x01 out-port))
         (else (write-valtype typeidxs t out-port))))
   (match-case t
      ((func (param . ?p) (result . ?r))
       (write-byte #x60 out-port)
       (write-vec p (lambda (t p) (write-valtype typeidxs t p)) out-port)
       (write-vec r (lambda (t p) (write-valtype typeidxs t p)) out-port))
      ((array ?t)
       (write-byte #x5E out-port)
       (write-fieldtype t out-port))
      ((struct (field . ?fds))
       (write-byte #x5F out-port)
       (write-vec fds write-fieldtype out-port))))

(define (write-subtype typeidxs st out-port)
   (define (get-typeidxs/comptype l)
      (match-case l
         ((?ct) (values '() ct))
         ((?x . ?rst)
          (multiple-value-bind (idxs ct)
             (get-typeidxs/comptype rst)
             (values (cons (hashtable-get typeidxs x) idxs) ct)))))
   (match-case st
      ((sub final . ?rst)
       (multiple-value-bind (idxs ct)
          (get-typeidxs/comptype rst)
          (unless (null? idxs)
             (write-byte #x4F out-port)
             (write-vec idxs (lambda (x p) (leb128-write-unsigned x p))
                        out-port))
          (write-comptype typeidxs ct out-port)))
      ((sub . ?rst)
       (multiple-value-bind (idxs ct)
          (get-typeidxs/comptype rst)
          (write-byte #x4F out-port)
          (write-vec idxs (lambda (x p) (leb128-write-unsigned x p))
                     out-port)
          (write-comptype typeidxs ct out-port)))
      (else (write-comptype typeidxs st out-port))))

(define (write-rectype typeidxs rt out-port)
   (define (write-typedef td out-port)
      (match-case td
         ((type (and (? symbol?) ?name) ?st)
          (write-subtype typeidxs st out-port))
         ((type ?st) (write-subtype typeidxs st out-port))))
   (match-case rt
      ((rec . ?v)
       (write-byte #x4E out-port)
       (write-vec v write-typedef out-port))
      (else (write-typedef rt out-port))))

(define (write-globaltype typeidxs gt out-port)
   (match-case gt
      ((mut ?vt)
       (write-valtype typeidxs vt out-port)
       (write-byte #x01 out-port))
      (else (write-valtype typeidxs gt out-port)
            (write-byte #x00))))

(define (write-section secid in-port out-port vec-len)
   (let* ((len (call-with-output-string
                  (lambda (p) (leb128-write-unsigned vec-len p))))
          (content (string-append len (close-output-port in-port)))
          (size (string-length content)))
      ; only one byte indicates that we have a 0 length vector (the only byte is
      ; the size of the vector), which means we can omit the section
      (unless (= size 1)
         (write-byte secid out-port)
         (write-string content out-port))))

(define (write-module m out-port)
   (let ((typep (open-output-string))
         (importp (open-output-string))
         (funcp (open-output-string))
         (codep (open-output-string))
         (memp (open-output-string))
         (typeidxs (make-hashtable))
         (funcidxs (make-hashtable))
         (globalidxs (make-hashtable))
         (memidxs (make-hashtable))
         (fieldidxs (make-hashtable))
         (defined-types (create-hashtable :eqtest equal?))
         (ntypes 0)
         (nimports 0)
         (ncodes 0)
         (nfuncs 0)
         (nmems 0))
      (display "\x00asm\x01\x00\x00\x00" out-port) ; magic and version

      (define (get-typeidx! t)
         (let ((h (hashtable-get defined-types t)))
            (if h
                h
                (begin
                   (hashtable-put! defined-types t ntypes)
                   (set! ntypes (+ 1 ntypes))
                   (- ntypes 1)))))

      ;;; to answer : est-ce qu'on devrait faire une phase de desuggaring ici ?
      (define (update-tables! m)
         (match-case m
            ; only support named imports for the moment (didn't find a use-case
            ; yet for unnamed imports)
            ((import ?- ?- (func (and ?id (? symbol?)) . ?-))
             (hashtable-put! funcidxs id nfuncs)
             (set! nfuncs (+ 1 nfuncs))
             (set! nimports (+ 1 nimports))
             (list m))
            ((type ?name ?st)
             (let* ((t (remove-names-subtype st))
                    (id (get-typeidx! t)))
                (hashtable-put! typeidxs name id)
                (if (= id (- ntypes 1)) ; has t already been defined ?
                    (list `(type ,name ,t))
                    '())))
            ((rec . ?lst)
             (list `(rec ,@(map car (remove null? (map update-tables! lst))))))
            ((func (and ?id (? symbol?)) . ?-)
             (hashtable-put! funcidxs id nfuncs)
             (set! nfuncs (+ 1 nfuncs))
             (set! ncodes (+ 1 ncodes))
             (list m))))

      ;; returns an id for the typeuse, a list of names for the parameters and
      ;; the rest of the program
      (define (write-typeuse! tu)
         (match-case tu
            ; to validate we should check that the eventual params/results are
            ; correct, when no params/results are given, we should generate
            ; dummy names, but the (type x) syntax is not used in bigloo
            (((type ?x) . ?rst)
             (multiple-value-bind (n - - tl)
                (get-names/params/results/tl rst)
                (values (hashtable-get typeidxs x) n tl)))
            ; we have params/results
            (else
             (multiple-value-bind (n p r tl) (get-names/params/results/tl tu)
                (let* ((t `(func (param ,@p) (result ,@r)))
                       (id (get-typeidx! t)))
                   (if (= id (- ntypes 1)) ; has t already been defined ?
                       (write-comptype typeidxs t typep))
                   (values id n tl))))))

      (define (write-import-desc! d)
         ; todo : tag memory table - not used in bigloo
         (match-case d
            ((or (func (? symbol?) . ?tu) (func . ?tu))
             (multiple-value-bind (id - -) (write-typeuse! tu)
                (write-byte #x00 importp)
                (leb128-write-unsigned id importp)))
            ((or (global (? symbol?) ?gt) (global ?gt))
             (begin (write-byte #x03 importp)
                    (write-globaltype typeidxs gt typep)))))

      (define (write-instruction! locals labls i out-port)
         (define (go i)
            (write-instruction! locals labls i out-port))

         (define (write-if-branch! bt b)
            (match-case b
               ((then . ?body)
                (begin (write-byte #x04 out-port)
                       (display bt out-port)
                       (for-each go body)))
               (((kwote else) . ?body)
                (begin (write-byte #x05 out-port)
                       (for-each go body)))
               (else (go b))))

         (define (compile-blocktype! p r)
            (match-case (cons p r)
               ((() . ()) "\x40")
               ((() . (?vt))
                (call-with-output-string
                   (lambda (p) (write-valtype typeidxs vt p))))
               (else
                (call-with-output-string
                   (lambda (op)
                      (let* ((t `(func (param ,@p) (result ,@r)))
                             (id (get-typeidx! t)))
                         (if (= id (- ntypes 1)) ; has t already been defined ?
                             (write-comptype typeidxs t typep))
                         (leb128-write-signed id op)))))))

         (define (index x l i)
            (cond ((null? l) (error "index" "unknown identifier" x))
                  ((eq? (car l) x) i)
                  (#t (index x (cdr l) (+ 1 i)))))

         (define (labelidx lab)
            (index lab labls 0))

         (define (localidx v)
            (index v locals 0))

         (match-case i
            ((if (and (? symbol?) ?label) . ?rst)
             (write-instruction! (cons label labls) locals
                                 `(if ,@rst) out-port))
            ((if . ?rst)
             (multiple-value-bind (- p r tl) (get-names/params/results/tl rst)
                (let ((bt (compile-blocktype! p r)))
                   (for-each (lambda (b) (write-if-branch! bt b)) tl)
                   (write-byte #x0B out-port))))
            ((block (and (? symbol?) ?label) . ?rst)
             (write-instruction! (cons label labls) locals
                                 `(block ,@rst) out-port))
            ((block . ?rst)
             (multiple-value-bind (- p r tl) (get-names/params/results/tl rst)
                (write-byte #x02 out-port)
                (display (compile-blocktype! p r) out-port)
                (for-each go tl)
                (write-byte #x0B out-port)))
            ((loop (and (? symbol?) ?label) . ?rst)
             (write-instruction! (cons label labls) locals
                                 `(loop ,@rst) out-port))
            ((loop . ?rst)
             (multiple-value-bind (- p r tl) (get-names/params/results/tl rst)
                (write-byte #x03 out-port)
                (display (compile-blocktype! p r) out-port)
                (for-each go tl)
                (write-byte #x0B out-port)))
            ((br ?label)
             (write-byte #x0C out-port)
             (leb128-write-unsigned (labelidx label) out-port))
            ((br_table . ?rst)
             (define (get-labls/default/tl l)
                (match-case l
                   (((and ?def (? symbol?)) (and ?hd (not (? symbol?))) . ?tl)
                    (values '() def (cons hd tl)))
                   (((and ?lab (? symbol?)) . ?rst)
                    (multiple-value-bind (l d tl) (get-labls/default/tl rst)
                       (values (cons lab l) d tl)))))
             (multiple-value-bind (lab def tl) (get-labls/default/tl rst)
                (for-each go tl)
                (write-byte #x0E out-port)
                (write-vec lab
                           (lambda (l p) (leb128-write-unsigned (labelidx l) p))
                           out-port)
                (leb128-write-unsigned (labelidx def) out-port)))
            ((global.get (and (? symbol?) ?v))
             (write-byte #x23 out-port)
             (leb128-write-unsigned (hashtable-get globalidxs v) out-port))
            ((global.set (and (? symbol?) ?v) . ?tl)
             (for-each go tl)
             (write-byte #x24 out-port)
             (leb128-write-unsigned (hashtable-get globalidxs v) out-port))
            ((local.get (and (? symbol?) ?v))
             (write-byte #x20 out-port)
             (leb128-write-unsigned (localidx v) out-port))
            ((local.set (and (? symbol?) ?v) . ?tl)
             (for-each go tl)
             (write-byte #x21 out-port)
             (leb128-write-unsigned (localidx v) out-port))
            ((i32.const (and (? number?) ?n))
             (write-byte #x41 out-port)
             (leb128-write-signed n out-port))
            ((i64.const (and (? number?) ?n))
             (write-byte #x42 out-port)
             (leb128-write-signed n out-port))
            (((and ?op (? symbol?)) . ?tl)
             (for-each go tl)
             (write-byte (hashtable-get *simple-opcodes* op) out-port))))

      (define (out-mod m)
         (match-case m
            ((import ?mod ?nm ?d)
             (begin (write-string mod importp)
                    (write-string nm importp)
                    (write-import-desc! d)))
            ((or (type . ?-) (rec . ?-))
             (write-rectype typeidxs m typep))
            ((func (export (and ?nm (? symbol?))) . ?rst)
             (out-mod `(func ,(fresh-var) (export ,nm) ,@rst)))
            ((func (and ?id (? symbol?)) (export (and ?nm (? symbol?))) . ?rst)
             (out-mod `(export ,nm (func ,id)))
             (out-mod `(func ,id ,@rst)))
            ((func (and (? symbol?) ?id) (import (and ?nm1 (? symbol?))
                                                 (and ?nm2 (? symbol?))) . ?tu)
             (out-mod `(import ,nm1 ,nm2 (func ,id ,@tu))))
            ((func (and (? symbol?) ?id) . ?rst)
             (multiple-value-bind (id par-nms tl) (write-typeuse! rst)
                (multiple-value-bind (loc-nms loc-tys body)
                                     (get-names/locals/tl tl)
                   (leb128-write-unsigned id funcp)
                   (let* ((loc-decl
                           (call-with-output-string
                              (lambda (p)
                                 (write-vec loc-tys
                                    (lambda (t p)
                                       (write-valtype typeidxs t p)) p))))
                          (code
                           (call-with-output-string
                              (lambda (p)
                                 (for-each
                                  (lambda (i)
                                     (write-instruction!
                                      '() (append par-nms loc-nms) i p))
                                   body)
                                 (write-byte #x0B p))))
                          (cont (string-append loc-decl code)))
                      (write-string cont codep)))))))

      (for-each update-tables! (cddr m))

      (for-each out-mod (cddr m))

      (write-section #x01 typep out-port ntypes)
      (write-section #x02 importp out-port nimports)
      (write-section #x03 funcp out-port ncodes)
      (write-section #x0A codep out-port ncodes)))

(define (main argv)
   (call-with-input-file (cadr argv)
     (lambda (ip)
        (call-with-output-file "out.wasm"
           (lambda (op)
              (write-module (read ip) op))))))
