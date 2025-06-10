(module binary
   (library srfi1)
   (import leb128))

(define dummy-integer 0)

(define (fresh-var)
   (set! dummy-integer (+ 1 dummy-integer))
   (string->symbol (string-append "__bigloo_wasm_dummy_id_"
                                  (number->string dummy-integer))))

(define *valtype-symbols* (make-hashtable))

(for-each (lambda (sym code) (hashtable-put! *valtype-symbols* sym code))
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

(define (valtype-symbol? s)
   (and (symbol? s) (hashtable-contains? *valtype-symbols* s)))

(define (get-results l)
   (match-case l
      (((result . ?t) . ?rst)
       (multiple-value-bind (r tl)
          (get-results rst)
          (values (append t r) tl)))
      (else (values '() l))))

(define (get-names/params/results/tl l)
   (match-case l
      (((param (and (? symbol?) ?name (not (? valtype-symbol?))) ?t) . ?rst)
       (multiple-value-bind (n p r tl)
          (get-names/params/results/tl rst)
          (values (cons name n) (cons t p) r tl)))
      (((param . ?t) . ?rst)
       (multiple-value-bind (n p r tl)
          (get-names/params/results/tl rst)
          (values (cons (fresh-var) n) (append t p) r tl))
      (else
       (multiple-value-bind (r tl) (get-results l) (values '() '() r tl)))))

(define (get-locals/tl l)
   (match-case l
      (((local (and (? symbol?) ?name) ?type) . ?rst)
       (multiple-value-bind (n t tl) (get-locals/tl rst)
          (values (cons name n) (const type t))))
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
       (multiple-value-bind (p r -)
          (get-params/results/tl t)
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
   (for-each (lambda (x) (wr x out-port))))

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
   (define (write-fieldtype t)
      (match-case t
         ((mut ?t)
          (write-valtype typeidxs t out-port)
          (write-byte #x01 out-port))
         (else (write-valtype typeidxs t out-port))))
   (match-case t
      ((func (param . ?p) (result . ?r))
       (write-byte #x60 out-port)
       (write-vec p (lambda (t) (write-valtype typeidxs t out-port)) out-port)
       (write-vec r (lambda (t) (write-valtype typeidxs t out-port)) out-port))
      ((array ?t)
       (write-byte #x5E out-port)
       (write-fieldtype t))
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
             (write-vec idxs (lambda (x) (leb128-write-unsigned x out-port))
                        out-port))
          (write-comptype typeidxs ct out-port)))
      ((sub . ?rst)
       (multiple-value-bind (idxs ct)
          (get-typeidxs/comptype rst)
          (write-byte #x4F out-port)
          (write-vec idxs (lambda (x) (leb128-write-unsigned x out-port))
                     out-port)
          (write-comptype typeidxs ct out-port)))
      (else (write-comptype typeidxs st out-port))))

(define (write-rectype typeidxs rt out-port)
   (define (write-typedef td)
      (match-case td
         ((type (and (? symbol?) ?name) ?st)
          (write-subtype typeidxs st out-port))
         ((type ?st) (write-subtype typeidxs st out-port))))
   (match-case rt
      ((rec . ?v)
       (write-byte #x4E out-port)
       (write-vec v write-typedef out-port))
      (else (write-typedef rt))))

(define (write-globaltype typeidxs gt out-port)
   (match-case gt
      ((mut ?vt)
       (write-valtype typeidxs vt out-port)
       (write-byte #x01 out-port))
      (else (write-valtype typeidxs gt out-port)
            (write-byte #x00))))

(define (write-section secid in-port out-port vec-len)
   (write-byte secid out-port)
   (let* ((len (call-with-output-string
                  (lambda (p) (leb128-write-unsigned vec-len p))))
          (content (append len (close-output-port in-port)))
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
         (defined-types (create-hashtable :eqtest equal?))
         (ntypes 0)
         (nimports 0)
         (ncodes 0)
         (nfuncs 0)
         (nmems 0))
      (write "\x00asm\x01\x00\x00\x00" out-port) ; magic and version

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
            ((import ?- ?- (func (and ?id (? symbol?) . ?-)))
             (hashtable-put! funcidxs id nimports)
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
            ((func ?id . ?-)
             (hashtable-put! funcidxs id ncodes)
             (set! ncodes (+ 1 ncodes))
             (list m))))

      ;; returns an id for the typeuse, and the rest of the list
      (define (write-typeuse! tu)
         (match-case tu
            ; to validate we should check that the eventual params/results are
            ; correct
            (((type ?x) . ?rst)
             (multiple-value-bind (- - tl)
                (get-params/results/tl rst)
                (values (hashtable-get typeidxs x) tl)))
            ; we have params/results
            (else
             (multiple-value-bind (p r tl) (get-params/results/tl tu)
                (let* ((t `(func (param ,@p) (result ,@r)))
                       (id (get-typeidx! t)))
                   (if (= id (- ntypes 1)) ; has t already been defined ?
                       (write-comptype typeidxs t typep))
                   (values id tl))))))

      (define (write-import-desc! d)
         ; todo : tag memory table - not used in bigloo
         (match-case d
            ((or (func (? symbol?) . ?tu) (func . ?tu))
             (multiple-value-bind (id -) (write-typeuse! tu)
                (write-byte #x00 importp)
                (leb128-write-unsigned id importp)))
            ((or (global (? symbol?) ?gt) (global ?gt))
             (begin (write-byte #x03 importp)
                    (write-globaltype typeidxs gt typep)))))

      (define (write-instruction! locals labls i)
         (define (go i)
            (write-instruction! locals labls i))

         (define (write-if-branch! bt b)
            (match-case b
               ((then . ?body)
                (begin (write-byte #x04 codep)
                       (display bt codep)
                       (for-each go body)))
               (('else . ?body)
                (begin (write-byte #x04 codep)
                       (for-each go body)))
               (else (go b))))

         (define (write-blocktype! p r)
            (match-case (cons p r)
               ((() ()) ("\x40"))
               ((() (?vt))
                (call-with-output-string
                   (lambda (p) (write-valtype typeidxs vt p))))
               (else
                (call-with-output-string
                   (lambda (p)
                      (let* ((t `(func (param ,@p) (result ,@r)))
                             (id (get-typeidx! t)))
                         (if (= id (- ntypes 1)) ; has t already been defined ?
                             (write-comptype typeidxs t typep))
                         (leb128-write-signed id p)))))))

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
             (write-instruction! (cons label labls) locals `(if ,@rst)))
            ((if . ?rst)
             (multiple-value-bind (p r tl) (get-params/results/tl rst)
                (let ((bt (write-blocktype! p r)))
                   (for-each (lambda (b) (write-if-branch! bt b)) tl)
                   (write-byte #x0B codep))))
            ((block (and (? symbol?) ?label) . ?rst)
             (write-instruction! (cons label labls) locals `(block ,@rst)))
            ((block . ?rst)
             (multiple-value-bind (p r tl) (get-params/results/tl rst)
                (write-byte #x02)
                (display (write-blocktype! p r) codep)
                (for-each go tl)
                (write-byte #x0B codep)))
            ((loop (and (? symbol?) ?label) . ?rst)
             (write-instruction! (cons label labls) locals `(loop ,@rst)))
            ((loop . ?rst)
             (multiple-value-bind (p r tl) (get-params/results/tl rst)
                (write-byte #x03)
                (display (write-blocktype! p r) codep)
                (for-each go tl)
                (write-byte #x0B codep)))
            ((unreachable) (write-byte #x00 codep))
            ((nop) (write-byte #x01 codep))
            ((br ?label)
             (write-byte #x0C codep)
             (leb128-write-unsigned (labelidx label) codep))
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
                (write-byte #x0E)
                (write-vec lab
                           (lambda (l p) (leb128-write-unsigned (labelidx l) p))
                           codep)
                (leb128-write-unsigned (labelidx def) codep)))
            ((global.get (and (? symbol?) ?v))
             (write-byte #x23 codep)
             (leb128-write-unsigned (hashtable-get globalidxs v) codep))
            ((global.set (and (? symbol?) ?v) . ?tl)
             (for-each go tl)
             (write-byte #x24 codep)
             (leb128-write-unsigned (hashtable-get globalidxs v) codep))
            ((local.get (and (? symbol?) ?v))
             (write-byte #x20 codep)
             (leb128-write-unsigned (localidx v) codep))
            ((local.set (and (? symbol?) ?v) . ?tl)
             (for-each go tl)
             (write-byte #x24 codep)
             (leb128-write-unsigned (localidx v) codep))))

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
            ((func (and (? symbol?) ?id) (type x) . ?rst)
             (multiple-value-bind (ln l body) (get-locals/tl tl)
                (write-byte #x0B codep)))
            ((func (and (? symbol?) ?id) . ?rst)
             (multiple-value-bind (pn p r tl) (get-named-params/results/tl rst)
                (let ((x (fresh-var)))
                   (out-mod )))
             )))
      (for-each out-mod (caddr m))
      (write-section #x01 typep out-port ntypes)
      (write-section #x02 importp out-port nimports)))
