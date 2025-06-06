(module binary
   (import leb128))

(define (get-params/results l)
   (define (get-results l)
      (match-case l
         (() '())
         (((result ?t) . ?rst)
          (append t (get-results rst)))))
   (match-case l
      (() (values '() '()))
      ; we don't support named parameters yet, they are not used by bigloo
      (((param . ?t) . ?rst)
       (multiple-value-bind (p r)
          (get-params/results rst)
          (values (append t p) r)))
      (else (values '() (get-results l)))))

(define (write-string n out-port)
   (leb128-write-unsigned (string-length n) out-port)
   (display n out-port))

(define (write-vec v wr out-port)
   (leb128-write-unsigned (length v) out-port)
   (for-each (lambda (x) (wr x out-port))))

(define (write-valtype typeidxs t out-port)
   (match-case t
      ((i32) (write-byte #x7F out-port))
      ((i64) (write-byte #x7E out-port))
      ((f32) (write-byte #x7D out-port))
      ((f64) (write-byte #x7C out-port))
      ((v128) (write-byte #x7B out-port))
      ; these two are not valtypes but we do not validate
      ((i8) (write-byte #x78 out-port))
      ((i16) (write-byte #x77 out-port))
      ((nofunc) (write-byte #x73 out-port))
      ((noextern) (write-byte #x72 out-port))
      ((none) (write-byte #x71 out-port))
      ((func) (write-byte #x70 out-port))
      ((extern) (write-byte #x6F out-port))
      ((any) (write-byte #x6E out-port))
      ((eq) (write-byte #x6D out-port))
      ((i31) (write-byte #x6C out-port))
      ((struct) (write-byte #x6B out-port))
      ((array) (write-byte #x6A out-port))
      ((? symbol?) (leb128-write-signed (hashtable-get typeidxs t) out-port))
      ((ref ?t) (begin (write-byte #x64 out-port)
                       (write-valtype typeidxs t out-port)))
      ((ref null ?t) (begin (write-byte #x63 out-port)
                            (write-valtype typeidxs t out-port)))))

(define (write-comptype typeidxs t out-port)
   (define (write-fieldtype t)
      (match-case t
         ((mut ?t) (begin (write-valtype typeidxs t out-port)
                          (write-byte #x01 out-port)))
         (else (write-valtype typeidxs t out-port))))
   (define (clean-field fd)
      (match-case fd
         ((or (field (? symbol?) ?ft) (field ?ft)) ft)))
   (match-case t
      ((func . ?rst)
       (multiple-value-bind (p r) (get-params/results rst)
          (write-byte #x60 out-port)
          (write-vec p (lambda (t) (write-valtype typeidxs t out-port)) out-port)
          (write-vec r (lambda (t) (write-valtype typeidxs t out-port)) out-port)))
      ((array ?t) (begin (write-byte #x5E out-port)
                         (write-fieldtype t)))
      ((struct . ?fds)
       (begin (write-byte #x5F out-port)
              (write-vec (map clean-field fds) write-fieldtype out-port)))))

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

(define (write-rectype typeidxs skipped-types rt out-port)
   (define (write-typedef td)
      (match-case td
         ((type (and (? symbol?) ?name) ?st)
          (unless (hashtable-contains? skipped-types name)
             (write-subtype typeidxs st out-port)))))
   (match-case rt
      ((rec . ?v) (begin (write-byte #x4E out-port)
                         (write-vec v write-typedef out-port)))
      (else (write-typedef rt))))

(define (write-globaltype typeidxs gt out-port)
   (match-case gt
      ((mut ?vt) (begin (write-valtype typeidxs vt out-port)
                        (write-byte #x01 out-port)))
      (else (write-valtype typeidxs gt out-port)
            (write-byte #x00))))

(define (write-section secid in-port out-port vec-len)
   (write-byte secid out-port)
   (let* ((len (call-with-output-string
                  (lambda (p) (leb128-write-unsigned vec-len p))))
          (content (append len (close-output-port in-port)))
          (size (string-length content)))
      ; only one byte indicates that we have a 0 length vector, which means we
      ; can omit the section
      (unless (= size 1)
         (write-byte secid out-port)
         (write-string content out-port))))

(define (write-module m out-port)
   (let ((typep (open-output-string))
         (importp (open-output-string))
         (funcp (open-output-string))
         (codep (open-output-string))
         (typeidxs (make-hashtable))
         (funcidxs (make-hashtable))
         (skipped-types (make-hashtable))
         (defined-types (create-hashtable :eqtest equal?))
         (ntypes 0)
         (nfuncs 0))
      (write "\x00asm\x01\x00\x00\x00" out-port) ; magic and version
      (define (write-typeuse! tu)
         (match-case tu
            ; to validate we should check that the eventual params/results are
            ; correct
            (((type ?x) . ?-)
             (hashtable-get typeidxs x))
            ; we have params/results
            (else
             (multiple-value-bind (p r)
                (get-params/results tu)
                (let* ((t `(func ,@p ,@r))
                       (h (hashtable-get defined-types t)))
                   (if h
                       h
                       (begin
                          (hashtable-put! defined-types t ntypes)
                          (set! ntypes (+ 1 ntypes))
                          (write-comptype
                              typeidxs `(func (param ,@p) (result ,@r))
                              typep))))))))
      (define (write-import-desc! d)
         ; todo : tag memory table - not used in bigloo
         (match-case d
            ((or (func (? symbol?) . ?tu) (func . ?tu))
             (begin (write-byte #x00 importp)
                    (leb128-write-unsigned (write-typeuse! tu) importp)))
            ((or (global (? symbol?) ?gt) (global ?gt))
             (begin (write-byte #x03 importp)
                    (write-globaltype typeidxs gt typep)))))
      (define (out-mod m)
         (match-case m
            ((import ?mod ?nm ?d)
             (begin (write-string mod importp)
                    (write-string nm importp)
                    (write-import-desc! d)))))))
