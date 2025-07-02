(module bin_binary
   (from (ast_node "Ast/node.scm"))
   (include "read-table.sch")
   (import leb128
           (type_type "Type/type.scm")
           (env_env "Env/env.scm"))
   (export (bin-file! p::prog file::bstring)))

(define-macro (read-opcodes name f)
   (call-with-input-file f
      (lambda (p)
         `(define ,name
             (let ((h (make-hashtable)))
                (for-each
                 (match-lambda
                   ((?sym . ?cont) (hashtable-put! h sym cont)))
                 ',(map (lambda (x) (cons (car x) (cddr x))) (cadr (read p))))
                h)))))

(read-table *valtype-symbols* "valtype-symbols.sch")
(read-table *type-abbreviations* "type-abbreviations.sch")
(define (valtype-symbol? s)
   (and (symbol? s) (hashtable-contains? *valtype-symbols* s)))

(define (type-abbreviation? s)
   (and (symbol? s) (hashtable-contains? *type-abbreviations* s)))

(read-opcodes *opcodes* "opcodes.sch")

(define type-op #f)

(define (bin-file! p::prog file::bstring)
   (set! type-op (open-output-string)))

(define-generic (write-param p::parameter op::output-port))

(define-method (write-param p::idxp op::output-port)
   (leb128-write-unsigned (-> p idx) op))

(define-method (write-param p::i32p op::output-port)
   (leb128-write-signed (-> p num) op))

(define-method (write-param p::i64p op::output-port)
   (leb128-write-signed (-> p num) op))

(define-method (write-param p::f32p op::output-port)
   (let* ((s (if (fixnum? (-> p num))
                 (float->ieee-string (fixnum->flonum (-> p num)))
                 (float->ieee-string (-> p num)))))
     (do ((i 3 (- i 1)))
         ((< i 0))
       (write-byte (char->integer (string-ref s i)) op))))

(define (write-type t op::output-port)
      (match-case t
         ((? valtype-symbol?)
          (write-byte (hashtable-get *valtype-symbols* t) op))
         ((? type-abbreviation?)
          (write-type (hashtable-get *type-abbreviations* t) op))
         ((? number?)
          (leb128-write-signed t op))
         ((ref ?t)
          (write-byte #x64 op)
          (write-type t op))
         ((ref null ?t)
          (write-byte #x63 op)
          (write-type t op))))

(define-method (write-param p::typep op::output-port)
   (write-type (-> p type) op))

(define-method (write-param p::f64p op::output-port)
   (let ((s (if (fixnum? (-> p num))
                (double->ieee-string (fixnum->flonum (-> p num)))
                (double->ieee-string (-> p num)))))
      (do ((i 7 (- i 1)))
          ((< i 0))
        (write-byte (char->integer (string-ref s i)) op))))

(define-generic (write-instruction i::instruction env::env op::output-port)
   (write (hashtable-get *opcodes* (-> i opcode))))

(define-method (write-instruction i::one-arg env::env op::output-port)
   (match-case (-> i opcode)
      (ref.test
       (with-access::typep (-> i x) (type)
          (write-byte #xFB op)
          (leb128-write-unsigned (if (nullable? type) 21 20) op)
          (write-type (reftype->heaptype type) op)))
      (ref.cast
       (with-access::typep (-> i x) (type)
          (write-byte #xFB op)
          (leb128-write-unsigned (if (nullable? type) 23 22) op)
          (write-type (reftype->heaptype type) op)))
      (array.get
       (write-byte #xFB op)
       (leb128-write-unsigned 11 op)
       (write-param (-> i x) op))
      (else
       (call-next-method)
       (write-param (-> i x) op))))

(define-method (write-instruction i::two-args env::env op::output-port)
   (match-case (-> i opcode)
      (struct.get
       (write-byte #xFB op)
       (leb128-write-unsigned 2 op)
       (write-param (-> i x) op)
       (write-param (-> i y) op))
      (else
       (call-next-method)
       (write-param (-> i x) op)
       (write-param (-> i y) op))))

(define-method (write-instruction i::three-args env::env op::output-port)
   (define (bool->number::bint b::bool) (if b 1 0))

   (match-case (-> i opcode)
      ((or br_on_cast br_on_cast_fail)
       (with-access::typep (-> i y) ((rt1 type))
          (with-access::typep (-> i z) ((rt2 type))
             (write-byte #xFB op)
             (if (eq? 'br_on_cast (-> i opcode))
                 (leb128-write-unsigned 24 op)
                 (leb128-write-unsigned 25 op))
             (write-byte (+ (bool->number (nullable? rt2))
                            (* 2 (bool->number (nullable? rt1)))) op)
             (write-param (-> i x) op)
             (write-type (reftype->heaptype rt1) op)
             (write-type (reftype->heaptype rt2) op))))
      (else
       (call-next-method)
       (write-param (-> i x) op)
       (write-param (-> i y) op)
       (write-param (-> i z) op))))

(define-method (write-instruction i::br_table env::env op::output-port)
   (write-byte #x0E op)
   (leb128-write-unsigned (-fx (length (-> i labels)) 1) op)
   (for-each (lambda (p) (write-param p op)) (-> i labels)))

(define-method (write-instruction i::sequence env::env op::output-port)
   (for-each (lambda (i) (write-instruction i env op)) (-> i body)))

(define (write-bt i::instruction env::env op::output-port)
   (match-case (cons (-> i intype) (-> i outtype))
      ((() . ()) (write-byte #x40 op))
      ((() . (?vt)) (write-type vt op))
      ((?p . ?r)
       (let ((x (-> env ntype)))
          (add-type! env #f `(deftype -1 ((sub final (func ,p ,r))) 0))
          (leb128-write-signed x op)))))

(define-method (write-instruction i::block env::env op::output-port)
   (write-byte #x02 op)
   (write-bt i env op)
   (call-next-method)
   (write-byte #x0B op))

(define-method (write-instruction i::loop env::env op::output-port)
   (write-byte #x03 op)
   (write-bt i env op)
   (call-next-method)
   (write-byte #x0B op))

(define-method (write-instruction i::if-else env::env op::output-port)
   (write-byte #x04 op)
   (write-bt i env op)
   (write-instruction (-> i then) env op)
   (write-byte #x05 op)
   (write-instruction (-> i else) env op)
   (write-byte #x0B op))

(define-generic (write-catch c::catch-branch op::output-port))

(define-method (write-catch c::catch op::output-port)
   (write-byte #x00 op)
   (write-param (-> c label) op)
   (write-param (-> c tag) op))

(define-method (write-catch c::catch_ref op::output-port)
   (write-byte #x01 op)
   (write-param (-> c label) op)
   (write-param (-> c tag) op))

(define-method (write-catch c::catch_all op::output-port)
   (write-byte #x02 op)
   (write-param (-> c label) op))

(define-method (write-catch c::catch_all_ref op::output-port)
   (write-byte #x03 op)
   (write-param (-> c label) op))

(define-method (write-instruction i::try_table env::env op::output-port)
   (write-byte #x1F op)
   (write-bt i env op)
   (leb128-write-unsigned (length (-> i catches)) op)
   (for-each (lambda (c) (write-catch c op)) (-> i catches))
   (call-next-method)
   (write-byte #x0B op))
