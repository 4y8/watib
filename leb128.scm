(module leb128
   (export (leb128-write-signed n out-port)
           (leb128-write-unsigned n out-port)))

(define (number->byte b)
   (cond
    ((elong? b) (elong->fixnum b))
    ((bignum? b) (bignum->fixnum b))
    (#t b)))

(define (new-write-byte b out-port)
   (cond ((elong? b)
          (write-byte (elong->fixnum b) out-port))
         ((bignum? b)
          (write-byte (bignum->fixnum b) out-port))
         (#t (write-byte b out-port))))

;; see section 5.2.2 of wasm spec and https://en.wikipedia.org/wiki/LEB128
(define (leb128-write-unsigned n out-port)
   (if (< n 128)
       (new-write-byte n out-port)
       (begin
          (new-write-byte (+ 128 (modulo n 128)) out-port)
          (leb128-write-unsigned (quotient n 128) out-port))))

(define (leb128-write-signed n out-port)
   (cond
      ((and (<= 0 n) (> 64 n))
       (new-write-byte n out-port))
      ((and (> 0 n) (<= -64 n))
       (new-write-byte (+ 128 n) out-port))
      (#t
       (let* ((m (modulo n 128)))
          (new-write-byte (+ 128 m) out-port)
          (leb128-write-signed (quotient (- n m) 128) out-port)))))
