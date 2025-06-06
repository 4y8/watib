(module leb128
   (export (leb128-write-signed n out-port)
           (leb128-write-unsigned n out-port)))

;; see section 5.2.2 of wasm spec and https://en.wikipedia.org/wiki/LEB128
(define (leb128-write-unsigned n out-port)
   (if (< n 128)
       (write-byte n out-port)
       (begin
          (write-byte (+ 128 (modulo n 128)) out-port)
          (leb128-write-unsigned (quotient n 128) out-port))))

(define (leb128-write-signed n out-port)
   (cond
      ((and (<= 0 n) (> 64 n))
       (write-byte n out-port))
      ((and (> 0 n) (<= -64 n))
       (write-byte (+ 128 n) out-port))
      (#t
       (let ((m (modulo n 128)))
          (write-byte (+ 128 (modulo n 128)) out-port)
          (leb128-write-signed (quotient (- n m) 128) out-port)))))
