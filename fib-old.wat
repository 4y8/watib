(module fib
   (main main))

(define (main argv)
   (let ((n (string->number (cadr argv)))
         (N (string->number (caddr argv)))
         (a 0)
         (b 1))
     (for-each
      (lambda (_)
        (set! a 0)
        (set! b 1)
        (do ((i 0 (+ i 1)))
            ((>= i n))
          (set! b (if (and (fixnum? a) (fixnum? b))
                      (+fx/ov a b)
                      (+ a b)))
          (set! a (if (and (fixnum? a) (fixnum? b))
                      (-fx b a)
                      (- b a))))) (iota N))
     (print a))
   0)

