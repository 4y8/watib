(module watib
   (main main)
   (library pthread srfi1)
   (import (val_validate "Val/validate.scm")
           (opt_optimise "Opt/optimise.scm")
           (bin_binary   "Bin/binary.scm")))

(define (main argv)
   (define input-file #f)
   (define nthreads 1)
   (define keep-going #f)
   (define output-file "a.out.wasm")
   (define silent #f)

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
      (("-o" ?file (help "Output binary format to FILE"))
       (set! output-file file))
      (else
       (set! input-file else)))

   (if input-file
       (call-with-input-file input-file
          (lambda (ip)
             (let ((p (valid-file (read ip #t) nthreads keep-going silent)))
                (if p
                    (begin
                       (opt-file! p nthreads)
                       (bin-file! p output-file))
                    (exit 1)))))))
