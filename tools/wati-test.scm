;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Interface with the official test suite.
;;
;; We take as input wast files. For a detailed description of the format, see:
;; https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#scripts.
;;
;; We output wast files where the module declarations have been taken by binary
;; inserts corresponding to watib's output on the module.

(module wati-test
   (import (val_validate "Val/validate.scm")
           (opt_optimise "Opt/optimise.scm")
           (asm_binary   "Asm/binary.scm"))
   (library pthread srfi1)
   (main main))

(define *nerr* 0)
(define *ntest* 0)

(define (test-file p::input-port)
   (define op (open-output-file "out.wast"))
   (do ((m (read p #t) (read p #t)))
       ((eof-object? m))
      (set! *ntest* (+ 1 *ntest*))
      (match-case m
         ((module . ?-)
          (with-handler
             (lambda (e)
                (if (isa? e &watib-validate-error)
                    (begin
                       (set! *nerr* (+ 1 *nerr*))
                       (warning/location
                        (cadr (cer m)) (caddr (cer m))
                        "wati-test"
                        "module was invalidated while it is valid --- "
                        (with-access::&watib-validate-error e (obj) obj)))
                    (raise e)))
             (let ((p::prog (valid-file m 1 #f #f)))
                (opt-file! p 1)
                (display "(module binary \"" op)
                (let* ((s (string-hex-extern (call-with-output-string
                                              (lambda (op) (asm-file! p op)))))
                       (n (string-length s)))
                  (do ((i 0 (+fx i 1)))
                      ((>= i n))
                     (when (even? i)
                        (display "\\" op))
                     (write-char (string-ref s i) op)))
                (display "\")\n" op))))
         (((or assert_malformed assert_invalid) ?m ?msg)
          (define err? #f)
          (with-handler
             (lambda (e)
                (if (isa? e &watib-validate-error)
                    (set! err? #t)
                    (raise e)))
             (valid-file m 1 #f #f))
          (unless err?
             (set! *nerr* (+ 1 *nerr*))
             (warning/location
              (cadr (cer m)) (caddr (cer m))
              "wati-test"
              (string-append
               "module was validated while it shouldn't because "
               msg))))
         ((assert_return (invoke ?s . ?args) ?r)
          (display "(assert_return (invoke " op)
          (write s op)
          (for-each (lambda (obj) (display obj op)) args)
          (display ")" op)
          (display r op)
          (display ")\n" op))))
   (close-output-port op))

(define (main argv)
   (define input-file #f)

   (args-parse (cdr argv)
      ((("--help" "-h") (help "Display this help message and quit"))
       (args-parse-usage #f))
      (else
       (set! input-file else)))

   (call-with-input-file input-file test-file)

   (printf "failed ~a/~a\n" *nerr* *ntest*)
   (unless (=fx *nerr* 0)
      (exit 1)))
