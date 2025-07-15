;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Interface with the official test suite.
;;
;; Semantics are not checked yet.

(module wati-test
   (import (val_validate "Val/validate.scm"))
   (library pthread srfi1)
   (main main))

(define *nerr* 0)
(define *ntest* 0)

(define (test-file p::input-port)
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
                    (raise e)
                    ))
             (valid-file m 1 #f #f)))
         ((assert_invalid ?m ?msg)
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
               msg)))))))

(define (main argv)
   (define input-files '())

   (args-parse (cdr argv)
      ((("--help" "-h") (help "Display this help message and quit"))
       (args-parse-usage #f))
      (else
       (set! input-files (cons else input-files))))

   (for-each (lambda (f) (call-with-input-file f test-file)) input-files)

   (printf "failed ~a/~a\n" *nerr* *ntest*))
