;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Command line interface gluing all the phases together.

(module watib
   (main main)
   (library pthread srfi1)
   (import (misc_parse   "Misc/parse.scm")
           (misc_list    "Misc/list.scm")
           (val_validate "Val/validate.scm")
           (opt_optimise "Opt/optimise.scm")
           (asm_binary   "Asm/binary.scm")
           (cfg_dump     "Opt/CFG/dump.scm")
           (cfg_walk     "Opt/CFG/walk.scm")
           (cfg_read     "Opt/CFG/read.scm")
           (env_env      "Env/env.scm")
           ))

;; the following is a hack as indices taken as number are not replaced with
;; their new index
(define (merge-files l)
   (define (get-mfs f)
      (call-with-input-file f
         (lambda (ip)
            (match-case (read ip #t)
               ((or (module (? ident?) . ?mfs) (module . ?mfs)) mfs)
               (?m (error/location "watib" "expected module" m
                                   (cadr (cer m)) (caddr (cer m))))))))

   (econcat (map get-mfs l)))

(define (main argv)
   (define input-files '())
   (define nthreads 1)
   (define keep-going #f)
   (define output-file "a.out.wasm")
   (define silent #f)
   (define validate-only #f)
   (define o-flags::opt-flags (instantiate::opt-flags))
   (define dump-cfg #f)
   (define dump-cfg-wat #f)
   (define cfg-file #f)

   (define (parse-args args)
      (args-parse args
         ((("--help" "-h") (help "Display this help message and quit"))
          (args-parse-usage #f))
         ((("--keep-going" "-k") (help "Continue when encountering an error"))
          (set! keep-going #t))
         (("--stop-after" ?n (help "Stop after encountering N errors"))
          (set! keep-going (string->number n)))
         (("-s" (help "Display less verbose error messages"))
          (set! silent #t))
         (("-j" ?n (help "Use multiple threads"))
          (set! nthreads (string->number n)))
         ((("--validate-only" "-v") (help "Validate only"))
          (set! validate-only #t))
         (("-O0" (help "Disable optimisations"))
          (with-access::opt-flags o-flags (testbr copyprop uncast unreachable
                                                  const puredrop peephole)
             (set! testbr #f)
             (set! copyprop #f)
             (set! uncast #f)
             (set! unreachable #f)
             (set! const #f)
             (set! puredrop #f)
             (set! peephole #f)))
         (("-O1" (help "Enable all optimisations (default)"))
          (with-access::opt-flags o-flags (testbr copyprop uncast unreachable
                                                  const puredrop peephole)
             (set! testbr #t)
             (set! copyprop #t)
             (set! uncast #t)
             (set! unreachable #t)
             (set! const #t)
             (set! puredrop #t)
             (set! peephole #t)))

         (("-fno-testbr" (help "Disable type based control flow rewriting"))
           (set! (-> o-flags testbr) #f))
         (("-ftestbr" (help "Enable type based control flow rewriting"))
           (set! (-> o-flags testbr) #t))

         (("-fno-copyprop" (help "Disable copy propagation"))
           (set! (-> o-flags copyprop) #f))
         (("-fcopyprop" (help "Enable copy propagation"))
           (set! (-> o-flags copyprop) #t))

         (("-fno-uncast" (help "Disable redundant type tests and casts elimination"))
           (set! (-> o-flags uncast) #f))
         (("-funcast" (help "Enable redundant type tests and casts elimination"))
           (set! (-> o-flags uncast) #t))

         (("-fno-unreachable" (help "Disable unreachable code elimination"))
           (set! (-> o-flags unreachable) #f))
         (("-funreachable" (help "Enable unreachable code elimination"))
           (set! (-> o-flags unreachable) #t))

         (("-fno-const" (help "Disable constant folding"))
           (set! (-> o-flags const) #f))
         (("-fconst" (help "Enable constant folding"))
           (set! (-> o-flags const) #t))

         (("-fno-puredrop" (help "Disable redundant drop elimination"))
           (set! (-> o-flags puredrop) #f))
         (("-fpuredrop" (help "Enable redundant drop elimination"))
           (set! (-> o-flags puredrop) #t))

         (("-fno-peephole" (help "Disable peephole elimination"))
           (set! (-> o-flags peephole) #f))
         (("-fpeephole" (help "Enable peephole elimination"))
           (set! (-> o-flags peephole) #t))

         (("-o" ?file (help "Output binary format to FILE"))
          (set! output-file file))

         (("--dump-cfg" ?func (help "Prints the CFG on FUNC in graphviz DOT format"))
          (set! dump-cfg (string->symbol func)))

         (("--dump-cfg-wat" ?func (help "Prints the CFG on FUNC in CFGWAT format"))
          (set! dump-cfg-wat (string->symbol func)))

         (("--read-cfg" ?file (help "Reads the CFG in FILE and dumps it"))
          (set! cfg-file file))

         (else
          (set! input-files (cons else input-files)))))

   (define (watib m)
      (let ((p (with-handler
          (lambda (e)
             (when (isa? e &watib-validate-error)
                (exit 1))
             (raise e))
          (valid-file m nthreads keep-going silent))))
     (cond
      ((not p)
       (exit 1))
      (validate-only
       (exit 0))
      (dump-cfg
       (with-access::prog p (funcs env)
          (let ((cfg (func->cfg
                      (vector-ref funcs (func-get-index env dump-cfg)))))
             (print-cfg-as-dot cfg)
             (call-with-output-file "out.wat"
                (lambda (op)
                   (with-output-to-port op
                      (lambda ()
                        (dump-instr (cfg->wasm cfg) 0))))))))
      (dump-cfg-wat
       (with-access::prog p (funcs env)
          (let ((cfg (func->cfg
                      (vector-ref funcs (func-get-index env dump-cfg-wat)))))
             (print-cfg-as-cfgwat cfg)
             (call-with-output-file "out.wat"
                (lambda (op)
                   (with-output-to-port op
                      (lambda ()
                        (dump-instr (cfg->wasm cfg) 0))))))))
        (else
         (opt-file! p nthreads o-flags)
         (call-with-output-file output-file
            (lambda (op) (asm-file! p op)))))))

   (parse-args (cdr argv))

   (cond
    (cfg-file
     (call-with-input-file cfg-file
        (lambda (ip)
           (multiple-value-bind (g::cfg p) (read-cfg/prog ip)
              (print-cfg-as-dot g)
              (call-with-output-file "out.wat"
                 (lambda (op)
                    (with-output-to-port op
                       (lambda ()
                          (dump-instr (cfg->wasm g) 0)
                          (call-with-output-file output-file
                             (lambda (op) (asm-file! p op)))))))))))
    ((null? input-files)
     (watib (read (current-input-port) #t)))
    (else (watib `(module ,@(merge-files input-files))))))
