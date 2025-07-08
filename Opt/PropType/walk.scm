;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Proapagation of type information, focusing on variables. No actual
;; optimisation is performed, only types are changed. It helps removing type
;; tests for instance.
;;
;; We rely on the fact that if the content of a variable is of type t, then it
;; is still of type t after a plain instruction if it's different from local.tee
;; or local.set.
;;
;; The actual type of the content of a variable has to be distinguished from the
;; declared type of a variable. Even though a variable's content is known to be
;; of given type we can't eliminate a cast if its declared type is too big.x

(module opt_proptype
   (from (ast_node "Ast/node.scm"))
   (export (proptype! f::func)))

(define (proptype! f::func)
   #f)
