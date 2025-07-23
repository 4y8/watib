;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Read CFGs for testing purposes.
;;
;; A CFG is a prelude followed by a type delcaration for the CFG and a
;; sequence of nodes. Such a graph corresponds to a Wasm function
;;
;; A node is of the form:
;; (node <name> (param <vt>*)
;;   (body <instr>*)
;;   (end <jump>))
;;
;; And a jump is of the form:
;; | (goto <name>)
;; | (if <name> <name>)
;; | (return <instr>)
;; | (on-cast <name> <name> <rt1> <rt2>)
;; | (switch <name>*)
;;
;; The prelude is of the form (prelude <module-decl>*), where <module-decl>
;; refers to the usual Wasm module declaration. It can be used to declare types
;; for instance. The type declaration of the CFG is of the form form:
;; (cfg (param <name> <vt>)*
;;      (result <vt>*)
;;      (local <name> <vt>)*
;;      (entry <name>)).

(module cfg_read
   (import (ast_node "Ast/node.scm"))
   (import (env_env "Env/env.scm"))
   (from (cfg_node "Opt/CFG/node.scm"))

   (export (read-cfg::cfg p::input-port)))

(define (read-jump::jump j nodes)
   (match-case j
      ((goto ?n)
       (instantiate::unconditional (dst (hashtable-get nodes n))))
      ((if ?t ?f)
       (instantiate::conditional (dst-true (hashtable-get nodes t))
                                 (dst-false (hashtable-get nodes t))))
      (else (error "watib" "expected jump got" j))))

(define (read-node::cfg-node n nodes)
   (match-case n
      ((node ?- (param . ?vts)
         (body . ?instrs)
         (end . ?j))
       ())

      (else (error "watib" "expected node got" n))))
