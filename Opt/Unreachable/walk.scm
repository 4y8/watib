;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Removal of some unreachable code.

(module opt_unreachable
   (from (ast_node "Ast/node.scm"))
   (export (unreachable! f::func)))

(define (unreachable! f::func)
   #f)
