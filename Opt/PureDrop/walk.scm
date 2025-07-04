;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Removal of expressions of the form (drop e) when e has no side effect.

(module opt_puredrop
   (from (ast_node "Ast/node.scm"))
   (include "Misc/read-table.sch")
   (export (puredrop! f::func)))

(read-table *side-effect* "Opt/PureDrop/side-effect-table.sch")

(define (puredrop! f::func)
   (side-effect!? (-> f body)))

(define-generic (side-effect!? i::instruction)
   ())
