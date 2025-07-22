;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Definitions of the data structures representing CFGs.
;;
;; We consider typed CFGs. Each basic block has an input and an output type.
;; They correpsond to the type of the basic block as a sequence of instructions.
;; Each edge is implicitly typed with the input type of its destination.

(module cfg_node
   (from (ast_node "Ast/node.scm"))

   (export (abstract-class jump::object)
           (class unconditional::jump
              dst::cfg-node)

           ;; no expression because the jump depends on the top of the stack
           (class conditional::jump
              dst-true::cfg-node
              dst-false::cfg-node)

           (class terminal::jump
              i::instruction)

           (class switch::jump
              dsts::pair-nil)

           (class on-cast::jump
              rt1::pair
              rt2::pair
              dst-cast::cfg-node
              dst-cast-fail::cfg-node)

           (class cfg-node::object
              body::pair-nil
              (idx::long (default 1)) ;; we take as indices integers smaller or
                                      ;; equal to 0, 1 thus means we don't have
                                      ;; one yet
              (preds::pair-nil (default '()))
              intype::pair-nil
              outtype::pair-nil
              end::jump)

           (class cfg::object
              entry::cfg-node
              size::long
              rpostorder::pair-nil)

           (generic get-succs j::jump)))

(define-generic (get-succs j::jump))

(define-method (get-succs j::unconditional)
   (list (-> j dst)))

(define-method (get-succs j::conditional)
   (list (-> j dst-true) (-> j dst-false)))

(define-method (get-succs j::terminal)
   '())

(define-method (get-succs j::switch)
   (-> j dsts))

(define-method (get-succs j::on-cast)
   (list (-> j dst-cast) (-> j dst-cast-fail)))
