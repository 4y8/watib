;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Definitions of the data structures representing CFGs.
;;
;; We consider typed CFGs. Each basic block has an input. They do not have
;; output type as their output type as a wasm block will be determined during
;; relooping (it depends on the following block if it ends with a branch).

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
              rt-dst::pair
              rt-src::pair
              dst-cast::cfg-node
              dst-cast-fail::cfg-node)

           (final-class cfg-node::object
              body::pair-nil
              (idx::long (default 1)) ;; we take as indices integers smaller or
                                      ;; equal to 0, 1 thus means we don't have
                                      ;; one yet
              (preds::pair-nil (default '()))
              intype::pair-nil
              end::jump)

           (class cfg::object
              entry::cfg-node
              size::long
              rpostorder::pair-nil
              func::func)

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
