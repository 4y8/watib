;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Definitions of the data structures representing CFGs.

(module cfg_node
   (export (abstract-class jump::object)
           (class direct-jump::jump
              dst::cfg-node)

           ;; no expression because the jump depends on the top of the stack
           (class if-jump::jump
              dst-true::cfg-node
              dst-false::cfg-node)

           (class cfg-node::object
              body::pair-nil
              (idx::long (default 1)) ;; we take as indices integers smaller or
                                      ;; equal to 0, 1 thus means we don't have
                                      ;; one yet
              (preds::pair-nil (default '()))
              end::jump)
           (generic get-succs j::jump)))

(define-generic (get-succs j::jump))

(define-method (get-succs j::direct-jump)
   (list (-> j dst)))

(define-method (get-succs j::if-jump)
   (list (-> j dst-true) (-> j dst-false)))
