;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Conversion from Wasm/CFG to CFG/Wasm.

(module cfg_walk
   (library srfi1)

   (from (cfg_node "Opt/CFG/node.scm"))
   (from (ast_node "Ast/node.scm"))

   (import (cfg_order "Opt/CFG/order.scm"))

   (export (func->cfg::cfg f::func)))

;; We implement the algorithm from Ramsey, N. (2022). Beyond Relooper: recursive
;; translation of unstructured control flow to structured control flow
;; (functional pearl). Proceedings of the ACM on Programming Languages, 6(ICFP),
;; 1-22.

(define (back-inedge? src::cfg-node dst::cfg-node)
   (>=fx (-> src idx) (-> dst idx)))

(define (loop-header? n::cfg-node)
   (any (lambda (src::cfg-node) (back-inedge? src n)) (-> n preds)))

(define (merge-node? n::cfg-node)
   (let* ((forward-pred? (lambda (src::cfg-node) (back-inedge? n src)))
          (tl (find-tail forward-pred? (-> n preds))))
      (and tl (any forward-pred? (cdr tl)))))

(define (merge-head? t::pair)
   (merge-node? (car t)))

(define (do-tree tree::pair ctx::pair-nil)
   (with-access::cfg-node (car tree) (idx intype)
      (if (loop-header? (car tree))
          (multiple-value-bind (body outtype)
             (node-within (car tree) (filter merge-head? (cdr tree))
              (cons `(loop-headed-by ,idx) ctx))
             (instantiate::loop
              (opcode 'loop)
              (intype intype)
              (body body)
              (parent (instantiate::modulefield))
              (outtype '()))) ;; what should really be outtype
          (multiple-value-bind (body outtype)
             (node-within (car tree) (filter merge-head? (cdr tree)) ctx)
             (instantiate::sequence
              (opcode 'nop)
              (body body)
              (intype intype)
              (outtype '())
              (parent (instantiate::modulefield)))))))

(define (do-branch src::cfg-node dst::cfg-node ctx::pair-nil)
   #f)

(define-generic (branch-after-body j::jump body::pair-nil ctx::pair-nil))

(define-method (branch-after-body j::unconditional body::pair-nil ctx::pair-nil)
   #f)

(define (node-within n::cfg-node trees::pair-nil ctx::pair-nil)
   (if (null? trees)
       #f))

(define (cfg->wasm entry::cfg-node doms::vector order::pair-nil n::long)
   (define tree-vect (make-vector n '()))
   (for-each (lambda (n::cfg-node)
               (let* ((p::cfg-node (vector-ref doms (-fx 0 (-> n idx)))))
                  (vector-set! tree-vect (-> p idx)
                               (cons n (vector-ref tree-vect
                                                   (-fx 0 (-> p idx)))))))
             order)

   (define (build-tree n::cfg-node)
      (define (node<? x::cfg-node y::cfg-node)
         (>fx (-> x idx) (-> y idx)))
      (cons n (map build-tree
                   (sort node<? (vector-ref tree-vect (-fx 0 (-> n idx)))))))

   (do-tree (build-tree entry) '()))

(define (func->cfg f::func)
   (define (build-node l::pair-nil seq-intype::pair-nil st::pair-nil
                       body::pair-nil next::cfg-node labs::pair-nil
                       exp-outtype::pair-nil)
      (define (end-current-block end::jump)
         (if (and (null? body) (isa? end unconditional))
             (with-access::unconditional end (dst) dst)
             (instantiate::cfg-node
              (intype seq-intype)
              (outtype st)
              (body (reverse body))
              (end end))))

      (cond
       ((null? l)
        (if (null? body) next
            (end-current-block (instantiate::unconditional (dst next)))))

       ((isa? (car l) if-then)
        (with-access::if-then (car l) (then intype outtype)
           (let* ((new-st (append (reverse outtype) (drop st (length intype))))
                  (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                           labs exp-outtype)))
              (end-current-block
               (instantiate::conditional
                (dst-true
                 (build-node (with-access::sequence then (body) body)
                             intype intype '() n (cons n labs) outtype))
                (dst-false
                 (if (isa? (car l) if-else)
                     (with-access::if-else (car l) (else)
                        (build-node (with-access::sequence else (body) body)
                                    intype intype '() n (cons n labs) outtype))
                     n)))))))

       ((isa? (car l) block)
        (with-access::block (car l) (intype outtype (block-body body))
           (let* ((new-st (append (reverse outtype) (drop st (length intype))))
                  (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                           labs exp-outtype)))
              (end-current-block
               (instantiate::unconditional
                (dst (build-node block-body intype intype '() n
                                 (cons n labs) outtype)))))))

       ((isa? (car l) loop)
        (with-access::loop (car l) (intype outtype (loop-body body))
           (let* ((new-st (append (reverse outtype) (drop st (length intype))))
                  (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                           labs exp-outtype))
                  ;; created to break cyclicity
                  (dummy-node::cfg-node (instantiate::cfg-node
                                         (body '())
                                         (intype '())
                                         (outtype '())
                                         (end (instantiate::unconditional
                                               (dst n)))))
                  (loop-head::cfg-node
                   (build-node loop-body intype intype '() n
                               (cons dummy-node labs) outtype)))

              (set! (-> dummy-node body) (-> loop-head body))
              (set! (-> dummy-node intype) (-> loop-head intype))
              (set! (-> dummy-node outtype) (-> loop-head outtype))
              (set! (-> dummy-node end) (-> loop-head end))

              (end-current-block
                (instantiate::unconditional (dst dummy-node))))))

       ((isa? (car l) try_table)
        (error "watib" "try_table not supported yet in CFG conversion" (car l)))

       (else
        (with-access::instruction (car l) (opcode intype outtype)
           (match-case opcode
              (br
               (with-access::one-arg (car l) (x)
                  (with-access::labelidxp x (idx)
                     (instantiate::cfg-node
                      (intype seq-intype)
                      (outtype exp-outtype)
                      (body (reverse body))
                      (end (instantiate::unconditional
                            (dst (list-ref labs idx))))))))

              (br_on_cast
               (with-access::three-args (car l) (x y z)
                  (with-access::labelidxp x (idx)
                     (end-current-block (instantiate::on-cast
                                         (dst-cast-fail
                                          (build-node (cdr l) st st '()
                                                      next labs exp-outtype))
                                         (rt-src y)
                                         (rt-dst z)
                                         (dst-cast (list-ref labs idx)))))))

              (else
               (let ((new-st (append (reverse outtype)
                                     (drop st (length intype)))))
                  (build-node (cdr l) new-st new-st (cons (car l) body)
                              next labs exp-outtype))))))))

   (let* ((ret-node (instantiate::cfg-node
                     (intype (cadr (-> f type)))
                     (outtype '())
                     (body '())
                     (end (instantiate::terminal
                           (i (instantiate::instruction
                               (opcode 'return)
                               (outtype '())
                               (intype (cadr (-> f type)))
                               (parent f)))))))
          (entry (build-node
                 (with-access::sequence (-> f body) (body) body)
                 '()
                 '()
                 '()
                 ret-node
                 (list ret-node)
                 (cadr (-> f type)))))
     (multiple-value-bind (-size rpostorder) (reverse-postorder! entry)
        (instantiate::cfg
         (entry entry)
         (size (-fx 0 -size))
         (rpostorder rpostorder)))))
