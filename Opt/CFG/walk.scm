;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Conversion from Wasm/CFG to CFG/Wasm.

(module cfg_walk
   (library srfi1)

   (from (cfg_node "Opt/CFG/node.scm"))
   (from (ast_node "Ast/node.scm")))

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

(define (do-tree entry::cfg-node doms::vector order::pair-nil ctx::pair-nil)
   #f)

(define (do-branch src dst ctx::pair-nil)
   #f)

(define (node-within n::cfg-node trees::pair-nil ctx::pair-nil)
   #f)

(define (cfg->wasm entry::cfg-node doms::vector order::pair-nil n::long)
   (define tree-vect (make-vector n '()))
   (for-each (lambda (n::cfg-node)
               (let* ((p::cfg-node (vector-ref doms (-fx 0 (-> n idx)))))
                  (vector-set! tree-vect (-> p idx)
                               (cons n (vector-ref tree-vect
                                                   (-fx 0 (-> p idx)))))))
             order)

   (define (build-tree n::cfg-node)
      (cons n (map build-tree (vector-ref tree-vect (-fx 0 (-> n idx))))))

   (do-tree doms order (build-tree entry) '()))

(define (wasm->cfg i::sequence)
   (define (build-node l::pair-nil seq-intype::pair-nil st::pair-nil
                       body::pair-nil next::cfg-node labs::pair-nil)
      (cond
       ((null? l) next)

       ((isa? (car l) if-then)
        (with-access::if-then (car l) (then intype outtype)
           (let* ((new-st (append outtype (drop st (length intype))))
                  (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                           labs)))
              (instantiate::cfg-node
               (intype seq-intype)
               (outtype st)
               (body (reverse body))
               (end (instantiate::conditional
                     (dst-true
                      (build-node (with-access::sequence then (body) body)
                                  intype intype '() n (cons n labs)))
                     (dst-false
                      (if (isa? (car l) if-else)
                          (with-access::if-else (car l) (else)
                           (build-node (with-access::sequence else (body) body)
                                  intype intype '() n (cons n labs)))
                          n))))))))

       ;; ((isa? (car l) block)
       ;;  (with-access::block (car l) (intype outtype body)
       ;;     (let* ((new-st (append outtype (drop st (length intype))))
       ;;            (n::cfg-node (build-node (cdr l) new-st new-st '() next
       ;;                                     labs)))
       ;;        (instantiate::cfg-node
       ;;         (intype seq-intype)
       ;;         (outtype st)
       ;;         (body (reverse body))
       ;;         (end (instantiate::unconditional
       ;;               (dst (build-node ))))))))

       ))
   #f)
