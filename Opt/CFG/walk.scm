;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Conversion from Wasm/CFG to CFG/Wasm.

(module cfg_walk
   (from (cfg_node "Opt/CFG/node.scm"))
   (from (ast_node "Ast/node.scm")))

;; We implement the algorithm from Ramsey, N. (2022). Beyond Relooper: recursive
;; translation of unstructured control flow to structured control flow
;; (functional pearl). Proceedings of the ACM on Programming Languages, 6(ICFP),
;; 1-22.

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
                                                   (-fx 0 (-> p idx))))))))

   (define (build-tree n::cfg-node)
      (cons n (map build-tree (vector-ref tree-vect (-fx 0 (-> n idx))))))

   (do-tree doms order (build-tree entry) '()))
