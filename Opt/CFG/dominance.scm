;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Compute dominance trees for CFGs.
;;
;; We follow the algorithm described in Cooper, K. D., Harvey, T. J., & Kennedy,
;; K. (2001). A simple, fast dominance algorithm. Software Practice &
;; Experience, 4(1-10), 1-8.

(module cfg_dominance
   (library srfi1)
   (from (cfg_node "Opt/CFG/node.scm"))
   (import (cfg_order "Opt/CFG/order.scm"))

   (export (dominance entry::cfg-node order::pair-nil n::long)))

(define (dominance entry::cfg-node order::pair-nil n::long)
   (let ((doms (make-vector n #f)))
      (vector-set! doms (-fx 0 (-> entry idx)) (-> entry idx))
      (define (step n::cfg-node)
         (fold (lambda () #f) #f (-> n preds)))
      #f))
