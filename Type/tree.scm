;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Getting subtypes of a type.

(module type_tree
   (library srfi1)
   (from (ast_node "Ast/node.scm"))
   (import (type_type "Type/type.scm"))
   (export
    (class subtyping::object
       dt::vector
       func::pair-nil
       array::pair-nil
       struct::pair-nil
       eq::pair-nil)
    (build-subtypes::subtyping env::env)
    (get-subvt::pair-nil vt sub::subtyping)))

(define (build-subtypes::subtyping env::env)
   (let* ((n (-> env ntype))
          (subtypes (make-vector n '()))
          (func '())
          (array '())
          (struct '())
          )

      (define (add-ht s::symbol i::long)
         (match-case s
            (func (set! func (cons i func)))
            (array (set! array (cons i array)))
            (struct (set! struct (cons i struct)))))

      (do ((i (-fx n 1) (-fx i 1)))
          ((<fx i 0))
        (let ((dt (vector-ref (-> env types) i)))
          (vector-set! subtypes i (cons i (vector-ref subtypes i)))
          (match-case (unroll-dt dt)
             ((or (sub final (?h . ?-)) (sub (?h . ?-))) (add-ht h i))
             ((or (sub final ?idx (?h . ?-)) (sub ?idx (?h . ?-)))
              (add-ht h i)
              (vector-set! subtypes (cer idx)
                           (lset-union =fx
                                       (vector-ref subtypes i)
                                       (vector-ref subtypes (cer idx))))))))

      (instantiate::subtyping
       (dt subtypes)
       (eq (iota n))
       (func func)
       (array array)
       (struct struct))))

(define (get-subht::pair-nil t sub::subtyping)
   (cond
    ((deftype? t) (vector-ref (-> sub dt) (cer t)))
    ((number? t) (vector-ref (-> sub dt) t))
    ((eq? 'eq t) (cons 'i31 (-> sub eq)))
    ((eq? 'func t) (-> sub func))
    ((eq? 'array t) (-> sub array))
    ((eq? 'struct t) (-> sub struct))
    ((eq? 'none t) '(none))
    ((eq? 'i31 t) '(i31))
    (else (error "get-subht" "unknown type" t))))

(define (get-subvt::pair-nil vt sub::subtyping)
   (match-case vt
      ((ref ?ht) (map (lambda (ht) `(ref ,ht)) (get-subht ht sub)))
      ((ref null none) '((ref null none)))
      ((ref null ?ht) (cons '(ref null none) (get-subvt `(ref ,ht) sub)))
      (?vt (list vt))))
