;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Removal of expressions of the form (drop e) when e has no side effect.
;;
;; The detection of side effects is coarse. For instance, jumps are considered
;; to be side effects. However in a block of label $l, jumping to $l should not
;; cause the block to be marked as having a side-effect.

(module opt_puredrop
   (library srfi1)
   (from (ast_node "Ast/node.scm"))
   (include "Misc/read-table.sch")
   (import (misc_list "Misc/list.scm"))
   (export (puredrop! f::func)))

(read-table *side-effect* "Opt/PureDrop/side-effect-table.sch")

(define (puredrop! f::func)
   (side-effect!? (-> f body)))

;; to allow for a finer side-effect detection, we would return something bigger
;; than a boolean, for instance, we could add a list of labels we can branch to
(define-generic (side-effect!?::bool i::instruction)
   (if (hashtable-contains? *side-effect* (-> i opcode))
       (hashtable-get *side-effect* (-> i opcode))
       #t))

(define-method (side-effect!?::bool i::try_table)
   (call-next-method)
   #t) ; can branch

(define-method (side-effect!?::bool i::if-then)
   (side-effect!? (-> i then)))

(define-method (side-effect!?::bool i::if-else)
   (let ((b (side-effect!? (-> i else))))
      (or (call-next-method) b)))

(define (isa-drop? i::instruction)
   (eq? (-> i opcode) 'drop))

(define-method (side-effect!?::bool i::sequence)
   (define (drops::pair-nil n::bint)
      (list-tabulate n (lambda (-) (instantiate::instruction
                                    (intype '(top))
                                    (outtype '())
                                    (opcode 'drop)
                                    (parent (-> i parent))))))

   (define side-effect? #f)

   (define (remove-pures::pair-nil l::pair-nil n::bint)
      (cond
       ((=fx 0 n) l)
       ((null? l) (drops n))
       (else
        (with-access::instruction (car l) (outtype)
           (let ((b (side-effect!? (car l))))
              (set! side-effect? (or b side-effect?))
              (if (length>=? outtype (+fx n 1))
                  (append (drops n) l)
                  (let ((m (length outtype)))
                     (if (=fx 0 m)
                         (append (car l) (remove-pures (cdr l) n))
                         (append (if b (append (drops m) (list (car l))) '())
                                 (remove-pures (cdr l) (-fx n m)))))))))))

   (define (walk-zipper::pair-nil left::pair-nil right::pair-nil)
      (if (null? right)
          (reverse left)
          (multiple-value-bind (pre suf) (span isa-drop? right)
             (if (null? pre)
                 (begin
                   (set! side-effect? (or (side-effect!? (car right))
                                          side-effect?))
                   (walk-zipper (cons (car right) left) (cdr right)))
                 (walk-zipper (remove-pures left (length pre)) suf)))))

   (set! (-> i body) (walk-zipper '() (-> i body)))
   side-effect?)
