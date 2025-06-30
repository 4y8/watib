(module opt_testbr
   (import (ast_node "Ast/node.scm"))
   (export (testbr! f::func)))

(define (testbr! f::func)
   (if-test->br! (-> f body)))

(define-generic (replace-var! i::instruction x::bint y::bint t)
   #f)

(define-method (replace-var! i::one-arg x::bint y::bint t)
   (if (and (eq? 'local.get (-> i opcode)))
       (with-access::variable (-> i x) (idx)
          (when (=fx idx x)
             (set! (-> i outtype) t)
             (set! idx y)))
       (if (eq? 'local.set (-> i opcode))
           (with-access::variable (-> i x) (idx)
              (=fx x idx))
           #f)))

(define-method (replace-var! i::block x::bint y::bint t)
   (define (walk-list!::bool l::pair-nil)
      (match-case l
         (() #f)
         ((?i . ?tl)
          (if (replace-var! i x y t)
              #t
              (walk-list! tl)))))
   (walk-list! (-> i body)))

(define-method (replace-var! i::if-then x::bint y::bint t)
   (replace-var! (-> i then) x y t))

(define-method (replace-var! i::if-else x::bint y::bint t)
   ; we do it like that because we do not want lazy evaluation
   (let ((b (replace-var! (-> i else) x y t)))
     (or (call-next-method) b)))

(define-method (replace-var! i::loop x::bint y::bint t)
   (replace-var! (-> i body) x y t))

(define-method (replace-var! i::try_table x::bint y::bint t)
   (replace-var! (-> i body) x y t))

(define-generic (incr-labels! i::instruction)
  #f)

(define-generic (incr-labels-param! p::parameter) #f)

(define-method (incr-labels-param! p::label)
   (set! (-> p idx) (+fx 1 (-> p idx))))

(define-method (incr-labels! i::one-arg)
   (incr-labels-param! (-> i x)))

(define-method (incr-labels! i::two-args)
   (incr-labels-param! (-> i x))
   (incr-labels-param! (-> i y)))

(define-method (incr-labels! i::three-args)
   (incr-labels-param! (-> i x))
   (incr-labels-param! (-> i y))
   (incr-labels-param! (-> i z)))

(define-method (incr-labels! i::block)
   (for-each incr-labels! (-> i body)))

(define-method (incr-labels! i::if-then)
   (incr-labels! (-> i then)))

(define-method (incr-labels! i::if-else)
   (incr-labels! (-> i else))
   (call-next-method))

(define-method (incr-labels! i::try_table)
   (define (incr-labels-catch! c::catch-branch)
     (set! (-> c label) (+fx 1 (-> c label))))
   (for-each incr-labels-catch! (-> i catches))
   (incr-labels! (-> i body)))

(define-generic (if-test->br! i::instruction)
   i)

(define-method (if-test->br! i::if-then)
   (if-test->br! (-> i then)))

(define-method (if-test->br! i::if-else)
   (if-test->br! (-> i else))
   (call-next-method))

(define-method (if-test->br! i::loop)
   (if-test->br! (-> i body)))

(define-method (if-test->br! i::try_table)
   (if-test->br! (-> i body)))

(define (isa-ref.test? i::instruction)
   (eq? (-> i opcode) 'ref.test))

(define (isa-local.get? i::instruction)
   (eq? (-> i opcode) 'local.get))

(define (local-add! f::func t::type)
   (define (append-length l::pair-nil i::bint)
      (if (null? l)
          (values (list t) i)
          (multiple-value-bind (tl n) (append-length (cdr l) (+fx 1 i))
             (values (cons (car l) tl) n))))

   (multiple-value-bind (l n) (append-length (-> f locals) 0)
      (set! (-> f locals) l)
      n))


(define (block-gen-else! lget::one-arg var::variable rt-src::type
                         rt-dst::type i::if-else)
   (let ((y (local-add! (-> i parent) rt-dst)))
      (replace-var! (-> i then) (-> var idx) y rt-dst)
      (incr-labels! (-> i else))
      (with-access::block (-> i then) ((body body))
         (set! body
               `(,(instantiate::block
                   (intype '())
                   (outtype (list rt-dst))
                   (parent (-> i parent))
                   (opcode 'block)
                   (body
                    `(,lget
                      ,(instantiate::three-args
                        (intype (list rt-src))
                        (outtype '())
                        (parent (-> i parent))
                        (opcode 'br_on_cast)
                        (x (instantiate::label (idx 0)))
                        (y (instantiate::type (type rt-src)))
                        (z (instantiate::type (type rt-dst))))
                      ,@(with-access::block (-> i else) ((body body)) body)
                      ; the type here is kind of a hack, we can't really do
                      ; better without maintaining the stack state
                      ,(instantiate::one-arg
                        (intype '())
                        (outtype (list rt-dst 'poly))
                        (parent (-> i parent))
                        (opcode 'br)
                        (x (instantiate::label (idx 1)))))))
                 ,(instantiate::one-arg
                   (intype (list rt-dst))
                   (outtype '())
                   (parent (-> i parent))
                   (opcode 'local.set)
                   (x (instantiate::variable (idx y))))
                 ,@body)))))

(define (block-gen-no-else! lget::one-arg var::variable rt-src::type
                            rt-dst::type i::if-then)
   (let ((y (local-add! (-> i parent) rt-dst)))
      (replace-var! (-> i then) (-> var idx) y rt-dst)
      (with-access::block (-> i then) ((body body))
         (set! body
               `(,lget
                 ,(instantiate::three-args
                   (intype (list rt-src))
                   (outtype (list rt-dst))
                   (parent (-> i parent))
                   (opcode 'br_on_cast_fail)
                   (x (instantiate::label (idx 0)))
                   (y (instantiate::type (type rt-src)))
                   (z (instantiate::type (type rt-dst))))
                 ,(instantiate::one-arg
                   (intype (list rt-dst))
                   (outtype '())
                   (parent (-> i parent))
                   (opcode 'local.set)
                   (x (instantiate::variable (idx y))))
                 ,@body))))
   (-> i then))

(define-method (if-test->br! i::block)
   (define (walk-list! l::pair-nil)
      (if (and (not (null? l)) (not (null? (cdr l))) (not (null? (cddr l)))
               (isa-local.get? (car l)) (isa-ref.test? (cadr l))
               (isa? if-then (caddr l)))
          (with-access::one-arg (car l) ((var x) (ot outtype))
             (with-access::one-arg (cadr l) ((rt x))
                (if (isa? if-else (caddr l))
                    (block-gen-else! (car l) var (car ot) rt (caddr l))
                    (block-gen-no-else! (car l) var (car ot) rt (caddr l)))
                (set-car! l (with-access::if-then (caddr l) ((then then)) then))
                (set-cdr! l (cdddr l))))
          (if (not (null? l))
              (walk-list! (cdr l)))))
   (walk-list! (-> i body)))
