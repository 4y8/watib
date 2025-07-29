(module opt_bbv
   (from (cfg_node "Opt/CFG/node.scm"))

   (library srfi1)
   (import (cfg_order "Opt/CFG/order.scm"))
   (import (ast_node "Ast/node.scm"))
   (import (type_type "Type/type.scm"))

   (static (abstract-class location
      pos::bint))

   (static (class register::location))

   (static (class onstack::location))

   (static (class types
      set::pair-nil))

   (static (class context
      registers::pair-nil
      stack::pair-nil
      equivalences::pair-nil))

   (static (class specialization
      origin::cfg-node
      id::bint
      version::cfg-node
      context::context))

   (static (class bbv-state
      (id-counter::long (default 10001))
      all-specializations::obj
      all-specializations-by-id::obj
      merge-history::obj
      queue::obj
      reachability::obj))

   (export (bbv::cfg g::cfg version-limit::bint)))

(define-method (object-print obj::location port::output-port f::procedure)
      (display "#\|" port)
      (display (class-name (object-class obj)) port)
      (display " ")
      (display (-> obj pos))
      (display "\|" port))

;; SET
(define (make-set . elements)
   (lset-union equal? '() elements))
(define (set-union . ss)
  (apply lset-union equal? ss))
(define (set-intersect s1 s2)
  (lset-intersection equal? s1 s2))
(define (set-difference s1 s2)
  (lset-difference equal? s1 s2))
(define (set-equal? s1 s2)
  (lset= equal? s1 s2))
(define (set-filter f lst)
   (filter f lst))
(define (set-map f lst)
   (apply make-set (map f lst)))
(define (set-empty? s) (null? s))

(define (make-types::types . elements)
   (instantiate::types (set (apply make-set elements))))

(define (types-map::types f::procedure t::types)
   (instantiate::types (set (set-map f (-> t set)))))

(define (types-filter::types f::procedure t::types)
   (instantiate::types (set (set-filter f (-> t set)))))

;; QUEUE
(define (make-queue) (cons '() '()))
(define (queue-empty? queue) (null? (car queue)))
(define (queue-get! queue)
  (let ((x (caar queue)))
    (set-car! queue (cdar queue))
    (if (queue-empty? queue) (set-cdr! queue '()))
    x))
(define (queue-put! queue x)
  (let ((entry (cons x '())))
    (if (queue-empty? queue)
      (set-car! queue entry)
      (set-cdr! (cdr queue) entry))
    (set-cdr! queue entry)
    x))
(define (queue-peek queue) (if (queue-empty? queue) #f (caar queue)))

;; CONTEXT
(define-generic (on-popn::location location::location n::bint) location)
(define-method (on-popn::onstack location::onstack n::bint)
   (instantiate::onstack (pos (- (-> location pos) n))))

(define-generic (on-pushn::location location::location n::bint) location)
(define-method (on-pushn::onstack location::onstack n::bint)
   (instantiate::onstack (pos (+ (-> location pos) n))))

(define-generic (on-pop::location location::location) (on-popn location 1))
(define-generic (on-push::location location::location) (on-pushn location 1))

(define-generic (location-equal?::bool loc1::location loc2::location))
(define-method (location-equal?::bool loc1::onstack loc2::location)
   (and (isa? loc2 onstack) (= (-> loc1 pos) (-> loc2 pos))))
(define-method (location-equal?::bool loc1::register loc2::location)
   (and (isa? loc2 register) (= (-> loc1 pos) (-> loc2 pos))))

(define (make-empty-context nregisters::bint)
   (instantiate::context
      (registers '())
      (stack '())
      (equivalences
         (map
            (lambda (pos) (list (instantiate::register (pos pos))))
            (iota nregisters)))))

;; TODO
(define (types-equal?::bool types1::types types2::types)
   (set-equal? types1 types2))

(define (stack-equal? stk1::pair-nil stk2::pair-nil)
   (and
      (= (length stk1) (length stk2))
      (let loop ((stk1 stk1) (stk2 stk2))
         (cond
            ((null? stk1) #t)
            ((types-equal? (cdar stk1) (cdar stk2)) (loop (cdr stk1) (cdr stk2)))
            (else #f)))))

(define (registers-equal? regs1::pair-nil regs2::pair-nil)
   (define (reg-order reg1::register reg2::register)
      (< (-> reg1 pos) (-> reg2 pos)))

   (define (order reg-type1 reg-type2) (reg-order (car reg-type1) (car reg-type2)))

   (let ((regs1 (sort order regs1))
         (regs2 (sort order regs2)))
      (and
         (= (length regs1) (length regs2))
         (let loop ((regs1 regs1) (regs2 regs2))
            (cond
               ((null? regs1) #t)
               ((and
                  (location-equal? (caar regs1) (caar regs2))
                  (types-equal? (cdar regs1) (cdar regs2))) (loop (cdr regs1) (cdr regs2)))
               (else #f))))))

(define (context-equal? ctx1::context ctx2::context)
   (with-access::context ctx1 ((registers1 registers) (stack1 stack) (equivalences1 equivalences))
   (with-access::context ctx2 ((registers2 registers) (stack2 stack) (equivalences2 equivalences))
      (and
         (equivalence-class-equal? equivalences1 equivalences2)
         (registers-equal? registers1 registers2)
         (stack-equal? stack1 stack2)))))

(define (context-register-set::context context::context reg::bint type::types)
   (let ((register-location (instantiate::register (pos reg))))
      (with-access::context context (registers)
         (context-remove-equivalence
            (duplicate::context
               context
               (registers
                  (cons
                     (cons register-location type)
                     (filter
                        (lambda (reg-type)
                           (with-access::register (car reg-type) (pos) (not (= pos reg))))
                        registers))))
            register-location))))

(define (context-remove-equivalence::context context::context loc::location)
   (duplicate::context
      context
      (equivalences
         (cons
            (list loc)
            (filter pair?
               (map
                  (lambda (equiv)
                     (filter
                        (lambda (equiv-loc)
                           (not (location-equal? loc equiv-loc)))
                        equiv))
                  (-> context equivalences)))))))

(define (equivalence-class-has?::bool equiv::pair-nil loc::location)
   (let loop ((equiv equiv))
      (and
         (pair? equiv)
         (or (location-equal? (car equiv) loc) (loop (cdr equiv))))))

(define (equivalence-class-equal?::bool equiv1::pair-nil equiv2::pair-nil)
   (and
      (= (length equiv1) (length equiv2))
      (let loop ((equiv1 equiv1))
         (cond
            ((null? equiv1) #t)
            ((equivalence-class-has? equiv2 (car equiv1)) (loop (cdr equiv1)))
            (else #f)))))

(define (context-get-equivalence-class::pair-nil context::context loc::location)
   (let loop ((classes (-> context equivalences)))
      (if (null? classes)
          (error 'context-get-equivalence-class::pair-nil "no equivalence class for" loc)
          (if (equivalence-class-has? (car classes) loc) (car classes) (loop (cdr classes))))))

(define (context-add-equivalence::context context::context loc1::location loc2::location)
   (duplicate::context
      context
      (equivalences
         (let ((equiv1 (context-get-equivalence-class context loc1))
               (equiv2 (context-get-equivalence-class context loc2)))
            (cons
               (if (eq? equiv1 equiv2) equiv1 (append equiv1 equiv2))
               (filter
                  (lambda (equiv) (not (or (eq? equiv equiv1) (eq? equiv equiv2))))
                  (-> context equivalences)))))))

(define (context-type-set::context context::context location::location type::types)
   (let ((equivalence-class (context-get-equivalence-class context location)))
      (define (location-in-equiv? loc)
         (let loop ((equiv equivalence-class))
            (if (null? equiv)
                #f
               (or (location-equal? loc (car equiv)) (loop (cdr equiv))))))

      (define (update-type loc-type)
         (if (location-in-equiv? (car loc-type)) (cons (car loc-type) type) loc-type))

      (duplicate::context context
         (registers (map update-type (-> context registers)))
         (stack (map update-type (-> context stack))))))

(define (context-push::context context::context type::types)
   (duplicate::context
      context
      (stack
         (cons
            (cons (instantiate::onstack (pos 0)) type)
            (map
               (lambda (onstack-type) (cons (on-push (car onstack-type)) (cdr onstack-type)))
               (-> context stack))))
      (equivalences
         (cons (list (instantiate::onstack (pos 0)))
          (map
                (lambda (equiv)
                  (map on-push equiv))
                (-> context equivalences))))))

(define (context-pop*::context context::context)
   (duplicate::context
      context
      (stack
         (cdr
            (map
               (lambda (onstack-type)  (cons (on-pop (car onstack-type)) (cdr onstack-type)))
               (-> context stack))))
      (equivalences
         (filter pair?
            (map
               (lambda (equiv)
                  (filter
                     (lambda (loc)
                       (not (and
                             (isa? loc onstack)
                             (< (with-access::onstack loc (pos) pos) 0))))
                     (map on-pop equiv)))
               (-> context equivalences))))))

(define (context-pop context::context)
   (values
      (context-pop* context)
      (cdar (-> context stack))))

(define (context-dropn context::context n::bint)
   (if (= n 0)
       context
       (context-dropn
         (multiple-value-bind (context type) (context-pop context) context)
         (- n 1))))

(define (context-drop context::context) (context-dropn context 1))

(define (context-stack-ref::types context::context location::onstack)
   (cdr (list-ref (-> context stack) (-> location pos))))

(define (context-register-ref::types context::context location::register)
   (with-trace 1 'register-ref
               (trace-item "registers=" (-> context registers) " loc=" location)
   (let loop ((registers (-> context registers)))
         (cond
            ((null? registers) (error 'context-register-ref "not found" location))
            ((location-equal? (caar registers) location) (cdar registers))
            (else (loop (cdr registers)))))))

(define (context-ref::types context::context location::location)
   (cond
      ((isa? location register) (context-register-ref context location))
      ((isa? location onstack) (context-stack-ref context location))
      (else (error 'context-ref "unknown location" location))))

(define (context-mov::context context::context from::location to::register)
   (let ((from-type
            (cond
               ((isa? from register) (context-register-ref context from))
               ((isa? from onstack) (context-stack-ref context from)))))
      (context-add-equivalence
         (context-register-set context (-> to pos) from-type)
         from
         to)))

(define (context-mov-pop::context context::context to::register)
   (context-pop (context-mov context (instantiate::onstack (pos 0)) to)))

(define (context-push-from-register::context context::context register::register)
   (context-add-equivalence
      (context-push context (context-register-ref context register))
      register
      (instantiate::onstack (pos 0))))

(define (context-narrow-null context::context location::location)
   (let* ((can-be-null? #f)
          (non-null
             (types-filter
                (lambda (x) x)
                (types-map
                   (match-lambda
                      ((ref null none) (set! can-be-null? #t) #f)
                      ((ref null ?ht) (set! can-be-null? #t) `(ref ,ht))
                      (?type type))
                   (context-ref context location)))))
      (values
         (if (set-empty? non-null) #f (context-type-set context location non-null))
         (if can-be-null? (context-type-set context location (make-types '(ref null none))) #f))))

;; HELPERS
(define (***NotImplemented*** name::symbol) (error "NotImplemented" name '?))

(define (hashtable-copy table)
   (let ((new-table (make-hashtable)))
      (hashtable-for-each
         table
         (lambda (k v) (hashtable-put! new-table k v)))
      new-table))

(define (make-init-state::bbv-state cfg::cfg)
   (let ((init (instantiate::bbv-state
                  (all-specializations (make-hashtable))
                  (all-specializations-by-id (make-hashtable))
                  (merge-history (make-hashtable))
                  (queue (make-queue))
                  (reachability (ssr-make-graph :source -1)))))
      (with-access::bbv-state init (all-specializations)
         (for-each
            (lambda (bb)
               (with-access::cfg-node bb (idx) (hashtable-put! all-specializations idx '())))
            (-> cfg rpostorder)))
      init))

(define (new-id!::bint state::bbv-state)
   (set! (-> state id-counter) (+ 1 (-> state id-counter)))
   (-> state id-counter))

(define (add-specialization! state::bbv-state specialization::specialization)
   (with-access::bbv-state state (all-specializations all-specializations-by-id)
   (with-access::specialization specialization (origin)
      (hashtable-put! all-specializations-by-id (-> specialization id) specialization)
      (hashtable-put!
         all-specializations
         (-> origin idx)
         (cons
            specialization
            (hashtable-get
               all-specializations
               (-> origin idx)))))))

(define (get-specialization-by-id::specialization state::bbv-state id::bint)
   (with-access::bbv-state state (all-specializations-by-id)
      (hashtable-get all-specializations-by-id id)))

(define (get-most-recent-merge::specialization state::bbv-state specialization::specialization)
   (with-access::bbv-state state (merge-history)
   (with-access::specialization specialization (id)
      (let loop ((id id))
         (let ((merged-to (hashtable-get merge-history id)))
            (if (or (not merged-to) (= merged-to id))
               (get-specialization-by-id state id)
               (loop merged-to)))))))

(define (get-specialization state::bbv-state origin::cfg-node target-context::context)
   (with-access::bbv-state state (all-specializations)
      (let loop ((versions (hashtable-get all-specializations (-> origin idx))))
         (if (null? versions)
            #f
            (with-access::specialization (car versions) (context)
               (if (context-equal? context target-context)
                  (get-most-recent-merge state specialization)
                  (loop (cdr versions))))))))

(define (get-specializations::pair-nil state::bbv-state specialization::specialization)
   (with-access::bbv-state state (all-specializations)
   (with-access::specialization specialization (origin)
      (hashtable-get all-specializations (-> origin idx)))))

(define (get-active-specializations::pair-nil state::bbv-state specialization::specialization)
   (map (lambda (spec) (reachable? state spec)) (get-specializations state specialization)))

(define (get-active-specializations-count::bint state::bbv-state specialization::specialization)
   (length (get-active-specializations state specialization)))

;; BBV ALGORITHM STEPS
(define (reachable? state::bbv-state specialization::specialization)
   (with-access::bbv-state state (reachability)
      (with-access::specialization specialization (id)
         (ssr-connected? reachability id))))

(define-generic (walk-jump instr::jump state::bbv-state context::context specialization::specialization))

(define-method (walk-jump instr::unconditional state::bbv-state context::context specialization::specialization)
   (with-access::unconditional instr (dst)
      (let ((jump-target::specialization (reach state dst context specialization)))
        (duplicate::unconditional instr (dst (-> jump-target version))))))

(define-method (walk-jump jump::conditional state::bbv-state context::context specialization::specialization)
   (let ((true-target::specialization (reach state (-> jump dst-true)
                                             (context-drop context) specialization))
         (false-target::specialization (reach state (-> jump dst-false)
                                             (context-drop context) specialization)))
     (duplicate::conditional jump (dst-true (-> true-target version))
                             (dst-false (-> false-target version)))))

(define-method (walk-jump instr::on-null state::bbv-state context::context specialization::specialization)
   (with-access::on-null instr (dst-null dst-non-null)
      (multiple-value-bind
         (context-non-null context-null)
         (context-narrow-null context (instantiate::onstack (pos 0)))
         (let ((spec-dest-non-null (and context-non-null (reach state dst-non-null context-non-null specialization)))
               (spec-dest-null (and context-null (reach state dst-null context-null specialization))))
            (cond
               ((and spec-dest-non-null spec-dest-null)
                  (duplicate::on-null
                     instr
                     (dst-non-null spec-dest-non-null)
                     (dst-null spec-dest-null)))
               (spec-dest-non-null (instantiate::unconditional (dst spec-dest-non-null)))
               (spec-dest-null (instantiate::unconditional (dst spec-dest-null)))
               (else (error 'walk-jump "no valid jump" instr)))))))

(define-generic (walk-instr instr::instruction state::bbv-state context::context)
   (with-trace 1 'walk-instr
               (trace-item "instr=" (-> instr opcode))
   (match-case (-> instr opcode)
      (array.len
       (values instr (context-push (context-drop context)
                                   (make-types (car (-> instr outtype))))))
      (i32.ge_u
       (values instr (context-push (context-dropn context 2)
                                   (make-types (car (-> instr outtype))))))
      (i32.eq
       (values instr (context-push (context-dropn context 2)
                                   (make-types (car (-> instr outtype))))))
      (else (error 'walk-instr "unknown instruction" (-> instr opcode))))))

(define-method (walk-instr instr::one-arg state::bbv-state context::context)
  (with-trace 1 'walk-instr
               (trace-item "instr=" (-> instr opcode) )
   (with-access::context context (stack registers)
   (with-access::one-arg instr (opcode x intype outtype)
      (match-case opcode
         (local.set
            (with-access::localidxp x (idx)
               (values
                  instr
                  (context-mov-pop context (instantiate::register (pos idx))))))
         (local.get
            (with-access::localidxp x (idx)
               (values
                  instr
                  (context-push-from-register context (instantiate::register (pos idx))))))
         (i32.const
            (values
               instr
               (context-push context (make-types (car outtype)))))
         (i64.const
            (values
               instr
               (context-push context (make-types (car outtype)))))
         (ref.null
            (values
               instr
               (context-push context (make-types '(ref null none)))))
         (struct.new
            (with-access::typeidxp x (idx)
               (values
                  instr
                  (context-push (context-dropn context (length intype)) (make-types (car outtype))))))
         (array.new
            (with-access::typeidxp x (idx)
               (values
                  instr
                  (context-push (context-dropn context 2) (make-types (car outtype))))))
         (array.get
            (with-access::typeidxp x (idx)
               (values
                  instr
                  (context-push
                     (context-dropn
                        (multiple-value-bind (non-null _)
                           (context-narrow-null context (instantiate::onstack (pos 1)))
                           non-null)
                        2)
                     (make-types (car outtype))))))
         (else (error 'walk-instr "unknown instruction" (-> instr opcode))))))))

(define (walk state::bbv-state specialization::specialization)
   (with-trace 1 'walk
   (with-access::specialization specialization (origin context version id)
   (with-access::cfg-node origin (body end)
      (with-access::cfg-node version (idx) (set! idx id))
      (trace-item "walking=" (-> specialization id))
      (let loop ((body body) (context context) (specialized-body '()))
         (if (null? body)
            (with-access::cfg-node version ((version-body body) (version-end end))
               (set! version-body (reverse specialized-body))
               (set! version-end (walk-jump end state context specialization)))
            (multiple-value-bind (instr next-context) (walk-instr (car body) state context)
               (loop (cdr body) next-context (cons instr specialized-body)))))))))

(define (reach::specialization state::bbv-state origin::cfg-node context::context from)
   (with-trace 1 'reach
   (with-access::bbv-state state (reachability)
      (let ((target
               (or
                  (let ((existing-spec (get-specialization state origin context)))
                     (trace-item "existing-spec="
                                 (if existing-spec (with-access::specialization existing-spec (id) id) #f))
                     existing-spec)

                  (let ((new-specialization::specialization
                        (instantiate::specialization
                           (origin origin)
                           (id (new-id! state))
                           (version (duplicate::cfg-node origin (body '()) (idx 'dummy)))
                           (context context))))
                     (add-specialization! state new-specialization)
                     (trace-item "enqueue=" (-> new-specialization id))
                     (queue-put! (-> state queue) new-specialization)
                     new-specialization))))
         (with-access::specialization target (id)
            (if from
               (ssr-add-edge! reachability (with-access::specialization from (id) id) id
                  :onconnect (lambda (reachable)
                                 (queue-put!
                                    (-> state queue)
                                    (get-specialization-by-id state reachable))))
               (ssr-add-edge! reachability -1 id))
            target)))))

(define (merge? state::bbv-state specialization::specialization version-limit::bint)
   (when (> (get-active-specializations-count state specialization) version-limit)
      (***NotImplemented*** 'merge?)))

;; BBV CORE ALGO
(define (bbv::cfg g::cfg version-limit::bint)
   (with-trace 1 'bbv
   (let ((state (make-init-state g)))
      (with-access::bbv-state state (queue)
      (with-access::cfg g (entry func)
      (with-access::func func (locals formals type)
         (let* ((init-context (make-empty-context (+ (length formals) (length locals))))
                (func-context
                  (let loop ((func-context init-context) (i 0) (args-types (car type)))
                     (if (null? args-types)
                         func-context
                         (loop
                           (context-register-set
                              func-context
                              i
                              (make-types (car args-types)))
                           (+ i 1)
                           (cdr args-types)))))
                (new-entry::specialization (reach state entry func-context #f)))
            (trace-item "head=" (-> new-entry id) " type=" type)
            (let loop ()
               (when (not (queue-empty? queue))
                  (let ((specialization::specialization (queue-get! queue)))
                    (trace-item "dequeue-id=" (-> specialization id))
                     (merge? state specialization version-limit)
                     (with-access::specialization specialization (version)
                     (with-access::cfg-node version (idx)
                        (trace-item "reachable?=" (reachable? state specialization) " dummy?=" (eq? idx 'dummy))
                        (if (and (reachable? state specialization) (eq? idx 'dummy))
                            (walk state specialization))))
                     (loop))))
            (let ((final-entry
                   (with-access::specialization
                      (get-most-recent-merge state new-entry)
                      (version) version)))
            (multiple-value-bind (-size rpostorder) (reverse-postorder! final-entry)
               (let ((new-cfg (instantiate::cfg
                                 (entry final-entry)
                                 (size (-fx 0 -size))
                                 (rpostorder rpostorder)
                                 (func func))))
                  new-cfg))))))))))
