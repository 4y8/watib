(module opt_bbv
   (from (cfg_node "Opt/CFG/node.scm"))

   (import (cfg_order "Opt/CFG/order.scm"))
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
      (id-counter::long (default 0))
      all-specializations::obj
      all-specializations-by-id::obj
      merge-history::obj
      queue::obj
      reachability::obj))

   (export (bbv::cfg g::cfg version-limit::bint)))

;; SET
(define (make-set . elements) elements)
(define (set-union . ss)
  (let loop ((lst ss) (result '()))
    (if (null? lst)
        result
        (loop (cdr lst)
              (set-union2 result (car lst))))))
(define (set-union2 s1 s2)
  (cond ((null? s1)
         s2)
        ((member (car s1) s2)
         (set-union2 (cdr s1) s2))
        (else
         (cons (car s1)
               (set-union2 (cdr s1) s2)))))
(define (set-intersect s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (cons (car s1)
               (set-intersect (cdr s1) s2)))
        (else
         (set-intersect (cdr s1) s2))))
(define (set-difference s1 s2)
  (cond ((null? s1)
         '())
        ((member (car s1) s2)
         (set-difference (cdr s1) s2))
        (else
         (cons (car s1)
               (set-difference (cdr s1) s2)))))
(define (set-equal? s1 s2)
  (and (null? (set-difference s1 s2))
       (null? (set-difference s2 s1))))
(define (set-filter f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (set-filter f (cdr lst))))
        (else          (set-filter f (cdr lst)))))

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
   (instantiate::context (registers '()) (stack '()) (equivalences '())))

;; TODO
(define (context-equal? ctx1 ctx) #t)

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
                        (lambda (equiv-loc-pair)
                           (not (location-equal? loc (car equiv-loc-pair))))
                        equiv))
                  (-> context equivalences)))))))

(define (equivalence-class-has?::bool equiv::pair-nil loc::location)
   (let loop ((equiv equiv))
      (and
         (pair? equiv)
         (or (location-equal? (car equiv) loc) (loop (cdr equiv))))))

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

(define (context-push::context context::context type::types)
   (duplicate::context
      context
      (stack
         (cons
            (cons (instantiate::onstack (pos 0)) type)
            (map
               (lambda (onstack-type)  (cons (on-push (car onstack-type)) (cdr onstack-type)))
               (-> context stack))))
      (equivalences
         (map
            (lambda (equiv)
               (map
                  (lambda (loc-type) (cons (on-push (car loc-type)) (cdr loc-type)))
                  equiv))
            (-> context equivalences)))))

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
                     (lambda (loc-type)
                        (let ((location (car loc-type)))
                           (not (and
                              (isa? location onstack)
                              (< (with-access::onstack location (pos) pos) 0)))))
                     (map
                        (lambda (loc-type) (cons (on-pop (car loc-type)) (cdr loc-type)))
                        equiv)))
               (-> context equivalences))))))

(define (context-pop context::context)
   (values
      (context-pop* context)
      (cdar (-> context stack))))
      
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
                  (reachability (ssr-make-graph)))))
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

(define (get-specialization::specialization state::bbv-state origin::cfg-node target-context::context)
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
      (map cdr (hashtable-get all-specializations (-> origin idx))))))

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
      (let ((jump-target (reach state dst context specialization)))
         (instantiate::unconditional (dst jump-target)))))

(define-generic (walk-instr instr::instruction state::bbv-state context::context))

(define-method (walk-instr instr::one-arg state::bbv-state context::context)
   (with-access::context context (stack registers)
   (with-access::one-arg instr (opcode x intype outtype)
   
      (match-case opcode
         (local.set
            (with-access::localidxp x (idx)
               (let* ((type (car stack))
                     (new-registers (vector-copy registers))
                     (new-stack (cdr stack)))
                  (vector-set! new-registers idx type)
                  (values
                     instr
                     (instantiate::context (registers new-registers) (stack new-stack) (equivalences '()))))))
         (i32.const
            (let ((type 'i32))
               (values
                  instr
                  (duplicate::context context (stack (cons type new-stack))))))
         (i64.const
            (let ((type 'i64))
               (values
                  instr
                  (duplicate::context context (stack (cons type new-stack))))))
         (ref.null
            (let ((type '(ref null none)))
               (values
                  instr
                  (duplicate::context context (stack (cons type new-stack))))))
         (struct.new
            (with-access::typeidxp x (idx)
               (let ((type `(ref ,idx)))
                  (values
                     instr
                     (duplicate::context context (stack (append outtype (drop stack (length intype)))))))))))))

(define (walk state::bbv-state specialization::specialization)
   (with-access::specialization specialization (origin context version id)
   (with-access::cfg-node origin (body end)
      (with-access::cfg-node version (idx) (set! idx id))
      (let loop ((body body) (context context) (specialized-body '()))
         (if (null? body)
            (with-access::cfg-node version ((version-body body) (version-end end))
               (set! version-body (reverse specialized-body))
               (set! version-end (walk-jump end state context specialization)))
            (multiple-value-bind (instr next-context) (walk-instr (car body) state context)
               (loop (cdr body) next-context (cons instr specialized-body))))))))

(define (reach::specialization state::bbv-state origin::cfg-node context::context from::specialization)
   (with-access::bbv-state state (reachability)
      (let ((target
               (or
                  (get-specialization state origin context)
                  (let ((new-specialization
                        (instantiate::specialization
                           (origin origin)
                           (id (new-id! state))
                           (version (duplicate::cfg-node origin (body '()) (idx 'dummy)))
                           (context context))))
                     (add-specialization! state specialization)
                     new-specialization))))
         (with-access::specialization target (id)
            (if from
               (ssr-add-edge! reachability (-> from id) id
                  :onconnect (lambda (reachable)
                                 (queue-put!
                                    (-> state queue)
                                    (get-specialization-by-id bbv-state reachable)))))
            target))))

(define (merge? state::bbv-state specialization::specialization version-limit::bint)
   (when (> (get-active-specializations-count state specialization) version-limit)
      (***NotImplemented*** 'merge?)))

;; BBV CORE ALGO
(define (bbv::cfg g::cfg version-limit::bint)
   (let ((state (make-init-state cfg)))
      (with-access::bbv-state state (queue)
      (with-access::cfg g (entry func)
         (let ((new-entry (reach state entry (make-empty-context (length (-> func locals))) #f)))
            (let loop ()
               (when (not (queue-empty? queue))
                  (let ((specialization (queue-get! queue)))
                     (merge? state specialization version-limit)
                     (with-access::specialization specialization (version)
                     (with-access::cfg-node version (idx)
                        (if (and (reachable? state specialization) (eq? idx 'dummy))
                            (walk state specialization))))
                     (loop))))
            (multiple-value-bind (-size rpostorder) (reverse-postorder! entry)
               (let ((new-cfg (instantiate::cfg
                                 (entry (get-most-recent-merge state new-entry))
                                 (size (-fx 0 -size))
                                 (rpostorder rpostorder)
                                 (func func))))
            
                  new-cfg)))))))