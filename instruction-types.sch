`(
  ;; section 3.4.1
  (i32.const (,i32) (() (i32)))
  (i64.const (,i64) (() (i64)))
  (f32.const (,f32) (() (f32)))
  (f64.const (,f64) (() (f64)))

  ; todo : rewrite these with symbol append to shorten them

  (i32.eqz  () ((i32) (i32)))
  (i32.eq   () ((i32 i32) (i32)))
  (i32.ne   () ((i32 i32) (i32)))
  (i32.lt_s () ((i32 i32) (i32)))
  (i32.lt_u () ((i32 i32) (i32)))
  (i32.gt_s () ((i32 i32) (i32)))
  (i32.gt_u () ((i32 i32) (i32)))
  (i32.le_s () ((i32 i32) (i32)))
  (i32.le_u () ((i32 i32) (i32)))
  (i32.ge_s () ((i32 i32) (i32)))
  (i32.ge_u () ((i32 i32) (i32)))

  (i64.eqz  () ((i64) (i32)))
  (i64.eq   () ((i64 i64) (i32)))
  (i64.ne   () ((i64 i64) (i32)))
  (i64.lt_s () ((i64 i64) (i32)))
  (i64.lt_u () ((i64 i64) (i32)))
  (i64.gt_s () ((i64 i64) (i32)))
  (i64.gt_u () ((i64 i64) (i32)))
  (i64.le_s () ((i64 i64) (i32)))
  (i64.le_u () ((i64 i64) (i32)))
  (i64.ge_s () ((i64 i64) (i32)))
  (i64.ge_u () ((i64 i64) (i32)))

  (f32.eq () ((f32 f32) (i32)))
  (f32.ne () ((f32 f32) (i32)))
  (f32.lt () ((f32 f32) (i32)))
  (f32.gt () ((f32 f32) (i32)))
  (f32.le () ((f32 f32) (i32)))
  (f32.ge () ((f32 f32) (i32)))

  (f64.eq () ((f64 f64) (i32)))
  (f64.ne () ((f64 f64) (i32)))
  (f64.lt () ((f64 f64) (i32)))
  (f64.gt () ((f64 f64) (i32)))
  (f64.le () ((f64 f64) (i32)))
  (f64.ge () ((f64 f64) (i32)))


  (i32.clz    () ((i32) (i32)))
  (i32.ctz    () ((i32) (i32)))
  (i32.popcnt () ((i32) (i32)))
  (i32.add    () ((i32 i32) (i32)))
  (i32.sub    () ((i32 i32) (i32)))
  (i32.mul    () ((i32 i32) (i32)))
  (i32.div_s  () ((i32 i32) (i32)))
  (i32.div_u  () ((i32 i32) (i32)))
  (i32.rem_s  () ((i32 i32) (i32)))
  (i32.rem_u  () ((i32 i32) (i32)))
  (i32.and    () ((i32 i32) (i32)))
  (i32.or     () ((i32 i32) (i32)))
  (i32.xor    () ((i32 i32) (i32)))
  (i32.shl    () ((i32 i32) (i32)))
  (i32.shr_s  () ((i32 i32) (i32)))
  (i32.shr_u  () ((i32 i32) (i32)))
  (i32.rotl   () ((i32 i32) (i32)))
  (i32.rotr   () ((i32 i32) (i32)))

  (i64.clz    () ((i64) (i64)))
  (i64.ctz    () ((i64) (i64)))
  (i64.popcnt () ((i64) (i64)))
  (i64.add    () ((i64 i64) (i64)))
  (i64.sub    () ((i64 i64) (i64)))
  (i64.mul    () ((i64 i64) (i64)))
  (i64.div_s  () ((i64 i64) (i64)))
  (i64.div_u  () ((i64 i64) (i64)))
  (i64.rem_s  () ((i64 i64) (i64)))
  (i64.rem_u  () ((i64 i64) (i64)))
  (i64.and    () ((i64 i64) (i64)))
  (i64.or     () ((i64 i64) (i64)))
  (i64.xor    () ((i64 i64) (i64)))
  (i64.shl    () ((i64 i64) (i64)))
  (i64.shr_s  () ((i64 i64) (i64)))
  (i64.shr_u  () ((i64 i64) (i64)))
  (i64.rotl   () ((i64 i64) (i64)))
  (i64.rotr   () ((i64 i64) (i64)))

  (f32.abs      () ((f32) (f32)))
  (f32.neg      () ((f32) (f32)))
  (f32.ceil     () ((f32) (f32)))
  (f32.floor    () ((f32) (f32)))
  (f32.trunc    () ((f32) (f32)))
  (f32.nearest  () ((f32) (f32)))
  (f32.sqrt     () ((f32) (f32)))
  (f32.add      () ((f32 f32) (f32)))
  (f32.sub      () ((f32 f32) (f32)))
  (f32.mul      () ((f32 f32) (f32)))
  (f32.div      () ((f32 f32) (f32)))
  (f32.min      () ((f32 f32) (f32)))
  (f32.max      () ((f32 f32) (f32)))
  (f32.copysign () ((f32 f32) (f32)))

  (f64.abs      () ((f64) (f64)))
  (f64.neg      () ((f64) (f64)))
  (f64.ceil     () ((f64) (f64)))
  (f64.floor    () ((f64) (f64)))
  (f64.trunc    () ((f64) (f64)))
  (f64.nearest  () ((f64) (f64)))
  (f64.sqrt     () ((f64) (f64)))
  (f64.add      () ((f64 f64) (f64)))
  (f64.sub      () ((f64 f64) (f64)))
  (f64.mul      () ((f64 f64) (f64)))
  (f64.div      () ((f64 f64) (f64)))
  (f64.min      () ((f64 f64) (f64)))
  (f64.max      () ((f64 f64) (f64)))
  (f64.copysign () ((f64 f64) (f64)))


  (i32.wrap_i64        () ((i64) (i32)))
  (i32.trunc_f64_s     () ((f64) (i32)))
  (i32.trunc_f64_u     () ((f64) (i32)))
  (i64.extend_i32_s    () ((i32) (i64)))
  (i64.extend_i32_u    () ((i32) (i64)))
  (i64.trunc_f64_s     () ((f64) (i64)))
  (i64.trunc_f64_u     () ((f64) (i64)))
  (f64.convert_i32_s   () ((i32) (f64)))
  (f64.convert_i32_u   () ((i32) (f64)))
  (f64.convert_i64_s   () ((i64) (f64)))
  (f64.convert_i64_u   () ((i64) (f64)))
  (f32.demote_f64      () ((f64) (f32)))
  (f64.convert_i64_s   () ((i64) (f64)))
  (f64.promote_f32     () ((f32) (f64)))
  (i32.reinterpret_f32 () ((f32) (i32)))
  (i64.reinterpret_f64 () ((f64) (i64)))
  (f32.reinterpret_i32 () ((i32) (f32)))
  (f64.reinterpret_i64 () ((i64) (f64)))

  ;; section 3.4.2
  (ref.null (,ht) ,(lambda (- ht) `(() ((ref null ,ht)))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.98
  (ref.func (,funcidx) ,(lambda (env x)
                           (unless (hashtable-contains? (env-refs env) x)
                              (raise `(undeclared-funcref ,x)))
                           `(() ((ref ,(get-func-type env x))))))

  ; by subsumption (section 3.4.12)
  (ref.is_null () (((ref null any)) (i32)))
  ; ref.as_non_null can't be treated here because there is a link between the
  ; input type and the output type, could be solved by giving the stack as an
  ; argument to these functions
  (ref.eq () (((ref null eq) (ref null eq)) (i32)))
  ; by subsumption
  (ref.test (,rt) ,(lambda (- rt) `((,rt) (i32))))
  ; by subsumption
  (ref.cast (,rt) ,(lambda (- rt) `((,rt) (,rt))))

  ;; section 3.4.3
  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.104
  (struct.new
   (,typeidx)
   ,(lambda (env x) `(,(map unpack-ft (get-struct-fldts env x)) ((ref ,x)))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.105
  (struct.new_default
   (,typeidx)
   ,(lambda (env x)
       (for-each (lambda (t) (unless (defaultable? t)
                               (raise `(expected-defaultable ,t))))
                 (map unpack-ft (get-struct-fldts env x)))
       `(() ((ref ,x)))))

  ;; in the following functions, fieldidx checks that x is a struct type
  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.106
  (struct.get
   (,typeidx ,fieldidx)
   ,(lambda (env x y)
       (let ((t (cadr (list-ref (get-struct-fldts env x) y))))
          (when (packedtype? t)
             (raise `(got-packed ,x ,y ,t)))
          `(((ref null ,x)) (,t)))))
  (struct.get_s
   (,typeidx ,fieldidx)
   ,(lambda (env x y)
       (let ((t (cadr (list-ref (get-struct-fldts env x) y))))
          (unless (packedtype? t)
             (raise `(expected-packed ,x ,y ,t)))
          `(((ref null ,x)) (,(unpack t))))))
  (struct.get_u
   (,typeidx ,fieldidx)
   ,(lambda (env x y)
       (let ((t (cadr (list-ref (get-struct-fldts env x) y))))
          (unless (packedtype? t)
             (raise `(expected-packed ,x ,y ,t)))
          `(((ref null ,x)) (,(unpack t))))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.107
  (struct.set
   (,typeidx ,fieldidx)
   ,(lambda (env x y)
       (let ((ft (list-ref (get-struct-fldts env x) y)))
          (unless (equal? (car ft) 'var)
             (raise `(set-const ,x ,y)))
          `(((ref null ,x) ,(unpack-ft ft)) ()))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.108
  (array.new
   (,typeidx)
   ,(lambda (env x) `((,(unpack-ft (get-array-ft env x)) i32) ((ref ,x)))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.109
  (array.new_default
   (,typeidx)
   ,(lambda (env x)
       (let ((ft (get-array-ft env x)))
         (unless (defaultable? (cadr ft))
              (raise `(expected-defaultable ,x ,(cadr ft))))
         `((i32) ((ref ,x))))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.110
  (array.new_fixed
   (,typeidx ,u32)
   ,(lambda (env x n)
       `(,(make-list n (unpack-ft (get-array-ft env x))) ((ref ,x)))))

  ; we do not support array.new_elem yet

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.112
  ; the call to dataidx assures that the data segment exists
  (array.new_data
   (,typeidx ,dataidx)
   ,(lambda (env x -)
       (let ((t (unpack-ft (get-array-ft env x))))
          (unless (or (vectype? t) (numtype? t))
             (raise `(expected-num-or-vec ,t)))
          `((i32 i32) ((ref ,x))))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.113
  (array.get
   (,typeidx)
   (lambda (env x)
      (let ((t (cadr (get-array-ft env x))))
         (when (packedtype? t)
            (raise `(got-packed ,x ,t)))
         `(((ref null ,x) i32) (,t)))))
  (array.get_s
   (,typeidx)
   (lambda (env x)
      (let ((t (cadr (get-array-ft env x))))
         (unless (packedtype? t)
            (raise `(expected-packed ,x ,t)))
         `(((ref null ,x) i32) (,(unpack t))))))
  (array.get_u
   (,typeidx)
   (lambda (env x)
      (let ((t (cadr (get-array-ft env x))))
         (unless (packedtype? t)
            (raise `(expected-packed ,x ,t)))
         `(((ref null ,x) i32) (,(unpack t))))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.114
  (array.set
   (,typeidx)
   (lambda (env x)
      (let ((ft (get-array-ft env x)))
         (unless (equal? (car ft) 'var)
            (raise `(set-const ,x)))
         `(((ref null ,x) i32 (unpack-ft ft)) ()))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.115
  (array.len () (((ref null array)) (i32)))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.116
  (array.fill
   (,typeidx)
   (lambda (env x)
      (let ((ft (get-array-ft env x)))
         (unless (equal? (car ft) 'var)
            (raise `(set-const ,x)))
         `(((ref null ,x) i32 (unpack-ft ft) i32) ()))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.117
  (array.copy
   (,typeidx ,typeidx)
   (lambda (env x y)
     (let ((ft1 (get-array-ft env x))
           (ft2 (get-array-ft env y)))
        (unless (equal? (car ft1) 'var)
           (raise `(set-const ,x)))
        (unless (<st= env (cadr ft2) (cadr ft1))
           (raise `(non-matching ,ft2 ,ft1)))
        `(((ref null ,x) i32 (ref null ,y) i32 i32) ()))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.118
  (array.init_data
    (,typeidx ,dataidx)
    (lambda (env x y)
      (let* ((ft (get-array-ft env x))
             (t (unpack-ft ft)))
         (unless (equal? (car ft) 'var)
            (raise `(set-const ,x)))
         (unless (or (vectype? t) (numtype? t))
             (raise `(expected-num-or-vec ,t)))
         `(((ref null ,x) i32 i32 i32) ()))))

  ; we do not support array.init_elem yet

  ;; section 3.4.4
  (ref.i31   () ((i32) ((ref i31))))
  (i31.get_s () (((ref i31)) (i32)))
  (i31.get_u () (((ref i31)) (i32)))

  ;; we do not support external reference instructions yet
  ;; we do not support vector instructions yet

  ;; section 3.4.7

  ; by subsumption
  (drop () ((any) ()))

  ; we do not support select yet; as it has an optionnal argument, we can't
  ; treat it here yet, an option would be to use additionnal symbols in the
  ; argument list to signal optionnal argument, the list would be something
  ; like: `((opt ,test ,idx))

  ;; section 3.4.8
  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.153
  (local.get
   (,localidx)
   (lambda (env x)
      (unless (local-init? env x)
         (raise `(get-unset-local ,x)))
      `(() (,(get-local-type env x)))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.154
  (local.set
   (,localidx)
   (lambda (env x) `((,(get-local-type env x)) () . (x))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.155
  (local.tee
   (,localidx)
   (lambda (env x) `((,(get-local-type env x))
                     (,(get-local-type env x)) . (x))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.156
  (global.get
   (,globalidx)
   (lambda (env x) `(() (,(cadr (get-global-type env x))))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.156
  (global.set
   (,globalidx)
   (let ((t (get-global-type env x)))
      (unless (car t)
         (raise `(set-const ,x)))
      `((,(cadr t)) ())))

  ;; section 3.4.11
  (nop () (() ()))

  ; empty argument by subsumption
  (unreachable () (() (poly)))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.191
  ; t_1 = epsilon by subsumption
  (br (,labelidx) (lambda (env l) `(,(get-label-type env l) (poly))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.192
  (br_if
   (,labelidx)
   (lambda (env l)
      (let ((t (get-label-type env l)))
         `((,@t i32) ,t))))

  ; br_table is dealt with in an-hoc way

  ; br_on_null is dealt with in an-hoc way, same reason as ref.as_non_null

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.195
  (br_on_non_null
   (,labelidx)
   ,(lambda (env l)
      (multiple-value-bind (t* tl) (get-label-last-rest env l)
         (unless (reftype? tl)
            (raise `(expected-reftype-label ,tl)))
         `((,@t* (ref null ,(reftype->heaptype tl))) ,t*))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.196
  (br_on_cast
   (,labelidx ,rt ,rt)
   (lambda (env l rt1 rt2)
      (multiple-value-bind (t* rt') (get-label-last-rest env l)
         (unless (reftype? rt')
            (raise `(expected-reftype-label ,rt')))
         (unless (<rt= env rt2 rt1)
            (raise `(non-matching ,rt2 ,rt1)))
         (unless (<rt= env rt2 rt')
            (raise `(non-matching ,rt2 ,rt')))
         `((,@t* ,rt1) (,@t* ,(diff rt1 rt2))))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.197
  (br_on_cast_fail
   (,labelidx ,rt ,rt)
   (lambda (env l rt1 rt2)
      (multiple-value-bind (t* rt') (get-label-last-rest env l)
         (unless (reftype? rt')
            (raise `(expected-reftype-label ,rt')))
         (unless (<rt= env rt2 rt1)
            (raise `(non-matching ,rt2 ,rt1)))
         (unless (<rt= env (type-diff rt1 rt2) rt')
            (raise `(non-matching (type-diff rt1 rt2) ,rt')))
         `((,@t* ,rt1) (,@t* ,rt2)))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.198
  (return () (lambda (env)
               (unless (env-return env)
                 (raise `cannot-return))
               ; once again, subsumption
               `(,(env-return env) (poly))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.199
  ; if x is a funcidx, its defined type has to expand to a type of the form
  ; (func (t1*) (t2))
  (call (,funcidx) (lambda (env x) (cdr (expand (get-func-type env x)))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.200
  (call_ref
   (,typeidx)
   (lambda (env x)
     (match-case (expand (get-type env x))
        ((func ?t1 ?t2) `((,@t1 (ref null ,x)) ,t2))
        (?t (raise `(expected-function ,t))))))

  ; we do not support call_indirect yet

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.202
  (return_call
   (,funcidx)
   (lambda (env x)
      (let ((t (expand (get-func-type env x))))
         (unless (env-return env)
            (raise `cannot-return))
         (unless (<res= env (cadr t) (env-return env))
            (raise `(non-matching ,(caddr t) ,(env-return env))))
         ; subsumption
         `(,(cadr t) (poly)))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.203
  (return_call_ref
   (,typeidx)
   (lambda (env x)
      (match-case (expand (get-type env x))
        ((func ?t1 ?t2)
         (unless (env-return env)
            (raise `cannot-return))
         (unless (<res= env t2 (env-return env))
            (raise `(non-matching t2 ,(env-return env))))
         `((,@t1 (ref null ,x)) (poly)))
        (?t (raise `(expected-function ,t))))))

  ; we do not support return_call_indirect yet

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.205
  (throw
   (,tagidx)
   ,(lambda (env x) `(,(cadr (expand (get-tag-type env x))) (poly))))

  ; https://webassembly.github.io/spec/versions/core/WebAssembly-3.0-draft.pdf#subsubsection*.206
  (throw_ref () (((ref null exn)) (poly))) ; subsumption over and over
  (i32.load8_s () ((i32) (i32)))
  (i32.load8_u () ((i32) (i32)))
  (i32.store8  () ((i32 i32) ())))
