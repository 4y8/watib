`(
  ;; section 3.4.1
  (i32.const (,i32) (() (i32)))
  (i64.const (,i64) (() (i64)))
  (f32.const (,f32) (() (f32)))
  (f64.const (,f64) (() (f64)))

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
  ; input type and the output type
  (ref.eq () (((ref null eq) (ref null eq)) (i32)))
  ; by subsumption
  (ref.test (,reftype) ,(lambda (- rt) `((,rt) (i32))))
  ; by subsumption
  (ref.cast (,reftype) ,(lambda (- rt) `((,rt) (,rt))))

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

  (array.len () (((ref null array)) (i32)))

  (array.fill
   (,typeidx)
   (lambda (env x)
      (let ((ft (get-array-ft env x)))
         (unless (equal? (car ft) 'var)
            (raise `(set-const ,x)))
         `(((ref null ,x) i32 (unpack-ft ft) i32) ()))))

  (array.copy
   (,typeidx ,typeidx)
   (lambda (env x y) '()))

  (ref.i32   () ((i32) ((ref i31))))
  (i31.get_s () (((ref i31)) (i32)))
  (i31.get_u () (((ref i31)) (i32)))

  (i32.load8_s () ((i32) (i32)))
  (i32.load8_u () ((i32) (i32)))
  (i32.store8  () ((i32 i32) ())))
