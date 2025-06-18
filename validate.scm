(module validate
   (include "read-table.sch")
   (main main))

(define (report f msg obj)
   (if (epair? obj)
       (error/location f msg obj (cadr (cer obj)) (caddr (cer obj)))
       (error f msg obj)))

(read-table *numtypes* "numtypes.sch")
(define (numtype? t)
   (hashtable-contains? *numtypes* t))

(define-struct env
   (ntypes 0)
   (types-table (make-hashtable)) ; to access with the names
   (types-vector (make-vector 0)) ; to access with the index
   )

(define (idx? x)
   (or (and (symbol? x) (equal? #\$ (string-ref (symbol->string x) 0)))
       (number? x)))

(define (get-type env t)
   (cond
    ((number? t)
     (if (< t (env-ntypes env))
         (vector-ref (env-types-vector env) t)
         (raise `(typeidx-out-of-range ,(env-ntypes env) ,t))))
    ((equal? #\$ (string-ref (symbol->string t) 0))
     (if (hashtable-contains? (env-types-table env) t)
         (hashtable-get (env-types-table env) t)
         (raise `(unknown-type ,t))))
    (#t (raise `(expected-type ,t)))))

;; deftypes (rec subtypes*).i are represented as (deftype . (subtypes* . i))
(define (deftype? t)
   (and (pair? t) (equal? 'deftype (car t))))

(define (deftype-head t)
   (match-case (list-ref (cadr t) (cddr t))
      ((or (sub final ?- (?hd . ?-))
           (sub final (?hd . ?-))
           (sub ?- (?hd . ?-))
           (sub (?hd . ?-)))
       hd)
      (else
       (report "deftype-head" "internal error, unexpected deftype form" t))))

(define (<ht= env t1 t2)
   (cond ((equal? t1 t2) #t)
         ((equal? t1 'bot) #t)
         ((idx? t1) (<ht= env (get-type env t1) t2))
         ((idx? t2) (<ht= env t1 (get-type env t2)))
         ((equal? t2 'any) (<ht= env t1 'eq))
         ((equal? t2 'eq)
          (or (equal? t1 'i31)
              (equal? t1 'struct)
              (equal? t1 'array)
              (and (deftype? t1) (<ht= env (deftype-head t1) 'eq))))
         ((and (deftype? t1) (symbol? t2))
          (equal? (deftype-head t1) t2))
         ((equal? 'none t1) (<ht= env t2 'any))
         ((equal? 'nofunc t1) (<ht= env t2 'func))
         ((equal? 'noextern t1) (<ht= env t2 'extern))
         (#t #f)))

(define (main argv)
   (display argv))
