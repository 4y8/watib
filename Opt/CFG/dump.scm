;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Print a CFG in graphviz's DOT format.

(module cfg_dump
   (import (ast_node "Ast/node.scm"))
   (from (cfg_node "Opt/CFG/node.scm"))

   (export (print-cfg-as-dot g::cfg)))

(define-method (object-display obj::cfg-node . op)
   (let ((port (if (pair? op)
                   (car op)
                   (current-output-port))))
      (fprintf port "node_~a" (-fx 0 (-> obj idx)))))

(define-method (object-display obj::instruction . op)
   (let ((port (if (pair? op)
                   (car op)
                   (current-output-port))))
      (display (-> obj opcode))))

(define-generic (dump-jump j::jump vis?::vector src::cfg-node))

(define (dump-types l::pair-nil)
   (for-each (lambda (t) (printf " ~a" t)) l))

(define (dump-arc src::cfg-node dst::cfg-node lab::bstring)
   (printf "\t~a -> ~a [label=\"~a :" src dst lab)
   (dump-types (-> dst intype))
   (printf "\"];\n"))

(define-method (dump-jump j::conditional vis?::vector src::cfg-node)
   (dump-arc src (-> j dst-true) "true")
   (dump-arc src (-> j dst-false) "false")
   (dump-node (-> j dst-false) vis?)
   (dump-node (-> j dst-true) vis?))

(define-method (dump-jump j::unconditional vis?::vector src::cfg-node)
   (dump-arc src (-> j dst) "")
   (dump-node (-> j dst) vis?))

(define-method (dump-jump j::on-cast vis?::vector src::cfg-node)
   (dump-arc src (-> j dst-cast) "cast")
   (dump-arc src (-> j dst-cast-fail) "cast fail")
   (dump-node (-> j dst-cast) vis?)
   (dump-node (-> j dst-cast-fail) vis?))

(define-method (dump-jump j::terminal vis?::vector src::cfg-node)
   (printf "\t~a -> return_~a;\n" src src))

(define (dump-node n::cfg-node vis?::vector)
   (unless (vector-ref vis? (-fx 0 (-> n idx)))
      (vector-set! vis? (-fx 0 (-> n idx)) #t)
      (printf "\t~a [shape=box, label=\"" n)
      (dump-types (-> n intype))
      (printf " -->")
      (dump-types (-> n outtype))
      (printf "\n\n")
      (for-each print (-> n body))
      (printf "\"];\n")
      (dump-jump (-> n end) vis? n)))

(define (print-cfg-as-dot g::cfg)
   (print "digraph CFG {")
   (let ((vis?::vector (make-vector (-> g size) #f)))
      (dump-node (-> g entry) vis?))
   (print "}"))
