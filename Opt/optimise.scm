(module opt_optimise
   (from (ast_node "Ast/node.scm"))
   (import (opt_testbr "Opt/TestBr/walk.scm"))
   (import (opt_uncast "Opt/UnCast/walk.scm"))
   (export (opt-file! p::prog nthreads::bint)))

(define (opt-file! p::prog nthreads::bint)
   (for-each testbr! (-> p funcs))
   (for-each (lambda (f) (uncast! (-> p env) f)) (-> p funcs)))
