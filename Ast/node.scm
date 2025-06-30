(module ast_node
   (export (class instruction::object
              intype::pair-nil
              outtype::pair-nil
              opcode::symbol)

           (abstract-class parameter)

           (class label::parameter
              idx::bint)

           (class one-arg::instruction
              x::parameter)

           (class two-args::instruction
              x::parameter
              y::parameter)

           (class br_table::instruction
              default::bint
              labels::pair-nil)

           (class if-then::instruction
              then::block)

           (class if-else::if-then
              else::block)

           (class block::instruction
              body::pair-nil)

           (class loop::instruction
              body::block)

           (abstract-class catch-branch::object)

           (class catch::catch-branch
              label::bint
              tagidx::bint)

           (class catch_ref::catch-branch
              label::bint
              tagidx::bint)

           (class catch_all::catch-branch
              label::bint)

           (class catch_all_ref::catch-branch
              label::bint)

           (class try_table::instruction
              catches::pair-nil
              body::block)))
