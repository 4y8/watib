(module ast_node
   (export (class func::object
              body::block
              locals::pair-nil)

           (class instruction::object
              intype::pair-nil
              outtype::pair-nil
              parent::func
              opcode::symbol)

           (abstract-class parameter)

           (class label::parameter
              idx::bint)

           (class variable::parameter
              idx::bint)

           (class type::parameter
              type)

           (class one-arg::instruction
              x::parameter)

           (class two-args::instruction
              x::parameter
              y::parameter)

           (class three-args::instruction
              x::parameter
              y::parameter
              z::parameter)

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

           (class catch-branch::object
              label::bint)

           (class catch::catch-branch
              tagidx::bint)

           (class catch_ref::catch-branch
              tagidx::bint)

           (class catch_all::catch-branch)

           (class catch_all_ref::catch-branch)

           (class try_table::instruction
              catches::pair-nil
              body::block)))
