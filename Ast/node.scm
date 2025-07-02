(module ast_node
   (export
           (class modulefield::object)

           (class func::modulefield
              body::block
              locals::pair-nil)

           (class instruction::object
              intype::pair-nil
              outtype::pair-nil
              parent::modulefield
              opcode::symbol)

           (abstract-class parameter)

           (class labelidxp::parameter
              idx::bint
              type::pair-nil)

           (class funcidxp::parameter
              idx::bint
              type::pair)

           (class localidxp::parameter
              init?::bool
              type
              idx::bint)

           (class typeidxp::parameter
              idx::bint
              type)

           (class tagidxp::parameter
              idx::bint
              type::pair)

           (class globalidxp::parameter
              idx::bint
              mut?::bool
              type)

           (class fieldidxp::parameter
              idx::bint
              mut?::bool
              type)

           (class memidxp::parameter
              idx::bint)
           (class dataidxp::parameter
              idx::bint)
           (class nump::parameter
              num)

           (class typep::parameter
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
              labels::pair)

           (class sequence::instruction
              body::pair-nil)

           (class if-then::instruction
              then::instruction)

           (class if-else::if-then
              else::instruction)

           (class block::sequence)

           (class loop::sequence)

           (class catch-branch::object
              label::labelidxp)

           (class catch::catch-branch
              tag::tagidxp)

           (class catch_ref::catch-branch
              tag::tagidxp)

           (class catch_all::catch-branch)

           (class catch_all_ref::catch-branch)

           (class try_table::sequence
              catches::pair-nil)))
