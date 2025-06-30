(module ast_node
   (export (class node::object
              type::pair
              (parameters::pair-nil (default '()))
              opcode::symbol)

           (class if-then::node
              then::block)

           (class if-else::if-then
              else::block)

           (class block::node
              body::pair-nil)

           (class loop::node
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

           (class try_table::node
              catches::pair-nil
              body::block)))
