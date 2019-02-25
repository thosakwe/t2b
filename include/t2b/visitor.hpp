// Copyright (c) 2019, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by a
// license that can be found in the LICENSE file.
#ifndef T2B_VISITOR_HPP
#define T2B_VISITOR_HPP

#include <t2b/ast.hpp>

namespace t2b
{
    class visitor
    {
    public:
        virtual void visit_command_call(command_call_ctx &ctx) = 0;

        virtual void visit_compilation_unit(compilation_unit_ctx &ctx) = 0;

        virtual void visit_constant_value(constant_value_ctx &ctx) = 0;

        virtual void visit_if(if_stmt_ctx &ctx) = 0;

        virtual void visit_macro(macro_ctx &ctx) = 0;

        virtual void visit_macro_stmt(macro_stmt_ctx &ctx) = 0;

        virtual void visit_times(times_stmt_ctx &ctx) = 0;

        virtual void visit_value_stmt(value_stmt_ctx &ctx) = 0;
    };
}


#endif //T2B_VISITOR_HPP
