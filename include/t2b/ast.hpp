// Copyright (c) 2019, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by a
// license that can be found in the LICENSE file.
#ifndef T2B_AST_HPP
#define T2B_AST_HPP

#include <string>
#include <vector>
#include <t2b/value.hpp>

namespace t2b
{
    class t2b_visitor;

    class value_ctx
    {

    };

    struct command_call_ctx : public value_ctx
    {
        std::string command_name;
        std::vector<value_ctx> arguments;
    };

    struct constant_value_ctx : public value_ctx
    {
        value constant;
    };

    class stmt_ctx
    {
    public:
        // TODO: Make this generic?
        virtual void accept(t2b_visitor &visitor) = 0;
    };

    class macro_ctx
    {
        std::vector<std::string> params;
        std::vector<stmt_ctx> body;
    };

    struct if_stmt_ctx : public stmt_ctx
    {

    };

    struct manda_stmt_ctx : public stmt_ctx
    {
        macro_ctx macro;
    };

    struct times_stmt_ctx : public stmt_ctx
    {
        std::vector<stmt_ctx> body;
    };

    struct compilation_unit_ctx
    {
        std::vector<stmt_ctx> stmts;
    };
}


#endif //T2B_AST_HPP
