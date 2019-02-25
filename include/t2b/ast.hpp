// Copyright (c) 2019, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by a
// license that can be found in the LICENSE file.
#ifndef T2B_AST_HPP
#define T2B_AST_HPP

#include <memory>
#include <string>
#include <vector>
#include <t2b/value.hpp>

namespace t2b
{
    class visitor;

    class ast_node
    {
    public:
        virtual void accept(visitor &visitor) = 0;
    };

    class value_ctx : public ast_node
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

    class stmt_ctx : public ast_node
    {
    };

    class macro_ctx
    {
        std::vector<std::string> params;
        std::vector<stmt_ctx> body;
    };

    class value_stmt_ctx : public stmt_ctx
    {
        std::unique_ptr<value_ctx> value;
    };

    struct if_stmt_ctx : public stmt_ctx
    {
        std::unique_ptr<value_ctx> condition;
        std::vector<stmt_ctx> body;
    };

    struct macro_stmt_ctx : public stmt_ctx
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
