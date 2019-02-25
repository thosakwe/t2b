// Copyright (c) 2018, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by an
// GPL-style license that can be found in the LICENSE file.
#ifndef T2B_T2B_H
#define T2B_T2B_H

#include <string>
#include <vector>

namespace t2b
{
    typedef struct
    {
        std::vector<std::string> parameters;
        std::string body;
    } t2b_function;

    typedef std::map<std::string, std::string> string_map;
    typedef std::map<std::string, t2b_function> macro_map;

    int exec(std::istream &stream, std::ostream &out, string_map &variables, macro_map &macros);
}

#endif //T2B_T2B_H
