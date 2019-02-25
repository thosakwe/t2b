// Copyright (c) 2019, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by a
// license that can be found in the LICENSE file.
#include <cstdint>
#include <t2b/ast.hpp>

namespace t2b
{
    enum value_type
    {
        i8, i16, i32, i64, u8, u16, u32, u64,
        f, d, boolean, str, macro
    };

    struct value
    {
        value_type type;

        union
        {
            int8_t i8;
            int16_t i16;
            int32_t i32;
            int64_t i64;
            uint8_t u8;
            uint16_t u16;
            uint32_t u32;
            uint64_t u64;
            bool boolean;
            char *str;
            void *macro; // TODO: Pointer to macro
        };
    };
}
