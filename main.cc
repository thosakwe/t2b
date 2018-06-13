// Copyright (c) 2018, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by a
// GPL license that can be found in the LICENSE file.
#include <cstdint>
#include <cstring>
#include <ctime>
#include <map>
#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>
#include "t2b.hpp"

using namespace t2b;

int main(int argc, const char **argv) {
    string_map variables;
    macro_map macros;

    if (argc < 2) return exec(std::cin, std::cout, variables, macros, false);

    if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
        time_t rawtime;
        struct tm *timeinfo;

        time(&rawtime);
        timeinfo = localtime(&rawtime);
        std::cout << "t2b 1.0: Copyright (c) 2018-" << (1900 + timeinfo->tm_year)
                  << " Tobechukwu Osakwe: 2018-06-11"
                  << std::endl;
        std::cout << std::endl << "usage: t2b [filename]" << std::endl;
        return 0;
    }

    std::ifstream ifs(argv[1]);

    if (!ifs) {
        std::cerr << "fatal error: could not open file" << std::endl;
        return 1;
    }

    return exec(ifs, std::cout, variables, macros, false);
}