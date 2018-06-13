// Copyright (c) 2018, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by a
// GPL-style license that can be found in the LICENSE file.
#include <cstdint>
#include <cstring>
#include <ctime>
#include <map>
#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>
#include <regex>
#include "t2b.hpp"

using namespace t2b;

#define HANDLE_NUM(symbol, command, input, type, size, hexMode) \
    if ((symbol) == (command)) {\
        int value;\
        std::string s = getstring(input, variables, macros, hexMode);\
        std::istringstream ss(s);\
        ss >> ((hexMode) ? std::hex : std::dec) >> value;\
        auto cast = static_cast<type>(value);\
        out.write(reinterpret_cast<const char*>(&cast), sizeof cast);\
        continue;\
    }

static int _exec(std::istream &stream, std::ostream &out, string_map &variables, macro_map &macros, bool hexMode);

void skipwspace(std::istream &stream) {
    while (!stream.eof() && iswspace(stream.peek())) stream.ignore();
}

std::string trim(const std::string &str) {
    size_t first = str.find_first_not_of(' ');
    if (std::string::npos == first) {
        return str;
    }
    size_t last = str.find_last_not_of(' ');
    return str.substr(first, (last - first + 1));
}

static std::regex rgx_hex("0x([A-Fa-f0-9]+)");
static std::regex rgx_oct("0o([0-7]+)");
static std::regex rgx_bin("0b([0-1]+)");

std::string getstring(std::istream &stream, string_map &variables, macro_map &macros, bool hexMode) {
    skipwspace(stream);
    if (stream.eof()) return std::string("");

    if ((char) stream.peek() == '(') {
        stream.get();
        char ch;
        std::string command;
        int parenCount = 1;

        while (!stream.eof()) {
            ch = (char) stream.get();

            if (ch == '\'') {
                command += ch;
                if (!stream.eof()) command += (char) stream.get();
            } else if (ch == '(') {
                parenCount++;
                command += ch;
            } else if (ch == ')') {
                if (--parenCount <= 0) {
                    break;
                } else {
                    command += ch;
                }
            } else {
                command += ch;
            }
        }

        /*if (parenCount > 0 && stream.eof()) {
            std::cerr << "fatal error: unclosed parentheses" << std::endl;
        }*/

        std::istringstream iss(command);
        std::stringstream ss;
        _exec(iss, ss, variables, macros, hexMode);
        return ss.str();
    }
    //std::cerr << (char) stream.peek() << std::endl;

    if ((char) stream.peek() != '"') {
        std::string str;
        skipwspace(stream);
        stream >> str;
        //std::getline(stream, str);
        str = trim(str);

        std::smatch m;

        if (std::regex_search(str, m, rgx_bin))
            return std::to_string(strtol(m.str(1).c_str(), nullptr, 2));
        if (std::regex_search(str, m, rgx_oct))
            return std::to_string(strtol(m.str(1).c_str(), nullptr, 8));
        if (std::regex_search(str, m, rgx_hex))
            return std::to_string(strtol(m.str(1).c_str(), nullptr, 16));

        return str;
    }

    std::string ss;
    stream.get();

    char ch;
    while (!stream.eof() && (ch = (char) stream.get()) != '"') {
        if (ch != '\\' && !stream.eof()) ss += ch;
        else if (ch == '\\') {
            ch = (char) stream.get();

            if (ch == 'b') ss += '\b';
            if (ch == 'f') ss += '\f';
            if (ch == 'r') ss += '\r';
            if (ch == 'n') ss += '\n';
            if (ch == 't') ss += '\t';
            else if (ch != 'u' || (!stream.eof() && stream.peek() != '{'))
                ss += ch;
            else {
                std::string number;
                stream.get();

                while (!stream.eof() && stream.peek() != '}') {
                    ch = (char) stream.get();
                    if (!isalnum(ch)) break;
                    number += ch;
                }

                if (!stream.eof()) stream.get();

                if (!number.empty()) {
                    ch = (char) strtol(number.c_str(), nullptr, 16);
                    ss += ch;
                }
            }
        }
    }

    return ss;
}

int t2b::exec(std::istream &stream, std::ostream &out, string_map &variables, macro_map &macros, bool hexMode) {
    int result = _exec(stream, out, variables, macros, hexMode);
    return (result == 0 || result == 2) ? 0 : result;
}

int _exec(std::istream &stream, std::ostream &out, string_map &variables, macro_map &macros, bool hexMode) {
    std::string command;

    while (!stream.eof()) {
        //command.clear();
        stream >> command;

        if (command.empty() || stream.fail()) continue;

        HANDLE_NUM(command, "i8", stream, int8_t, 8, hexMode);
        HANDLE_NUM(command, "i16", stream, int16_t, 16, hexMode);
        HANDLE_NUM(command, "i32", stream, int32_t, 32, hexMode);
        HANDLE_NUM(command, "i64", stream, int64_t, 64, hexMode);
        HANDLE_NUM(command, "u8", stream, uint8_t, 8, hexMode);
        HANDLE_NUM(command, "u16", stream, uint16_t, 16, hexMode);
        HANDLE_NUM(command, "u32", stream, uint32_t, 32, hexMode);
        HANDLE_NUM(command, "u64", stream, uint64_t, 64, hexMode);

        if (command.at(0) == '#') {
            // Allow comments
            std::string s;
            std::getline(stream, s);
            skipwspace(stream);
        } else if (command == "d") {
            std::string s = getstring(stream, variables, macros, hexMode);
            std::stringstream ss(s);
            double value;
            ss >> value;
            out.write(reinterpret_cast<const char *>(&value), sizeof value);
            continue;
        } else if (command == "f") {
            std::string s = getstring(stream, variables, macros, hexMode);
            std::stringstream ss(s);
            float value;
            ss >> value;
            out.write(reinterpret_cast<const char *>(&value), sizeof value);
            continue;
        } else if (command == "hex") {
            return _exec(stream, out, variables, macros, !hexMode);
        } else if (command == "len") {
            std::string str = getstring(stream, variables, macros, hexMode);
            uint64_t len = str.length();
            out.write(reinterpret_cast<const char *>(&len), sizeof len);
        } else if (command == "size") {
            std::string str = getstring(stream, variables, macros, hexMode);
            uint64_t size = str.size();
            out.write(reinterpret_cast<const char *>(&size), sizeof size);
        } else if (command == "str") {
            std::string str = getstring(stream, variables, macros, hexMode);
            out << str;
        } else if (command == "strl") {
            std::string str = getstring(stream, variables, macros, hexMode);
            out << str << std::endl;
        } else if (command == "endl") {
            out << std::endl;
        } else if (command == "not") {
            auto *str = getstring(stream, variables, macros, hexMode).c_str();
            uint8_t input = *((uint8_t *) str);
            uint8_t value = input != 1 ? (uint8_t) 1 : (uint8_t) 0;
            out.write(reinterpret_cast<const char *>(&value), sizeof value);
            continue;
        } else if (command == "if") {
            auto *str = getstring(stream, variables, macros, hexMode).c_str();
            uint8_t value = *((uint8_t *) str);
            std::string s;
            std::stringstream ss;
            int ifCount = 1;

            while (!stream.eof()) {
                stream >> s;

                if (s == "if") {
                    ifCount++;
                    ss << s << " ";
                } else if (s == "endif") {
                    if (--ifCount <= 0)
                        break;
                    else ss << s << " ";
                } else {
                    ss << s << " ";
                }
            }

            if (value == 1) {
                std::istringstream iss(ss.str());
                int result = _exec(iss, out, variables, macros, hexMode);
                if (result != 0) return result;
            }

            continue;
        } else if (command == "get") {
            auto it = variables.find(getstring(stream, variables, macros, hexMode));
            if (it != variables.end()) out << it->second;
        } else if (command == "return") {
            return 2;
        } else if (command == "set") {
            std::string name = getstring(stream, variables, macros, hexMode);
            std::string value = getstring(stream, variables, macros, hexMode);
            variables[name] = value;
        } else if (command == "=") {
            std::string left = getstring(stream, variables, macros, hexMode),
                    right = getstring(stream, variables, macros, hexMode);
            //std::cerr << "Left: <" << left << ">, right: <" << right << ">" << std::endl;
            uint8_t value = (left.compare(right)) == 0 ? (uint8_t) 1 : (uint8_t) 0;
            out.write(reinterpret_cast<const char *>(&value), sizeof value);
        } else if (command == "times") {
            long times;
            std::stringstream times_value(getstring(stream, variables, macros, hexMode));
            times_value >> (hexMode ? std::hex : std::dec) >> times;
            skipwspace(stream);

            // Read until 'endtimes'
            std::stringstream ss;
            unsigned long timesCount = 1;
            std::string s;

            while (!stream.eof()) {
                stream >> s;

                if (s == "times") {
                    timesCount++;
                    ss << s << " ";
                } else if (s == "endtimes") {
                    if (--timesCount <= 0)
                        break;
                    else ss << s << " ";
                } else {
                    ss << s << " ";
                }
            }

            /*if (stream.eof() && timesCount > 0) {
                std::cerr << R"(fatal error: unclosed "times" loop.")" << t << std::endl;
                return 1;
            }*/

            std::string body = ss.str();

            for (unsigned long i = 0; i < times; i++) {
                std::istringstream iss(ss.str());
                variables["i"] = std::to_string(i);
                int result = _exec(iss, out, variables, macros, hexMode);
                if (result != 0) return result;
            }

            continue;
        } else if (command == "macro") {
            auto name = getstring(stream, variables, macros, hexMode);
            if (!name.empty() && !stream.eof()) {
                std::string s;
                t2b_function function{};
                int macroCount = 1;

                while (!stream.eof()) {
                    stream >> s;

                    if (s == "begin") break;
                    else {
                        function.parameters.push_back(s);
                    }
                }

                while (!stream.eof()) {
                    stream >> s;
                    if (s == "endmacro") {
                        if (--macroCount <= 0) break;
                        else function.body += s + " ";
                    } else if (s == "macro") {
                        macroCount++;
                        function.body += s + " ";
                    } else {
                        function.body += s + " ";
                    }
                }

                macros[name] = function;
            }
        } else {
            auto it = macros.find(command);

            if (it != macros.end()) {
                auto function = it->second;

                for (const auto &parameter : function.parameters) {
                    variables[parameter] = getstring(stream, variables, macros, hexMode);
                }

                std::istringstream iss(function.body);
                int result = _exec(iss, out, variables, macros, hexMode);
                if (result != 0 && result != 2) return result;
            } else {
                std::cerr << "fatal error: invalid command: \"" << command << "\"" << std::endl;
                return 1;
            }
        }
    }

    return 0;
}