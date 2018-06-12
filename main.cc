// Copyright (c) 2018, Tobechukwu Osakwe.
//
// All rights reserved.
//
// Use of this source code is governed by an
// MIT-style license that can be found in the LICENSE file.
#include <cstdint>
#include <ctime>
#include <map>
#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>

typedef struct
{
    std::vector<std::string> parameters;
    std::string body;
} t2b_function;

typedef std::map<std::string, std::string> string_map;
typedef std::map<std::string, t2b_function> macro_map;

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

int exec(std::istream &stream, std::ostream &out, string_map &variables, macro_map &macros, bool hexMode);

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

void skipwspace(std::istream &stream) {
    while (!stream.eof() && iswspace(stream.peek())) stream.ignore();
}

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
        exec(iss, ss, variables, macros, hexMode);
        return ss.str();
    }
    //std::cerr << (char) stream.peek() << std::endl;

    if ((char) stream.peek() != '"') {
        std::string str;
        skipwspace(stream);
        stream >> str;
        //std::getline(stream, str);
        return str;
    }

    std::string ss;
    stream.get();

    char ch;
    while (!stream.eof() && (ch = (char) stream.get()) != '"') {
        if (ch != '\\' && !stream.eof()) ss += ch;
        else if (ch == '\\') {
            ch = (char) stream.get();

            if (ch != 'u' || (!stream.eof() && stream.peek() != '{'))
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

int exec(std::istream &stream, std::ostream &out, string_map &variables, macro_map &macros, bool hexMode) {
    std::string command;

    while (!stream.eof()) {
        stream >> command;

        if (command.empty()) continue;

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
            return exec(stream, out, variables, macros, !hexMode);
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
                int result = exec(iss, out, variables, macros, hexMode);
                if (result != 0) return result;
            }

            continue;
        } else if (command == "get") {
            auto it = variables.find(getstring(stream, variables, macros, hexMode));
            if (it != variables.end()) out << it->second;
        } else if (command == "set") {
            std::string name = getstring(stream, variables, macros, hexMode);
            std::string value = getstring(stream, variables, macros, hexMode);
            variables[name] = value;
        } else if (command == "=") {
            std::string left = getstring(stream, variables, macros, hexMode),
                    right = getstring(stream, variables, macros, hexMode);
            uint8_t value = left == right ? (uint8_t) 1 : (uint8_t) 0;
            out.write(reinterpret_cast<const char *>(&value), sizeof value);
        } else if (command == "times") {
            int times;
            std::stringstream times_value(getstring(stream, variables, macros, hexMode));
            times_value >> (hexMode ? std::hex : std::dec) >> times;
            skipwspace(stream);

            // Read until 'endtimes'
            std::stringstream ss;
            int timesCount = 1;
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

            for (int i = 0; i < times; i++) {
                std::istringstream iss(ss.str());
                variables["i"] = std::to_string(i);
                int result = exec(iss, out, variables, macros, hexMode);
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
                int result = exec(iss, out, variables, macros, hexMode);
                if (result != 0) return result;
            } else {
                std::cerr << "fatal error: invalid command: \"" << command << "\"" << std::endl;
                return 1;
            }
        }
    }

    return 0;
}