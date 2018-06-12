# t2b
A wicked-powerful text macro language for building binary files.
Supports comments, looping, Unicode, variables, conditionals, macros and recursion.

TLDR; Check out `example/`.

## Usage
```bash
$ t2b <filename> here
$ t2b # Read directly from stdin
```

`t2b` always writes to stdout. To output to a file, simply use a pipe (`|`).

## Language
Newlines are solely for the sake of readability; all whitespace is the same.

```t2b
# This is a comment!
#
# Comments must be on their own line.

# Emit a byte. In DECIMAL.
u8 10

# Toggle hex mode ON.
# In hex mode, numbers will be interpreted as hexadecimal.
hex
u8 10

# Toggle hex mode OFF.
hex

# Spit out a signed integer.
i64 25677

# Print a string (no line break)
str hello

# Print with a line break.
strl hello

# Wrap in quotes to capture whitespace.
strl "hello world!"

# Escapes are supported.
str "hello, world!\n"

# Unicode?
str "\u{1234}"

# Print a newline.
endl

# Do something 5 times.
# Indentation is purely for readability.
times 5
    u8 23
    u32 24
    times 10
        # We can nest loops
        str "50 times!!!"
    endtimes
endtimes

# Capture the output of another command.
# Oh, and store it into a variable.
set foo (u8 33)

# Access its value.
set bar (get foo)

# Emit its value 3 times.
times 3 get foo

# Create a simple macro.
macro emit_twice x
begin
    times 2 (get x)
endmacro

# Call it!
emit_twice 24
```

## Why?
The need for such a program arose when I was working on writing a simple VM.
Manually hex-editing files for an ever changing bytecode spec is tedious, error-prone,
and most of all - *sucky*.

Now there's a lightweight way to do just that.

## Supported Commands
* `u8...u64` - Emit unsigned integer
* `i8...i64` - Emit signed integer
* `hex` - Toggle hex mode on/off (defaults to OFF)
* `str <expr>` - Write a string
* `strl <expr>` - Write a string AND newline
* `endl` - Write a newline
* `not <expr>` Boolean NOT a char
* `if <cond> <pred> endif` Execute `<pred>` if `<cond> == 1`
* `get <expr>` - Fetch the global variable named `expr`
* `set <expr1> <expr2>` - Assign the global variable named `expr1` to `expr2`
* `=` - Compare two values, return `0` or `1`
* `times <count> <pred> endtimes` - Execute `<pred>` `<count>` times. `i` is always set to the current iteration's index.
* `macro <name> <param-names...> begin <pred> endmacro` - Declare a custom macro named `<name>`.

# What's next?
It's now feasible to write a machine code compiler in shell. Hooray.
Not sure why you would ever do that to yourself, though.