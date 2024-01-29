# python-pdp11-asm
A python implementation of Unix v7 'as'

# Running the assembler

Given an example file:

    % echo "mov r1,r2; mov bozo,r3; .data; bozo: 77" > foo.s
    % python3 main.py foo.s

you will be treated to the SIMH "DO" (.ini file) format output:

    D 0 10102
    D 2 16703
    D 4 0
    D 6 77

which matches the bytes you would get from running

    % as foo.s
    % od a.out

on a unix v7 pdp-11 (e.g., under SIMH)

The assembler implements the same syntax and semantics as unix v7 'as', with some exceptions (see SEMANTICS section)

## main.py options

Argument --format controls the output format, and the default output stream:
 * --format do :: the default. SIMH "DO" command format, default output on stdout
 * --format load :: SIMH binary, suitable for LOAD command. Default output to a.simh
 * --format 407 :: unix v7 a.out format, NO SYMBOL TABLE. Default output to a.out

A non-zero base address ("origin") can be set via --org.

The output stream can be set using -o/--output

If --sys is given, the default (unix v7) symbols for various system calls (read, write, fork, and so forth) are defined. Equivalent to prepending /usr/include/sys.s on an actual v7 unix system.

# TESTS

To run the assembler tests:

    % ./TESTS

This runs the unittest modules in various files

# SEMANTICS

When I started the project, the goal I set for myself was "however 'as' behaves, do that".

This turned out to be surprisingly difficult, as there are some very quirky, undocumented, implementation-specific behaviors/semantics within 'as' for things that are "legal" but have little or no practical purpose.

Here are a few examples:

    foo = mov
    foo r1,r2
    mov = movb

This assembles as "movb r1,r2" ... the symbol 'foo' seems to have been assigned an indirect reference to the symbol 'mov', so subsequent changes to it stay live.

But the behavior is symbol-type specific. For example:

    bar = 1
    foo = bar
    foo            / just putting the value of foo into the byte stream
    bar = 2        / did foo "stay live" to the value of bar?

This puts a 1, not a 2, into the output bytestream.

This is especially puzzling:

    foo = clr
    foo r1                     / this becomes MOVB R1, 0
    clr = movb                 / because of this

this assembles to 0o110167, 0o000000   ('movb R1,0')

So the 'foo=clr' statement seemed to be operative for parsing 'foo r1' but the byte sequence generated used the "clr = movb" statement and filled in a zero for the missing operand.


Once we accept that as plausible, this of course is an error:

    foo = clr
    foo r1,r2
    clr = movb
    
The unix v7 'as' program reports a syntax error on "foo r1,r2" in this example.

It's definitely true that real code uses forward references and it was important to get the useful forms of those forward reference semantics correct.

As implemented here, many of the quirks of 'as' function the same, but some of the truly bizarre ones (like the above examples) do not.

## Special Variable: ..
Setting the (unix v7 'as') special variable '..' is effectively equivalent to specifying a .org statement (see SPECIAL FEATURES below). So, for example:

    % echo ".. = 10000 ; mov r1,r2" > foo.s
    % python3 main.py foo.s
    D 10000 10102


## Fidelity for real code

The unix v7 assembler is itself written in assembler, and a 'wc -l' on the unix v7 /usr/src/cmd/as *.s files yields a little over 3500 lines. To test this python implementation, that code was assembled on unix v7 and with this assembler. The resulting byte sequences are identical; the a.out files are identical except for the symbol table.

# SPECIAL FEATURES

There is no intent to produce a complete tool-chain for standalone PDP-11 programming but for convenience of anyone doing bare-metal work, two directives beyond those found in 'as' are provided:

 * .org
 * .boundary

A .org statement sets the "origin" to the address. For example:

    % echo ".org 10000 ; mov r1,r2" > foo.s
    % python3 main.py foo.s

will produce:

    D 10000 10102

Each segment (.text, .data., .bss) can have a separate .org statement:

    % echo ".org 10000 ; mov r1,r2; .data; .org 10010; 777" > foo.s
    % python3 main.py foo.s
    D 10000 10102
    D 10010 777

Note that the a.out format cannot represent this; use 'do' or 'load' files.

A .boundary statement will round the origin up to the next given boundary:

    % echo "mov r1,r2; .data; .boundary 10000; 777" > foo.s
    % python3 main.py foo.s
    D 0 10102
    D 10000 777

## About that BSS segment
The BSS segment, of course, requires run-time support to initialize the entire range of it to zero, or else it has to be represented as explicit zeros in the output file. This implementation does the latter, as it is meant primarily for use in bare-metal programming. If you are already programming for the unix runtime environment, use the unix v7 tool chain!

# GENERIC Tokenizer class
The file tokenizer.py is a generic regexp-based tokenizer that may be useful on its own (it is not specific to the assembler). See how it is used in astokens.py (e.g., how the rules are set up) and also see the file itself for further documentation.
