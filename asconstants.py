# MIT License
#
# Copyright (c) 2024 Neil Webber
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# various constants .. register names, opcodes, etc

REGISTERS = {
    'r0': 0,
    'r1': 1,
    'r2': 2,
    'r3': 3,
    'r4': 4,
    'r5': 5,
    'r6': 6,
    'sp': 6,
    'r7': 7,
    'pc': 7,
}


BRANCH_CODES = {
    'br': 0o000400,
    'bne': 0o001000,
    'beq': 0o001400,
    'bpl': 0o100000,
    'bmi': 0o100400,
    'bvc': 0o102000,
    'bvs': 0o102400,
    'bcc': 0o103000,
    'bec': 0o103000,         # "error clear" == "carry clear"
    'bhis': 0o103000,
    'bcs': 0o103400,
    'bes': 0o103400,         # "error set" == "carry set"
    'blo': 0o103400,
    'bge': 0o002000,
    'blt': 0o002400,
    'bgt': 0o003000,
    'ble': 0o003400,
    'bhi': 0o101000,
    'blos': 0o101400
}


SINGLEOPERANDS = {
    'jmp': 0o000100,
    'clr': 0o005000,
    'clrb': 0o105000,
    'com': 0o005100,
    'comb': 0o105100,
    'inc': 0o005200,
    'incb': 0o105200,
    'dec': 0o005300,
    'decb': 0o105300,
    'neg': 0o005400,
    'negb': 0o105400,
    'adc': 0o005500,
    'adcb': 0o105500,
    'sbc': 0o005600,
    'sbcb': 0o105600,
    'tst': 0o005700,
    'tstb': 0o105700,
    'ror': 0o006000,
    'rorb': 0o106000,
    'rol': 0o006100,
    'rolb': 0o106100,
    'asr': 0o006200,
    'asrb': 0o106200,
    'asl': 0o006300,
    'aslb': 0o106300,
    'mark': 0o006400,
    'mfpi': 0o006500,
    'mfpd': 0o106500,
    'mtpi': 0o006600,
    'mtpd': 0o106600,
    'sxt': 0o006700,
    'swab': 0o000300,
}


DOUBLEOPERANDS = {
    'mov': 0o010000,
    'movb': 0o110000,
    'cmp': 0o020000,
    'cmpb': 0o120000,
    'bit': 0o030000,
    'bitb': 0o130000,
    'bic': 0o040000,
    'bicb': 0o140000,
    'bis': 0o050000,
    'bisb': 0o150000,
    'add': 0o060000,
    'sub': 0o160000,
}

# "63" refers src being a fully-general 6-bit mode and the dst restricted
# to register only. The fields are also flipped in the instruction
# encoding (opode::dst::src). See, for example, class TwoOper63.
D63OPERANDS = {
    'mul': 0o070000,
    'mpy': 0o070000,           # v7 'as' has this as alias
    'div': 0o071000,
    'dvd': 0o071000,           # v7 'as' has this as alias
    'ash': 0o072000,
    'als': 0o072000,           # v7 alias
    'ashc': 0o073000,
    'alsc': 0o073000,          # v7 alias
}

# "36" is register-only src, fully-general 6-bit dst.
D36OPERANDS = {
    'xor': 0o074000,
    'jsr': 0o004000,
}

ZEROOPERANDS = {
    'nop': 0o000240,
    'clc': 0o000241,
    'clv': 0o000242,
    'clz': 0o000244,
    'cln': 0o000250,
    'ccc': 0o000257,
    'sec': 0o000261,
    'sev': 0o000262,
    'sez': 0o000264,
    'sen': 0o000270,
    'scc': 0o000277,
    # technically there are all the other combinations but ... meh
    # define them as literals if needed
}

OTHEROPERANDS = {
    'sob': 0o077000,
    'rts': 0o000200,
}
