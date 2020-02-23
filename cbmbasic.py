#!/usr/bin/env python3
"""This module provides stuff concerning the BASIC language found in 8-bit
machines made by Commodore Business Machines."""

class Basic(object):
    """This class holds info about Commodore Basic."""

    def __init__(self, basic_version = 7, machine = 128):
        """Valid basic versions are 1, 2, 3.5 and 7.\nValid machines are 64, 264, 128."""
        if basic_version not in [1, 2, 3.5, 7]:
            raise Exception("Basic version is not in [1, 2, 3.5, 7]")
        if machine not in [64, 264, 128]:
            raise Exception("Machine is not in [64, 264, 128]")
        self.basic_version = basic_version
        self.machine = machine
        self.petscii64 = [
# 0x00-0x1f lower control characters
"{0x00}",       "{^a}",         "{^b}",             "{^c(stop)}",
"{^d}",         "{white}",      "{^f}",             "{^g}",
"{^h(lock)}",   "{^i(unlock)}", "{^j}",             "{^k}",
"{^l}",         "{return}",     "{^n(lowercase)}",  "{^o}",
"{^p}",         "{down}",       "{rvs on}",         "{home}",
"{del}",        "{^u}",         "{^v}",             "{^w}",
"{^x}",         "{^y}",         "{^z}",             "{^[}",
"{red}",        "{right}",      "{green}",          "{blue}",
# 0x20-0x5f printables
" ",    "!",    "\"",   "#",    "$",        "%",    "&",            "'",
"(",    ")",    "*",    "+",    ",",        "-",    ".",            "/",
"0",    "1",    "2",    "3",    "4",        "5",    "6",            "7",
"8",    "9",    ":",    ";",    "<",        "=",    ">",            "?",
"@",    "a",    "b",    "c",    "d",        "e",    "f",            "g",
"h",    "i",    "j",    "k",    "l",        "m",    "n",            "o",
"p",    "q",    "r",    "s",    "t",        "u",    "v",            "w",
"x",    "y",    "z",    "[",    "{pound}",  "]",    "{up arrow}",   "{left arrow}",
# 0x60-0x7f printable, but not generated by CBM keyboard
"{shift *}",    "A",    "B",    "C",            "D",        "E",            "F",    "G",
"H",            "I",    "J",    "K",            "L",        "M",            "N",    "O",
"P",            "Q",    "R",    "S",            "T",        "U",            "V",    "W",
"X",            "Y",    "Z",    "{shift +}",    "{cbm -}",  "{shift -}",    "{pi}", "{cbm *}",
# 0x80-0x9f upper control characters
"{0x80}",   "{orange}",         "{0x82}",               "{0x83(run)}",
"{0x84}",   "{f1}",             "{f3}",                 "{f5}",
"{f7}",     "{f2}",             "{f4}",                 "{f6}",
"{f8}",     "{shift return}",   "{0x8e(uppercase)}",    "{0x8f}",
"{black}",  "{up}",             "{rvs off}",            "{clr}",
"{inst}",   "{brown}",          "{light red}",          "{gray 1}",
"{gray 2}", "{light green}",    "{light blue}",         "{gray 3}",
"{purple}", "{left}",           "{yellow}",             "{cyan}",
# 0xa0-0xbf mostly graphics characters, these are generated by CBM keyboard
"{shift space}",    "{cbm k}",          "{cbm i}",      "{cbm t}",
"{cbm @}",          "{cbm g}",          "{cbm +}",      "{cbm m}",
"{cbm pound}",      "{shift pound}",    "{cbm n}",      "{cbm q}",
"{cbm d}",          "{cbm z}",          "{cbm s}",      "{cbm p}",
"{cbm a}",          "{cbm e}",          "{cbm r}",      "{cbm w}",
"{cbm h}",          "{cbm j}",          "{cbm l}",      "{cbm y}",
"{cbm u}",          "{cbm o}",          "{shift @}",    "{cbm f}",
"{cbm c}",          "{cbm x}",          "{cbm v}",      "{cbm b}",
# 0xc0-0xdf printables again, these are generated by CBM keyboard, except for 0xde (use 0xff)
"{shift *}",    "A",    "B",    "C",            "D",        "E",            "F",    "G",
"H",            "I",    "J",    "K",            "L",        "M",            "N",    "O",
"P",            "Q",    "R",    "S",            "T",        "U",            "V",    "W",
"X",            "Y",    "Z",    "{shift +}",    "{cbm -}",  "{shift -}",    "{pi}", "{cbm *}",
# 0xe0-0xff graphics characters again, but these codes are not generated by CBM keyboard
"{shift space}",    "{cbm k}",          "{cbm i}",      "{cbm t}",
"{cbm @}",          "{cbm g}",          "{cbm +}",      "{cbm m}",
"{cbm pound}",      "{shift pound}",    "{cbm n}",      "{cbm q}",
"{cbm d}",          "{cbm z}",          "{cbm s}",      "{cbm p}",
"{cbm a}",          "{cbm e}",          "{cbm r}",      "{cbm w}",
"{cbm h}",          "{cbm j}",          "{cbm l}",      "{cbm y}",
"{cbm u}",          "{cbm o}",          "{shift @}",    "{cbm f}",
"{cbm c}",          "{cbm x}",          "{cbm v}",      "{pi}"
]

    def get_petscii_table(self):
        """Return petscii table for the chosen machine."""
        if self.machine == 64:
            return self.petscii64
        elif self.machine == 264:
            # 264 codes only differ slightly:
            petscii264 = self.petscii64[:]
            petscii264[27]  = "{^[(escape)}"
            petscii264[130] = "{0x82(flash on)}"
            petscii264[131] = "{0x83(flash off)}"
            petscii264[140] = "{0x8c(help)}"
            # ted colors differ from vic, so just give keys:
            petscii264[144] = "{^1}"
            petscii264[5]   = "{^2}"
            petscii264[28]  = "{^3}"
            petscii264[159] = "{^4}"
            petscii264[156] = "{^5}"
            petscii264[30]  = "{^6}"
            petscii264[31]  = "{^7}"
            petscii264[158] = "{^8}"
            petscii264[129] = "{cbm 1}"
            petscii264[149] = "{cbm 2}"
            petscii264[150] = "{cbm 3}"
            petscii264[151] = "{cbm 4}"
            petscii264[152] = "{cbm 5}"
            petscii264[153] = "{cbm 6}"
            petscii264[154] = "{cbm 7}"
            petscii264[155] = "{cbm 8}"
            return petscii264
        elif self.machine == 128:
            # c128 codes are slightly different:
            petscii128 = self.petscii64[:]
            petscii128[7]   = "{^g(bell)}"
            petscii128[8]   = "{^h}"
            petscii128[9]   = "\t"  #"{^i(tab)}"
            petscii128[10]  = "{^j(line feed)}"
            petscii128[11]  = "{^k(lock)}"
            petscii128[12]  = "{^l(unlock)}"
            petscii128[24]  = "{^x(set tab)}"
            petscii128[27]  = "{^[(escape)}"
            petscii128[2]   = "{^b(underline on)}"
            petscii128[130] = "{0x82(underline off)}"
            petscii128[15]  = "{^o(flash on)}"
            petscii128[143] = "{0x8f(flash off)}"
            petscii128[132] = "{0x84(help)}"
            return petscii128
        else:
            raise Exception("Invalid machine!")

    def get_token_dict(self):
        """Return a token dictionary for the chosen basic version."""
        # whether trailing space is actually output depends on next byte!
        strings = [
# basic 1:
"end ",     "for ",     "next ",    "data ",
"input#",   "input ",   "dim ",     "read ",
"let ",     "goto ",    "run ",     "if ",
"restore ", "gosub ",   "return ",  "rem ",
"stop ",    "on ",      "wait ",    "load ",
"save ",    "verify ",  "def ",     "poke ",
"print#",   "print ",   "cont ",    "list ",
"clr ",     "cmd ",     "sys ",     "open ",
"close ",   "get ",     "new ",     "tab(",
"to ",      "fn ",      "spc(",     "then ",
"not ",     "step ",    "+ ",       "- ",
"* ",       "/ ",       "^ ",       "and ",
"or ",      "> ",       "= ",       "< ",
"sgn",      "int",      "abs",      "usr",
"fre",      "pos",      "sqr",      "rnd",
"log",      "exp",      "cos",      "sin",
"tan",      "atn",      "peek",     "len",
"str$",     "val",      "asc",      "chr$",
"left$",    "right$",   "mid$",
# basic 1 also contains token 0xff (see below), which is pi
                                    "go ", # basic 2, only for "GO TO" and, in basic 7, "GO64"
# basic 3.5:
"rgr",      "rclr",     "rlum",         "joy",
# (RLUM was removed in basic 7, 0xce was re-used as a prefix code)
"rdot",     "dec",      "hex$",         "err$",
"instr",    "else ",    "resume ",      "trap ",
"tron ",    "troff ",   "sound ",       "vol ",
"auto ",    "pudef ",   "graphic ",     "paint ",
"char ",    "box ",     "circle ",      "gshape ",
# (GSHAPE/SSHAPE/DRAW are called PASTE/CUT/LINE in basic 10)
"sshape ",  "draw ",    "locate ",      "color ",
"scnclr ",  "scale ",   "help ",        "do ",
"loop ",    "exit ",    "directory ",   "dsave ",
# (DIRECTORY is called DIR in basic 10)
"dload ",
"header ",  # dos command "n"
"scratch ", # dos command "s"
"collect ", # dos command "v"
"copy ",    # dos command "c" with one source
"rename ",  # dos command "r"
"backup ",  # dos command "d"
"delete ",
"renumber ","key ",     "monitor ",     "using ",
# "USING" is for PRINT USING, UNTIL/WHILE are for DO/LOOP
"until ",   "while ",   "{0xfe}",       "{pi}"
# (basic 7 uses 0xfe as a prefix code)
# token 0xff is pi (3.14159...), rendered as greek letter.
# pi is in basic 1.
]
        if self.basic_version not in [1, 2, 3.5, 7]:
            raise Exception("Invalid basic version!")
        # TODO - add code so token table is modified according to version!
        tokens = dict()
        first_token = 0x80
        for index, string in enumerate(strings):
            #tokens[string] = index + first_token
            tokens[index + first_token] = string
        # add fused/artificial tokens (for easier C/Python substitutions later on)
        tokens[0xa123] = "get#" # "get" from channel    (-> get_channel)
        tokens[0xa1f9] = "getkey "
        tokens[0xb200] = "= "   # assignment
        tokens[0xb2b2] = "= "   # test for equality     (-> "==")
        tokens[0xb3b1] = "<> "  # test for inequality   (-> "!=")
        tokens[0xb1b2] = ">= "  # test for "greater or equal"
        tokens[0xb3b2] = "<= "  # test for "smaller or equal"
        tokens[0xcba4] = "go to "   # alternative to GOTO
        tokens[0xd682] = "resume next"  # now "next" cannot influence indentation
        # now add CE extensions
        first_token = 0xce01
        for index, string in enumerate([
        "{0xce 0x01}",  "pot",      "bump",
"pen",  # basic 10 calls this LPEN?
        "rsppos",       "rsprite",  "rspcolor",
"xor",  "rwindow",      "pointer"
]):
            #tokens[string] = index + first_token
            tokens[index + first_token] = string
        # now add FE extensions
        first_token = 0xfe01
        for index, string in enumerate([
# 0x00 is always line terminator, so there is no 0xfe 0x00 token:
                "{0xfe 0x01}",  "bank ",        "filter ",
"play ",        "tempo ",       "movspr ",      "sprite ",
"sprcolor ",    "rreg ",        "envelope ",    "sleep ",
"catalog ",     "dopen ",       "append ",      "dclose ",
# append is dos OPEN with ",a" mode
"bsave ",       "bload ",       "record ",      "concat ",
# record is dos command "p", concat is dos command "c" with more than one source
"dverify ",     "dclear ",      "sprsav ",      "collision ",
"begin ",       "bend ",        "window ",      "boot ",
"width ",       "sprdef ",      "quit ",        "stash ",
# quit is an "unimplemented command"
# stash/fetch/swap are called "dma" in basic 10?
"{0xfe 0x20}",  "fetch ",       "{0xfe 0x22}",  "swap ",
# 0xfe 0x20 and 0xfe 0x22 are not used (0x20 is space, 0x22 is double quote)
"off ",         "fast ",        "slow "
# off is an "unimplemented command"
]):
            #tokens[string] = index + first_token
            tokens[index + first_token] = string
        return tokens


class Program(object):
    """Class to represent a program"""

    def __init__(self, fd):
        """Read program from file."""
        self.body = fd.read(65536)
        assert len(self.body) >= 4, "File is too short to be a valid basic program."
        assert len(self.body) < 65536, "File is too long to be a cbm basic program."
        self.load_address = self.body[0] + 256 * self.body[1]
        # TODO - now scan and put into "lines" list and optional ml postfix

#if __name__ == "__main__":
#    main()
