#!/usr/bin/env python3
import sys

import cbmbasic
#import argparse

VERSION = "18"
LAST_CHANGE = "13 Aug 2021"
RENUMBER = False#True # FIXME, make a CLI option for this!

# here are some token definitions. for the full lookup table see "cbmbasic.py"
# "fused" and "artificial" tokens are used to facilitate renaming to Python/C later on
TOKEN_FOR       = 0x81
TOKEN_NEXT      = 0x82
TOKEN_DATA      = 0x83
TOKEN_INPUTHASH = 0x84
TOKEN_LET       = 0x88
TOKEN_GOTO      = 0x89
TOKEN_RUN       = 0x8a
TOKEN_IF        = 0x8b
TOKEN_RESTORE   = 0x8c
TOKEN_GOSUB     = 0x8d
TOKEN_RETURN    = 0x8e
TOKEN_REM       = 0x8f
TOKEN_ON        = 0x91
TOKEN_DEF       = 0x96
TOKEN_PRINTHASH = 0x98
TOKEN_GET       = 0xa1
TOKEN_GETHASH   = 0xa123    # artificial token
TOKEN_GETKEY    = 0xa1f9    # fused token
TOKEN_TAB_      = 0xa3  # includes '('
TOKEN_TO        = 0xa4
TOKEN_FN        = 0xa5
TOKEN_SPC_      = 0xa6  # includes '('
TOKEN_THEN      = 0xa7
TOKEN_AND       = 0xaf
TOKEN_OR        = 0xb0
# comparisons, may be replaced by artificial/fused tokens:
TOKEN_GREATER   = 0xb1  # will be replaced when used together with '='
TOKEN_EQUALS    = 0xb2  # will be replaced when alone
TOKEN_SMALLER   = 0xb3  # will be replaced when used together with '='
TOKEN_ASSIGNMENT        = 0xb200    # artificial token
TOKEN_EQUALITY          = 0xb2b2    # artificial token ("==" would be a syntax error in basic)
TOKEN_UNEQUAL           = 0xb3b1    # fused
TOKEN_GREATEROREQUAL    = 0xb1b2    # fused
TOKEN_SMALLEROREQUAL    = 0xb3b2    # fused
# a few more of the regular tokens we need:
TOKEN_MID       = 0xca
TOKEN_GO        = 0xcb
TOKEN_GO_TO     = 0xcba4    # fused token
TOKEN_RGR       = 0xcc
TOKEN_INSTR     = 0xd4
TOKEN_ELSE      = 0xd5
TOKEN_RESUME    = 0xd6
TOKEN_RESUMENEXT    = 0xd682    # fused, now "next" cannot influence indentation
TOKEN_TRAP      = 0xd7
TOKEN_DO        = 0xeb
TOKEN_LOOP      = 0xec
TOKEN_EXIT      = 0xed
TOKEN_DIRECTORY = 0xee
TOKEN_BACKUP    = 0xf6
TOKEN_KEY       = 0xf9
TOKEN_CATALOG   = 0xfe0c
TOKEN_DCLEAR    = 0xfe15
TOKEN_COLLISION = 0xfe17
TOKEN_BEGIN     = 0xfe18
TOKEN_BEND      = 0xfe19


# helper dict to find out if block-ending tokens match block starter
block_starter = dict()
block_starter[TOKEN_NEXT] = TOKEN_FOR
block_starter[TOKEN_ELSE] = TOKEN_IF
block_starter[TOKEN_LOOP] = TOKEN_DO

Pass = 1


def out(string):
    if Pass == 1:
        return
    print(string, end="")


def DOS_command_with_awful_syntax(token):
    """Return whether token uses special DOS syntax with marker characters."""
    # basic 3.5 has a consecutive bunch...
    if token >= TOKEN_DIRECTORY and token <= TOKEN_BACKUP:
        return True # includes COLLECT, COPY, DLOAD, DSAVE, HEADER, RENAME and SCRATCH
    # ...and basic 7 has another.
    if token >= TOKEN_CATALOG and token <= TOKEN_DCLEAR:
        return True # includes APPEND, BLOAD, BSAVE, CONCAT, DOPEN, DCLOSE, DVERIFY and RECORD
    return False


def short(string):
    """Strip the trailing space off a token."""
    if string[-1] == " ":
        return string[:-1]
    return string


def sanitized(string):
    """Replace problematic characters in string."""
    string = string.replace('$', 'S')
    string = string.replace('%', 'X')
    string = string.replace('#', 'H')   # was '{hash}'
    return string


def alphanum(byte):
    """Check if valid character for var name."""
    if byte == None:
        return False
    if byte >= ord("A") and byte <= ord("Z"):
        return True
    if byte >= ord("0") and byte <= ord("9"):
        return True
    return False


class Source(object):
    """This class holds the original basic program"""

    def __init__(self, fd):
        self.program = cbmbasic.Program(fd)
        self.load_address = self.program.load_address
        self.body = self.program.body
        print("File created by debasic version " + VERSION + " (" + LAST_CHANGE + ")")
        print("Load address is 0x%04x (%u).\n" % (self.load_address, self.load_address))

    def reset(self):
        self.read_idx = 1   # invalid! will be incremented on read!

    def at(self, idx):
        """Return byte from index, or None if beyond data."""
        if idx >= len(self.body):
            return None
        return self.body[idx]

    def peek_reset(self):
        """Reset peek pointer to "next" byte."""
        self.peek_idx = self.read_idx

    def next(self):
        """Return "next" byte, advancing read pointer.
           Calling this resets the "peek" pointer."""
        byte = self.at(self.read_idx + 1)
        if byte != None:
            self.read_idx += 1
            self.peek_reset()
        return byte

    def goback(self):
        """Decrement read pointer so the next fetch will re-read the old byte."""
        # this was added so when reading the GOTO of an IFcondGOTO, we can
        # easily process THEN instead and process GOTO in the next iteration.
        self.read_idx -= 1
        self.peek_reset()

    def peek(self):
        """Read bytes without advancing "real" read pointer. Afterward,
           call either peek_reset() or peeked_are_read(), so there won't
           be trouble if some other part of the program tries to peek
           the same location later on."""
        byte = self.at(self.peek_idx + 1)
        if byte != None:
            self.peek_idx += 1
        return byte

    def peeked_are_read(self):
        """Mark peeked bytes as having been read."""
        self.read_idx = self.peek_idx

    def peek_for_nonspace(self):
        """Skip spaces and return what comes after - may be 'None'!"""
        byte = self.peek()
        while byte == ord(" "):
            byte = self.peek()
        return byte

    def address(self):
        """Return memory address of latest byte."""
        return self.load_address + self.read_idx - 2


# state machine for adding spaces:
SPACING_NOSPACE     = 0 # at start of line, around " :"
SPACING_QUOTE       = 2 # '"', space gets added before
SPACING_ADDSPACE    = 3 # after ",;)", space gets added after, but not within
SPACING_TOKEN       = 4 # at tokens, space gets added before
SPACING_NORMAL      = 5 # characters like var names and "("
SPACING_OTHER       = 6 # _after_ tokens


class CoderList(object):
    """The simplest output encoder is the base class for all others."""

    def __init__(self):
        self.indent = ""
        self.indents = 1
        self.indent_change_in_next_line = 0
        self.output_indentation = False
        self.last_indent = 1
        self.blockreasons = dict()

    def spacing(self, transition = None, newstate = None):
        pass

    def change_indent(self, amount, reason):
        self.indents += amount
        if amount > 0:
            self.last_indent = self.indents
        if reason:
            self.blockreasons[self.indents] = reason

    def reason_for_indent(self):
        if self.indents in self.blockreasons:
            return self.blockreasons[self.indents]
        return None

    def change_next_line_indent(self, amount):
        self.indent_change_in_next_line += amount

    def new_line(self, number, refs):
        out(str(number) + "\t")

    def check_indentation(self):
        """Dummy version, should be overwritten by child class."""
        if self.output_indentation:
            self.output_indentation = False # done for now

    def start_statement(self, byte):
        self.output_indentation = True  # start before next output
        self.spacing(newstate = SPACING_NOSPACE)

    def quotes(self, str):
        """Output a double quote."""
        self.check_indentation()
        out(str)

    def quoted(self, str):
        """Output something inside double quotes."""
        self.check_indentation()
        out(str)

    def arraybracket(self, str):
        """Output brackets for array access.
           Used so () can be converted to []."""
        self.check_indentation()
        out(str)

    def raw(self, str):
        """Output something outside of double quotes, but not a token."""
        self.check_indentation()
        out(str)

    def optional(self, str):
        """Output superfluous spaces or colons, or LET."""
        self.check_indentation()
        out(str)

    def token(self, token, string):
        """Output a token."""
        self.check_indentation()
        out(string)

    def autounquote(self):
        """Handle quotes still open at end of line (and it's not a comment)."""
        pass

    def statement_separator(self, str):
        """Handle end of statement. This might get called with ':', '\n'
           or empty string (after 'THEN' and 'ELSE')."""
        # if we have some "end of statement" stuff prepared, print it now!
        out(str)

    def begin_extra(self, address):
        out("\nFound extra bytes after end of program, maybe machine code?\n")
        out(";ACME 0.96.4\n	* = $%04x\n" % address)
        self.extras_in_line = 0

    def extra_byte(self, byte):
        if self.extras_in_line == 0:
            out("	!hex")
        elif self.extras_in_line == 8:
            out("  ")
        out(" %02x" % byte)
        self.extras_in_line += 1
        if self.extras_in_line == 16:
            out("\n")
            self.extras_in_line = 0


class CoderSpacing(CoderList):
    """This class adds some spaces to improve readability."""

    def __init__(self):
        super().__init__()
        self.spacing_state = SPACING_NOSPACE

    def spacing(self, transition = None, newstate = None):
        if transition != None:
            if self.spacing_state != transition:
                if self.spacing_state != SPACING_NOSPACE:
                    out(" ")
                self.spacing_state = transition
        if newstate != None:
            self.spacing_state = newstate


class CoderIndent(CoderSpacing):
    """This class outputs the program as basic, but with indentation."""

    def __init__(self):
        super().__init__()
        self.indent = "\t"

    def new_line(self, number, refs):
        out(str(number))
        self.indents += self.indent_change_in_next_line
        self.indent_change_in_next_line = 0

    def check_indentation(self):
        """Make sure indentation does not underflow."""
        if self.output_indentation:
            if self.indents < 1:
                out("INDENT UNDERFLOW, RESETTING\n")
                self.indents = 1
            out(self.indents * self.indent);
            self.output_indentation = False # done for now
            self.last_indent = self.indents

    def start_statement(self, byte):
        super().start_statement(byte)
        self.print_after_statement = "\n"

    def statement_separator(self, str):
        """Handle end of statement. This might get called with ':', '\n'
           or empty string (after 'THEN' and 'ELSE')."""
        if self.print_after_statement != None:
            out(self.print_after_statement)
            self.print_after_statement = None


class CoderRef(CoderIndent):
    """This class adds reference counts before lines"""

    def new_line(self, number, refs):
        if refs != None:
            out("\n" + str(number))
            sep = " ["
            for ref in refs:
                out(sep + str(refs[ref]) + "*" + ref)
                sep = ", "
            out("]\n")
        self.indents += self.indent_change_in_next_line
        self.indent_change_in_next_line = 0


class CoderStd(CoderRef):
    """This class adds transformations to ==, !=, []."""

    def __init__(self):
        super().__init__()
        self.tokenlookup = {
            TOKEN_ASSIGNMENT:   "=",
            TOKEN_EQUALITY:     "==",
            TOKEN_UNEQUAL:      "!="
        }

    def arraybracket(self, str):
        """Output brackets for array access.
           Used so () can be converted to []."""
        self.check_indentation()
        if str == "(":
            str = "["
        elif str == ")":
            str = "]"
        out(str)

    def optional(self, str):
        """Output superfluous spaces or colons, or LET."""
        pass

    def token(self, token, string):
        """Output a token."""
        if token in self.tokenlookup:
            string = self.tokenlookup[token]
        self.check_indentation()
        out(string)


class CoderPython(CoderStd):
    """This class outputs the program in a Python-like fashion."""

    def __init__(self):
        super().__init__()
        self.indent = "    "
        self.tokenlookup = {
            TOKEN_INPUTHASH:    "input_channel ",
            TOKEN_REM:          "#",
            TOKEN_PRINTHASH:    "print_channel ",
            TOKEN_GETHASH:      "get_channel ",
            TOKEN_THEN:         "",
            TOKEN_ASSIGNMENT:   "=",
            TOKEN_EQUALITY:     "==",
            TOKEN_UNEQUAL:      "!=",
            TOKEN_GO_TO:        "goto",
            TOKEN_ELSE:         "else:",
            TOKEN_RESUMENEXT:   "resume_next",
            TOKEN_BEGIN:        "#begin",
            TOKEN_BEND:         "#bend"
        }

    def start_statement(self, byte):
        self.output_indentation = True  # start before next output
        self.spacing(newstate = SPACING_NOSPACE)
        if byte in [TOKEN_IF, TOKEN_FOR, TOKEN_DO]:
            self.print_after_statement = ":\n"
        else:
            self.print_after_statement = "\n"

    def raw(self, str):
        """Output something outside of double quotes, but not a token."""
        self.check_indentation()
        out(sanitized(str))

    def token(self, token, string):
        """Output a token."""
        if token in self.tokenlookup:
            string = self.tokenlookup[token]
        else:
            string = sanitized(string)
        self.check_indentation()
        out(string)

    def autounquote(self):
        """Handle quotes still open at end of line (and it's not a comment)."""
        out('"')


class CoderC(CoderStd):
    """This class outputs the program in a C-like fashion."""

    def __init__(self):
        super().__init__()
        self.indent = "\t"
        self.tokenlookup = {
            TOKEN_FOR:          "for (",
            TOKEN_INPUTHASH:    "input_channel(",
            TOKEN_IF:           "if (",
            TOKEN_REM:          "//",
            TOKEN_PRINTHASH:    "print_channel(",
            TOKEN_GETHASH:      "get_channel(",
            TOKEN_THEN:         "",
            #TOKEN_NOT:          ?,
            #TOKEN_POWEROF:      ?,
            TOKEN_AND:          "&",
            TOKEN_OR:           "|",
            TOKEN_ASSIGNMENT:   "=",
            TOKEN_EQUALITY:     "==",
            TOKEN_UNEQUAL:      "!=",
            TOKEN_GO_TO:        "goto",
            TOKEN_ELSE:         "else {",
            TOKEN_RESUMENEXT:   "resume_next(",
            TOKEN_DO:           "do (",
            TOKEN_LOOP:         "loop (",
            TOKEN_BEGIN:        "//begin",
            TOKEN_BEND:         "//bend"
        }
        self.keep = [
            "goto", "gosub", "return",
# these are consecutive, so there's a range comparison {
# (THEN does not matter, it gets checked earlier)
#                       "tab(", "to", "fn", "spc(", "then",
#   "not", "step", "+", "-", "*", "/", "^", "and",
#   "or", ">", "=", "<", "sgn", "int", "abs", "usr",
#   "fre", "pos", "sqr", "rnd", "log", "exp", "cos", "sin",
#   "tan", "atn", "peek", "len", "str$", "val", "asc", "chr$",
#   "left$", "right$", "mid$", 
# }
# these are consecutive, so there's a range comparison {
#   "rgr", "rclr", "rlum", "joy", "rdot", "dec", "hex$", "err$",
#   "instr"
# }
            "exit", "using", "until", "while", "{pi}"
        ]

    def start_statement(self, byte):
        self.output_indentation = True  # start before next output
        self.spacing(newstate = SPACING_NOSPACE)
        if byte in [TOKEN_IF, TOKEN_FOR, TOKEN_DO]:
            self.print_after_statement = ") {\n"
        elif byte in [TOKEN_REM, TOKEN_ELSE]:
            self.print_after_statement = "\n"
        elif byte in [TOKEN_GOTO, TOKEN_GO, TOKEN_GOSUB, TOKEN_RETURN, TOKEN_EXIT]:
            self.print_after_statement = ";\n"
        elif byte >= 0x80:
            self.print_after_statement = ");\n"
        else:
            self.print_after_statement = ";\n"

    def raw(self, str):
        """Output something outside of double quotes, but not a token."""
        self.check_indentation()
        out(sanitized(str))

    def check_indentation(self, same_line = False):
        """Make sure indentation does not underflow."""
        if self.output_indentation:
            if self.indents < 1:
                out("INDENT UNDERFLOW, RESETTING\n")
                self.indents = 1
            while self.last_indent - self.indents > 1:
                self.last_indent -= 1
                out((self.last_indent * self.indent) + "}\n")
            if self.last_indent > self.indents:
                self.last_indent -= 1
                out((self.last_indent * self.indent) + "}")
                if same_line:
                    out(" ")
                else:
                    out("\n")
                    out(self.indents * self.indent);
            else:
                out(self.indents * self.indent);
            self.output_indentation = False # done for now
            self.last_indent = self.indents

    def token(self, token, string):
        """Output a token."""
        if token in [TOKEN_BEGIN, TOKEN_BEND]:
            self.print_after_statement = "\n"
        if token in self.tokenlookup:
            string = self.tokenlookup[token]
        elif string in self.keep:
            pass
        elif token >= TOKEN_TAB_ and token <= TOKEN_MID:
            string = sanitized(string)
        elif token >= TOKEN_RGR and token <= TOKEN_INSTR:
            string = sanitized(string)
        elif ((token & 0xff00) == 0xce00):
            # all 0xceXY are functions, so do not add "("
            string = sanitized(string)
        else:
            # FIXME - make sure fused tokens like <= and >= are not processed here!
            string = sanitized(string) + "("
        self.check_indentation(token in [TOKEN_NEXT, TOKEN_ELSE, TOKEN_LOOP])
        out(string)


#def get_arguments():
#   parser = argparse.ArgumentParser(description = "De-tokenize CBM BASIC programs")
#   parser.add_argument("--python", action = "store_true", help = "output Python-like")
#   parser.add_argument("--c", action = "store_true", help = "output C-like")
#   return parser.parse_args()


class Parser(object):
    """Parse a program a byte at a time."""

    def __init__(self, coder, source, refs, renums, basic_version = 7, machine = 128):
        self.coder = coder
        self.source = source
        self.version = basic_version
        basic = cbmbasic.Basic(basic_version, machine)
        self.petscii = basic.get_petscii_table()
        self.tokens = basic.get_token_dict()
        self.process = self.expect_link_low
        self.linerefs = refs
        self.renums = renums
        self.line_number_reason = None  # valid reasons would be TOKEN_GOTO and friends
        self.line_number_string = ""    # string to collect line number digits
        self.found_valid_end = False
        self.next_equals_assigns = False    # to tell "=" and "==" apart

    def parse(self):
        self.source.reset()
        while True:
            byte = self.source.next()
            if byte == None:
                break
            else:
                self.process(byte)

    def start_line_number_mode(self, token):
        """Enable scanning of line numbers after GOTO/GO_TO/GOSUB/THEN/ELSE/RESUME/TRAP/..."""
        self.line_number_reason = token
        self.line_number_string = ""

    def end_line_number_mode(self, nodigit):
        """Process line number ref after illegal character has been found."""
#LIST       ?
#EL         ? forget it. RENUMBER only fixes "EL=value" comparisons, but what about "l1=el:..."?
#RENUMBER   no!
#DELETE     no!
        return_value = self.line_number_string
        # step1: GOTO, GO_TO and GOSUB default to zero if no number given, so add prefix:
        if self.line_number_reason in [TOKEN_GOTO, TOKEN_GO_TO, TOKEN_GOSUB]:
            self.line_number_string = "0" + self.line_number_string
        # step2: THEN/ELSE<number> count as GOTO (but do not default to zero, so keep order of steps)
        if self.line_number_reason in [TOKEN_THEN, TOKEN_ELSE]:
            reftoken = short(self.tokens[TOKEN_GOTO])
        else:
            reftoken = short(self.tokens[self.line_number_reason])

        # if string is not empty, handle it
        if self.line_number_string != "":
            number = int(self.line_number_string)
            if Pass == 1:
                # note ref
                if number not in self.linerefs:
                    self.linerefs[number] = dict()
                linedict = self.linerefs[number]
                if reftoken in linedict:
                    linedict[reftoken] += 1
                else:
                    linedict[reftoken] = 1
            else:
                # renum?
                if number in self.renums:
                    return_value = str(self.renums[number])

        # ON<value>GOTO/GOSUB allows several comma-separated numbers:
        # (this algo accepts "ON<value>GO TO"; basic would not. but we're not here to do syntax checks)
        if self.start_of_statement == TOKEN_ON and nodigit == ord(","):
            self.line_number_string = ""    # keep accepting line numbers
        else:
            self.line_number_reason = None  # stop doing it

        return return_value # either the real or the renumbered one

    def merge_comparisons(self, first_token):
        """Check next non-space. If it is a comparison, try to merge the
           two tokens into one."""
        second_token = self.source.peek_for_nonspace()
        # another comparison?
        if second_token not in [TOKEN_GREATER, TOKEN_EQUALS, TOKEN_SMALLER]:
            self.source.peek_reset()
            return first_token  # no, so keep what we have
        # check combo, allowed are <>, ><, >=, =>, <=, =<
        if (first_token, second_token) in [(TOKEN_SMALLER, TOKEN_GREATER), (TOKEN_GREATER, TOKEN_SMALLER)]:
            self.source.peeked_are_read()
            return TOKEN_UNEQUAL
        if (first_token, second_token) in [(TOKEN_GREATER, TOKEN_EQUALS), (TOKEN_EQUALS, TOKEN_GREATER)]:
            self.source.peeked_are_read()
            return TOKEN_GREATEROREQUAL
        if (first_token, second_token) in [(TOKEN_SMALLER, TOKEN_EQUALS), (TOKEN_EQUALS, TOKEN_SMALLER)]:
            self.source.peeked_are_read()
            return TOKEN_SMALLEROREQUAL
        # all other combinations are mistakes, so return what we had, mistake will be in output just like in input
        self.source.peek_reset()
        return first_token

    def statement_sep(self, sep):
        """Do end-of-statement checks because of separator."""
        # unmatched parentheses?
        if self.statement_parentheses[-1] != None:
            out("ERROR, statement has non-closed parenthesis.\n")
        self.coder.statement_separator(sep)

    def process_token(self, token):
        """Process token."""
        # try to merge stuff like "> =" into a single (artificial) token
        if token in [TOKEN_GREATER, TOKEN_EQUALS, TOKEN_SMALLER]:
            token = self.merge_comparisons(token)
            # tell assignment/comparison apart
            if token == TOKEN_EQUALS:
                if self.next_equals_assigns:
                    self.next_equals_assigns = False
                    token = TOKEN_ASSIGNMENT
                else:
                    token = TOKEN_EQUALITY
        elif token == TOKEN_GET:
            next = self.source.peek_for_nonspace()
            if next == ord("#"):
                self.source.peeked_are_read()
                token = TOKEN_GETHASH
            elif next == TOKEN_KEY:
                self.source.peeked_are_read()
                token = TOKEN_GETKEY
            else:
                self.source.peek_reset()
        elif token == TOKEN_RESUME:
            if self.source.peek_for_nonspace() == TOKEN_NEXT:
                self.source.peeked_are_read()
                token = TOKEN_RESUMENEXT
            else:
                self.source.peek_reset()
        elif token == TOKEN_GO:
            if self.source.peek_for_nonspace() == TOKEN_TO:
                self.source.peeked_are_read()
                token = TOKEN_GO_TO
            else:
                self.source.peek_reset()
        elif token == TOKEN_GOTO:
            # convert IFcondGOTO to IFcondTHENGOTO
            if self.start_of_statement == TOKEN_IF:
                self.source.goback()    # make sure the GOTO will be read again
                token = TOKEN_THEN  # and pretend we have found a THEN instead
        # check if we need to fetch a second byte:
        if self.version == 7:
            if token == 0xce or token == 0xfe:
                second_byte = self.source.peek()
                if second_byte != None and second_byte != 0:
                    # FIXME - might be an extension, so check allowed range!
                    self.source.peeked_are_read()
                    tok2 = (token << 8) + second_byte
                    if self.start_of_statement == token:
                        self.start_of_statement = tok2
                    token = tok2    # from here on, use extended token
                else:
                    self.source.peek_reset()
                    print("Found token 0x%02x 0x00, this should not happen.\n" % token, file=sys.stderr)
        # do we need to care about that awful special syntax for DOS commands?
        self.DOS_flag = DOS_command_with_awful_syntax(token)
        # do we need to start collecting line number digits? (for THEN/ELSE, see end of method)
        if token in [TOKEN_GOTO, TOKEN_GO_TO, TOKEN_GOSUB, TOKEN_RUN, TOKEN_RESTORE, TOKEN_RESUME, TOKEN_TRAP]:
            self.start_line_number_mode(token)
        # checking for block-ending tokens must be done before output
        if token in [TOKEN_NEXT, TOKEN_ELSE, TOKEN_LOOP]:
            if self.coder.reason_for_indent() == block_starter[token]:
                self.coder.change_indent(-1, None)
            else:
                out("CAUTION, the next statement would end a block, but not the one that is open.\n")
        elif token == TOKEN_BEND:
            self.coder.change_next_line_indent(-1)
        # check spacing around token
        string = self.tokens[token]
        if string[-1] == " ":
            self.coder.spacing(transition = SPACING_TOKEN, newstate = SPACING_OTHER)
        else:
            self.coder.spacing(transition = SPACING_TOKEN, newstate = SPACING_NOSPACE)
        # actual output
        self.coder.token(token, short(string))
        # after output, check for block-starting tokens
        if token in [TOKEN_FOR, TOKEN_IF, TOKEN_ELSE, TOKEN_DO]:
            self.coder.change_indent(1, token)
        if token == TOKEN_IF:
            self.coder.change_next_line_indent(-1)
        elif token == TOKEN_BEGIN:
            self.coder.change_next_line_indent(1)   # compensate for IF
        # after THEN and ELSE, start new statements
        if token in [TOKEN_THEN, TOKEN_ELSE]:
            self.statement_sep("")
            self.process = self.start_statement
            # if next byte is a digit, inject a GOTO
            next = self.source.peek_for_nonspace()
            self.source.peek_reset()
            if next != None:
                if next >= ord("0") and next <= ord("9"):
                    self.process(TOKEN_GOTO)

    def found_end_of_line(self):
        self.process = self.expect_link_low

# state machine:

    def expect_link_low(self, link_low):
        """Process start of line, which is low byte of line link."""
        link_high = self.source.next()
        if link_high == None:
            return  # give up
        #link_ptr = link_low + 256 * link_high
        if link_high == 0:
            self.found_valid_end = True
            self.process = self.begin_extra_bytes
            return
        # read line number
        line_low = self.source.next()
        if line_low == None:
            return  # give up
        line_high = self.source.next()
        if line_high == None:
            return  # give up
        line_number = line_low + 256 * line_high
        # check refs for line number
        if line_number in self.linerefs:
            refs = self.linerefs[line_number]
        else:
            refs = None
        # renum?
        if line_number in self.renums:
            line_number = self.renums[line_number]
        self.coder.new_line(line_number, refs)
        self.quoted = False
        self.process = self.start_statement

    def start_statement(self, byte):
        """Process start of statement. Stuff like spaces, colons and LET will get ignored here right away."""
        if byte in [ord(" "), ord(":")]:
            self.coder.optional(self.petscii[byte])
        elif byte == TOKEN_LET:
            self.coder.optional(short(self.tokens[byte]))
            # no change to self.process, so next byte counts as start of statement
        else:
            self.next_equals_assigns = (byte in [TOKEN_FOR, TOKEN_DEF] or (byte >= ord("A") and byte <= ord("Z")))
            self.start_of_statement = byte
            self.MIDS_assigns = byte == TOKEN_MID   # we cannot use "start_of_statement" for this, because this flag gets cleared upon use
            self.coder.start_statement(byte)
            self.statement_last_before_paren = None # for ()-to-[]
            self.statement_parentheses = [None] # for ()-to-[]
            self.FN_flag = False    # for ()-to-[]
            self.DOS_flag = False   # for ()-to-[]
            self.process = self.process_byte
            self.process(byte)

    def process_byte(self, byte):
        # are we in line number mode? then check/collect digit
        if self.line_number_reason != None:
            if byte >= ord("0") and byte <= ord("9"):
                self.line_number_string = self.line_number_string + chr(byte)
                self.coder.spacing(transition = SPACING_NORMAL)
                self.statement_last_before_paren = byte
                return  # do not output line number until complete
            else:
                if byte != ord(" "):
                    out = self.end_line_number_mode(byte)
                    self.coder.raw(out) # output either real or renum'd one
        # first check for NUL...
        if byte == 0:
            # quotes still open at end of line? close, except in comments:
            if self.quoted and self.start_of_statement != TOKEN_REM:
                self.coder.autounquote()
            self.statement_sep("\n")
            self.found_end_of_line()    # ...because it tops everything else
        elif byte == ord('"'):
            # then check for double quotes, because string contents are handled differently
            if self.quoted:
                self.coder.spacing(newstate = SPACING_OTHER)    # closing
            else:
                self.coder.spacing(transition = SPACING_QUOTE)  # opening
            self.coder.quotes(self.petscii[byte])
            self.quoted = not self.quoted
            self.statement_last_before_paren = None
        else:
            # handling of all other bytes depends on context:
            if self.quoted:
                # get string representation
                string = self.petscii[byte]
                # check if we can compress control codes
                if string == " " or string.startswith("{"):
                    length = 1
                    while self.source.peek() == byte:
                        self.source.peeked_are_read()
                        length += 1
                    self.source.peek_reset()
                    if length > 1:
                        if string == " ":
                            if length > 4:
                                string = "{" + str(length) + " space}"
                            else:
                                string = length * " "
                        else:
                            string = "{" + str(length) + " " + string[1:]
                # output string representation
                self.coder.quoted(string)
            else:
                # outside of quotes
                if byte == ord(":") and self.start_of_statement != TOKEN_REM:
                    self.statement_sep(":")
                    self.process = self.start_statement
                else:
                    if byte < 0x80:
                        # spacing
                        if byte == ord(" "):
                            self.coder.spacing(newstate = SPACING_NOSPACE)
                        elif byte == ord("("):
                            self.coder.spacing(transition = SPACING_NORMAL, newstate = SPACING_NOSPACE)
                        elif chr(byte) in ",;)":
                            self.coder.spacing(newstate = SPACING_ADDSPACE)
                        else:
                            self.coder.spacing(transition = SPACING_NORMAL)
                        # special check for second arg of COLLISION
                        if byte == ord(",") and self.start_of_statement == TOKEN_COLLISION:
                            self.start_line_number_mode(TOKEN_COLLISION)
                        # ()-to-[] and MID$(...) as assignment:
                        if byte == ord("("):
                            # does this '(' open an array access?
                            if alphanum(self.statement_last_before_paren) and not self.FN_flag and (self.DOS_flag == False or self.statement_parentheses[-1]):
                                self.statement_parentheses.append("[")  # remember for when closing
                                self.coder.arraybracket("(")
                            else:
                                self.statement_parentheses.append("(")  # remember for when closing
                                self.coder.raw(self.petscii[byte])
                            self.FN_flag = False
                        elif byte == ord(")"):
                            # does this ')' close an array access?
                            if self.statement_parentheses[-1] == "[":
                                self.statement_parentheses.pop()
                                self.coder.arraybracket(")")
                            elif self.statement_parentheses[-1] == "(":
                                self.statement_parentheses.pop()
                                self.coder.raw(self.petscii[byte])
                                # extra check for "MID$(...)="
                                if self.MIDS_assigns:
                                    self.MIDS_assigns = False
                                    self.next_equals_assigns = True # the next non-space should be the correct '='!
                            else:
                                self.coder.raw(self.petscii[byte])
                                out("ERROR, statement closes non-opened parenthesis.\n")
                        else:
                            self.coder.raw(self.petscii[byte])
                        # ignore spaces, '%' and '$' when remembering char before '(':
                        if chr(byte) not in " %$":
                            self.statement_last_before_paren = byte
                    else:
                        # token
                        self.FN_flag = byte == TOKEN_FN # needed
                        if byte in [TOKEN_TAB_, TOKEN_SPC_]:
                            self.statement_parentheses.append("(")
                        self.process_token(byte)
                        self.statement_last_before_paren = None

    def begin_extra_bytes(self, byte):
        self.coder.begin_extra(self.source.address())
        self.process = self.handle_extra_byte
        self.process(byte)

    def handle_extra_byte(self, byte):
        self.coder.extra_byte(byte)


def process_file(mode, inputfile):
    global  Pass

    if mode == '--list':
        coderclass = CoderList
    elif mode == '--spaced':
        coderclass = CoderSpacing
    elif mode == '--indent':
        coderclass = CoderIndent
    elif mode == '--ref':
        coderclass = CoderRef
    elif mode == '--std':
        coderclass = CoderStd
    elif mode == '--python':
        coderclass = CoderPython
    elif mode == '--c':
        coderclass = CoderC
    else:
        print("Error: Mode not recognized.", file = sys.stderr)
        show_help_and_die()
    with open(inputfile, "rb") as fd:
        source = Source(fd)
        fd.close()

    # TODO - insert machine type check:
    # if it's "auto", try to decide basic version by source.load_address:
    #   0x0801  C64
    #   0x1001  VIC20/C16/C116/+4
    #   0x1201  expanded VIC20
    #   0x1c01  C128
    #   0x4001  +4/C128 with graphics
    version = 7 # for now, assume Basic 7.0
    machine = 128   # for now, assume C-128
    refs = dict()
    renums = dict()
    for Pass in range(1, 3):
        coder = coderclass()
        parser = Parser(coder, source, refs, renums, version, machine)
        parser.parse()
        # renum?
        if refs and RENUMBER:
            refdnums = [num for num in refs]
            refdnums.sort()
            new = 0
            for num in refdnums:
                renums[num] = new
                new += 1
    print("")
    # check "program did not end early" flag and tell user!
    if not parser.found_valid_end:
        print("ERROR, basic program does not have a valid end marker.\n")


def die(msg):
    print(msg, file = sys.stderr)
    sys.exit(1)


def show_help_and_die():
    print(
"Syntax:\n" +
sys.argv[0], "[MODE] INPUTFILE\n"
"MODE must be one of --list, --spaced, --indent, --ref, --std, --python or --c.\n"
"FIXME - show help\n",
        file = sys.stderr)
    sys.exit(1)


def main():
    if len(sys.argv) == 2:
        process_file("--std", sys.argv[1])
    elif len(sys.argv) == 3:
        process_file(sys.argv[1], sys.argv[2])
    else:
        print("Error: Wrong number of arguments", file = sys.stderr)
        show_help_and_die()


if __name__ == '__main__':
    main()
