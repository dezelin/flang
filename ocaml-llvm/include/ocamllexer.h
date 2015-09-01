//
//  Copyright (c) 2015, Aleksandar Dezelin
//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are met:
//
//  1. Redistributions of source code must retain the above copyright notice, this
//     list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
//  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
//  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
//  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#ifndef FLANG_OCAMLLEXER_H
#define FLANG_OCAMLLEXER_H

// #define BOOST_SPIRIT_LEXERTL_DEBUG

#include <boost/spirit/include/lex_lexertl.hpp>


namespace ocaml
{
namespace lexer
{

using namespace boost::spirit;

enum class TokenIds
{
    Any = lex::min_token_id + 1
};

template<typename Lexer>
struct OCamlLexer : lex::lexer<Lexer> {
    OCamlLexer() : OCamlLexer::base_type() {

        //
        // See: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html
        //

        lowercase_letter = "[a-z]";
        uppercase_letter = "[A-Z]";
        letter = "(" + lowercase_letter + "|" + uppercase_letter + ")";
        ident = "(" + letter + "|[_])(" + letter + "|[0-9]|[_]|['])*";
        capitalized_ident = uppercase_letter + "(" + letter + "|[0-9]|[_]|['])*";
        lowercase_ident = lowercase_letter + "(" + letter + "|[0-9]|[_]|['])*";

        integer_literal =
            "[-]?(0b|0B)([0-1])([0-1]|[_])*"                            // binary
            "|[-]?(0o|0O)([0-7])([0-7]|[_])*"                           // octal
            "|[-]?(0x|0X)([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]|[_])*"   // hexadecimal
            "|[-]?[0-9]([0-9]|[_])*";                                   // decimal

        float_literal = "[-]?[0-9]([0-9]|[_])*([.]([0-9]|[_])*)?([eE][+-][0-9]([0-9]|[_])*)?";

        escape_sequence =
            "\\([\\]|[']|[n]|[t]|[b]|[r]|[ ])"
            "|\\[0-9][0-9][0-9]"
            "|\\[x]([0-9]|[A-F]∣[a-f])([0-9]|[A-F][a-f])";

        char_literal = "['].[']|[']" + escape_sequence + "[']";

        string_character = ".|" + escape_sequence + "|([\\n]|[\\r\\n])([ ]|[\\t])*";

        string_literal = "";

    }

    lex::token_def<> letter, lowercase_letter, uppercase_letter, ident, capitalized_ident, lowercase_ident,
        integer_literal, float_literal, escape_sequence, char_literal, string_character, string_literal;
};

} // namespace lexer
} // namespace ocaml

#endif //FLANG_OCAMLLEXER_H
