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

#ifndef FLANG_OCAMLGRAMMAR_H
#define FLANG_OCAMLGRAMMAR_H

#include "ocamlast.h"
#include "ocamlids.h"
#include "ocamllexer.h"
#include <boost/spirit/include/qi.hpp>

namespace ocaml
{
namespace parser
{

using namespace boost::phoenix;
using namespace boost::spirit;

template <typename Iterator, typename Lexer>
struct OCamlGrammar : qi::grammar<Iterator>
{
    OCamlGrammar(Lexer const &l)
        : OCamlGrammar::base_type(start)
    {
        //
        // Lexical
        //

        lowercase_ident %= eps >> l.lowercase_ident;
        capitalized_ident %= eps >> l.capitalized_ident;
        ident %= eps >> l.ident;
        lowercase_ident %= eps >> l.lowercase_ident;
        label_name %= eps >> l.label_name;
        label %= eps >> l.label;
        optlabel %= eps >> l.optlabel;
        integer_literal %= eps >> l.integer_literal;
        float_literal %= eps >> l.float_literal;
        char_literal %= eps >> l.char_literal;
        string_literal %= eps >> l.string_literal;

        start = eps;
    }

    qi::rule<Iterator> start;

    //
    // Lexical
    //

    qi::rule<Iterator, ocaml::ast::lowercase_ident> lowercase_ident;
    qi::rule<Iterator, ocaml::ast::capitalized_ident> capitalized_ident;
    qi::rule<Iterator, ocaml::ast::ident> ident;
    qi::rule<Iterator, ocaml::ast::label_name> label_name;
    qi::rule<Iterator, ocaml::ast::label> label;
    qi::rule<Iterator, ocaml::ast::optlabel> optlabel;
    qi::rule<Iterator, ocaml::ast::integer_literal> integer_literal;
    qi::rule<Iterator, ocaml::ast::float_literal> float_literal;
    qi::rule<Iterator, ocaml::ast::char_literal> char_literal;
    qi::rule<Iterator, ocaml::ast::string_literal> string_literal;

};

} // namespace grammar
} // namespace ocaml

#endif //FLANG_OCAMLGRAMMAR_H
