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

//#define BOOST_SPIRIT_DEBUG

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

template <typename Iterator>
struct OCamlGrammar : qi::grammar<Iterator>
{
    template <typename Lexer>
    OCamlGrammar(Lexer const &tok)
        : OCamlGrammar::base_type(start)
    {
        start = qi::eps;
        
        //
        // Lexical
        //
        namespace qi = boost::spirit::qi;
        
        capitalized_ident %= 
            qi::eps >> tok.capitalized_ident
            ;
        
        lowercase_ident %= 
            qi::eps >> tok.lowercase_ident
            ;
        
        ident %= 
            qi::eps 
            >> (tok.lowercase_ident | tok.capitalized_ident)
            ;
            
        label_name %= 
            qi::eps >> tok.lowercase_ident
            ;
            
        label %= 
            qi::eps >> tok.label
            ;
            
        optlabel %= 
            qi::eps >> tok.optlabel
            ;
            
        integer_literal %= 
            qi::eps >> tok.integer_literal
            ;
            
        float_literal %= 
            qi::eps >> tok.float_literal
            ;
            
        char_literal %= 
            qi::eps >> tok.char_literal
            ;
            
        string_literal %= 
            qi::eps >> tok.string_literal
            ;

        BOOST_SPIRIT_DEBUG_NODE(capitalized_ident);
        BOOST_SPIRIT_DEBUG_NODE(lowercase_ident);
        BOOST_SPIRIT_DEBUG_NODE(ident);
        BOOST_SPIRIT_DEBUG_NODE(label_name);
        BOOST_SPIRIT_DEBUG_NODE(label);
        BOOST_SPIRIT_DEBUG_NODE(optlabel);
        BOOST_SPIRIT_DEBUG_NODE(integer_literal);
        BOOST_SPIRIT_DEBUG_NODE(float_literal);
        BOOST_SPIRIT_DEBUG_NODE(char_literal);
        BOOST_SPIRIT_DEBUG_NODE(string_literal);
    }

    qi::rule<Iterator> start;

    //
    // Lexical
    //

    qi::rule<Iterator, ocaml::ast::capitalized_ident> capitalized_ident;
    qi::rule<Iterator, ocaml::ast::lowercase_ident> lowercase_ident;
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

