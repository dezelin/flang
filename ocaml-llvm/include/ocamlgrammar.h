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
#include <boost/spirit/include/phoenix.hpp>

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
        namespace lexer = ocaml::lexer;
        namespace qi = boost::spirit::qi;

        start = qi::eps;

        //
        // Lexical
        //

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

        //
        // Names
        //

        infix_symbol %=
            qi::eps
            >> (tok.infix_symbol
                // Include these reserved sequences
                | tok.dollardollar
                | tok.lesserminus
                | tok.lessercolon
                | tok.dollarcolon
                | tok.lessless
                | tok.greatgreat
                | tok.minusgreater)
            ;

        operation %=
            qi::eps
            >> (qi::tokenid(lexer::Tokens::Asterisk)
                | qi::tokenid(lexer::Tokens::Plus)
                | qi::tokenid(lexer::Tokens::Minus)
                | qi::tokenid(lexer::Tokens::MinusDot)
                | qi::tokenid(lexer::Tokens::Equal)
                | qi::tokenid(lexer::Tokens::BangEqual)
                | qi::tokenid(lexer::Tokens::Lesser)
                | qi::tokenid(lexer::Tokens::Greater)
                | qi::tokenid(lexer::Tokens::Or)
                | qi::tokenid(lexer::Tokens::BarBar)
                | qi::tokenid(lexer::Tokens::Ampersand)
                | qi::tokenid(lexer::Tokens::AmpAmp)
                | qi::tokenid(lexer::Tokens::ColonEqual)
                | qi::tokenid(lexer::Tokens::Mod)
                | qi::tokenid(lexer::Tokens::Land)
                | qi::tokenid(lexer::Tokens::Lor)
                | qi::tokenid(lexer::Tokens::Lxor)
                | qi::tokenid(lexer::Tokens::Lsl)
                | qi::tokenid(lexer::Tokens::Lsr)
                | qi::tokenid(lexer::Tokens::Asr))
            ;

        infix_op %=
            operation | infix_symbol
            ;

        prefix_symbol %=
            qi::eps
            >> (tok.prefix_symbol
                // Include these reserved sequences
                | tok.bangequal
                | tok.questquest)
            ;

        operator_name %=
            prefix_symbol | infix_op
            ;

        value_name %=
            lowercase_ident
            | (qi::omit[tok.lbrace] >> operator_name >> qi::omit[tok.rbrace])
            ;

        constr_name %=
            capitalized_ident
            ;

        tag_name %=
            capitalized_ident
            ;

        typeconstr_name %=
            lowercase_ident
            ;

        field_name %=
            lowercase_ident
            ;

        module_name %=
            capitalized_ident
            ;

        modtype_name %=
            ident
            ;

        class_name %=
            lowercase_ident
            ;

        inst_var_name %=
            lowercase_ident
            ;

        method_name %=
            lowercase_ident
            ;

        value_path %=
            -(module_path >> qi::omit[tok.dot]) >> value_name
            ;

        constr =
            //
            // This is a BNF notation for the rule:
            //
            //  (module_path >> qi::omit[tok.dot]) >> constr_name
            //
            // Boost Spirit rules are greedy so module_path subrule eats all
            // capitalized_ident tokens including the last one for the
            // constr_name subrule.
            //
            (module_name >> *(qi::omit[tok.dot] >> module_name)) [
                    // Construct ast::constr from module_name tokens
                    _val = construct<ast::constr>(_1, _2)
                ]
            ;

        typeconstr %=
            -(extended_module_path >> qi::omit[tok.dot]) >> typeconstr_name
            ;

        field %=
            -(module_path >> qi::omit[tok.dot]) >> field_name
            ;

        modtype_path %=
            -(extended_module_path >> qi::omit[tok.dot]) >> modtype_name
            ;

        class_path %=
            -(module_path >> qi::omit[tok.dot]) >> class_name
            ;

        classtype_path %=
            -(extended_module_path >> qi::omit[tok.dot]) >> class_name
            ;

        module_path %=
            module_name >> *(qi::omit[tok.dot] >> module_name)
            ;

        extended_module_path %=
            extended_module_name >> *(qi::omit[tok.dot] >> extended_module_name)
            ;

        extended_module_name %=
            module_name
            >> *(qi::omit[tok.lbrace] >> extended_module_path >> qi::omit[tok.rbrace])
            ;

        BOOST_SPIRIT_DEBUG_NODE(infix_symbol);
        BOOST_SPIRIT_DEBUG_NODE(operation);
        BOOST_SPIRIT_DEBUG_NODE(infix_op);
        BOOST_SPIRIT_DEBUG_NODE(prefix_symbol);
        BOOST_SPIRIT_DEBUG_NODE(operator_name);
        BOOST_SPIRIT_DEBUG_NODE(value_name);
        BOOST_SPIRIT_DEBUG_NODE(constr_name);
        BOOST_SPIRIT_DEBUG_NODE(tag_name);
        BOOST_SPIRIT_DEBUG_NODE(typeconstr_name);
        BOOST_SPIRIT_DEBUG_NODE(field_name);
        BOOST_SPIRIT_DEBUG_NODE(module_name);
        BOOST_SPIRIT_DEBUG_NODE(modtype_name);
        BOOST_SPIRIT_DEBUG_NODE(class_name);
        BOOST_SPIRIT_DEBUG_NODE(inst_var_name);
        BOOST_SPIRIT_DEBUG_NODE(method_name);

        BOOST_SPIRIT_DEBUG_NODE(value_path);
        BOOST_SPIRIT_DEBUG_NODE(constr);
        BOOST_SPIRIT_DEBUG_NODE(typeconstr);
        BOOST_SPIRIT_DEBUG_NODE(field);
        BOOST_SPIRIT_DEBUG_NODE(modtype_path);
        BOOST_SPIRIT_DEBUG_NODE(class_path);
        BOOST_SPIRIT_DEBUG_NODE(classtype_path);
        BOOST_SPIRIT_DEBUG_NODE(module_path);
        BOOST_SPIRIT_DEBUG_NODE(extended_module_path);
        BOOST_SPIRIT_DEBUG_NODE(extended_module_name);
    }

    qi::rule<Iterator> start;

    //
    // Lexical
    //

    qi::rule<Iterator, ocaml::ast::capitalized_ident()> capitalized_ident;
    qi::rule<Iterator, ocaml::ast::lowercase_ident()> lowercase_ident;
    qi::rule<Iterator, ocaml::ast::ident()> ident;
    qi::rule<Iterator, ocaml::ast::label_name()> label_name;
    qi::rule<Iterator, ocaml::ast::label()> label;
    qi::rule<Iterator, ocaml::ast::optlabel()> optlabel;
    qi::rule<Iterator, ocaml::ast::integer_literal()> integer_literal;
    qi::rule<Iterator, ocaml::ast::float_literal()> float_literal;
    qi::rule<Iterator, ocaml::ast::char_literal()> char_literal;
    qi::rule<Iterator, ocaml::ast::string_literal()> string_literal;

    //
    // Names
    //

    qi::rule<Iterator, ocaml::ast::infix_symbol()> infix_symbol;
    qi::rule<Iterator, ocaml::ast::operation()> operation;
    qi::rule<Iterator, ocaml::ast::infix_op()> infix_op;
    qi::rule<Iterator, ocaml::ast::prefix_symbol()> prefix_symbol;
    qi::rule<Iterator, ocaml::ast::operator_name()> operator_name;
    qi::rule<Iterator, ocaml::ast::value_name()> value_name;
    qi::rule<Iterator, ocaml::ast::constr_name()> constr_name;
    qi::rule<Iterator, ocaml::ast::tag_name()> tag_name;
    qi::rule<Iterator, ocaml::ast::typeconstr_name()> typeconstr_name;
    qi::rule<Iterator, ocaml::ast::field_name()> field_name;
    qi::rule<Iterator, ocaml::ast::module_name()> module_name;
    qi::rule<Iterator, ocaml::ast::modtype_name()> modtype_name;
    qi::rule<Iterator, ocaml::ast::class_name()> class_name;
    qi::rule<Iterator, ocaml::ast::inst_var_name()> inst_var_name;
    qi::rule<Iterator, ocaml::ast::method_name()> method_name;

    qi::rule<Iterator, ocaml::ast::value_path()> value_path;
    qi::rule<Iterator, ocaml::ast::constr()> constr;
    qi::rule<Iterator, ocaml::ast::typeconstr()> typeconstr;
    qi::rule<Iterator, ocaml::ast::field()> field;
    qi::rule<Iterator, ocaml::ast::modtype_path()> modtype_path;
    qi::rule<Iterator, ocaml::ast::class_path()> class_path;
    qi::rule<Iterator, ocaml::ast::classtype_path()> classtype_path;
    qi::rule<Iterator, ocaml::ast::module_path()> module_path;
    qi::rule<Iterator, ocaml::ast::extended_module_path()> extended_module_path;
    qi::rule<Iterator, ocaml::ast::extended_module_name()> extended_module_name;
};

} // namespace grammar
} // namespace ocaml

#endif //FLANG_OCAMLGRAMMAR_H

