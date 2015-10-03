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
            qi::as_string[tok.capitalized_ident]
            ;

        lowercase_ident %=
            qi::as_string[tok.lowercase_ident]
            ;

        ident %=
            qi::as_string[tok.lowercase_ident]
            | qi::as_string[tok.capitalized_ident]
            ;

        label_name %=
            qi::as_string[tok.lowercase_ident]
            ;

        label %=
            qi::as_string[tok.label]
            ;

        optlabel %=
            qi::as_string[tok.optlabel]
            ;

        integer_literal %=
            tok.integer_literal >> qi::eps
            ;

        float_literal %=
            tok.float_literal >> qi::eps
            ;

        char_literal %=
            tok.char_literal >> qi::eps
            ;

        string_literal %=
            qi::as_string[tok.string_literal]
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
            qi::as_string[
                (tok.infix_symbol
                // Include these reserved sequences
                | tok.dollardollar
                | tok.lesserminus
                | tok.lessercolon
                | tok.dollarcolon
                | tok.lessless
                | tok.greatgreat
                | tok.minusgreater)]
            ;

        operation %=
            (qi::tokenid(lexer::Tokens::Asterisk)
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
            | qi::tokenid(lexer::Tokens::Asr)) >> qi::eps
            ;

        infix_op %=
            operation | infix_symbol
            ;

        prefix_symbol %=
            qi::as_string[
                (tok.prefix_symbol
                // Include these reserved sequences
                | tok.bangequal
                | tok.questquest)]
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
            -module_path_wl >> constr_name
            ;

        typeconstr %=
            -(extended_module_path >> qi::omit[tok.dot]) >> typeconstr_name
            ;

        field %=
            -(module_path >> qi::omit[tok.dot]) >> field_name
            ;

        modtype_path %=
            -extended_module_path_wl >> modtype_name
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

        // Module path but without the last module name
        // for rules that have capitalized_ident as the last subrule.
        module_path_wl %=
            module_name >> qi::omit[tok.dot] >>
                (
                    *(qi::hold[module_name >> qi::omit[tok.dot]])
                )
            ;

        extended_module_path %=
            extended_module_name >> *(qi::omit[tok.dot] >> extended_module_name)
            ;

        // Extended module path but without the last module name
        // for rules that have capitalized_ident as the last subrule.
        extended_module_path_wl %=
            extended_module_name >> qi::omit[tok.dot] >>
                (
                    *(qi::hold[extended_module_name >> qi::omit[tok.dot]])
                )
            ;

        extended_module_name %=
            module_name
            >> *(qi::omit[tok.lbrace] >> extended_module_path >> qi::omit[tok.rbrace])
            ;

        //
        // Type expressions
        //

        typexpr %=
            function_types
            | tuple_types
            | ident_type_variable
            | anon_type_variable
            | parenthesized_types
            | constructed_no_param
            ;
            /*
            | typexpr >> +(qi::omit[tok.asterisk] >> typexpr)
            | typeconstr
            | typexpr >> typeconstr
            | qi::omit[tok.lbrace] >> typexpr >> *(qi::omit[tok.comma] >> typexpr)
                >> qi::omit[tok.rbrace] >> typeconstr
            | typexpr >> qi::omit[tok.kas] >> qi::omit[tok.apostrophe] >> ident
            | polymorphic_variant_type
            | qi::omit[tok.lesser] >> -qi::omit[tok.dotdot] >> qi::omit[tok.greater]
            | qi::omit[tok.lesser] >>
                method_type
                >> *((qi::omit[tok.semicolon] | qi::omit[tok.semicolon] >> qi::omit[tok.dotdot]) >> method_type)
              >> qi::omit[tok.greater]
            | qi::omit[tok.hash] >> class_path
            | typexpr >> qi::omit[tok.hash] >> class_path
            | qi::omit[tok.lbrace]
                >> typexpr >> *(qi::omit[tok.comma] >> typexpr)
              >> qi::omit[tok.rbrace]
              >> qi::omit[tok.hash]
              >> class_path
            ;

        poly_typexpr %=
            typexpr
            | +(qi::omit[tok.apostrophe] >> ident) >> qi::omit[tok.dot] >> typexpr
            ;

        method_type %=
            method_name >> qi::omit[tok.colon] >> poly_typexpr
            ;

        polymorphic_variant_type %=
            qi::omit[tok.lbracket]
                >> tag_spec_first
                >> *(qi::omit[tok.bar] >> tag_spec)
                >> qi::omit[tok.rbracket]
            | qi::omit[tok.lbracketgreater]
              >> -tag_spec
              >> *(qi::omit[tok.bar] >> tag_spec)
              >> qi::omit[tok.rbracket]
            | qi::omit[tok.lbracketlesser]
              >> -qi::omit[tok.bar]
              >> tag_spec_full
              >> *(qi::omit[tok.bar] >> tag_spec_full)
              >> -(qi::omit[tok.greater] >> +(qi::omit[tok.graveaccent] >> tag_name))
              >> qi::omit[tok.rbracket]
            ;

        tag_spec_first %=
            qi::omit[tok.graveaccent] >> tag_name >> -(qi::omit[tok.kof] >> typexpr)
            | -typexpr >> qi::omit[tok.bar] >> tag_spec
            ;

        tag_spec %=
            qi::omit[tok.graveaccent] >> tag_name >> -(qi::omit[tok.kof] >> typexpr)
            | typexpr
            ;

        tag_spec_full %=
            qi::omit[tok.graveaccent] >> tag_name
                >> -(qi::omit[tok.kof] >> -qi::omit[tok.ampersand] >> typexpr
                    >> *(qi::omit[tok.ampersand] >> typexpr))
            | typexpr
            ;
*/
        ident_type_variable =
            // FIXME: A bug in Boost Spirit?
            // This gives a compilation error:
            //  qi::omit[tok.apostrophe] >> ident
            (tok.apostrophe >> ident)
            [_val = construct<ocaml::ast::ident>(_2)]
            ;

        anon_type_variable %=
            qi::tokenid(lexer::Tokens::Underscore) >> qi::eps
            ;

        parenthesized_types %=
            qi::omit[tok.lbrace] >> typexpr >> qi::omit[tok.rbrace]
            ;

        function_types %=
            -((label_name | optlabel) >> qi::omit[tok.colon])
                >> function_types_typexpr
                >> qi::omit[tok.minusgreater]
                >> function_types_typexpr
            ;

        function_types_typexpr %=
            tuple_types
            | ident_type_variable
            | anon_type_variable
            | parenthesized_types
            | constructed_no_param
            ;

        tuple_types %=
            tuple_types_typexpr >> +(qi::omit[tok.asterisk] >> tuple_types_typexpr)
            ;

        tuple_types_typexpr %=
            ident_type_variable
            | anon_type_variable
            | parenthesized_types
            | constructed_no_param
            | function_types
            ;

        constructed_no_param %=
            typeconstr
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
        BOOST_SPIRIT_DEBUG_NODE(module_path_wl);
        BOOST_SPIRIT_DEBUG_NODE(extended_module_path);
        BOOST_SPIRIT_DEBUG_NODE(extended_module_path_wl);
        BOOST_SPIRIT_DEBUG_NODE(extended_module_name);

        BOOST_SPIRIT_DEBUG_NODE(typexpr);
        BOOST_SPIRIT_DEBUG_NODE(poly_typexpr);
        BOOST_SPIRIT_DEBUG_NODE(method_type);
        BOOST_SPIRIT_DEBUG_NODE(polymorphic_variant_type);
        BOOST_SPIRIT_DEBUG_NODE(tag_spec_first);
        BOOST_SPIRIT_DEBUG_NODE(tag_spec);
        BOOST_SPIRIT_DEBUG_NODE(tag_spec_full);

        BOOST_SPIRIT_DEBUG_NODE(ident_type_variable);
        BOOST_SPIRIT_DEBUG_NODE(anon_type_variable);
        BOOST_SPIRIT_DEBUG_NODE(parenthesized_types);
        BOOST_SPIRIT_DEBUG_NODE(function_types);
        BOOST_SPIRIT_DEBUG_NODE(function_types_typexpr);
        BOOST_SPIRIT_DEBUG_NODE(tuple_types);
        BOOST_SPIRIT_DEBUG_NODE(tuple_types_typexpr);
        BOOST_SPIRIT_DEBUG_NODE(constructed_no_param);
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
    qi::rule<Iterator, ocaml::ast::module_path()> module_path_wl;
    qi::rule<Iterator, ocaml::ast::extended_module_path()> extended_module_path;
    qi::rule<Iterator, ocaml::ast::extended_module_path()> extended_module_path_wl;
    qi::rule<Iterator, ocaml::ast::extended_module_name()> extended_module_name;

    //
    // Type expressions
    //

    qi::rule<Iterator, ocaml::ast::typexpr()> typexpr;
    qi::rule<Iterator, ocaml::ast::poly_typexpr()> poly_typexpr;
    qi::rule<Iterator, ocaml::ast::method_type()> method_type;
    qi::rule<Iterator, ocaml::ast::polymorphic_variant_type()> polymorphic_variant_type;
    qi::rule<Iterator, ocaml::ast::tag_spec_first()> tag_spec_first;
    qi::rule<Iterator, ocaml::ast::tag_spec()> tag_spec;
    qi::rule<Iterator, ocaml::ast::tag_spec_full()> tag_spec_full;

    qi::rule<Iterator, ocaml::ast::ident()> ident_type_variable;
    qi::rule<Iterator, ocaml::ast::anon_type_variable()> anon_type_variable;
    qi::rule<Iterator, ocaml::ast::typexpr()> parenthesized_types;
    qi::rule<Iterator, ocaml::ast::function_typexpr()> function_types;
    qi::rule<Iterator, ocaml::ast::typexpr()> function_types_typexpr;
    qi::rule<Iterator, ocaml::ast::tuple_typexpr()> tuple_types;
    qi::rule<Iterator, ocaml::ast::typexpr()> tuple_types_typexpr;
    qi::rule<Iterator, ocaml::ast::typeconstr()> constructed_no_param;
};

} // namespace grammar
} // namespace ocaml

#endif //FLANG_OCAMLGRAMMAR_H

