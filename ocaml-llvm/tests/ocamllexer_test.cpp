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

#define BOOST_TEST_MODULE OCamlLexerTest
#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK

#include "ocamllexer.h"

#define BOOST_SPIRIT_LEXERTL_DEBUG

#include <boost/test/unit_test.hpp>

using namespace boost::spirit;

BOOST_AUTO_TEST_SUITE(OCamlLexerTest)

BOOST_AUTO_TEST_CASE(LexerTest)
{
    typedef std::string::iterator base_iterator_type;

    // lexer type
    typedef lex::lexertl::actor_lexer<lex::lexertl::token<base_iterator_type>>
        lexer_type;

    // OCaml lexer
    ocaml::lexer::OCamlLexer<lexer_type> ocamlLexer;
    std::string contentToLex = "type text = docstring list\n"
        "\n"
        "let empty_text = []\n"
        "\n"
        "let text_loc = {txt = \"ocaml.text\"; loc = Location.none}\n"
        "\n"
        "let text_attr ds =\n"
        "  let open Asttypes in\n"
        "  let open Parsetree in\n"
        "  let exp =\n"
        "    { pexp_desc = Pexp_constant (Const_string(ds.ds_body, None));\n"
        "      pexp_loc = ds.ds_loc;\n"
        "      pexp_attributes = []; }\n"
        "  in\n"
        "  let item =\n"
        "    { pstr_desc = Pstr_eval (exp, []); pstr_loc = exp.pexp_loc }\n"
        "  in\n"
        "    (text_loc, PStr [item])\n"
        "\n"
        "let add_text_attrs dsl attrs =\n"
        "  (List.map text_attr dsl) @ attrs ~sssss ?sssss";
    base_iterator_type first = contentToLex.begin();
    bool r = lex::tokenize(first, contentToLex.end(), ocamlLexer);
    if (!r) {
        std::string rest(first, contentToLex.end());
        std::cerr << "Lexical analysis failed\n" << "stopped at: \""
            << rest << "\"\n";
    }

    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_SUITE_END()
