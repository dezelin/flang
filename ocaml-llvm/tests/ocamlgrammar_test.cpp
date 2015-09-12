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

#define BOOST_TEST_MODULE OCamlGrammarTest
#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK

#define BOOST_SPIRIT_LEXERTL_DEBUG
#define BOOST_SPIRIT_DEBUG

#include "ocamlast.h"
#include "ocamllexer.h"
#include "ocamlgrammar.h"

#include <boost/filesystem.hpp>
#include <boost/test/floating_point_comparison.hpp>
#include <boost/test/unit_test.hpp>

#include <iomanip>
#include <fstream>

using namespace boost::filesystem;
using namespace boost::spirit;

typedef std::string::const_iterator base_iterator_type;

// This is the lexer token type to use.
typedef lex::lexertl::token<base_iterator_type> token_type_;

typedef lex::lexertl::actor_lexer<token_type_> lexer_type;

typedef lexer_type::iterator_type lexer_iterator_type;

typedef ocaml::lexer::OCamlLexer<lexer_type> ocaml_lexer_type;

typedef ocaml::parser::OCamlGrammar<lexer_iterator_type> ocaml_grammar_type;

// Lexer
ocaml_lexer_type gLexer;

// Grammar
ocaml_grammar_type gGrammar(gLexer);


std::string read_from_file(std::string const &filePath)
{
    std::string ctx, line;
    std::ifstream file(filePath);
    if (file.is_open()) {
        while (getline(file, line)) {
            ctx += line + "\n";
        }

        file.close();
    }
    else {
        std::cout << "Can't open file " << filePath << std::endl;
    }

    return ctx;
}

template<typename ParserExpr, typename Attribute>
bool parse_string(std::string const& content, ParserExpr const& expr, Attribute& attr)
{
    base_iterator_type first = content.begin();
    base_iterator_type last = content.end();
    lexer_iterator_type lfirst = gLexer.begin(first, last);
    lexer_iterator_type llast = gLexer.end();
    bool r = qi::parse(lfirst, llast, expr, attr);
    return r;
}

BOOST_AUTO_TEST_SUITE(OCamlGrammarTest)

//
// Lexical
//

BOOST_AUTO_TEST_CASE(GrammarTest_capitalized_ident)
{
    ocaml::ast::capitalized_ident ident;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.capitalized_ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_lowercase_ident)
{
    ocaml::ast::lowercase_ident ident;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.lowercase_ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_ident)
{
    ocaml::ast::ident ident;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);

    ident = ocaml::ast::ident();
    content = "Test";
    r = parse_string(content, gGrammar.ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_label_name)
{
    ocaml::ast::label_name label_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.label_name, label_name);
    BOOST_CHECK(r);
    BOOST_CHECK(label_name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_label)
{
    ocaml::ast::label label;
    std::string content = "~test";
    bool r = parse_string(content, gGrammar.label, label);
    BOOST_CHECK(r);
    BOOST_CHECK(label.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_optlabel)
{
    ocaml::ast::optlabel optlabel;
    std::string content = "?test";
    bool r = parse_string(content, gGrammar.optlabel, optlabel);
    BOOST_CHECK(r);
    BOOST_CHECK(optlabel.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_integer_literal)
{
    ocaml::ast::integer_literal integer_literal;
    std::string content = "23";
    bool r = parse_string(content, gGrammar.integer_literal, integer_literal);
    BOOST_CHECK(r);
    BOOST_CHECK(integer_literal.val == atoi(content.c_str()));
}

BOOST_AUTO_TEST_CASE(GrammarTest_float_literal)
{
    ocaml::ast::float_literal float_literal;
    std::string content = "1.23";
    bool r = parse_string(content, gGrammar.float_literal, float_literal);
    BOOST_CHECK(r);
    BOOST_CHECK_CLOSE(float_literal.val, atof(content.c_str()), 0.0001);
}

BOOST_AUTO_TEST_CASE(GrammarTest_char_literal)
{
    ocaml::ast::char_literal char_literal;
    std::string content = "'t'";
    bool r = parse_string(content, gGrammar.char_literal, char_literal);
    BOOST_CHECK(r);
    BOOST_CHECK(char_literal.val == content[0]);
}

BOOST_AUTO_TEST_CASE(GrammarTest_string_literal)
{
    ocaml::ast::string_literal string_literal;
    std::string content = "\"test\"";
    bool r = parse_string(content, gGrammar.string_literal, string_literal);
    BOOST_CHECK(r);
    BOOST_CHECK(string_literal.val == content);
}

//
// Names
//

BOOST_AUTO_TEST_CASE(GrammarTest_infix_op)
{
    using namespace ocaml;
    using namespace ocaml::lexer;
    
    struct token { 
        std::string symbol; 
        Tokens tokenId;
    }; 
    
    struct token operations[] = {
        { "*",          Tokens::Asterisk },
        { "+",          Tokens::Plus }, 
        { "-",          Tokens::Minus },
        { "-.",         Tokens::MinusDot },
        { "=",          Tokens::Equal },
        { "!=",         Tokens::BangEqual },
        { "<",          Tokens::Lesser },
        { ">",          Tokens::Greater },
        { "or",         Tokens::Or },
        { "||",         Tokens::BarBar },
        { "&",          Tokens::Ampersand },
        { "&&",         Tokens::AmpAmp },
        { ":=",         Tokens::ColonEqual },
        { "mod",        Tokens::Mod },
        { "land",       Tokens::Land },
        { "lor",        Tokens::Lor },
        { "lxor",       Tokens::Lxor },
        { "lsl",        Tokens::Lsl },
        { "lsr",        Tokens::Lsr },
        { "asr",        Tokens::Asr }
    };
    
    std::cout << "First token: " << Tokens::Blank << "\n";
    
    for (struct token op : operations) {
        ast::infix_op infix_op;
        bool r = parse_string(std::string(op.symbol), gGrammar.infix_op, infix_op);
        BOOST_CHECK(r);
        BOOST_CHECK(boost::get<ast::operation>(infix_op).op == op.tokenId);
    }
    
    std::string infix_symbols[] = {
        "=", "<", ">", "@", "^", "|", "&", "+", "-", "*", "/", "$", "%"
    };
    
    std::string operator_chars[] = {
        "!", "$", "%", "&", "*", "+", "-", ".", "/", ":", "<", "=", ">", "?", 
        "@", "^", "|", "~"
    };
    
    for(std::string i : operator_chars) {
        for(std::string j : infix_symbols) {
            std::string op = j + i;
            ast::infix_op infix_op;
            bool r = parse_string(op, gGrammar.infix_op, infix_op);
            BOOST_CHECK(r);
            
            // Skip operations tested above
            if (op != "&&" && op != "-." && op != "||")
                BOOST_CHECK(boost::get<ast::infix_symbol>(infix_op).symbol == op);

            for(std::string k : infix_symbols) {
                std::string op = j + i + k;
                ast::infix_op infix_op;
                bool r = parse_string(op, gGrammar.infix_op, infix_op);
                BOOST_CHECK(r);
                BOOST_CHECK(boost::get<ast::infix_symbol>(infix_op).symbol == op);
            }
        }
    }
}

/*
BOOST_AUTO_TEST_CASE(GrammarTest_ocaml_distribution)
{
    path dataDir(OCAML_TEST_CASE_DATA_DIR);
    BOOST_CHECK(exists(dataDir));
    BOOST_CHECK(is_directory(dataDir));

    std::vector<path> files;
    copy(recursive_directory_iterator(dataDir), recursive_directory_iterator(),
        std::back_inserter(files));

    sort(files.begin(), files.end());
    auto new_end = std::remove_if(files.begin(), files.end(), [](path file) {
        return file.extension().string() != ".ml" && file.extension().string()
            != ".mli";
    });

    files.erase(new_end, files.end());

    std::cout << "Lexing files from Ocaml distribution:\n";
    std::for_each(files.begin(), files.end(), [&](path file) {
        std::cout << ">> File: " << file << "\n";
        // OCaml lexer
        std::string fileContent = read_from_file(file.string());
        base_iterator_type first = fileContent.begin();
        bool r = lex::tokenize_and_parse(first, fileContent.end(),
            gLexer, gGrammar);
        if (!r) {
            std::string rest(first, fileContent.end());
            std::cerr << file.string() << ":\n";
            std::cerr << "Lexical analysis failed\n" << "stopped at: \""
                << rest << "\"\n";
        }

        BOOST_CHECK(r);
    });
}
*/

BOOST_AUTO_TEST_SUITE_END()
