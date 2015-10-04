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

//#define BOOST_SPIRIT_LEXERTL_DEBUG
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

using namespace ocaml;
using namespace ocaml::lexer;
using namespace ocaml::parser;

typedef std::string::const_iterator base_iterator_type;

// This is the lexer token type to use.
typedef lex::lexertl::token<base_iterator_type> token_type_;

typedef lex::lexertl::actor_lexer<token_type_> lexer_type;

typedef lexer_type::iterator_type lexer_iterator_type;

typedef OCamlLexer<lexer_type> ocaml_lexer_type;

typedef OCamlGrammar<lexer_iterator_type> ocaml_grammar_type;

// Lexer
ocaml_lexer_type gLexer;

// Grammar
ocaml_grammar_type gGrammar(gLexer);

std::string infix_symbols[] = {
    "=", "<", ">", "@", "^", "|", "&", "+", "-", "*", "/", "$", "%"
};

std::string operator_chars[] = {
    "!", "$", "%", "&", "*", "+", "-", ".", "/", ":", "<", "=", ">", "?",
    "@", "^", "|", "~"
};

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
    ast::capitalized_ident ident;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.capitalized_ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_lowercase_ident)
{
    ast::lowercase_ident ident;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.lowercase_ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_ident)
{
    ast::ident ident;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);

    ident = ast::ident();
    content = "Test";
    r = parse_string(content, gGrammar.ident, ident);
    BOOST_CHECK(r);
    BOOST_CHECK(ident.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_label_name)
{
    ast::label_name label_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.label_name, label_name);
    BOOST_CHECK(r);
    BOOST_CHECK(label_name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_label)
{
    ast::label label;
    std::string content = "~test";
    bool r = parse_string(content, gGrammar.label, label);
    BOOST_CHECK(r);
    BOOST_CHECK(label.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_optlabel)
{
    ast::optlabel optlabel;
    std::string content = "?test";
    bool r = parse_string(content, gGrammar.optlabel, optlabel);
    BOOST_CHECK(r);
    BOOST_CHECK(optlabel.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_integer_literal)
{
    ast::integer_literal integer_literal;
    std::string content = "23";
    bool r = parse_string(content, gGrammar.integer_literal, integer_literal);
    BOOST_CHECK(r);
    BOOST_CHECK(integer_literal.val == atoi(content.c_str()));
}

BOOST_AUTO_TEST_CASE(GrammarTest_float_literal)
{
    ast::float_literal float_literal;
    std::string content = "1.23";
    bool r = parse_string(content, gGrammar.float_literal, float_literal);
    BOOST_CHECK(r);
    BOOST_CHECK_CLOSE(float_literal.val, atof(content.c_str()), 0.0001);
}

BOOST_AUTO_TEST_CASE(GrammarTest_char_literal)
{
    ast::char_literal char_literal;
    std::string content = "'t'";
    bool r = parse_string(content, gGrammar.char_literal, char_literal);
    BOOST_CHECK(r);
    BOOST_CHECK(char_literal.val == content[0]);
}

BOOST_AUTO_TEST_CASE(GrammarTest_string_literal)
{
    ast::string_literal string_literal;
    std::string content = "\"test\"";
    bool r = parse_string(content, gGrammar.string_literal, string_literal);
    BOOST_CHECK(r);
    BOOST_CHECK(string_literal.val == content);
}

//
// Names
//
/*
BOOST_AUTO_TEST_CASE(GrammarTest_infix_op)
{
    for (struct token op : operations) {
        ast::infix_op infix_op;
        bool r = parse_string(std::string(op.symbol), gGrammar.infix_op, infix_op);
        BOOST_CHECK(r);
        BOOST_CHECK(boost::get<ast::operation>(infix_op).op == op.tokenId);
    }

    for(auto i : operator_chars) {
        for(std::string j : infix_symbols) {
            std::string op = j + i;
            ast::infix_op infix_op;
            bool r = parse_string(op, gGrammar.infix_op, infix_op);
            BOOST_CHECK(r);

            // Skip operations tested above
            if (op != "&&" && op != "-." && op != "||")
                BOOST_CHECK(boost::get<ast::infix_symbol>(infix_op).symbol == op);

            for(auto k : infix_symbols) {
                std::string op = j + i + k;
                ast::infix_op infix_op;
                bool r = parse_string(op, gGrammar.infix_op, infix_op);
                BOOST_CHECK(r);
                BOOST_CHECK(boost::get<ast::infix_symbol>(infix_op).symbol == op);
            }
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_operator_name)
{
    //
    // prefix_symbol part
    //

    // "!" is prefix_symbol

    std::string op = "!";
    ast::operator_name operator_name;
    bool r = parse_string(op, gGrammar.operator_name, operator_name);
    BOOST_CHECK(r);
    BOOST_CHECK(boost::get<ast::prefix_symbol>(operator_name).symbol == op);

    // ! { operator-char } | (? | ~) { operator-char } +
    std::string prefixes[] = { "!", "?", "~" };

    for(auto prefix : prefixes) {
        for(auto i : operator_chars) {
            std::string op = prefix + i;
            ast::operator_name operator_name;
            bool r = parse_string(op, gGrammar.operator_name, operator_name);
            BOOST_CHECK(r);
            BOOST_CHECK(boost::get<ast::prefix_symbol>(operator_name).symbol == op);

            for(auto j : operator_chars) {
                std::string op = prefix + i + j;
                ast::operator_name operator_name;
                bool r = parse_string(op, gGrammar.operator_name, operator_name);
                BOOST_CHECK(r);
                BOOST_CHECK(boost::get<ast::prefix_symbol>(operator_name).symbol == op);
            }
        }
    }

    //
    // infix_op part
    //

    for (struct token op : operations) {
        std::string symbol(op.symbol);
        ast::operator_name operator_name;
        bool r = parse_string(symbol, gGrammar.operator_name, operator_name);
        BOOST_CHECK(r);

        // Skip prefix symbols tested above
        if (symbol != "!=") {
            ast::infix_op infix_op = boost::get<ast::infix_op>(operator_name);
            BOOST_CHECK(boost::get<ast::operation>(infix_op).op == op.tokenId);
        }
    }

    for(auto i : operator_chars) {
        for(std::string j : infix_symbols) {
            std::string op = j + i;
            ast::operator_name operator_name;
            bool r = parse_string(op, gGrammar.operator_name, operator_name);
            BOOST_CHECK(r);

            ast::infix_op infix_op = boost::get<ast::infix_op>(operator_name);

            if (op == "&&")
                BOOST_CHECK(boost::get<ast::operation>(infix_op).op == Tokens::AmpAmp);
            else if (op == "-.")
                BOOST_CHECK(boost::get<ast::operation>(infix_op).op == Tokens::MinusDot);
            else if (op == "||")
                BOOST_CHECK(boost::get<ast::operation>(infix_op).op == Tokens::BarBar);
            else
                BOOST_CHECK(boost::get<ast::infix_symbol>(infix_op).symbol == op);

            for(auto k : infix_symbols) {
                std::string op = j + i + k;
                ast::operator_name operator_name;
                bool r = parse_string(op, gGrammar.operator_name, operator_name);
                BOOST_CHECK(r);
                ast::infix_op infix_op = boost::get<ast::infix_op>(operator_name);
                BOOST_CHECK(boost::get<ast::infix_symbol>(infix_op).symbol == op);
            }
        }
    }
}
*/
BOOST_AUTO_TEST_CASE(GrammarTest_value_name)
{
    ast::value_name value_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.value_name, value_name);
    BOOST_CHECK(r);
    ast::lowercase_ident lowercase_ident = boost::get<ast::lowercase_ident>(value_name);
    BOOST_CHECK(lowercase_ident.name == content);

    value_name = ast::value_name();
    content = "!=";
    r = parse_string("(" + content + ")", gGrammar.value_name, value_name);
    BOOST_CHECK(r);
    ast::operator_name operator_name = boost::get<ast::operator_name>(value_name);
    ast::prefix_symbol prefix_symbol = boost::get<ast::prefix_symbol>(operator_name);
    BOOST_CHECK(prefix_symbol.symbol == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_constr_name)
{
    ast::constr_name constr_name;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.constr_name, constr_name);
    BOOST_CHECK(r);
    BOOST_CHECK(constr_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_tag_name)
{
    ast::tag_name tag_name;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.tag_name, tag_name);
    BOOST_CHECK(r);
    BOOST_CHECK(tag_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typeconstr_name)
{
    ast::typeconstr_name typeconstr_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.typeconstr_name, typeconstr_name);
    BOOST_CHECK(r);
    BOOST_CHECK(typeconstr_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_field_name)
{
    ast::field_name field_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.field_name, field_name);
    BOOST_CHECK(r);
    BOOST_CHECK(field_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_module_name)
{
    ast::module_name module_name;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.module_name, module_name);
    BOOST_CHECK(r);
    BOOST_CHECK(module_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_modtype_name)
{
    ast::modtype_name modtype_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.modtype_name, modtype_name);
    BOOST_CHECK(r);
    BOOST_CHECK(modtype_name.name.name == content);

    modtype_name = ast::modtype_name();
    content = "Test";
    r = parse_string(content, gGrammar.modtype_name, modtype_name);
    BOOST_CHECK(r);
    BOOST_CHECK(modtype_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_class_name)
{
    ast::class_name class_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.class_name, class_name);
    BOOST_CHECK(r);
    BOOST_CHECK(class_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_inst_var_name)
{
    ast::inst_var_name inst_var_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.inst_var_name, inst_var_name);
    BOOST_CHECK(r);
    BOOST_CHECK(inst_var_name.name.name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_method_name)
{
    ast::method_name method_name;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.method_name, method_name);
    BOOST_CHECK(r);
    BOOST_CHECK(method_name.name.name == content);
}


BOOST_AUTO_TEST_CASE(GrammarTest_value_path)
{
    ast::value_path value_path;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.value_path, value_path);
    BOOST_CHECK(r);
    BOOST_CHECK(boost::get<ast::lowercase_ident>(value_path.name).name == content);
    BOOST_CHECK(!value_path.path.is_initialized());

    std::string additional;
    for(int i = 0; i < 10; ++i) {
        value_path = ast::value_path();
        additional += "Test.";
        r = parse_string(additional + content, gGrammar.value_path, value_path);
        BOOST_CHECK(r);
        BOOST_CHECK(boost::get<ast::lowercase_ident>(value_path.name).name == content);
        BOOST_CHECK(value_path.path.is_initialized());
        BOOST_CHECK(value_path.path.get().name.name.name == "Test");
        if (i == 0)
            BOOST_CHECK(!value_path.path.get().other.is_initialized());
        else {
            BOOST_CHECK(value_path.path.get().other.is_initialized());
            BOOST_CHECK(value_path.path.get().other.get().size() == i);
            for(ast::module_name const& name : value_path.path.get().other.get())
                BOOST_CHECK(name.name.name == "Test");
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_constr)
{
    ast::constr constr;
    std::string content = "TTest";
    bool r = parse_string(content, gGrammar.constr, constr);
    BOOST_CHECK(r);
    BOOST_CHECK(constr.name.name.name == content);
    BOOST_CHECK(!constr.path.is_initialized());

    std::string additional;
    std::string moduleName = "Test";
    for(int i = 0; i < 10; ++i) {
        constr = ast::constr();
        additional += moduleName + ".";
        r = parse_string(additional + content, gGrammar.constr, constr);
        BOOST_CHECK(r);
        BOOST_CHECK(constr.name.name.name == content);
        BOOST_CHECK(constr.path.is_initialized());

        if (i == 0) {
            BOOST_CHECK(!constr.path.get().other.is_initialized());
            BOOST_CHECK(constr.path.get().name.name.name == moduleName);
        } else {
            BOOST_CHECK(constr.path.get().name.name.name == moduleName);
            BOOST_CHECK(constr.path.get().other.is_initialized());
            BOOST_CHECK(constr.path.get().other.get().size() == i);
            for(ast::module_name const& name : constr.path.get().other.get())
                BOOST_CHECK(name.name.name == moduleName);
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_typeconstr)
{
    ast::typeconstr typeconstr;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.typeconstr, typeconstr);
    BOOST_CHECK(r);
    BOOST_CHECK(typeconstr.name.name.name == content);
    BOOST_CHECK(!typeconstr.path.is_initialized());

    std::string modulePath;
    std::string moduleName = "Test";
    for(int i = 0; i < 10; ++i) {
        typeconstr = ast::typeconstr();
        modulePath += moduleName + ".";
        r = parse_string(modulePath + content, gGrammar.typeconstr, typeconstr);
        BOOST_CHECK(r);
        BOOST_CHECK(typeconstr.name.name.name == content);
        BOOST_CHECK(typeconstr.path.is_initialized());
        BOOST_CHECK(typeconstr.path.get().name.name.name.name == moduleName);
        if (i == 0)
            BOOST_CHECK(!typeconstr.path.get().other.is_initialized());
        else {
            BOOST_CHECK(typeconstr.path.get().other.is_initialized());
            for(ast::extended_module_name const& name : typeconstr.path.get().other.get()) {
                BOOST_CHECK(name.name.name.name == moduleName);
                BOOST_CHECK(!name.paths.is_initialized());
            }
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_field)
{
    ast::field field;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.field, field);
    BOOST_CHECK(r);
    BOOST_CHECK(field.name.name.name == content);
    BOOST_CHECK(!field.path.is_initialized());

    std::string additional;
    for(int i = 0; i < 10; ++i) {
        field = ast::field();
        additional += "Test.";
        r = parse_string(additional + content, gGrammar.field, field);
        BOOST_CHECK(r);
        BOOST_CHECK(field.name.name.name == content);
        BOOST_CHECK(field.path.is_initialized());
        BOOST_CHECK(field.path.get().name.name.name == "Test");
        if (i == 0)
            BOOST_CHECK(!field.path.get().other.is_initialized());
        else {
            BOOST_CHECK(field.path.get().other.is_initialized());
            BOOST_CHECK(field.path.get().other.get().size() == i);
            for(ast::module_name const& name : field.path.get().other.get())
                BOOST_CHECK(name.name.name == "Test");
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_modtype_path)
{
    ast::modtype_path modtype_path;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.modtype_path, modtype_path);
    BOOST_CHECK(r);
    BOOST_CHECK(modtype_path.name.name.name == content);
    BOOST_CHECK(!modtype_path.path.is_initialized());

    std::string modulePath;
    std::string moduleName = "Test";
    for(int i = 0; i < 10; ++i) {
        modtype_path = ast::modtype_path();
        modulePath += moduleName + ".";
        r = parse_string(modulePath + content, gGrammar.modtype_path, modtype_path);
        BOOST_CHECK(r);
        BOOST_CHECK(modtype_path.name.name.name == content);
        BOOST_CHECK(modtype_path.path.is_initialized());
        BOOST_CHECK(modtype_path.path.get().name.name.name.name == moduleName);
        if (i == 0)
            BOOST_CHECK(!modtype_path.path.get().other.is_initialized());
        else {
            BOOST_CHECK(modtype_path.path.get().other.is_initialized());
            for(ast::extended_module_name const& name : modtype_path.path.get().other.get()) {
                BOOST_CHECK(name.name.name.name == moduleName);
                BOOST_CHECK(!name.paths.is_initialized());
            }
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_class_path)
{
    ast::class_path class_path;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.class_path, class_path);
    BOOST_CHECK(r);
    BOOST_CHECK(class_path.name.name.name == content);
    BOOST_CHECK(!class_path.path.is_initialized());

    std::string additional;
    for(int i = 0; i < 10; ++i) {
        class_path = ast::class_path();
        additional += "Test.";
        r = parse_string(additional + content, gGrammar.class_path, class_path);
        BOOST_CHECK(r);
        BOOST_CHECK(class_path.name.name.name == content);
        BOOST_CHECK(class_path.path.is_initialized());
        BOOST_CHECK(class_path.path.get().name.name.name == "Test");
        if (i == 0)
            BOOST_CHECK(!class_path.path.get().other.is_initialized());
        else {
            BOOST_CHECK(class_path.path.get().other.is_initialized());
            BOOST_CHECK(class_path.path.get().other.get().size() == i);
            for(ast::module_name const& name : class_path.path.get().other.get())
                BOOST_CHECK(name.name.name == "Test");
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_classtype_path)
{
    ast::classtype_path classtype_path;
    std::string content = "test";
    bool r = parse_string(content, gGrammar.classtype_path, classtype_path);
    BOOST_CHECK(r);
    BOOST_CHECK(classtype_path.name.name.name == content);
    BOOST_CHECK(!classtype_path.path.is_initialized());

    std::string modulePath;
    std::string moduleName = "Test";
    for(int i = 0; i < 10; ++i) {
        classtype_path = ast::classtype_path();
        modulePath += moduleName + ".";
        r = parse_string(modulePath + content, gGrammar.classtype_path, classtype_path);
        BOOST_CHECK(r);
        BOOST_CHECK(classtype_path.name.name.name == content);
        BOOST_CHECK(classtype_path.path.is_initialized());
        BOOST_CHECK(classtype_path.path.get().name.name.name.name == moduleName);
        if (i == 0)
            BOOST_CHECK(!classtype_path.path.get().other.is_initialized());
        else {
            BOOST_CHECK(classtype_path.path.get().other.is_initialized());
            for(ast::extended_module_name const& name : classtype_path.path.get().other.get()) {
                BOOST_CHECK(name.name.name.name == moduleName);
                BOOST_CHECK(!name.paths.is_initialized());
            }
        }
    }

}

BOOST_AUTO_TEST_CASE(GrammarTest_module_path)
{
    ast::module_path module_path;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.module_path, module_path);
    BOOST_CHECK(r);
    BOOST_CHECK(module_path.name.name.name == content);
    BOOST_CHECK(!module_path.other.is_initialized());

    std::string additional;
    for(int i = 0; i < 10; ++i) {
        module_path = ast::module_path();
        additional += "." + content;
        r = parse_string(content + additional, gGrammar.module_path, module_path);
        BOOST_CHECK(r);
        BOOST_CHECK(module_path.name.name.name == content);
        BOOST_CHECK(module_path.other.is_initialized());
        BOOST_CHECK(module_path.other.get().size() == i + 1);
        for(ast::module_name const& name : module_path.other.get())
            BOOST_CHECK(name.name.name == content);
    }
}

// Module path but without the last module name
// for rules that have capitalized_ident as the last subrule.
BOOST_AUTO_TEST_CASE(GrammarTest_module_path_wl)
{
    ast::module_path module_path_wl;
    std::string content = "TTTTT";
    bool r = parse_string(content + ".", gGrammar.module_path_wl, module_path_wl);
    BOOST_CHECK(r);
    BOOST_CHECK(module_path_wl.name.name.name == content);
    BOOST_CHECK(!module_path_wl.other.is_initialized());

    std::string additional;
    std::string moduleName = "Test";
    for(int i = 0; i < 10; ++i) {
        module_path_wl = ast::module_path();
        additional += moduleName + ".";
        r = parse_string(additional + content, gGrammar.module_path_wl, module_path_wl);
        BOOST_CHECK(r);
        BOOST_CHECK(module_path_wl.name.name.name != content);
        BOOST_CHECK(module_path_wl.name.name.name == moduleName);
        if (i == 0)
            BOOST_CHECK(!module_path_wl.other.is_initialized());
        else {
            BOOST_CHECK(module_path_wl.other.is_initialized());
            BOOST_CHECK(module_path_wl.other.get().size() == i);
            for(ast::module_name const& name : module_path_wl.other.get())
                BOOST_CHECK(name.name.name == moduleName);
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_extended_module_name)
{
    ast::extended_module_name extended_module_name;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.extended_module_name, extended_module_name);
    BOOST_CHECK(r);
    BOOST_CHECK(extended_module_name.name.name.name == content);
    BOOST_CHECK(!extended_module_name.paths.is_initialized());

    std::string additional;
    for(int i = 0; i < 10; ++i) {
        extended_module_name = ast::extended_module_name();
        additional += "." + content;
        r = parse_string(content + "(" + content + additional + ")", gGrammar.extended_module_name, extended_module_name);
        BOOST_CHECK(r);
        BOOST_CHECK(extended_module_name.name.name.name == content);
        BOOST_CHECK(extended_module_name.paths.is_initialized());
        BOOST_CHECK(extended_module_name.paths.get().size() == 1);
        for(ast::extended_module_path const& path : extended_module_name.paths.get()) {
            BOOST_CHECK(path.name.name.name.name == content);
            BOOST_CHECK(path.other.is_initialized());
            BOOST_CHECK(path.other.get().size() == i + 1);
            for(ast::extended_module_path const& other : path.other.get()) {
                BOOST_CHECK(other.name.name.name.name == content);
                BOOST_CHECK(!other.other.is_initialized());
            }
        }
    }
}

BOOST_AUTO_TEST_CASE(GrammarTest_extended_module_path)
{
    ast::extended_module_path extended_module_path;
    std::string content = "Test";
    bool r = parse_string(content, gGrammar.extended_module_path, extended_module_path);
    BOOST_CHECK(r);
    BOOST_CHECK(extended_module_path.name.name.name.name == content);
    BOOST_CHECK(!extended_module_path.other.is_initialized());

    std::string additional;
    for(int i = 0; i < 10; ++i) {
        extended_module_path = ast::extended_module_path();
        additional += "." + content;
        r = parse_string(content + additional, gGrammar.extended_module_path, extended_module_path);
        BOOST_CHECK(r);
        /*
        BOOST_CHECK(extended_module_path.name.name.name.name == content);
        BOOST_CHECK(extended_module_path.other.is_initialized());
        BOOST_CHECK(extended_module_path.other.get().size() == i + 1);
        for(ast::extended_module_name const& name : extended_module_path.other.get())
            BOOST_CHECK(name.name.name == content);
        */
    }
}

//
// Type expressions
//

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_type_variable)
{
    ast::typexpr typexpr;
    std::string content = "ident";
    bool r = parse_string("'" + content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);
    BOOST_CHECK(boost::get<ast::ident>(typexpr).name == content);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_anon_type_variable)
{
    ast::typexpr typexpr;
    std::string content = "_";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);
    BOOST_CHECK(boost::get<ast::anon_type_variable>(typexpr).var ==
        ocaml::lexer::Tokens::Underscore);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_parenthesized_types)
{
    ast::typexpr typexpr;
    std::string content = "ident";
    bool r = parse_string("('" + content + ")", gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);
    BOOST_CHECK(boost::get<ast::ident>(typexpr).name == content);

    typexpr = ast::typexpr();
    content = "(_)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);
    BOOST_CHECK(boost::get<ast::anon_type_variable>(typexpr).var ==
        ocaml::lexer::Tokens::Underscore);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_function_types_optlabel)
{
    //
    // optlabel
    //
    ast::typexpr typexpr;
    std::string content = "?label: 'ident1 -> 'ident2";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ast::function_typexpr ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    ast::optlabel optlabel = boost::get<ast::optlabel>(ftypexpr.label.get());
    BOOST_CHECK(optlabel.name == "?label");

    ast::ident ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");

    ident = boost::get<ast::ident>(ftypexpr.retexpr);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "?label: _ -> _";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    optlabel = boost::get<ast::optlabel>(ftypexpr.label.get());
    BOOST_CHECK(optlabel.name == "?label");

    ast::anon_type_variable anon_var = boost::get<ast::anon_type_variable>(ftypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.retexpr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    typexpr = ast::typexpr();
    content = "?label: ('ident1) -> ('ident2)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    optlabel = boost::get<ast::optlabel>(ftypexpr.label.get());
    BOOST_CHECK(optlabel.name == "?label");

    ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");

    ident = boost::get<ast::ident>(ftypexpr.retexpr);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "?label: 'ident1 * 'ident2 -> 'ident3 * 'ident4";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    optlabel = boost::get<ast::optlabel>(ftypexpr.label.get());
    BOOST_CHECK(optlabel.name == "?label");

    ast::tuple_typexpr ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.expr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident2");

    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.retexpr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident3");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident4");

    typexpr = ast::typexpr();
    content = "?label: 'ident1 * _ * ('ident2 -> _) * ('ident3 * 'ident4) -> _ * (Ident1.ident5)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    optlabel = boost::get<ast::optlabel>(ftypexpr.label.get());
    BOOST_CHECK(optlabel.name == "?label");

    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.expr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 3);
    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.other[0]);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ftypexpr = boost::get<ast::function_typexpr>(ttypexpr.other[1]);
    ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident2");
    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.retexpr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ttypexpr = boost::get<ast::tuple_typexpr>(ttypexpr.other[2]);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident3");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident4");

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.retexpr);

    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    BOOST_CHECK(ttypexpr.other.size() == 1);
    ast::typeconstr typeconstr = boost::get<ast::typeconstr>(ttypexpr.other[0]);
    BOOST_CHECK(typeconstr.path.is_initialized());
    ast::extended_module_path path = typeconstr.path.get();
    BOOST_CHECK(!path.other.is_initialized());
    BOOST_CHECK(path.name.name.name.name == "Ident1");
    BOOST_CHECK(typeconstr.name.name.name == "ident5");
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_function_types_label_name)
{
    //
    // label_name
    //
    ast::typexpr typexpr;
    std::string content = "label: 'ident1 -> 'ident2";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ast::function_typexpr ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    ast::label_name label_name = boost::get<ast::label_name>(ftypexpr.label.get());
    BOOST_CHECK(label_name.name == "label");

    ast::ident ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");

    ident = boost::get<ast::ident>(ftypexpr.retexpr);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "label: _ -> _";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    label_name = boost::get<ast::label_name>(ftypexpr.label.get());
    BOOST_CHECK(label_name.name == "label");

    ast::anon_type_variable anon_var = boost::get<ast::anon_type_variable>(ftypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.retexpr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    typexpr = ast::typexpr();
    content = "label: ('ident1) -> ('ident2)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    label_name = boost::get<ast::label_name>(ftypexpr.label.get());
    BOOST_CHECK(label_name.name == "label");

    ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");

    ident = boost::get<ast::ident>(ftypexpr.retexpr);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "label: 'ident1 * 'ident2 -> 'ident3 * 'ident4";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    label_name = boost::get<ast::label_name>(ftypexpr.label.get());
    BOOST_CHECK(label_name.name == "label");

    ast::tuple_typexpr ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.expr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident2");

    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.retexpr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident3");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident4");

    typexpr = ast::typexpr();
    content = "label: 'ident1 * _ * ('ident2 -> _) * ('ident3 * 'ident4) -> _ * (Ident1.ident5)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(ftypexpr.label.is_initialized());

    label_name = boost::get<ast::label_name>(ftypexpr.label.get());
    BOOST_CHECK(label_name.name == "label");

    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.expr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 3);
    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.other[0]);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ftypexpr = boost::get<ast::function_typexpr>(ttypexpr.other[1]);
    ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident2");
    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.retexpr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ttypexpr = boost::get<ast::tuple_typexpr>(ttypexpr.other[2]);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident3");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident4");

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.retexpr);

    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    BOOST_CHECK(ttypexpr.other.size() == 1);
    ast::typeconstr typeconstr = boost::get<ast::typeconstr>(ttypexpr.other[0]);
    BOOST_CHECK(typeconstr.path.is_initialized());
    ast::extended_module_path path = typeconstr.path.get();
    BOOST_CHECK(!path.other.is_initialized());
    BOOST_CHECK(path.name.name.name.name == "Ident1");
    BOOST_CHECK(typeconstr.name.name.name == "ident5");
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_function_types_no_label)
{
    //
    // no label
    //
    ast::typexpr typexpr;
    std::string content = "'ident1 -> 'ident2";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ast::function_typexpr ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(!ftypexpr.label.is_initialized());

    ast::ident ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");

    ident = boost::get<ast::ident>(ftypexpr.retexpr);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "_ -> _";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(!ftypexpr.label.is_initialized());

    ast::anon_type_variable anon_var = boost::get<ast::anon_type_variable>(ftypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.retexpr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    typexpr = ast::typexpr();
    content = "('ident1) -> ('ident2)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(!ftypexpr.label.is_initialized());

    ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");

    ident = boost::get<ast::ident>(ftypexpr.retexpr);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "'ident1 * 'ident2 -> 'ident3 * 'ident4";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(!ftypexpr.label.is_initialized());

    ast::tuple_typexpr ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.expr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident2");

    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.retexpr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident3");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident4");

    typexpr = ast::typexpr();
    content = "'ident1 * _ * ('ident2 -> _) * ('ident3 * 'ident4) -> _ * (Ident1.ident5)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    BOOST_CHECK(!ftypexpr.label.is_initialized());

    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.expr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 3);
    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.other[0]);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ftypexpr = boost::get<ast::function_typexpr>(ttypexpr.other[1]);
    ident = boost::get<ast::ident>(ftypexpr.expr);
    BOOST_CHECK(ident.name == "ident2");
    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.retexpr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ttypexpr = boost::get<ast::tuple_typexpr>(ttypexpr.other[2]);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident3");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident4");

    ftypexpr = boost::get<ast::function_typexpr>(typexpr);
    ttypexpr = boost::get<ast::tuple_typexpr>(ftypexpr.retexpr);

    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    BOOST_CHECK(ttypexpr.other.size() == 1);
    ast::typeconstr typeconstr = boost::get<ast::typeconstr>(ttypexpr.other[0]);
    BOOST_CHECK(typeconstr.path.is_initialized());
    ast::extended_module_path path = typeconstr.path.get();
    BOOST_CHECK(!path.other.is_initialized());
    BOOST_CHECK(path.name.name.name.name == "Ident1");
    BOOST_CHECK(typeconstr.name.name.name == "ident5");
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_tuple_types)
{
    ast::typexpr typexpr;
    std::string content = "'ident1 * 'ident2";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ast::tuple_typexpr ttypexpr = boost::get<ast::tuple_typexpr>(typexpr);
    ast::ident ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "_ * _";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ttypexpr = boost::get<ast::tuple_typexpr>(typexpr);
    ast::anon_type_variable anon_var = boost::get<ast::anon_type_variable>(ttypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);
    BOOST_CHECK(ttypexpr.other.size() == 1);
    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.other[0]);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    typexpr = ast::typexpr();
    content = "('ident1) * ('ident2)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ttypexpr = boost::get<ast::tuple_typexpr>(typexpr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    ident = boost::get<ast::ident>(ttypexpr.other[0]);
    BOOST_CHECK(ident.name == "ident2");

    typexpr = ast::typexpr();
    content = "('ident1) * _";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ttypexpr = boost::get<ast::tuple_typexpr>(typexpr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 1);
    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.other[0]);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    typexpr = ast::typexpr();
    content = "('ident1) * _ * (?label: _ -> _) * (Ident1.ident2)";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ttypexpr = boost::get<ast::tuple_typexpr>(typexpr);
    ident = boost::get<ast::ident>(ttypexpr.expr);
    BOOST_CHECK(ident.name == "ident1");
    BOOST_CHECK(ttypexpr.other.size() == 3);
    anon_var = boost::get<ast::anon_type_variable>(ttypexpr.other[0]);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ast::function_typexpr ftypexpr = boost::get<ast::function_typexpr>(ttypexpr.other[1]);

    ast::optlabel optlabel = boost::get<ast::optlabel>(ftypexpr.label.get());
    BOOST_CHECK(optlabel.name == "?label");

    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.expr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    anon_var = boost::get<ast::anon_type_variable>(ftypexpr.retexpr);
    BOOST_CHECK(anon_var.var == ocaml::lexer::Tokens::Underscore);

    ast::typeconstr typeconstr = boost::get<ast::typeconstr>(ttypexpr.other[2]);
    BOOST_CHECK(typeconstr.path.is_initialized());
    ast::extended_module_path path = typeconstr.path.get();
    BOOST_CHECK(!path.other.is_initialized());
    BOOST_CHECK(path.name.name.name.name == "Ident1");
    BOOST_CHECK(typeconstr.name.name.name == "ident2");
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_constructed_no_param)
{
    ast::typexpr typexpr;
    std::string content = "ident1";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    ast::typeconstr typeconstr = boost::get<ast::typeconstr>(typexpr);
    BOOST_CHECK(!typeconstr.path.is_initialized());
    BOOST_CHECK(typeconstr.name.name.name == "ident1");

    typexpr = ast::typexpr();
    content = "Ident1.ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typeconstr = boost::get<ast::typeconstr>(typexpr);
    BOOST_CHECK(typeconstr.path.is_initialized());
    ast::extended_module_path path = typeconstr.path.get();
    BOOST_CHECK(!path.other.is_initialized());
    BOOST_CHECK(path.name.name.name.name == "Ident1");
    BOOST_CHECK(typeconstr.name.name.name == "ident2");
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_constructed_unary_param)
{
    ast::typexpr typexpr;
    std::string content = "'ident1 Ident1.ident2";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "_ Ident1.ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(_) Ident1.ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(?label: _ -> _) Ident1.ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(_ * _) Ident1.ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(Ident1.ident2) Ident3.ident4";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_constructed_nary_param)
{
    ast::typexpr typexpr;
    std::string content = "('ident1, 'ident2) Ident1.ident3";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(_, _) Ident1.ident1";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(_, 'ident1) Ident1.ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(?label1: _ -> 'ident1, ?label2: 'ident2 -> _) Ident1.ident3";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(_ * 'ident1, _ * 'ident2) Ident1.ident3";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(Ident1.ident1, Ident2.ident2) Ident3.ident3";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_aliased_or_recursive)
{
    ast::typexpr typexpr;
    std::string content = "'ident1 as 'ident2";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "_ as 'ident1";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(_) as 'ident1";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    //FIXME: "(((((_))))) as 'ident1" is too slow
    typexpr = ast::typexpr();
    content = "((_)) as 'ident1";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(?label1: _ -> 'ident1) as 'ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "?label1: _ -> 'ident1 as 'ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "(_ * 'ident1) as 'ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "_ * 'ident1 as 'ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "Ident1.ident1 as 'ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "ident1 as 'ident2";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_tag_spec_full)
{
    ast::tag_spec_full tag_spec_full;
    std::string content = "`Ident1";
    bool r = parse_string(content, gGrammar.tag_spec_full, tag_spec_full);
    BOOST_CHECK(r);

    tag_spec_full = ast::tag_spec_full();
    content = "`Ident1 of &'ident1";
    r = parse_string(content, gGrammar.tag_spec_full, tag_spec_full);
    BOOST_CHECK(r);

    tag_spec_full = ast::tag_spec_full();
    content = "`Ident1 of &'ident1 &'ident2";
    r = parse_string(content, gGrammar.tag_spec_full, tag_spec_full);
    BOOST_CHECK(r);

    tag_spec_full = ast::tag_spec_full();
    content = "'ident1";
    r = parse_string(content, gGrammar.tag_spec_full, tag_spec_full);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_tag_spec)
{
    ast::tag_spec tag_spec;
    std::string content = "`Ident1";
    bool r = parse_string(content, gGrammar.tag_spec, tag_spec);
    BOOST_CHECK(r);

    tag_spec = ast::tag_spec();
    content = "`Ident1 of 'ident1";
    r = parse_string(content, gGrammar.tag_spec, tag_spec);
    BOOST_CHECK(r);

    tag_spec = ast::tag_spec();
    content = "'ident1";
    r = parse_string(content, gGrammar.tag_spec, tag_spec);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_tag_spec_first)
{
    ast::tag_spec_first tag_spec_first;
    std::string content = "`Ident1";
    bool r = parse_string(content, gGrammar.tag_spec_first, tag_spec_first);
    BOOST_CHECK(r);

    tag_spec_first = ast::tag_spec_first();
    content = "`Ident1 of 'ident1";
    r = parse_string(content, gGrammar.tag_spec_first, tag_spec_first);
    BOOST_CHECK(r);

    tag_spec_first = ast::tag_spec_first();
    content = "'ident1 | `Ident1 of 'ident2";
    r = parse_string(content, gGrammar.tag_spec_first, tag_spec_first);
    BOOST_CHECK(r);

    // FIXME: typexpr catches content first and ends in infinite loop
    //tag_spec_first = ast::tag_spec_first();
    //content = "| `Ident1 of 'ident1";
    //r = parse_string(content, gGrammar.tag_spec_first, tag_spec_first);
    //BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_exact_variant_type)
{
    ast::exact_variant_type exact_variant_type;
    std::string content = "[`Ident1]";
    bool r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "[`Ident1 of 'ident1]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "[`Ident1 of 'ident1 | `Ident2]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "[`Ident1 of 'ident1 | `Ident2 of 'ident2]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "[`Ident1 of 'ident1 | 'ident2]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "[`Ident1 of 'ident1 | 'ident2 | 'ident3 | `Ident2 of 'ident4]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "['ident1 | 'ident2]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "['ident1 | `Ident1 of 'ident2]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);

    exact_variant_type = ast::exact_variant_type();
    content = "['ident1 | `Ident1]";
    r = parse_string(content, gGrammar.exact_variant_type, exact_variant_type);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_opened_variant_type)
{
    ast::opened_variant_type opened_variant_type;
    std::string content = "[>`Ident1]";
    bool r = parse_string(content, gGrammar.opened_variant_type, opened_variant_type);
    BOOST_CHECK(r);

    // FIXME: Infinite recursion in typexpr
    //opened_variant_type = ast::opened_variant_type();
    //content = "[>]";
    //r = parse_string(content, gGrammar.opened_variant_type, opened_variant_type);
    //BOOST_CHECK(r);

    opened_variant_type = ast::opened_variant_type();
    content = "[>`Ident1 | `Ident2]";
    r = parse_string(content, gGrammar.opened_variant_type, opened_variant_type);
    BOOST_CHECK(r);

    // FIXME: Infinite recursion in typexpr
    //opened_variant_type = ast::opened_variant_type();
    //content = "[> | `Ident2]";
    //r = parse_string(content, gGrammar.opened_variant_type, opened_variant_type);
    //BOOST_CHECK(r);

    opened_variant_type = ast::opened_variant_type();
    content = "[>`Ident1 | `Ident2 | `Ident3]";
    r = parse_string(content, gGrammar.opened_variant_type, opened_variant_type);
    BOOST_CHECK(r);

    opened_variant_type = ast::opened_variant_type();
    content = "[>`Ident1 | `Ident2 of 'ident1 | 'ident2]";
    r = parse_string(content, gGrammar.opened_variant_type, opened_variant_type);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_closed_variant_type)
{
    ast::closed_variant_type closed_variant_type;
    std::string content = "[<`Ident1]";
    bool r = parse_string(content, gGrammar.closed_variant_type, closed_variant_type);
    BOOST_CHECK(r);

    closed_variant_type = ast::closed_variant_type();
    content = "[< | `Ident1]";
    r = parse_string(content, gGrammar.closed_variant_type, closed_variant_type);
    BOOST_CHECK(r);

    closed_variant_type = ast::closed_variant_type();
    content = "[< | `Ident1 | `Ident2]";
    r = parse_string(content, gGrammar.closed_variant_type, closed_variant_type);
    BOOST_CHECK(r);

    closed_variant_type = ast::closed_variant_type();
    content = "[< `Ident1 | `Ident2 > `Ident3]";
    r = parse_string(content, gGrammar.closed_variant_type, closed_variant_type);
    BOOST_CHECK(r);

    closed_variant_type = ast::closed_variant_type();
    content = "[< `Ident1 | `Ident2 > `Ident3 `Ident4]";
    r = parse_string(content, gGrammar.closed_variant_type, closed_variant_type);
    BOOST_CHECK(r);

    closed_variant_type = ast::closed_variant_type();
    content = "[< `Ident1 | 'ident1 | _ > `Ident3 ]";
    r = parse_string(content, gGrammar.closed_variant_type, closed_variant_type);
    BOOST_CHECK(r);
}

BOOST_AUTO_TEST_CASE(GrammarTest_typexpr_polymorphic_variant_type)
{
    ast::typexpr typexpr;
    std::string content = "[`Ident1]";
    bool r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[`Ident1 of 'ident1]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[`Ident1 of 'ident1 | `Ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[`Ident1 of 'ident1 | `Ident2 of 'ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[`Ident1 of 'ident1 | 'ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[`Ident1 of 'ident1 | 'ident2 | 'ident3 | `Ident2 of 'ident4]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "['ident1 | 'ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "['ident1 | `Ident1 of 'ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "['ident1 | `Ident1]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[>`Ident1]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    // FIXME: Infinite recursion in typexpr
    typexpr = ast::typexpr();
    content = "[>]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[>`Ident1 | `Ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    // FIXME: Infinite recursion in typexpr
    typexpr = ast::typexpr();
    content = "[> | `Ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[>`Ident1 | `Ident2 | `Ident3]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[>`Ident1 | `Ident2 of 'ident1 | 'ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[<`Ident1]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[< | `Ident1]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[< | `Ident1 | `Ident2]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[< `Ident1 | `Ident2 > `Ident3]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[< `Ident1 | `Ident2 > `Ident3 `Ident4]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "[< `Ident1 | 'ident1 | _ > `Ident3 ]";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

    typexpr = ast::typexpr();
    content = "?label: [< `Ident1 | 'ident1 | _ > `Ident3 ] -> _";
    r = parse_string(content, gGrammar.typexpr, typexpr);
    BOOST_CHECK(r);

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
