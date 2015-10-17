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

#include "Parser.h"

#include <ocamlast.h>
#include <ocamllexer.h>
#include <ocamlgrammar.h>

using namespace boost::spirit;
using namespace ocaml;
using namespace ocaml::lexer;
using namespace ocaml::parser;

namespace OCaml
{

class ParserPriv
{
    typedef std::string::const_iterator base_iterator_type;
    typedef lex::lexertl::token<base_iterator_type> token_type_;
    typedef lex::lexertl::actor_lexer<token_type_> lexer_type;
    typedef lexer_type::iterator_type lexer_iterator_type;
    typedef OCamlLexer<lexer_type> OCamlLexerType;
    typedef OCamlGrammar<lexer_iterator_type> OCamlGrammarType;

public:
    ParserPriv(Parser *q);
    ParserPriv(ParserPriv const& other);

    //
    // Lexical rules
    //
    bool parse(const std::string& content,
        ocaml::ast::capitalized_ident& ident);
    bool parse(const std::string& content,
        ocaml::ast::lowercase_ident& ident);
    bool parse(const std::string& content,
        ocaml::ast::ident& ident);
    bool parse(const std::string& content,
        ocaml::ast::label_name& name);
    bool parse(const std::string& content,
        ocaml::ast::label& label);
    bool parse(const std::string& content,
        ocaml::ast::optlabel& label);
    bool parse(const std::string& content,
        ocaml::ast::integer_literal& integer_lit);
    bool parse(const std::string& content,
        ocaml::ast::float_literal& float_lit);
    bool parse(const std::string& content,
        ocaml::ast::char_literal& char_lit);
    bool parse(const std::string& content,
        ocaml::ast::string_literal& string_lit);

    //
    // Names rules
    //
    bool parse(const std::string& content,
        ocaml::ast::value_name& name);
    bool parse(const std::string& content,
        ocaml::ast::operator_name& name);
    bool parse(const std::string& content,
        ocaml::ast::infix_op& op);
    bool parse(const std::string& content,
        ocaml::ast::constr_name& name);
    bool parse(const std::string& content,
        ocaml::ast::tag_name& name);
    bool parse(const std::string& content,
        ocaml::ast::typeconstr_name& name);
    bool parse(const std::string& content,
        ocaml::ast::field_name& name);
    bool parse(const std::string& content,
        ocaml::ast::module_name& name);
    bool parse(const std::string& content,
        ocaml::ast::modtype_name& name);
    bool parse(const std::string& content,
        ocaml::ast::class_name& name);
    bool parse(const std::string& content,
        ocaml::ast::inst_var_name& name);
    bool parse(const std::string& content,
        ocaml::ast::method_name& name);
    bool parse(const std::string& content,
        ocaml::ast::value_path& path);
    bool parse(const std::string& content,
        ocaml::ast::constr& constr);
    bool parse(const std::string& content,
        ocaml::ast::typeconstr& constr);
    bool parse(const std::string& content,
        ocaml::ast::field& field);
    bool parse(const std::string& content,
        ocaml::ast::modtype_path& path);
    bool parse(const std::string& content,
        ocaml::ast::class_path& path);
    bool parse(const std::string& content,
        ocaml::ast::classtype_path& path);
    bool parse(const std::string& content,
        ocaml::ast::module_path& path);
    bool parse(const std::string& content,
        ocaml::ast::extended_module_path& path);
    bool parse(const std::string& content,
        ocaml::ast::extended_module_name& name);

private:
    template<typename ParserExpr, typename Attribute>
    bool parseString(std::string const& content,
        ParserExpr const& expr, Attribute& attr);

private:
    Parser *_q;
    OCamlLexerType _lexer;
    OCamlGrammarType _grammar;
};

Parser::Parser()
    : _p(new ParserPriv(this))
{
}

Parser::~Parser()
{
}

Parser::Parser(const Parser& other)
{
    _p.reset(new ParserPriv(*(other._p)));
}

Parser::Parser(Parser&& other)
    : Parser()
{
    swap(other);
}

Parser& Parser::operator =(Parser other)
{
    swap(other);
    return *this;
}

void Parser::swap(Parser& other)
{
    std::swap(_p, other._p);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::capitalized_ident& ident)
{
    return _p->parse(content, ident);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::lowercase_ident& ident)
{
    return _p->parse(content, ident);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::ident& ident)
{
    return _p->parse(content, ident);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::label_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::label& label)
{
    return _p->parse(content, label);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::optlabel& label)
{
    return _p->parse(content, label);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::integer_literal& integer_lit)
{
    return _p->parse(content, integer_lit);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::float_literal& float_lit)
{
    return _p->parse(content, float_lit);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::char_literal& char_lit)
{
    return _p->parse(content, char_lit);
}

bool Parser::parse(const std::string& content,
    ocaml::ast::string_literal& string_lit)
{
    return _p->parse(content, string_lit);
}

bool Parser::parse(std::string const& content, ocaml::ast::value_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::operator_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::infix_op& op)
{
    return _p->parse(content, op);
}

bool Parser::parse(std::string const& content, ocaml::ast::constr_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::tag_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content,
    ocaml::ast::typeconstr_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::field_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::module_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::modtype_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::class_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::inst_var_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::method_name& name)
{
    return _p->parse(content, name);
}

bool Parser::parse(std::string const& content, ocaml::ast::value_path& path)
{
    return _p->parse(content, path);
}

bool Parser::parse(std::string const& content, ocaml::ast::constr& constr)
{
    return _p->parse(content, constr);
}

bool Parser::parse(std::string const& content, ocaml::ast::typeconstr& constr)
{
    return _p->parse(content, constr);
}

bool Parser::parse(std::string const& content, ocaml::ast::field& field)
{
    return _p->parse(content, field);
}

bool Parser::parse(std::string const& content, ocaml::ast::modtype_path& path)
{
    return _p->parse(content, path);
}

bool Parser::parse(std::string const& content, ocaml::ast::class_path& path)
{
    return _p->parse(content, path);
}

bool Parser::parse(std::string const& content, ocaml::ast::classtype_path& path)
{
    return _p->parse(content, path);
}

bool Parser::parse(std::string const& content, ocaml::ast::module_path& path)
{
    return _p->parse(content, path);
}

bool Parser::parse(std::string const& content,
    ocaml::ast::extended_module_path& path)
{
    return _p->parse(content, path);
}

bool Parser::parse(std::string const& content,
    ocaml::ast::extended_module_name& name)
{
    return _p->parse(content, name);
}

//
// Private implementation
//

ParserPriv::ParserPriv(Parser *q)
    : _q(q), _grammar(_lexer)
{
}

ParserPriv::ParserPriv(ParserPriv const& other)
    : _q(other._q), _grammar(_lexer)
{
    // _grammar and _lexer can't be copied
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::capitalized_ident& ident)
{
    return parseString(content, _grammar.capitalized_ident, ident);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::lowercase_ident& ident)
{
    return parseString(content, _grammar.lowercase_ident, ident);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::ident& ident)
{
    return parseString(content, _grammar.ident, ident);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::label_name& name)
{
    return parseString(content, _grammar.label_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::label& label)
{
    return parseString(content, _grammar.label, label);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::optlabel& label)
{
    return parseString(content, _grammar.optlabel, label);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::integer_literal& integer_lit)
{
    return parseString(content, _grammar.integer_literal, integer_lit);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::float_literal& float_lit)
{
    return parseString(content, _grammar.float_literal, float_lit);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::char_literal& char_lit)
{
    return parseString(content, _grammar.char_literal, char_lit);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::string_literal& string_lit)
{
    return parseString(content, _grammar.string_literal, string_lit);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::value_name& name)
{
    return parseString(content, _grammar.value_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::operator_name& name)
{
    return parseString(content, _grammar.operator_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::infix_op& op)
{
    return parseString(content, _grammar.infix_op, op);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::constr_name& name)
{
    return parseString(content, _grammar.constr_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::tag_name& name)
{
    return parseString(content, _grammar.tag_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::typeconstr_name& name)
{
    return parseString(content, _grammar.typeconstr_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::field_name& name)
{
    return parseString(content, _grammar.field_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::module_name& name)
{
    return parseString(content, _grammar.module_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::modtype_name& name)
{
    return parseString(content, _grammar.modtype_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::class_name& name)
{
    return parseString(content, _grammar.class_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::inst_var_name& name)
{
    return parseString(content, _grammar.inst_var_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::method_name& name)
{
    return parseString(content, _grammar.method_name, name);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::value_path& path)
{
    return parseString(content, _grammar.value_path, path);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::constr& constr)
{
    return parseString(content, _grammar.constr, constr);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::typeconstr& constr)
{
    return parseString(content, _grammar.typeconstr, constr);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::field& field)
{
    return parseString(content, _grammar.field, field);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::modtype_path& path)
{
    return parseString(content, _grammar.modtype_path, path);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::class_path& path)
{
    return parseString(content, _grammar.class_path, path);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::classtype_path& path)
{
    return parseString(content, _grammar.classtype_path, path);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::module_path& path)
{
    return parseString(content, _grammar.module_path, path);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::extended_module_path& path)
{
    return parseString(content, _grammar.extended_module_path, path);
}

bool ParserPriv::parse(const std::string& content,
    ocaml::ast::extended_module_name& name)
{
    return parseString(content, _grammar.extended_module_name, name);
}

template<typename ParserExpr, typename Attribute>
bool ParserPriv::parseString(std::string const& content,
    ParserExpr const& expr, Attribute& attr)
{
    base_iterator_type first = content.begin();
    base_iterator_type last = content.end();
    lexer_iterator_type lfirst = _lexer.begin(first, last);
    lexer_iterator_type llast = _lexer.end();
    bool r = qi::parse(lfirst, llast, expr, attr);
    if (lfirst != llast)
        return false;

    return r;
}

} /* namespace OCaml */
