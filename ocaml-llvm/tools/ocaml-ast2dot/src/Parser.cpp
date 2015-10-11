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
    std::swap(*this, other);
}

Parser& Parser::operator =(Parser other)
{
    std::swap(*this, other);
    return *this;
}

void Parser::swap(Parser& other)
{
    std::swap(_p, other._p);
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

} /* namespace OCaml */
