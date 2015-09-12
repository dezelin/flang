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

#ifndef FLANG_OCAMLIDS_H
#define FLANG_OCAMLIDS_H

#include <boost/fusion/include/io.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>

namespace ocaml
{
namespace lexer
{

using namespace boost::spirit;

enum class Tokens
    : std::size_t
{
    //
    // Lexical
    //

    Unknown = lex::min_token_id,
    Blank,
    Ident,
    CapitalizedIdent,
    LowercaseIdent,
    Integer,
    Float,
    String,
    Label,
    OptLabel,
    PrefixOp,
    InfixOp,

    //
    // Reserved keywords
    //

    And,
    As,
    Assert,
    Asr,
    Begin,
    Class,
    Constraint,
    Do,
    Done,
    Downto,
    Else,
    End,
    Exception,
    External,
    False,
    For,
    Fun,
    Function,
    Functor,
    If,
    In,
    Include,
    Inherit,
    Initializer,
    Land,
    Lazy,
    Let,
    LinenumDir,
    Lor,
    Lsl,
    Lsr,
    Lxor,
    Match,
    Method,
    Mod,
    Module,
    Mutable,
    New,
    Object,
    Of,
    Open,
    Or,
    Parser,
    Private,
    Rec,
    Sig,
    Struct,
    Then,
    To,
    True,
    Try,
    Type,
    Val,
    Value,
    Virtual,
    When,
    While,
    With,

    //
    // Reserved sequences
    //

    BangEqual,
    Hash,
    Ampersand,
    AmpAmp,
    Apostrophe,
    LBrace,
    RBrace,
    Asterisk,
    Plus,
    Comma,
    Minus,
    MinusDot,
    MinusGreater,
    Dot,
    DotDot,
    Colon,
    ColonColon,
    ColonEqual,
    ColonGreater,
    Semicolon,
    SemiSemi,
    Lesser,
    LesserMinus,
    Equal,
    Greater,
    GreaterRBracket,
    GreaterRCurly,
    Question,
    LBracket,
    LBracketLesser,
    LBracketGreater,
    LBracketBar,
    RBracket,
    Underscore,
    GraveAccent,
    LCurly,
    LCurlyLesser,
    Bar,
    BarRBracket,
    BarBar,
    RCurly,
    Tilde,
    Dollar,
    DollarDollar,
    DollarColon,
    LesserColon,
    LessLess,
    GreatGreat,
    QuestQuest
};

// Print function for debugging
inline std::ostream& operator<<(std::ostream& out, Tokens token)
{
    out << static_cast<std::size_t>(token); 
    return out;
}

} // namespace lexer
} // namespace ocaml

#endif //FLANG_OCAMLIDS_H
