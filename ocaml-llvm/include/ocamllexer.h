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

#ifndef FLANG_OCAMLLEXER_H
#define FLANG_OCAMLLEXER_H

#include "ocamlids.h"

//#define BOOST_SPIRIT_LEXERTL_DEBUG

#include <boost/phoenix/statement/if.hpp>
#include <boost/spirit/include/lex_lexertl.hpp>

#include <string>

namespace ocaml
{
namespace lexer
{

using namespace boost::phoenix;
using namespace boost::spirit;

//
// OCaml lexer class
// See: http://caml.inria.fr/pub/docs/manual-ocaml/lex.html
//

template<typename Lexer>
class OCamlLexer
    : public lex::lexer<Lexer>
{
    const std::string kBlank = "([ ]|[\\t]|[\\r]|[\\n]|[\\f])+";

    const std::string kAny = ".";

    const std::string kCommentBegin = "[\\(][\\*]";
    const std::string kCommentEnd = "[\\*][\\)]";

    const std::string kLowercaseLetter = "[a-z]";

    const std::string kUppercaseLetter = "[A-Z]";

    const std::string kLetter = "(" + kLowercaseLetter + "|" +
        kUppercaseLetter + ")";

    const std::string kIdent = "(" + kLetter + "|[_])(" + kLetter +
        "|[0-9]|[_]|['])*";

    const std::string kCapitalizedIdent = kUppercaseLetter + "(" + kLetter +
        "|[0-9]|[_]|['])*";

    const std::string kLowercaseIdent = kLowercaseLetter + "(" + kLetter +
        "|[0-9]|[_]|['])*";

    const std::string kInteger =
        "[\\-]?(0b|0B)([0-1])([0-1]|[_])*"          // binary
            "|[\\-]?(0o|0O)([0-7])([0-7]|[_])*"     // octal
            "|[\\-]?(0x|0X)([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]|[_])*"   // hexadecimal
            "|[\\-]?[0-9]([0-9]|[_])*";                                   // decimal

    const std::string kFloat =
        "[\\-]?[0-9]([0-9]|[_])*([\\.]([0-9]|[_])*)?([eE][\\+\\-][0-9]"
            "([0-9]|[_])*)?";

    const std::string kEscapeSequence =
        "[\\\\]([\\\\]|[\\\"]|[']|[n]|[t]|[b]|[r]|[ ])"
            "|[\\\\][0-9][0-9][0-9]"
            "|[\\\\][x]([0-9]|[A-F]∣[a-f])([0-9]|[A-F][a-f])";

    const std::string kCharLiteral = "['].[']|['](" + kEscapeSequence + ")[']";

    const std::string kStringCharacter = "[^\\\"]|(" +
        kEscapeSequence +
        ")|[\\\\]([\\n]|[\\r\\n])([ ]|[\\t])*";

    const std::string kStringLiteral = "[\\\"](" +
        kStringCharacter + ")"
        "*[\\\"]";

    const std::string kLabel = "~" + kLowercaseIdent;

    const std::string kOptLabel = "\\?" + kLowercaseIdent;

    const std::string kOperatorChar = "!|\\$|%|&|\\*|\\+|-|\\.|\\/|:|<|=|>|\\?|@|\\^|\\||~";

    const std::string kPrefixSymbol = "[!](" + kOperatorChar + ")*|[?~](" +
        kOperatorChar +
        ")+";

    const std::string kInfixSymbol = "(=|<|>|@|\\^|\\||&|\\+|-|\\*|\\/|\\$|%)(" +
            kOperatorChar + ")*";

    const std::string kLinenumDirective = "#[0-9]+|#[0-9]+" + kStringLiteral;

    ///////////////////////////////////////////////////////////////////////////////
    // Simple custom semantic action function object used to print the matched
    // input sequence for a particular token
    template<typename Char, typename Traits>
    struct echo_input_functor
    {
        echo_input_functor(std::basic_ostream<Char, Traits> &os_)
            : os(os_) { }

        // This is called by the semantic action handling code during the lexing
        template<typename Iterator, typename Context>
        void operator()(Iterator const &/*b*/, Iterator const &/*e*/,
            BOOST_SCOPED_ENUM(boost::spirit::lex::pass_flags) &, std::size_t &,
            Context &) const
        {
            //std::cout << std::string(b, e) << "\n";
        }

        std::basic_ostream<Char, Traits> &os;
    };

    template<typename Char, typename Traits>
    inline echo_input_functor<Char, Traits>
    echo_input(std::basic_ostream<Char, Traits> &os)
    {
        return echo_input_functor<Char, Traits>(os);
    }

    // Custom semantic action function object used to switch the
    // state of the lexer
    struct set_lexer_state
    {
        set_lexer_state(char const *state_, int level_)
            : level(level_)
            , state(state_) { }

        // This is called by the semantic action handling code during the lexing
        template<typename Iterator, typename Context>
        void operator()(Iterator const &, Iterator const &,
            BOOST_SCOPED_ENUM(boost::spirit::lex::pass_flags) &, std::size_t &,
            Context &ctx) const
        {
            // Change state back to "INITIAL" only if comment nesting level is 0
            if (level != 0 || state != "INITIAL") {
                ctx.set_state_name(state.c_str());
            }
        }

        int level;
        std::string state;
    };

    //
    // Internal token definition type to accept Tokens enum class
    //
    template<typename T = lex::unused_type>
    struct lexer_token_def
        : public lex::token_def<T>
    {
        //
        // Template function to convert enum class to it's underlying
        // integral type

        template<typename E>
        constexpr auto to_underlying(E e)
        -> typename std::underlying_type<E>::type
        {
            return static_cast<typename std::underlying_type<E>::type>(e);
        }

        explicit lexer_token_def(std::string const &def, Tokens id =
        Tokens())
            : lex::token_def<T>(def, to_underlying(id))
        {

        }
    };

public:
    OCamlLexer()
        : OCamlLexer::base_type()
        //
        // Lexical
        //

        , blank(kBlank, Tokens::Blank)
        , comment_begin(kCommentBegin)
        , comment_begin2(kCommentBegin) // for COMMENT state
        , comment_end(kCommentEnd)
        , comment_blank(kBlank)
        , comment_any(kAny)
        , lowercase_letter(kLowercaseLetter)
        , uppercase_letter(kUppercaseLetter)
        , letter(kLetter)
        , ident(kIdent, Tokens::Ident)
        , capitalized_ident(kCapitalizedIdent, Tokens::CapitalizedIdent)
        , lowercase_ident(kLowercaseIdent, Tokens::LowercaseIdent)
        , integer_literal(kInteger, Tokens::Integer)
        , float_literal(kFloat, Tokens::Float)
        , escape_sequence(kEscapeSequence)
        , char_literal(kCharLiteral)
        , string_character(kStringCharacter)
        , string_literal(kStringLiteral, Tokens::String)
        , label_name(kLowercaseIdent)
        , label(kLabel, Tokens::Label)
        , optlabel(kOptLabel, Tokens::OptLabel)
        , operator_char(kOperatorChar)
        , prefix_symbol(kPrefixSymbol, Tokens::PrefixOp)
        , infix_symbol(kInfixSymbol, Tokens::InfixOp)
        , linenum_directive(kLinenumDirective, Tokens::LinenumDir)

        //
        // Reserved keywords
        //

        , kand("and", Tokens::And)
        , kas("as", Tokens::As)
        , kassert("assert", Tokens::Assert)
        , kasr("asr", Tokens::Asr)
        , kbegin("begin", Tokens::Begin)
        , kclass("class", Tokens::Class)
        , kconstraint("constraint", Tokens::Constraint)
        , kdo("do", Tokens::Do)
        , kdone("done", Tokens::Done)
        , kdownto("downto", Tokens::Downto)
        , kelse("else", Tokens::Else)
        , kend("end", Tokens::End)
        , kexception("exception", Tokens::Exception)
        , kexternal("external", Tokens::External)
        , kfalse("false", Tokens::False)
        , kfor("for", Tokens::For)
        , kfun("fun", Tokens::Fun)
        , kfunction("function", Tokens::Function)
        , kfunctor("functor", Tokens::Functor)
        , kif("if", Tokens::If)
        , kin("in", Tokens::In)
        , kinclude("include", Tokens::Include)
        , kinherit("inherit", Tokens::Inherit)
        , kinitializer("initializer", Tokens::Initializer)
        , kland("land", Tokens::Land)
        , klazy("lazy", Tokens::Lazy)
        , klet("let", Tokens::Let)
        , klor("lor", Tokens::Lor)
        , klsl("lsl", Tokens::Lsl)
        , klsr("lsr", Tokens::Lsr)
        , klxor("lxor", Tokens::Lxor)
        , kmatch("match", Tokens::Match)
        , kmethod("method", Tokens::Method)
        , kmod("mod", Tokens::Mod)
        , kmodule("module", Tokens::Module)
        , kmutable("mutable", Tokens::Mutable)
        , knew("new", Tokens::New)
        , kobject("object", Tokens::Object)
        , kof("of", Tokens::Of)
        , kopen("open", Tokens::Open)
        , kor("or", Tokens::Or)
        , kparser("parser", Tokens::Parser)
        , kprivate("private", Tokens::Private)
        , krec("rec", Tokens::Rec)
        , ksig("sig", Tokens::Sig)
        , kstruct("struct", Tokens::Struct)
        , kthen("then", Tokens::Then)
        , kto("to", Tokens::To)
        , ktrue("true", Tokens::True)
        , ktry("try", Tokens::Try)
        , ktype("type", Tokens::Type)
        , kval("val", Tokens::Val)
        , kvalue("value", Tokens::Value)
        , kvirtual("virtual", Tokens::Virtual)
        , kwhen("when", Tokens::When)
        , kwhile("while", Tokens::While)
        , kwith("with", Tokens::With)

        //
        // Reserved sequences
        //

        , bangequal("!=", Tokens::BangEqual)
        , hash("#", Tokens::Hash)
        , ampersand("&", Tokens::Ampersand)
        , ampamp("&&", Tokens::AmpAmp)
        , apostrophe("'", Tokens::Apostrophe)
        , lbrace("\\(", Tokens::LBrace)
        , rbrace("\\)", Tokens::RBrace)
        , asterisk("\\*", Tokens::Asterisk)
        , plus("\\+", Tokens::Plus)
        , comma(",", Tokens::Comma)
        , minus("\\-", Tokens::Minus)
        , minusdot("\\-\\.", Tokens::MinusDot)
        , minusgreater("\\->", Tokens::MinusGreater)
        , dot("\\.", Tokens::Dot)
        , dotdot("\\.\\.", Tokens::DotDot)
        , colon(":", Tokens::Colon)
        , coloncolon("::", Tokens::ColonColon)
        , colonequal(":=", Tokens::ColonEqual)
        , colongreater(":>", Tokens::ColonGreater)
        , semicolon(";", Tokens::Semicolon)
        , semisemi(";;", Tokens::SemiSemi)
        , lesser("<", Tokens::Lesser)
        , lesserminus("<\\-", Tokens::LesserMinus)
        , equal("=", Tokens::Equal)
        , greater(">", Tokens::Greater)
        , greaterrbracket(">\\]", Tokens::GreaterRBracket)
        , greaterrcurly(">\\}", Tokens::GreaterRCurly)
        , question("\\?", Tokens::Question)
        , lbracket("\\[", Tokens::LBracket)
        , lbracketlesser("\\[<", Tokens::LBracketLesser)
        , lbracketgreater("\\[>", Tokens::LBracketGreater)
        , lbracketbar("\\[\\|", Tokens::LBracket)
        , rbracket("\\]", Tokens::RBracket)
        , underscore("_", Tokens::Underscore)
        , graveaccent("`", Tokens::GraveAccent)
        , lcurly("\\{", Tokens::LCurly)
        , lcurlylesser("\\{<", Tokens::LCurlyLesser)
        , bar("\\|", Tokens::Bar)
        , barrbracket("\\|\\]", Tokens::BarRBracket)
        , barbar("\\|\\|", Tokens::BarBar)
        , rcurly("\\}", Tokens::RCurly)
        , tilde("~", Tokens::Tilde)
        , dollar("\\$", Tokens::Dollar)
        , dollardollar("\\$\\$", Tokens::DollarDollar)
        , dollarcolon("\\$:", Tokens::DollarColon)
        , lessercolon("<:", Tokens::LesserColon)
        , lessless("<<", Tokens::LessLess)
        , greatgreat(">>", Tokens::GreatGreat)
        , questquest("\\?\\?", Tokens::QuestQuest)

        , level(0)
    {
        // Reserved keywords
        this->self
            = kand[echo_input(std::cout)]
            | kas[echo_input(std::cout)]
            | kassert[echo_input(std::cout)]
            | kasr[echo_input(std::cout)]
            | kbegin[echo_input(std::cout)]
            | kclass[echo_input(std::cout)]
            | kconstraint[echo_input(std::cout)]
            | kdo[echo_input(std::cout)]
            | kdone[echo_input(std::cout)]
            | kdownto[echo_input(std::cout)]
            | kelse[echo_input(std::cout)]
            | kend[echo_input(std::cout)]
            | kexception[echo_input(std::cout)]
            | kexternal[echo_input(std::cout)]
            | kfalse[echo_input(std::cout)]
            | kfor[echo_input(std::cout)]
            | kfun[echo_input(std::cout)]
            | kfunction[echo_input(std::cout)]
            | kfunctor[echo_input(std::cout)]
            | kif[echo_input(std::cout)]
            | kin[echo_input(std::cout)]
            | kinclude[echo_input(std::cout)]
            | kinherit[echo_input(std::cout)]
            | kinitializer[echo_input(std::cout)]
            | kland[echo_input(std::cout)]
            | klazy[echo_input(std::cout)]
            | klet[echo_input(std::cout)]
            | klor[echo_input(std::cout)]
            | klsl[echo_input(std::cout)]
            | klsr[echo_input(std::cout)]
            | klxor[echo_input(std::cout)]
            | kmatch[echo_input(std::cout)]
            | kmethod[echo_input(std::cout)]
            | kmod[echo_input(std::cout)]
            | kmodule[echo_input(std::cout)]
            | kmutable[echo_input(std::cout)]
            | knew[echo_input(std::cout)]
            | kobject[echo_input(std::cout)]
            | kof[echo_input(std::cout)]
            | kopen[echo_input(std::cout)]
            | kor[echo_input(std::cout)]
            | kparser[echo_input(std::cout)]
            | kprivate[echo_input(std::cout)]
            | krec[echo_input(std::cout)]
            | ksig[echo_input(std::cout)]
            | kstruct[echo_input(std::cout)]
            | kthen[echo_input(std::cout)]
            | kto[echo_input(std::cout)]
            | ktrue[echo_input(std::cout)]
            | ktry[echo_input(std::cout)]
            | ktype[echo_input(std::cout)]
            | kval[echo_input(std::cout)]
            | kvalue[echo_input(std::cout)]
            | kvirtual[echo_input(std::cout)]
            | kwhen[echo_input(std::cout)]
            | kwhile[echo_input(std::cout)]
            | kwith[echo_input(std::cout)];

        // Reserved sequences
        this->self
            += bangequal[echo_input(std::cout)]
            | hash[echo_input(std::cout)]
            | ampersand[echo_input(std::cout)]
            | ampamp[echo_input(std::cout)]
            | apostrophe[echo_input(std::cout)]
            | lbrace[echo_input(std::cout)]
            | rbrace[echo_input(std::cout)]
            | asterisk[echo_input(std::cout)]
            | plus[echo_input(std::cout)]
            | comma[echo_input(std::cout)]
            | minus[echo_input(std::cout)]
            | minusdot[echo_input(std::cout)]
            | minusgreater[echo_input(std::cout)]
            | dot[echo_input(std::cout)]
            | dotdot[echo_input(std::cout)]
            | colon[echo_input(std::cout)]
            | coloncolon[echo_input(std::cout)]
            | colonequal[echo_input(std::cout)]
            | colongreater[echo_input(std::cout)]
            | semicolon[echo_input(std::cout)]
            | semisemi[echo_input(std::cout)]
            | lesser[echo_input(std::cout)]
            | lesserminus[echo_input(std::cout)]
            | equal[echo_input(std::cout)]
            | greater[echo_input(std::cout)]
            | greaterrbracket[echo_input(std::cout)]
            | greaterrcurly[echo_input(std::cout)]
            | question[echo_input(std::cout)]
            | lbracket[echo_input(std::cout)]
            | lbracketlesser[echo_input(std::cout)]
            | lbracketgreater[echo_input(std::cout)]
            | lbracketbar[echo_input(std::cout)]
            | rbracket[echo_input(std::cout)]
            | underscore[echo_input(std::cout)]
            | graveaccent[echo_input(std::cout)]
            | lcurly[echo_input(std::cout)]
            | lcurlylesser[echo_input(std::cout)]
            | bar[echo_input(std::cout)]
            | barrbracket[echo_input(std::cout)]
            | barbar[echo_input(std::cout)]
            | rcurly[echo_input(std::cout)]
            | tilde[echo_input(std::cout)]
            | dollar[echo_input(std::cout)]
            | dollardollar[echo_input(std::cout)]
            | dollarcolon[echo_input(std::cout)]
            | lessercolon[echo_input(std::cout)]
            | lessless[echo_input(std::cout)]
            | greatgreat[echo_input(std::cout)]
            | questquest[echo_input(std::cout)];

        // Infix and prefix symbols must be defined after reserved sequences
        this->self
            += infix_symbol[echo_input(std::cout)]
            | prefix_symbol[echo_input(std::cout)];

        this->self
            += capitalized_ident[echo_input(std::cout)]
            | lowercase_ident[echo_input(std::cout)]
            | ident[echo_input(std::cout)]
            | integer_literal[echo_input(std::cout)]
            | float_literal[echo_input(std::cout)]
            | char_literal[echo_input(std::cout)]
            | string_literal[echo_input(std::cout)]
            | label[echo_input(std::cout)]
            | optlabel[echo_input(std::cout)]
            | linenum_directive[echo_input(std::cout)];


        // Ignore whitespace
        this->self += blank[lex::_pass = lex::pass_flags::pass_ignore];

        // Comments
        this->self += comment_begin[set_lexer_state("COMMENT", level = 0)];
        this->self("COMMENT")
            = comment_begin2[set_lexer_state("COMMENT", ++level)]
            | comment_any
            | comment_blank
            | comment_end[set_lexer_state("INITIAL", --level)];
    }

    lexer_token_def<lex::omit> blank;

    lexer_token_def<> comment_begin, comment_begin2, comment_end,
        comment_blank, comment_any;

    lexer_token_def<> lowercase_letter, uppercase_letter, letter,
        ident, capitalized_ident, lowercase_ident, integer_literal,
        float_literal, escape_sequence, char_literal, string_character,
        string_literal, label_name, label, optlabel, operator_char,
        prefix_symbol, infix_symbol, linenum_directive;

    lexer_token_def<> kand, kas, kassert, kasr, kbegin, kclass, kconstraint,
        kdo, kdone, kdownto, kelse, kend, kexception, kexternal, kfalse,
        kfor, kfun, kfunction, kfunctor, kif, kin, kinclude, kinherit,
        kinitializer, kland, klazy, klet, klor, klsl, klsr, klxor, kmatch,
        kmethod, kmod, kmodule, kmutable, knew, kobject, kof, kopen, kor,
        kparser, kprivate, krec, ksig, kstruct, kthen, kto, ktrue, ktry, ktype,
        kval, kvalue, kvirtual, kwhen, kwhile, kwith;

    lexer_token_def<> bangequal, hash, ampersand, ampamp, apostrophe, lbrace,
        rbrace, asterisk, plus, comma, minus, minusdot, minusgreater, dot,
        dotdot, colon, coloncolon, colonequal, colongreater, semicolon, semisemi,
        lesser, lesserminus, equal, greater, greaterrbracket, greaterrcurly,
        question, lbracket, lbracketlesser, lbracketgreater, lbracketbar, rbracket,
        underscore, graveaccent, lcurly, lcurlylesser, bar, barrbracket, barbar,
        rcurly, tilde, dollar, dollardollar, dollarcolon, lessercolon, lessless,
        greatgreat, questquest;

private:
    int level;
};

} // namespace lexer
} // namespace ocaml

#endif //FLANG_OCAMLLEXER_H
