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

#ifndef PARSER_H_
#define PARSER_H_

#include <ocamlast.h>

#include <memory>

namespace OCaml
{

class ParserPriv;
class Parser
{
public:
    Parser();
    Parser(Parser const& other);
    Parser(Parser&& other);
    virtual ~Parser();

    Parser& operator =(Parser other);

    void swap(Parser& other);

    //
    // Lexical
    //
    bool parse(std::string const& content,
        ocaml::ast::capitalized_ident& ident);
    bool parse(std::string const& content, ocaml::ast::lowercase_ident& ident);
    bool parse(std::string const& content, ocaml::ast::ident& ident);
    bool parse(std::string const& content, ocaml::ast::label_name& name);
    bool parse(std::string const& content, ocaml::ast::label& label);
    bool parse(std::string const& content, ocaml::ast::optlabel& label);
    bool parse(std::string const& content,
        ocaml::ast::integer_literal& integer_lit);
    bool parse(std::string const& content,
        ocaml::ast::float_literal& float_lit);
    bool parse(std::string const& content, ocaml::ast::char_literal& char_lit);
    bool parse(std::string const& content,
        ocaml::ast::string_literal& string_lit);

    //
    // Names
    //
    bool parse(std::string const& content, ocaml::ast::value_name& name);
    bool parse(std::string const& content, ocaml::ast::operator_name& name);
    bool parse(std::string const& content, ocaml::ast::infix_op& op);
    bool parse(std::string const& content, ocaml::ast::constr_name& name);
    bool parse(std::string const& content, ocaml::ast::tag_name& name);
    bool parse(std::string const& content, ocaml::ast::typeconstr_name& name);
    bool parse(std::string const& content, ocaml::ast::field_name& name);
    bool parse(std::string const& content, ocaml::ast::module_name& name);
    bool parse(std::string const& content, ocaml::ast::modtype_name& name);
    bool parse(std::string const& content, ocaml::ast::class_name& name);
    bool parse(std::string const& content, ocaml::ast::inst_var_name& name);
    bool parse(std::string const& content, ocaml::ast::method_name& name);
    bool parse(std::string const& content, ocaml::ast::value_path& path);
    bool parse(std::string const& content, ocaml::ast::constr& constr);
    bool parse(std::string const& content, ocaml::ast::typeconstr& constr);
    bool parse(std::string const& content, ocaml::ast::field& field);
    bool parse(std::string const& content, ocaml::ast::modtype_path& path);
    bool parse(std::string const& content, ocaml::ast::class_path& path);
    bool parse(std::string const& content, ocaml::ast::classtype_path& path);
    bool parse(std::string const& content, ocaml::ast::module_path& path);
    bool parse(std::string const& content,
        ocaml::ast::extended_module_path& path);
    bool parse(std::string const& content,
        ocaml::ast::extended_module_name& name);

    //
    // Type expressions
    //
    bool parse(std::string const& content, ocaml::ast::typexpr& expr);

    //
    // Constants
    //
    bool parse(std::string const& content, ocaml::ast::constant& constant);

private:
    std::unique_ptr<ParserPriv> _p;
};

} /* namespace OCaml */

#endif /* PARSER_H_ */
