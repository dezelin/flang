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

#ifndef GRAPHGENERATOR_H_
#define GRAPHGENERATOR_H_

#include "Graph.h"

#include <ocamlast.h>

#include <memory>

namespace OCaml
{

class GraphGeneratorPriv;
class GraphGenerator
{
public:
    GraphGenerator();
    GraphGenerator(Graph& graph);
    GraphGenerator(GraphGenerator const& other);
    GraphGenerator(GraphGenerator&& other);
    virtual ~GraphGenerator();

    GraphGenerator& operator =(GraphGenerator other);
    void swap(GraphGenerator& other);

    //
    // Lexical
    //
    bool operator()(ocaml::ast::capitalized_ident const& ident) const;
    bool operator()(ocaml::ast::lowercase_ident const& ident) const;
    bool operator()(ocaml::ast::ident const& ident) const;
    bool operator()(ocaml::ast::label_name const& name) const;
    bool operator()(ocaml::ast::label const& label) const;
    bool operator()(ocaml::ast::optlabel const& label) const;
    bool operator()(ocaml::ast::integer_literal const& integer_lit) const;
    bool operator()(ocaml::ast::float_literal const& float_lit) const;
    bool operator()(ocaml::ast::char_literal const& char_lit) const;
    bool operator()(ocaml::ast::string_literal const& string_lit) const;

    //
    // Names
    //
    bool operator()(ocaml::ast::value_name const& name) const;
    bool operator()(ocaml::ast::operator_name const& name) const;
    bool operator()(ocaml::ast::infix_op const& op) const;
    bool operator()(ocaml::ast::constr_name const& name) const;
    bool operator()(ocaml::ast::tag_name const& name) const;
    bool operator()(ocaml::ast::typeconstr_name const& name) const;
    bool operator()(ocaml::ast::field_name const& name) const;
    bool operator()(ocaml::ast::module_name const& name) const;
    bool operator()(ocaml::ast::module_name_list const& list) const;
    bool operator()(ocaml::ast::modtype_name const& name) const;
    bool operator()(ocaml::ast::class_name const& name) const;
    bool operator()(ocaml::ast::inst_var_name const& name) const;
    bool operator()(ocaml::ast::method_name const& name) const;
    bool operator()(ocaml::ast::value_path const& path) const;
    bool operator()(ocaml::ast::constr const& constr) const;
    bool operator()(ocaml::ast::typeconstr const& constr) const;
    bool operator()(ocaml::ast::field const& field) const;
    bool operator()(ocaml::ast::modtype_path const& path) const;
    bool operator()(ocaml::ast::class_path const& path) const;
    bool operator()(ocaml::ast::classtype_path const& path) const;
    bool operator()(ocaml::ast::module_path const& path) const;
    bool operator()(ocaml::ast::extended_module_path const& path) const;
    bool operator()(ocaml::ast::extended_module_path_list const& list) const;
    bool operator()(ocaml::ast::extended_module_name const& name) const;
    bool operator()(ocaml::ast::extended_module_name_list const& list) const;

    //
    // Type expressions
    //
    bool operator()(ocaml::ast::typexpr const& expr) const;

private:
    std::unique_ptr<GraphGeneratorPriv> _p;
};

} /* namespace OCaml */

#endif /* GRAPHGENERATOR_H_ */
