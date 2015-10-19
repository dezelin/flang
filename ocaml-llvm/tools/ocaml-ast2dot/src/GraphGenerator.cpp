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

#include "GraphGenerator.h"

#include <string>
#include <type_traits>

namespace OCaml
{

class GraphGeneratorPriv
{
public:
    GraphGeneratorPriv(GraphGenerator *q);
    GraphGeneratorPriv(GraphGenerator *q, Graph& graph);
    GraphGeneratorPriv(GraphGeneratorPriv const& other);

    Graph* getGraph() const;

    //
    // Lexical
    //
    bool generate(ocaml::ast::capitalized_ident const& ident,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::lowercase_ident const& ident,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::ident const& ident,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::ident_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::label_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::label const& label,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::optlabel const& label,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::integer_literal const& integer_lit,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::float_literal const& float_lit,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::char_literal const& char_lit,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::string_literal const& string_lit,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::lexer::Tokens tok,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;

    //
    // Names
    //
    bool generate(ocaml::ast::value_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::operator_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::infix_op const& op,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::constr_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::typeconstr_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::field_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::module_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::module_name_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::modtype_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::class_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::inst_var_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::method_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::value_path const& path,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::constr const& constr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::typeconstr const& constr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::field const& field,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::modtype_path const& path,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::class_path const& path,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::classtype_path const& path,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::module_path const& path,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::extended_module_path const& path,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::extended_module_path_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::extended_module_name const& name,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::extended_module_name_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;

    //
    // Type expressions
    //
    bool generate(ocaml::ast::typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::typexpr_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::function_types_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::function_types_typexpr_label const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tuple_types_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::function_types_typexpr_rr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::constructed_types_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tuple_types_typexpr_rr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::aliased_types_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::constructed_types_typexpr_rr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::abbreviation_types_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::aliased_types_typexpr_rr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::non_rr_types_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::abbreviation_types_typexpr_rr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::anon_type_variable_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::parenthesized_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::constructed_nary_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_of const& spec,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_of_list_expr_list const& tag,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_of_list const& tag,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_or const& spec,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_first const& spec,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec const& spec,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_full const& spec,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_spec_full_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::tag_name_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::exact_variant_type const& var,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::opened_variant_type const& var,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::closed_variant_type const& var,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::polymorphic_variant_type const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::object_row_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::explicit_polymorphic_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::polymorphic_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::method_type const& type,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::method_type_list const& list,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::object_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::octothorpe_class_path_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::octothorpe_list_typexpr const& expr,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;

    //
    // Constants
    //
    bool generate(ocaml::ast::const_false const& constant,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::const_true const& constant,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::const_unit const& constant,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::const_empty_record const& constant,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::const_empty_list const& constant,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::const_empty_array const& constant,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;
    bool generate(ocaml::ast::constant const& constant,
        Graph::VertexId parentId = -1,
        std::string const& parentEdgeName = "") const;

private:
    GraphGenerator *_q;
    Graph *_graph;
};

GraphGenerator::GraphGenerator()
    : _p(new GraphGeneratorPriv(this))
{
}

GraphGenerator::GraphGenerator(Graph& graph)
    : _p(new GraphGeneratorPriv(this, graph))
{
}

GraphGenerator::~GraphGenerator()
{
}

GraphGenerator::GraphGenerator(const GraphGenerator& other)
{
    _p.reset(new GraphGeneratorPriv(*other._p));
}

GraphGenerator::GraphGenerator(GraphGenerator&& other)
    : GraphGenerator()
{
    swap(other);
}

GraphGenerator& GraphGenerator::operator =(GraphGenerator other)
{
    swap(other);
    return *this;
}

void GraphGenerator::swap(GraphGenerator& other)
{
    std::swap(_p, other._p);
}

bool GraphGenerator::operator()(
    ocaml::ast::capitalized_ident const& ident) const
    {
    return _p->generate(ident);
}

bool GraphGenerator::operator()(
    ocaml::ast::lowercase_ident const& ident) const
    {
    return _p->generate(ident);
}

bool GraphGenerator::operator()(
    ocaml::ast::ident const& ident) const
    {
    return _p->generate(ident);
}

bool GraphGenerator::operator()(
    ocaml::ast::label_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(
    ocaml::ast::label const& label) const
    {
    return _p->generate(label);
}

bool GraphGenerator::operator()(
    ocaml::ast::optlabel const& label) const
    {
    return _p->generate(label);
}

bool GraphGenerator::operator()(
    ocaml::ast::integer_literal const& integer_lit) const
    {
    return _p->generate(integer_lit);
}

bool GraphGenerator::operator()(
    ocaml::ast::float_literal const& float_lit) const
    {
    return _p->generate(float_lit);
}

bool GraphGenerator::operator()(
    ocaml::ast::char_literal const& char_lit) const
    {
    return _p->generate(char_lit);
}

bool GraphGenerator::operator()(
    ocaml::ast::string_literal const& string_lit) const
    {
    return _p->generate(string_lit);
}

bool GraphGenerator::operator()(ocaml::ast::value_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::operator_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::infix_op const& op) const
    {
    return _p->generate(op);
}

bool GraphGenerator::operator()(ocaml::ast::constr_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::tag_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::typeconstr_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::field_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::module_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::module_name_list const& list) const
    {
    return _p->generate(list);
}

bool GraphGenerator::operator()(ocaml::ast::modtype_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::class_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::inst_var_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::method_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::value_path const& path) const
    {
    return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::constr const& constr) const
    {
    return _p->generate(constr);
}

bool GraphGenerator::operator()(ocaml::ast::typeconstr const& constr) const
    {
    return _p->generate(constr);
}

bool GraphGenerator::operator()(ocaml::ast::field const& field) const
    {
    return _p->generate(field);
}

bool GraphGenerator::operator()(ocaml::ast::modtype_path const& path) const
    {
    return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::class_path const& path) const
    {
    return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::classtype_path const& path) const
    {
    return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::module_path const& path) const
    {
    return _p->generate(path);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_path const& path) const
    {
    return _p->generate(path);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_path_list const& list) const
    {
    return _p->generate(list);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_name const& name) const
    {
    return _p->generate(name);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_name_list const& list) const
    {
    return _p->generate(list);
}

bool GraphGenerator::operator()(
    ocaml::ast::typexpr const& expr) const
    {
    return _p->generate(expr);
}

bool GraphGenerator::operator()(
    ocaml::ast::constant const& constant) const
    {
    return _p->generate(constant);
}

//
// Private implementation
//

GraphGeneratorPriv::GraphGeneratorPriv(GraphGenerator* q)
    : _q(q), _graph(nullptr)
{
}

GraphGeneratorPriv::GraphGeneratorPriv(GraphGenerator* q, Graph& graph)
    : _q(q), _graph(&graph)
{
}

GraphGeneratorPriv::GraphGeneratorPriv(const GraphGeneratorPriv& other)
{
    _q = other._q;
    _graph = other._graph;
}

Graph* GraphGeneratorPriv::getGraph() const
{
    return _graph;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::capitalized_ident const& ident,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::capitalized_ident");
    v.addProperty("name", ident.name);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::lowercase_ident const& ident,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::lowercase_ident");
    v.addProperty("name", ident.name);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::ident const& ident, Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::ident");
    v.addProperty("name", ident.name);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::ident_list const& list,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::ident_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::ident const& ident : list)
        generate(ident, v.getId(), std::to_string(i++));
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::label_name const& name, Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::label_name");
    v.addProperty("name", name.name);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::label const& label, Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::label");
    v.addProperty("name", label.name);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::optlabel const& label, Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::optlabel");
    v.addProperty("name", label.name);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::integer_literal const& integer_lit,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::integer_literal");
    v.addProperty("val", integer_lit.val);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::float_literal const& float_lit,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::float_literal");
    v.addProperty("val", float_lit.val);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::char_literal const& char_lit,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::char_literal");
    v.addProperty("val", char_lit.val);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::string_literal const& string_lit,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::string_literal");
    v.addProperty("val", string_lit.val);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::lexer::Tokens tok,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::lexer::Tokens");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;
    std::string val = std::to_string(static_cast<utype>(tok));
    v.addProperty("token", val);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

struct VariantGeneratorVisitor: public boost::static_visitor<>
{
    VariantGeneratorVisitor(GraphGeneratorPriv const *gg,
        Graph::VertexId parentId)
        : _gg(gg), _parentId(parentId)
    {
    }

    template<typename T>
    void operator()(T& field) const
        {
        _gg->generate(field, _parentId);
    }

    GraphGeneratorPriv const *_gg;
    Graph::VertexId _parentId;
};

bool GraphGeneratorPriv::generate(ocaml::ast::value_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::value_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), name);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::operator_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::operator_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), name);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::infix_op const& op,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::infix_op");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), op);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::constr_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::constr_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::typeconstr_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::typeconstr_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::field_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::field_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::module_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::module_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::module_name_list const& list,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::module_name_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::module_name const& module_name : list)
        generate(module_name, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::modtype_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::modtype_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::class_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::class_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::inst_var_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::inst_var_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::method_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::method_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::value_path const& path,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::value_path");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    if (path.path.is_initialized())
        generate(path.path.get(), v.getId(), "path");

    return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::constr const& constr,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::constr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    if (constr.path.is_initialized())
        generate(constr.path.get(), v.getId(), "path");

    return generate(constr.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::typeconstr const& constr,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::typeconstr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    if (constr.path.is_initialized())
        generate(constr.path.get(), v.getId(), "path");

    return generate(constr.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::field const& field,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::field");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    if (field.path.is_initialized())
        generate(field.path.get(), v.getId(), "path");

    return generate(field.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::modtype_path const& path,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::modtype_path");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    if (path.path.is_initialized())
        generate(path.path.get(), v.getId(), "path");

    return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::class_path const& path,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::class_path");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    if (path.path.is_initialized())
        generate(path.path.get(), v.getId(), "path");

    return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::classtype_path const& path,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::classtype_path");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    if (path.path.is_initialized())
        generate(path.path.get(), v.getId(), "path");

    return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::module_path const& path,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::module_path");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    generate(path.name, v.getId(), "name");
    if (path.other.is_initialized())
        generate(path.other.get(), v.getId(), "other");

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::extended_module_path const& path,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::extended_module_path");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    generate(path.name, v.getId(), "name");
    if (path.other.is_initialized())
        generate(path.other.get(), v.getId(), "other");

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::extended_module_path_list const& list,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::extended_module_path_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::extended_module_path const& path : list)
        generate(path, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::extended_module_name const& name,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::extended_module_name");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);

    generate(name.name, v.getId(), "name");
    if (name.paths.is_initialized())
        generate(name.paths.get(), v.getId(), "paths");

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::extended_module_name_list const& list,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::extended_module_name_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::extended_module_name const& name : list)
        generate(name, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::typexpr const& expr,
    Graph::VertexId parentId, std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), expr);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::typexpr_list const& list,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::typexpr_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::typexpr const& expr : list)
        generate(expr, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::function_types_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::function_types_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    if (expr.label.is_initialized())
        generate(expr.label.get(), v.getId(), "label");

    generate(expr.tuple_expr.get(), v.getId(), "tuple_expr");
    generate(expr.function_expr_rr.get(), v.getId(), "function_expr_rr");
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::function_types_typexpr_label const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::function_types_typexpr_label");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), expr);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tuple_types_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tuple_types_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.constructed_expr.get(), v.getId(), "constructed_expr");
    generate(expr.tuple_expr_rr.get(), v.getId(), "tuple_expr_rr");
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::function_types_typexpr_rr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::function_types_typexpr_rr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    if (expr.expr)
        generate(*expr.expr, v.getId(), "expr");

    if (expr.function_expr_rr)
        generate(*expr.function_expr_rr, v.getId(), "function_expr_rr");

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::constructed_types_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::constructed_types_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.aliased_expr.get(), v.getId(), "aliased_expr");
    generate(expr.constructed_expr_rr.get(), v.getId(), "constructed_expr_rr");
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::tuple_types_typexpr_rr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tuple_types_typexpr_rr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.exprList, v.getId(), "exprList");
    if (expr.tuple_expr_rr)
        generate(*expr.tuple_expr_rr, v.getId(), "tuple_expr_rr");

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::aliased_types_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::aliased_types_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.abbreviation_expr.get(), v.getId(), "abbreviation_expr");
    generate(expr.aliased_expr_rr.get(), v.getId(), "aliased_expr_rr");
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::constructed_types_typexpr_rr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::constructed_types_typexpr_rr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.constr, v.getId(), "constr");
    if (expr.constructed_expr_rr)
        generate(*expr.constructed_expr_rr, v.getId(), "constructed_expr_rr");

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::abbreviation_types_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::abbreviation_types_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.non_rr_expr.get(), v.getId(), "non_rr_expr");
    generate(expr.abbreviation_expr_rr.get(), v.getId(),
        "abbreviation_expr_rr");
    return true;

}

bool GraphGeneratorPriv::generate(
    ocaml::ast::aliased_types_typexpr_rr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::aliased_types_typexpr_rr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr._ident, v.getId(), "_ident");
    if (expr.aliased_expr_rr)
        generate(*expr.aliased_expr_rr, v.getId(), "aliased_expr_rr");

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::non_rr_types_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::non_rr_types_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), expr);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::abbreviation_types_typexpr_rr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::abbreviation_types_typexpr_rr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.path, v.getId(), "path");
    if (expr.abbreviation_expr_rr)
        generate(*expr.abbreviation_expr_rr, v.getId(), "abbreviation_expr_rr");

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::anon_type_variable_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::anon_type_variable_typexpr");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;
    std::string var = std::to_string(static_cast<utype>(expr.var));
    v.addProperty("var", var);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::parenthesized_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::parenthesized_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.expr, v.getId(), "expr");
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::constructed_nary_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::constructed_nary_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.expr, v.getId(), "expr");
    if (expr.other.is_initialized())
        generate(expr.other.get(), v.getId(), "other");

    generate(expr.constr, v.getId(), "constr");
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec_of const& spec,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_of");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(spec.name, v.getId(), "name");
    if (spec.expr.is_initialized())
        generate(spec.expr.get(), v.getId(), "expr");

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::tag_spec_of_list_expr_list const& tag,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_of_list_expr_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(tag.expr, v.getId(), "expr");
    if (tag.other.is_initialized())
        generate(tag.other.get(), v.getId(), "other");

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec_of_list const& spec,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_of_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(spec.name, v.getId(), "name");
    if (spec.exprList.is_initialized())
        generate(spec.exprList.get(), v.getId(), "exprList");

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec_or const& spec,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_or");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    if (spec.expr.is_initialized())
        generate(spec.expr.get(), v.getId(), "expr");

    return generate(spec.tag, v.getId(), "tag");
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec_first const& spec,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_first");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), spec);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec_list const& list,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::tag_spec const& spec : list)
        generate(spec, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec const& spec,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), spec);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec_full const& spec,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_full");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), spec);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_spec_full_list const& list,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_spec_full_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::tag_spec_full const& spec : list)
        generate(spec, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_name_list const& list,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::tag_name_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::tag_name const& name : list)
        generate(name, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::exact_variant_type const& var,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::exact_variant_type");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(var.first, v.getId(), "first");
    if (var.other.is_initialized())
        generate(var.other.get(), v.getId(), "other");
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::opened_variant_type const& var,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::opened_variant_type");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    if (var.first.is_initialized())
        generate(var.first.get(), v.getId(), "first");

    if (var.other.is_initialized())
        generate(var.other.get(), v.getId(), "other");
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::closed_variant_type const& var,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::closed_variant_type");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(var.first, v.getId(), "first");
    if (var.other.is_initialized())
        generate(var.other.get(), v.getId(), "other");

    if (var.tags.is_initialized())
        generate(var.tags.get(), v.getId(), "tags");
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::polymorphic_variant_type const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::polymorphic_variant_type");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), expr);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::object_row_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::object_row_typexpr");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;
    std::string ellipsis = std::to_string(static_cast<utype>(expr.ellipsis));
    v.addProperty("ellipsis", ellipsis);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::explicit_polymorphic_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::explicit_polymorphic_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    if (expr.identList.is_initialized())
        generate(expr.identList.get(), v.getId(), "identList");

    generate(expr.expr, v.getId(), "expr");
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::polymorphic_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::polymorphic_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), expr);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::method_type const& type,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::method_type");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(type.name, v.getId(), "name");
    generate(type.expr, v.getId(), "expr");
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::method_type_list const& list,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::method_type_list");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    int i = 0;
    for (ocaml::ast::method_type const& type : list)
        generate(type, v.getId(), std::to_string(i++));

    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::object_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::object_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.type, v.getId(), "type");
    if (expr.other.is_initialized())
        generate(expr.other.get(), v.getId(), "other");

    if (expr.ellipsis.is_initialized()) {
        typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;
        std::string ellipsis = std::to_string(
            static_cast<utype>(expr.ellipsis.get()));
        v.addProperty("ellipsis", ellipsis);
    }

    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::octothorpe_class_path_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::octothorpe_class_path_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.path, v.getId(), "path");
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::octothorpe_list_typexpr const& expr,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::octothorpe_list_typexpr");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    generate(expr.expr, v.getId(), "expr");
    if (expr.other.is_initialized())
        generate(expr.other.get(), v.getId(), "other");

    generate(expr.path, v.getId(), "path");
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::const_false const& constant,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::const_false");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;

    std::string false_ = std::to_string(static_cast<utype>(constant.false_));
    v.addProperty("false_", false_);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::const_true const& constant,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::const_true");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;

    std::string true_ = std::to_string(static_cast<utype>(constant.true_));
    v.addProperty("true_", true_);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::const_unit const& constant,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::const_unit");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;

    std::string opened = std::to_string(static_cast<utype>(constant.opened));
    v.addProperty("opened", opened);
    std::string closed = std::to_string(static_cast<utype>(constant.closed));
    v.addProperty("closed", closed);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::const_empty_record const& constant,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::const_empty_record");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;

    std::string begin = std::to_string(static_cast<utype>(constant.begin));
    v.addProperty("begin", begin);
    std::string end = std::to_string(static_cast<utype>(constant.end));
    v.addProperty("end", end);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::const_empty_list const& constant,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::const_empty_list");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;

    std::string opened = std::to_string(static_cast<utype>(constant.opened));
    v.addProperty("opened", opened);
    std::string closed = std::to_string(static_cast<utype>(constant.closed));
    v.addProperty("closed", closed);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::const_empty_array const& constant,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::const_empty_array");

    typedef std::underlying_type<ocaml::lexer::Tokens>::type utype;

    std::string opened = std::to_string(static_cast<utype>(constant.opened));
    v.addProperty("opened", opened);
    std::string closed = std::to_string(static_cast<utype>(constant.closed));
    v.addProperty("closed", closed);

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    return true;
}

bool GraphGeneratorPriv::generate(
    ocaml::ast::constant const& constant,
    Graph::VertexId parentId,
    std::string const& parentEdgeName) const
    {
    Graph::Vertex v = Graph::Vertex::create();
    v.addProperty("type", "ocaml::ast::constant");

    Graph::EdgeList edges;
    if (parentId != -1)
        edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

    _graph->addVertex(v, edges);
    boost::apply_visitor(
        VariantGeneratorVisitor(this, v.getId()), constant);
    return true;
}

} /* namespace OCaml */
