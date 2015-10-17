//  All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are met:
//
//  1. Redistributions of source code must retain the above copyright notice,
//  this
//     list of conditions and the following disclaimer.
//  2. Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
//  AND
//  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
//  IMPLIED
//  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
//  FOR
//  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
//  DAMAGES
//  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
//  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
//  THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#include "GraphGenerator.h"

#include <string>

namespace OCaml {

class GraphGeneratorPriv {
 public:
  GraphGeneratorPriv(GraphGenerator* q);
  GraphGeneratorPriv(GraphGenerator* q, Graph& graph);
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
  bool generate(ocaml::ast::ident const& ident, Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::label_name const& name,
                Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::label const& label, Graph::VertexId parentId = -1,
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

  //
  // Names
  //
  bool generate(ocaml::ast::value_name const& name,
                Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::operator_name const& name,
                Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::infix_op const& op, Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::constr_name const& name,
                Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::tag_name const& name, Graph::VertexId parentId = -1,
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
  bool generate(ocaml::ast::constr const& constr, Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::typeconstr const& constr,
                Graph::VertexId parentId = -1,
                std::string const& parentEdgeName = "") const;
  bool generate(ocaml::ast::field const& field, Graph::VertexId parentId = -1,
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

 private:
  GraphGenerator* _q;
  Graph* _graph;
};

GraphGenerator::GraphGenerator() : _p(new GraphGeneratorPriv(this)) {}

GraphGenerator::GraphGenerator(Graph& graph)
    : _p(new GraphGeneratorPriv(this, graph)) {}

GraphGenerator::~GraphGenerator() {}

GraphGenerator::GraphGenerator(const GraphGenerator& other) {
  _p.reset(new GraphGeneratorPriv(*other._p));
}

GraphGenerator::GraphGenerator(GraphGenerator&& other) : GraphGenerator() {
  swap(other);
}

GraphGenerator& GraphGenerator::operator=(GraphGenerator other) {
  swap(other);
  return *this;
}

void GraphGenerator::swap(GraphGenerator& other) { std::swap(_p, other._p); }

bool GraphGenerator::operator()(
    ocaml::ast::capitalized_ident const& ident) const {
  return _p->generate(ident);
}

bool GraphGenerator::operator()(
    ocaml::ast::lowercase_ident const& ident) const {
  return _p->generate(ident);
}

bool GraphGenerator::operator()(ocaml::ast::ident const& ident) const {
  return _p->generate(ident);
}

bool GraphGenerator::operator()(ocaml::ast::label_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::label const& label) const {
  return _p->generate(label);
}

bool GraphGenerator::operator()(ocaml::ast::optlabel const& label) const {
  return _p->generate(label);
}

bool GraphGenerator::operator()(
    ocaml::ast::integer_literal const& integer_lit) const {
  return _p->generate(integer_lit);
}

bool GraphGenerator::operator()(
    ocaml::ast::float_literal const& float_lit) const {
  return _p->generate(float_lit);
}

bool GraphGenerator::operator()(
    ocaml::ast::char_literal const& char_lit) const {
  return _p->generate(char_lit);
}

bool GraphGenerator::operator()(
    ocaml::ast::string_literal const& string_lit) const {
  return _p->generate(string_lit);
}

bool GraphGenerator::operator()(ocaml::ast::value_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::operator_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::infix_op const& op) const {
  return _p->generate(op);
}

bool GraphGenerator::operator()(ocaml::ast::constr_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::tag_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::typeconstr_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::field_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::module_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(
    ocaml::ast::module_name_list const& list) const {
  return _p->generate(list);
}

bool GraphGenerator::operator()(ocaml::ast::modtype_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::class_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::inst_var_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::method_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(ocaml::ast::value_path const& path) const {
  return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::constr const& constr) const {
  return _p->generate(constr);
}

bool GraphGenerator::operator()(ocaml::ast::typeconstr const& constr) const {
  return _p->generate(constr);
}

bool GraphGenerator::operator()(ocaml::ast::field const& field) const {
  return _p->generate(field);
}

bool GraphGenerator::operator()(ocaml::ast::modtype_path const& path) const {
  return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::class_path const& path) const {
  return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::classtype_path const& path) const {
  return _p->generate(path);
}

bool GraphGenerator::operator()(ocaml::ast::module_path const& path) const {
  return _p->generate(path);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_path const& path) const {
  return _p->generate(path);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_path_list const& list) const {
  return _p->generate(list);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_name const& name) const {
  return _p->generate(name);
}

bool GraphGenerator::operator()(
    ocaml::ast::extended_module_name_list const& list) const {
  return _p->generate(list);
}

//
// Private implementation
//

GraphGeneratorPriv::GraphGeneratorPriv(GraphGenerator* q)
    : _q(q), _graph(nullptr) {}

GraphGeneratorPriv::GraphGeneratorPriv(GraphGenerator* q, Graph& graph)
    : _q(q), _graph(&graph) {}

GraphGeneratorPriv::GraphGeneratorPriv(const GraphGeneratorPriv& other) {
  _q = other._q;
  _graph = other._graph;
}

Graph* GraphGeneratorPriv::getGraph() const { return _graph; }

bool GraphGeneratorPriv::generate(ocaml::ast::capitalized_ident const& ident,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::capitalized_ident");
  v.addProperty("name", ident.name);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::lowercase_ident const& ident,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::lowercase_ident");
  v.addProperty("name", ident.name);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::ident const& ident,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::ident");
  v.addProperty("name", ident.name);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::label_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::label_name");
  v.addProperty("name", name.name);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::label const& label,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::label");
  v.addProperty("name", label.name);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::optlabel const& label,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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
    ocaml::ast::integer_literal const& integer_lit, Graph::VertexId parentId,
    std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::integer_literal");
  v.addProperty("val", integer_lit.val);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::float_literal const& float_lit,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::float_literal");
  v.addProperty("val", float_lit.val);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::char_literal const& char_lit,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::char_literal");
  v.addProperty("val", char_lit.val);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::string_literal const& string_lit,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::string_literal");
  v.addProperty("val", string_lit.val);

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return true;
}

struct VariantGeneratorVisitor : public boost::static_visitor<> {
  VariantGeneratorVisitor(GraphGeneratorPriv const* gg,
                          Graph::VertexId parentId)
      : _gg(gg), _parentId(parentId) {}

  template <typename T>
  void operator()(T& field) const {
    _gg->generate(field, _parentId);
  }

  GraphGeneratorPriv const* _gg;
  Graph::VertexId _parentId;
};

bool GraphGeneratorPriv::generate(ocaml::ast::value_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::value_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  boost::apply_visitor(VariantGeneratorVisitor(this, v.getId()), name);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::operator_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::operator_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  boost::apply_visitor(VariantGeneratorVisitor(this, v.getId()), name);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::infix_op const& op,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::infix_op");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  boost::apply_visitor(VariantGeneratorVisitor(this, v.getId()), op);
  return true;
}

bool GraphGeneratorPriv::generate(ocaml::ast::constr_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::constr_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::tag_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::tag_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::typeconstr_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::typeconstr_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::field_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::field_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::module_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::module_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::module_name_list const& list,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::modtype_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::class_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::class_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::inst_var_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::inst_var_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::method_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::method_name");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);
  return generate(name.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::value_path const& path,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::value_path");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);

  if (path.path.is_initialized()) generate(path.path.get(), v.getId(), "path");

  return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::constr const& constr,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::modtype_path");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);

  if (path.path.is_initialized()) generate(path.path.get(), v.getId(), "path");

  return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::class_path const& path,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::class_path");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);

  if (path.path.is_initialized()) generate(path.path.get(), v.getId(), "path");

  return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::classtype_path const& path,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
  Graph::Vertex v = Graph::Vertex::create();
  v.addProperty("type", "ocaml::ast::classtype_path");

  Graph::EdgeList edges;
  if (parentId != -1)
    edges.push_back(Graph::Edge(parentId, v.getId(), parentEdgeName));

  _graph->addVertex(v, edges);

  if (path.path.is_initialized()) generate(path.path.get(), v.getId(), "path");

  return generate(path.name, v.getId(), "name");
}

bool GraphGeneratorPriv::generate(ocaml::ast::module_path const& path,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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

bool GraphGeneratorPriv::generate(ocaml::ast::extended_module_path const& path,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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
    ocaml::ast::extended_module_path_list const& list, Graph::VertexId parentId,
    std::string const& parentEdgeName) const {
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

bool GraphGeneratorPriv::generate(ocaml::ast::extended_module_name const& name,
                                  Graph::VertexId parentId,
                                  std::string const& parentEdgeName) const {
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
    ocaml::ast::extended_module_name_list const& list, Graph::VertexId parentId,
    std::string const& parentEdgeName) const {
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

} /* namespace OCaml */
