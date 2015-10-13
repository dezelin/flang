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

namespace OCaml
{

class GraphGeneratorPriv
{
public:
    GraphGeneratorPriv(GraphGenerator *q);
    GraphGeneratorPriv(GraphGenerator *q, Graph& graph);
    GraphGeneratorPriv(GraphGeneratorPriv const& other);

    Graph* getGraph() const;

    bool generate(ocaml::ast::capitalized_ident const& ident) const;

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
    ocaml::ast::capitalized_ident const& ident) const
{
    Graph::Vertex v;
    v.addProperty("type", "ocaml::ast::capitalized_ident");
    v.addProperty("name", ident.name);

    Graph::EdgeList edges;
    edges.push_back(Graph::Edge(v.getId(), v.getId()));

    _graph->addVertex(v, edges);
    return true;
}

} /* namespace OCaml */
