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

#include "Graph.h"

#include <boost/graph/graphviz.hpp>

#include <map>
#include <sstream>

namespace OCaml
{

class GraphPriv
{
public:
    GraphPriv(Graph *q);
    GraphPriv(GraphPriv const& other);

    void addVertex(Graph::Vertex const& v, Graph::EdgeList const& edges);

    std::string toString() const;

private:
    Graph *_q;

    typedef std::map<Graph::Vertex::VertexId, Graph::Vertex> VertexMap;
    VertexMap _vertices;
    Graph::EdgeList _edges;
};

Graph::Graph()
    : _p(new GraphPriv(this))
{
}

Graph::~Graph()
{
}

Graph::Graph(const Graph& other)
{
    _p.reset(new GraphPriv(*other._p));
}

Graph::Graph(Graph&& other)
    : Graph()
{
    swap(other);
}

Graph& Graph::operator =(Graph other)
{
    swap(other);
    return *this;
}

void Graph::swap(Graph& other)
{
    std::swap(_p, other._p);
}

std::string Graph::toString() const
{
    return _p->toString();
}

void Graph::addVertex(Vertex const& v, EdgeList const& edges)
{
    _p->addVertex(v, edges);
}

//
// Private implementation
//

GraphPriv::GraphPriv(Graph *q)
    : _q(q)
{
}

GraphPriv::GraphPriv(GraphPriv const& other)
{
    _q = other._q;
    _vertices = other._vertices;
    _edges = other._edges;
}

void GraphPriv::addVertex(Graph::Vertex const& v, Graph::EdgeList const& edges)
{
    _vertices[v.getId()] = v;
    _edges.insert(_edges.end(), edges.begin(), edges.end());
}

template<class VertexMap>
class VertexWriter
{
public:
    VertexWriter(VertexMap const& map)
        : _map(map)
    {
    }
    template<class VertexId>
    void operator()(std::ostream& out, const VertexId& id) const
    {
        Graph::Vertex vertex = _map.at(id);
        out << "[shape=box]";
        out << "[label=\"";
        for(auto prop : vertex.getProperties()) {
            out << prop.first << "=" << prop.second << std::endl;
        }
        out << "\"]";
    }
private:
    VertexMap _map;
};

std::string GraphPriv::toString() const
{
    typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS> GraphType;

    std::stringstream ss;
    GraphType g(_edges.size());
    for (Graph::Edge const& edge : _edges) {
        boost::add_edge(edge.first, edge.second, g);
    }

    boost::write_graphviz(ss, g, VertexWriter<VertexMap>(_vertices));
    return ss.str();
}

//
// Vertex implementation
//

class Graph::VertexPriv
{
public:
    VertexPriv(Vertex *q);
    VertexPriv(VertexPriv const& other);

    Graph::Vertex::VertexId getId() const;

    void addProperty(std::string const& name, std::string const& value);

    Graph::Vertex::Properties const& getProperties() const;

private:
    static Graph::Vertex::VertexId nextId();

private:
    Vertex *_q;
    Graph::Vertex::VertexId _id;
    std::map<std::string, std::string> _properties;
};

Graph::Vertex::Vertex()
    : _p(new VertexPriv(this))
{
}

Graph::Vertex::Vertex(const Vertex& other)
{
    _p.reset(new VertexPriv(*other._p));
}

Graph::Vertex::Vertex(Vertex&& other)
    : Vertex()
{
    swap(other);
}

Graph::Vertex::~Vertex()
{
}

Graph::Vertex& Graph::Vertex::operator =(Vertex other)
{
    swap(other);
    return *this;
}

void Graph::Vertex::swap(Vertex& other)
{
    std::swap(_p, other._p);
}

Graph::Vertex::VertexId Graph::Vertex::getId() const
{
    return _p->getId();
}

Graph::Vertex::Properties const& Graph::Vertex::getProperties() const
{
    return _p->getProperties();
}

void Graph::Vertex::createProperty(std::string const& name,
    std::string const& value)
{
    _p->addProperty(name, value);
}

//
// Private implementation
//

Graph::VertexPriv::VertexPriv(Vertex *q)
    : _q(q), _id(nextId())
{
}

Graph::VertexPriv::VertexPriv(VertexPriv const& other)
{
    _q = other._q;
    _id = other._id;
    _properties = other._properties;
}

Graph::Vertex::VertexId Graph::VertexPriv::getId() const
{
    return _id;
}

Graph::Vertex::Properties const& Graph::VertexPriv::getProperties() const
{
    return _properties;
}

void Graph::VertexPriv::addProperty(std::string const& name,
    std::string const& value)
{
    _properties.emplace(name, value);
}

Graph::Vertex::VertexId Graph::VertexPriv::nextId()
{
    static Graph::Vertex::VertexId id = 0;
    return id++;
}

} /* namespace OCaml */
