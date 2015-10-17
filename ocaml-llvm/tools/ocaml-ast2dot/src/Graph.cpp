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

    Graph::EdgeList const& getEdges() const;

    std::string toString() const;

private:
    Graph *_q;

    typedef std::map<Graph::VertexId, Graph::Vertex> VertexMap;
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

Graph::EdgeList const& GraphPriv::getEdges() const
{
    return _edges;
}

class EdgeLabelWriter
{
public:
    EdgeLabelWriter(GraphPriv const *g)
        : _g(g)
    {
    }

    template<class Edge>
    void operator()(std::ostream& out, Edge const& edge) const
        {
        for (Graph::Edge const& e : _g->getEdges()) {
            if (e.getFirst() != edge.m_source || e.getSecond() != edge.m_target)
                continue;

            std::string name = e.getName();
            if (!name.empty())
                out << "[label=" << name << "]";
        }
    }

private:
    GraphPriv const *_g;
};

template<class VertexMap>
class VertexLabelWriter
{
public:
    VertexLabelWriter(VertexMap const& map)
        : _map(map)
    {
    }
    template<class VertexId>
    void operator()(std::ostream& out, VertexId const& id) const
        {
        if (_map.find(id) == _map.end()) {
            out << "[shape=circle,label=\"Unknown vertex\"]";
            return;
        }

        Graph::Vertex vertex = _map.at(id);
        out << "[shape=none,margin=0,label=<";
        out
            << "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\" cellpadding=\"4\">";
        out << "<tr><td bgcolor=\"lightblue\">"
            << vertex.getProperties().at("type")
            << "</td></tr>";
        for (auto prop : vertex.getProperties()) {
            if (prop.first == "type")
                continue;

            out << "<tr><td align=\"left\">"
                << prop.first
                << ": "
                << prop.second
                << "</td></tr>";
        }
        out << "</table>>]";
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
        boost::add_edge(edge.getFirst(), edge.getSecond(), g);
    }

    boost::write_graphviz(ss, g, VertexLabelWriter<VertexMap>(_vertices),
        EdgeLabelWriter(this));
    return ss.str();
}

//
// Edge implementation
//

class Graph::EdgePriv
{
public:
    EdgePriv(Edge *q, Graph::VertexId first, Graph::VertexId second,
        std::string const& edgeName);
    EdgePriv(EdgePriv const& other);

    Graph::VertexId getFirst() const;
    Graph::VertexId getSecond() const;
    std::string const& getName() const;

private:
    Edge *_q;
    Graph::VertexId _first;
    Graph::VertexId _second;
    std::string _name;
};

Graph::Edge::Edge(VertexId first, VertexId second, std::string const& edgeName)
    : _p(new EdgePriv(this, first, second, edgeName))
{
}

Graph::Edge::Edge(Edge const& other)
{
    _p.reset(new EdgePriv(*other._p));
}

Graph::Edge::Edge(Edge&& other)
    : Edge()
{
    swap(other);
}

Graph::Edge::~Edge()
{
}

Graph::Edge& Graph::Edge::operator =(Edge other)
{
    swap(other);
    return *this;
}

void Graph::Edge::swap(Edge& other)
{
    std::swap(_p, other._p);
}

Graph::VertexId Graph::Edge::getFirst() const
{
    return _p->getFirst();
}

Graph::VertexId Graph::Edge::getSecond() const
{
    return _p->getSecond();
}

std::string const& Graph::Edge::getName() const
{
    return _p->getName();
}

//
// Vertex implementation
//

class Graph::VertexPriv
{
public:
    VertexPriv(Vertex *q, Graph::VertexId id);
    VertexPriv(VertexPriv const& other);

    Graph::VertexId getId() const;

    void addProperty(std::string const& name, std::string const& value);

    Graph::Vertex::Properties const& getProperties() const;

private:
    Vertex *_q;
    Graph::VertexId _id;
    std::map<std::string, std::string> _properties;
};

Graph::Vertex::Vertex(VertexId id)
    : _p(new VertexPriv(this, id))
{
}

Graph::Vertex::Vertex(Vertex const& other)
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

Graph::Vertex Graph::Vertex::create()
{
    static Graph::VertexId id = 0;
    return Vertex(id++);
}

Graph::VertexId Graph::Vertex::getId() const
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
// Private Edge implementation
//

Graph::EdgePriv::EdgePriv(Edge *q, Graph::VertexId first,
    Graph::VertexId second, std::string const& edgeName)
    : _q(q), _first(first), _second(second), _name(edgeName)
{
}

Graph::EdgePriv::EdgePriv(EdgePriv const& other)
{
    _q = other._q;
    _first = other._first;
    _second = other._second;
    _name = other._name;
}

Graph::VertexId Graph::EdgePriv::getFirst() const
{
    return _first;
}

Graph::VertexId Graph::EdgePriv::getSecond() const
{
    return _second;
}

std::string const& Graph::EdgePriv::getName() const
{
    return _name;
}

//
// Private Vertex implementation
//

Graph::VertexPriv::VertexPriv(Vertex *q, Graph::VertexId id)
    : _q(q), _id(id)
{
}

Graph::VertexPriv::VertexPriv(VertexPriv const& other)
{
    _q = other._q;
    _id = other._id;
    _properties = other._properties;
}

Graph::VertexId Graph::VertexPriv::getId() const
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

} /* namespace OCaml */
