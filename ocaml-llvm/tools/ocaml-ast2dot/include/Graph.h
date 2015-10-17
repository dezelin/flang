//
//  Copyright (c) 2015, Aleksandar Dezelin
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

#ifndef GRAPH_H_
#define GRAPH_H_

#include <map>
#include <memory>
#include <sstream>
#include <vector>

namespace OCaml {

class GraphPriv;
class Graph {
 public:
  typedef std::size_t VertexId;

  class EdgePriv;
  class Edge {
   public:
    Edge(VertexId first = VertexId(), VertexId second = VertexId(),
         std::string const& edgeName = "");
    Edge(Edge const& other);
    Edge(Edge&& other);
    virtual ~Edge();

    Edge& operator=(Edge other);
    void swap(Edge& other);

    VertexId getFirst() const;
    VertexId getSecond() const;
    std::string const& getName() const;

   private:
    std::unique_ptr<EdgePriv> _p;
  };

  class VertexPriv;
  class Vertex {
   public:
    typedef std::map<std::string, std::string> Properties;

   public:
    Vertex(VertexId id = VertexId());
    Vertex(Vertex const& other);
    Vertex(Vertex&& other);
    virtual ~Vertex();

    static Vertex create();

    Vertex& operator=(Vertex other);
    void swap(Vertex& other);

    VertexId getId() const;

    template <typename T, typename U>
    void addProperty(T& name, U& value);

    Properties const& getProperties() const;

   private:
    void createProperty(std::string const& name, std::string const& value);

   private:
    std::unique_ptr<VertexPriv> _p;
  };

  typedef std::vector<Edge> EdgeList;
  typedef std::vector<Vertex> VertexList;

 public:
  Graph();
  Graph(Graph const& other);
  Graph(Graph&& other);
  virtual ~Graph();

  Graph& operator=(Graph other);
  void swap(Graph& other);

  std::string toString() const;

  void addVertex(Vertex const& v, EdgeList const& edges);

 private:
  std::unique_ptr<GraphPriv> _p;
};

#include "impl/Graph.inc"

} /* namespace OCaml */

#endif /* GRAPH_H_ */
