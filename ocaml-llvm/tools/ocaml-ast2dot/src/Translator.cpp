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
#include "GraphGenerator.h"
#include "Parser.h"
#include "Translator.h"

#include <fstream>
#include <functional>
#include <iostream>

namespace OCaml
{

class TranslatorPriv
{
public:
    explicit TranslatorPriv(Translator *q);
    explicit TranslatorPriv(Translator *q, const Options& options);
    TranslatorPriv(TranslatorPriv const& other);

    int run();

private:
    Options const& getOptions() const;
    std::string readContent(std::istream& input);
    void writeContent(std::ostream& output, std::string const& content);
    int translate(std::istream& input, std::ostream& output);

    static Graph* createCapitalizedIdentGraph(std::string const& content);
    static Graph* createLowercaseIdentGraph(std::string const& content);
    static Graph* createIdentGraph(std::string const& content);
    static Graph* createLabelNameGraph(std::string const& content);
    static Graph* createLabelGraph(std::string const& content);
    static Graph* createOptLabelGraph(std::string const& content);
    static Graph* createIntegerLiteralGraph(std::string const& content);
    static Graph* createFloatLiteralGraph(std::string const& content);
    static Graph* createCharLiteralGraph(std::string const& content);
    static Graph* createStringLiteralGraph(std::string const& content);

private:
    Translator *_q;
    Options _options;
};

Translator::Translator()
    : _p(new TranslatorPriv(this))
{
}

Translator::Translator(const Options& options)
    : _p(new TranslatorPriv(this, options))
{
}

Translator::~Translator()
{
}

Translator::Translator(const Translator& other)
{
    _p.reset(new TranslatorPriv(*other._p));
}

Translator::Translator(Translator&& other)
    : Translator()
{
    swap(other);
}

Translator& Translator::operator =(Translator other)
{
    swap(other);
    return *this;
}

void Translator::swap(Translator& other)
{
    std::swap(_p, other._p);
}

int Translator::run()
{
    return _p->run();
}

//
// Private implementation
//

TranslatorPriv::TranslatorPriv(Translator *q)
    : _q(q)
{
}

TranslatorPriv::TranslatorPriv(Translator *q, const Options& options)
    : _q(q), _options(options)
{
}

TranslatorPriv::TranslatorPriv(TranslatorPriv const& other)
{
    _q = other._q;
    _options = other._options;
}

int TranslatorPriv::run()
{
    int ret = -1;
    std::ifstream fileInput;
    std::ofstream fileOutput;

    try {
        if (!_options.isStdInput()) {
            fileInput.open(_options.getInputFile());
            if (!fileInput.good()) {
                std::cerr << "error: failed to open input file" << std::endl;
                return -1;
            }
        }

        if (!_options.isStdOutput()) {
            fileOutput.open(_options.getOutputFile());
            if (!fileOutput.good()) {
                std::cerr << "error: failed to open output file" << std::endl;
                return -1;
            }
        }

        std::istream& input = _options.isStdInput() ? std::cin : fileInput;
        std::ostream& output = _options.isStdOutput() ? std::cout : fileOutput;
        ret = translate(input, output);
    }
    catch (std::exception& e) {
        std::cerr << "error: " << e.what() << std::endl;
    }
    catch (...) {
        std::cerr << "Exception of unknown type" << std::endl;
    }

    if (!_options.isStdInput())
        fileInput.close();

    if (!_options.isStdOutput())
        fileOutput.close();

    return ret;
}

Options const& TranslatorPriv::getOptions() const
{
    return _options;
}

std::string TranslatorPriv::readContent(std::istream& input)
{
    std::string ctx, line;
    while (std::getline(input, line))
        ctx += line + "\n";

    return ctx;
}

void TranslatorPriv::writeContent(std::ostream& output,
    std::string const& content)
{
    output << content;
}

int TranslatorPriv::translate(std::istream& input, std::ostream& output)
{
    typedef std::function<Graph*(std::string const&)> TranslatorType;
    std::map<Options::Rules, TranslatorType> translatorMap =
        {
            //
            // Map of grammar to graph translators
            //
            { Options::Rules::CapitalizedIdent,
                TranslatorPriv::createCapitalizedIdentGraph },
            { Options::Rules::LowercaseIdent,
                TranslatorPriv::createLowercaseIdentGraph },
            { Options::Rules::Ident,
                TranslatorPriv::createIdentGraph },
            { Options::Rules::LabelName,
                TranslatorPriv::createLabelNameGraph },
            { Options::Rules::Label,
                TranslatorPriv::createLabelGraph },
            { Options::Rules::OptLabel,
                TranslatorPriv::createOptLabelGraph },
            { Options::Rules::IntegerLiteral,
                TranslatorPriv::createIntegerLiteralGraph },
            { Options::Rules::FloatLiteral,
                TranslatorPriv::createFloatLiteralGraph },
            { Options::Rules::CharLiteral,
                TranslatorPriv::createCharLiteralGraph },
            { Options::Rules::StringLiteral,
                TranslatorPriv::createStringLiteralGraph }
        };

    Options::Rules rule = _options.getSelectedRule();
    if (translatorMap.find(rule) == translatorMap.end()) {
        std::cerr << "error: Unknown rule selected" << std::endl;
        return -1;
    }

    TranslatorType translator = translatorMap[rule];
    std::string content = readContent(input);
    std::unique_ptr<Graph> graph(translator(content));
    if (graph)
        writeContent(output, graph->toString());

    return 0;
}

Graph* TranslatorPriv::createCapitalizedIdentGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::capitalized_ident ident;
    if (!parser.parse(content, ident))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(ident))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createLowercaseIdentGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::lowercase_ident ident;
    if (!parser.parse(content, ident))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(ident))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createIdentGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::ident ident;
    if (!parser.parse(content, ident))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(ident))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createLabelNameGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::label_name label_name;
    if (!parser.parse(content, label_name))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(label_name))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createLabelGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::label label;
    if (!parser.parse(content, label))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(label))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createOptLabelGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::optlabel label;
    if (!parser.parse(content, label))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(label))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createIntegerLiteralGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::integer_literal integer_lit;
    if (!parser.parse(content, integer_lit))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(integer_lit))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createFloatLiteralGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::float_literal float_lit;
    if (!parser.parse(content, float_lit))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(float_lit))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createCharLiteralGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::char_literal char_lit;
    if (!parser.parse(content, char_lit))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(char_lit))
        return nullptr;

    return graph.release();
}

Graph* TranslatorPriv::createStringLiteralGraph(std::string const& content)
{
    Parser parser;
    ocaml::ast::string_literal string_lit;
    if (!parser.parse(content, string_lit))
        return nullptr;

    std::unique_ptr<Graph> graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(string_lit))
        return nullptr;

    return graph.release();
}

} /* namespace OCaml */
