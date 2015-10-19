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

    template<typename T>
    static Graph* createGraph(std::string const& content);

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
    std::map < Options::Rules, TranslatorType> translatorMap =
    {
        //
        // Lexical rules translators
        //
        {   Options::Rules::CapitalizedIdent,
            TranslatorPriv::createGraph<ocaml::ast::capitalized_ident>},
        {   Options::Rules::LowercaseIdent,
            TranslatorPriv::createGraph<ocaml::ast::lowercase_ident>},
        {   Options::Rules::Ident,
            TranslatorPriv::createGraph<ocaml::ast::ident>},
        {   Options::Rules::LabelName,
            TranslatorPriv::createGraph<ocaml::ast::label_name>},
        {   Options::Rules::Label,
            TranslatorPriv::createGraph<ocaml::ast::label>},
        {   Options::Rules::OptLabel,
            TranslatorPriv::createGraph<ocaml::ast::optlabel>},
        {   Options::Rules::IntegerLiteral,
            TranslatorPriv::createGraph<ocaml::ast::integer_literal>},
        {   Options::Rules::FloatLiteral,
            TranslatorPriv::createGraph<ocaml::ast::float_literal>},
        {   Options::Rules::CharLiteral,
            TranslatorPriv::createGraph<ocaml::ast::char_literal>},
        {   Options::Rules::StringLiteral,
            TranslatorPriv::createGraph<ocaml::ast::string_literal>},
        //
        // Names rule translators
        //
        {   Options::Rules::ValueName,
            TranslatorPriv::createGraph<ocaml::ast::value_name>},
        {   Options::Rules::OperatorName,
            TranslatorPriv::createGraph<ocaml::ast::operator_name>},
        {   Options::Rules::InfixOp,
            TranslatorPriv::createGraph<ocaml::ast::infix_op>},
        {   Options::Rules::ConstrName,
            TranslatorPriv::createGraph<ocaml::ast::constr_name>},
        {   Options::Rules::TagName,
            TranslatorPriv::createGraph<ocaml::ast::tag_name>},
        {   Options::Rules::TypeconstrName,
            TranslatorPriv::createGraph<ocaml::ast::typeconstr_name>},
        {   Options::Rules::FieldName,
            TranslatorPriv::createGraph<ocaml::ast::field_name>},
        {   Options::Rules::ModuleName,
            TranslatorPriv::createGraph<ocaml::ast::module_name>},
        {   Options::Rules::ModtypeName,
            TranslatorPriv::createGraph<ocaml::ast::modtype_name>},
        {   Options::Rules::ClassName,
            TranslatorPriv::createGraph<ocaml::ast::class_name>},
        {   Options::Rules::InstVarName,
            TranslatorPriv::createGraph<ocaml::ast::inst_var_name>},
        {   Options::Rules::MethodName,
            TranslatorPriv::createGraph<ocaml::ast::method_name>},
        {   Options::Rules::ValuePath,
            TranslatorPriv::createGraph<ocaml::ast::value_path>},
        {   Options::Rules::Constr,
            TranslatorPriv::createGraph<ocaml::ast::constr>},
        {   Options::Rules::Typeconstr,
            TranslatorPriv::createGraph<ocaml::ast::typeconstr>},
        {   Options::Rules::Field,
            TranslatorPriv::createGraph<ocaml::ast::field>},
        {   Options::Rules::ModtypePath,
            TranslatorPriv::createGraph<ocaml::ast::modtype_path>},
        {   Options::Rules::ClassPath,
            TranslatorPriv::createGraph<ocaml::ast::class_path>},
        {   Options::Rules::ClasstypePath,
            TranslatorPriv::createGraph<ocaml::ast::classtype_path>},
        {   Options::Rules::ModulePath,
            TranslatorPriv::createGraph<ocaml::ast::module_path>},
        {   Options::Rules::ExtendedModulePath,
            TranslatorPriv::createGraph<ocaml::ast::extended_module_path>},
        {   Options::Rules::ExtendedModuleName,
            TranslatorPriv::createGraph<ocaml::ast::extended_module_name>},
        //
        // Type expression rules translators
        //
        {   Options::Rules::Typexpr,
            TranslatorPriv::createGraph<ocaml::ast::typexpr>},

        //
        // Constants rules translators
        //
        {   Options::Rules::Constant,
            TranslatorPriv::createGraph<ocaml::ast::constant>}

    }
    ;

    Options::Rules rule = _options.getSelectedRule();
    if (translatorMap.find(rule) == translatorMap.end()) {
        std::cerr << "error: Unknown rule selected" << std::endl;
        return -1;
    }

    TranslatorType translator = translatorMap[rule];
    std::string content = readContent(input);
    std::unique_ptr < Graph > graph(translator(content));
    if (graph)
        writeContent(output, graph->toString());

    return 0;
}

template<typename T>
Graph* TranslatorPriv::createGraph(std::string const& content)
{
    T t;
    Parser parser;
    if (!parser.parse(content, t))
        return nullptr;

    std::unique_ptr < Graph > graph(new Graph);
    GraphGenerator generator(*graph);
    if (!generator(t))
        return nullptr;

    return graph.release();
}

} /* namespace OCaml */
