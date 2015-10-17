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

#include "Options.h"

namespace OCaml
{

class OptionsPriv
{
public:
    explicit OptionsPriv(Options* q);
    explicit OptionsPriv(Options* q, po::variables_map const& vm);

    OptionsPriv(OptionsPriv const& other);

    std::string const& getInputFile() const;
    std::string const& getOutputFile() const;

    bool isStdInput() const;
    bool isStdOutput() const;

    Options::Rules getSelectedRule() const;
    void setSelectedRule(Options::Rules rule);

    void parseOptions(po::variables_map const& vm);

private:
    Options* _q;
    std::string _inputFile;
    std::string _outputFile;
    Options::Rules _selectedRule;
};

Options::Options()
    : _p(new OptionsPriv(this))
{
}

Options::Options(po::variables_map const& vm)
    : _p(new OptionsPriv(this, vm))
{
}

Options::Options(Options const& other)
{
    _p.reset(new OptionsPriv(*other._p));
}

Options::Options(Options&& other)
    : Options()
{
    swap(other);
}

Options::~Options()
{
}

Options& Options::operator=(Options other)
{
    swap(other);
    return *this;
}

void Options::swap(Options& other)
{
    std::swap(_p, other._p);
}

std::string const& Options::getInputFile() const
{
    return _p->getInputFile();
}

std::string const& Options::getOutputFile() const
{
    return _p->getOutputFile();
}

bool Options::isStdInput() const
{
    return _p->isStdInput();
}

bool Options::isStdOutput() const
{
    return _p->isStdOutput();
}

Options::Rules Options::getSelectedRule() const
{
    return _p->getSelectedRule();
}

void Options::parseOptions(po::variables_map const& vm)
{
    _p->parseOptions(vm);
}

//
// Private implementation
//

OptionsPriv::OptionsPriv(Options* q)
    : _q(q)
{
}

OptionsPriv::OptionsPriv(Options* q, po::variables_map const& vm)
    : _q(q)
{
    parseOptions(vm);
}

OptionsPriv::OptionsPriv(OptionsPriv const& other)
{
    _q = other._q;
    _inputFile = other._inputFile;
    _outputFile = other._outputFile;
    _selectedRule = other._selectedRule;
}

std::string const& OptionsPriv::getInputFile() const
{
    return _inputFile;
}

std::string const& OptionsPriv::getOutputFile() const
{
    return _outputFile;
}

bool OptionsPriv::isStdInput() const
{
    return _inputFile.empty();
}

bool OptionsPriv::isStdOutput() const
{
    return _outputFile.empty();
}

Options::Rules OptionsPriv::getSelectedRule() const
{
    return _selectedRule;
}

void OptionsPriv::setSelectedRule(Options::Rules rule)
{
    _selectedRule = rule;
}

void OptionsPriv::parseOptions(po::variables_map const& vm)
{
    if (vm.count("input-file"))
        _inputFile = vm["input-file"].as<std::string>();

    if (vm.count("output-file"))
        _outputFile = vm["output-file"].as<std::string>();

    typedef std::pair<std::string, Options::Rules> RulePair;
    RulePair rulePairs[] = {
        //
        // Lexical rules options
        //
        { "capitalized-ident", Options::Rules::CapitalizedIdent },
        { "lowercase-ident", Options::Rules::LowercaseIdent },
        { "ident", Options::Rules::Ident },
        { "label-name", Options::Rules::LabelName },
        { "label", Options::Rules::Label },
        { "optlabel", Options::Rules::OptLabel },
        { "integer-literal", Options::Rules::IntegerLiteral },
        { "float-literal", Options::Rules::FloatLiteral },
        { "char-literal", Options::Rules::CharLiteral },
        { "string-literal", Options::Rules::StringLiteral },
        //
        // Names rules options
        //
        { "value-name", Options::Rules::ValueName },
        { "operator-name", Options::Rules::OperatorName },
        { "infix-op", Options::Rules::InfixOp },
        { "constr-name", Options::Rules::ConstrName },
        { "tag-name", Options::Rules::TagName },
        { "typeconstr-name", Options::Rules::TypeconstrName },
        { "field-name", Options::Rules::FieldName },
        { "module-name", Options::Rules::ModuleName },
        { "modtype-name", Options::Rules::ModtypeName },
        { "class-name", Options::Rules::ClassName },
        { "inst-var-name", Options::Rules::InstVarName },
        { "method-name", Options::Rules::MethodName },
        { "value-path", Options::Rules::ValuePath },
        { "constr", Options::Rules::Constr },
        { "typeconstr", Options::Rules::Typeconstr },
        { "field", Options::Rules::Field },
        { "modtype-path", Options::Rules::ModtypePath },
        { "class-path", Options::Rules::ClassPath },
        { "classtype-path", Options::Rules::ClasstypePath },
        { "module-path", Options::Rules::ModulePath },
        { "extended-module-path", Options::Rules::ExtendedModulePath },
        { "extended-module-name", Options::Rules::ExtendedModuleName } };

    for (RulePair pair : rulePairs) {
        if (vm.count(pair.first)) {
            setSelectedRule(pair.second);
            return;
        }
    }
}

} /* namespace OCaml */
