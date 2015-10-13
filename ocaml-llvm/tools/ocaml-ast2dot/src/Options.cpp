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

const std::string Options::kInputFileOption = "input-file";
const std::string Options::kOutputFileOption = "output-file";
const std::string Options::kCapitalizedIdent = "capitalized_ident";
const std::string Options::kLowercaseIdent = "lowercase_ident";
const std::string Options::kIdent = "ident";
const std::string Options::kLabelName = "label_name";
const std::string Options::kLabel = "label";
const std::string Options::kOptLabel = "optlabel";
const std::string Options::kIntegerLiteral = "integer_literal";
const std::string Options::kFloatLiteral = "float_literal";
const std::string Options::kCharLiteral = "char_literal";
const std::string Options::kStringLiteral = "string_literal";

class OptionsPriv
{
public:
    explicit OptionsPriv(Options *q);
    explicit OptionsPriv(Options *q, po::variables_map const& vm);

    OptionsPriv(OptionsPriv const& other);

    std::string const& getInputFile() const;
    std::string const& getOutputFile() const;

    bool isStdInput() const;
    bool isStdOutput() const;

    Options::Rules getSelectedRule() const;
    void setSelectedRule(Options::Rules rule);

    void parseOptions(po::variables_map const& vm);

private:
    Options *_q;
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

Options& Options::operator =(Options other)
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

OptionsPriv::OptionsPriv(Options *q)
    : _q(q)
{
}

OptionsPriv::OptionsPriv(Options *q, po::variables_map const& vm)
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
    if (vm.count(Options::kInputFileOption))
        _inputFile = vm[Options::kInputFileOption].as<std::string>();

    if (vm.count(Options::kOutputFileOption))
        _outputFile = vm[Options::kOutputFileOption].as<std::string>();

    typedef std::pair<std::string, Options::Rules> RulePair;
    RulePair rulePairs[] = {
        //
        // Lexical rules options
        //
        { Options::kCapitalizedIdent, Options::Rules::CapitalizedIdent },
        { Options::kLowercaseIdent, Options::Rules::LowercaseIdent },
        { Options::kIdent, Options::Rules::Ident },
        { Options::kLabelName, Options::Rules::LabelName },
        { Options::kLabel, Options::Rules::Label },
        { Options::kOptLabel, Options::Rules::OptLabel },
        { Options::kIntegerLiteral, Options::Rules::IntegerLiteral },
        { Options::kFloatLiteral, Options::Rules::FloatLiteral },
        { Options::kCharLiteral, Options::Rules::CharLiteral },
        { Options::kStringLiteral, Options::Rules::StringLiteral }
    };

    for(RulePair pair : rulePairs) {
        if (vm.count(pair.first)) {
            setSelectedRule(pair.second);
            return;
        }
    }
}

} /* namespace OCaml */
