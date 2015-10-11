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

const std::string Options::kInputFileOption = "input-file";
const std::string Options::kOutputFileOption = "output-file";


class OptionsPriv
{
public:
    explicit OptionsPriv(Options *q);
    explicit OptionsPriv(Options *q, po::variables_map const& vm);

    OptionsPriv(OptionsPriv const& other);

    std::string const& getInputFile() const;
    std::string const& getOutputFile() const;

    void parseOptions(po::variables_map const& vm);

private:
    Options *_q;
    std::string _inputFile;
    std::string _outputFile;
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
    std::swap(*this, other);
}

Options::~Options()
{
}

Options& Options::operator =(Options other)
{
    std::swap(*this, other);
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
}

std::string const& OptionsPriv::getInputFile() const
{
    return _inputFile;
}

std::string const& OptionsPriv::getOutputFile() const
{
    return _outputFile;
}

void OptionsPriv::parseOptions(po::variables_map const& vm)
{
    if (vm.count(Options::kInputFileOption))
        _inputFile = vm[Options::kInputFileOption].as<std::string>();

    if (vm.count(Options::kOutputFileOption))
        _outputFile = vm[Options::kOutputFileOption].as<std::string>();
}
