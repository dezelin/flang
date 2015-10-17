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

#ifndef OPTIONS_H_
#define OPTIONS_H_

#include <boost/program_options.hpp>

#include <memory>

namespace OCaml
{

namespace po = boost::program_options;

class OptionsPriv;
class Options
{
public:
    static const std::string kInputFileOption;
    static const std::string kOutputFileOption;
    static const std::string kCapitalizedIdent;
    static const std::string kLowercaseIdent;
    static const std::string kIdent;
    static const std::string kLabelName;
    static const std::string kLabel;
    static const std::string kOptLabel;
    static const std::string kIntegerLiteral;
    static const std::string kFloatLiteral;
    static const std::string kCharLiteral;
    static const std::string kStringLiteral;

    enum class Rules
            : int {
            Unknown = 0,

            //
            // Lexical
            //
        CapitalizedIdent,
        LowercaseIdent,
        Ident,
        LabelName,
        Label,
        OptLabel,
        IntegerLiteral,
        FloatLiteral,
        CharLiteral,
        StringLiteral,

        //
        // Names
        //
        ValueName,
        OperatorName,
        InfixOp,
        ConstrName,
        TagName,
        TypeconstrName,
        FieldName,
        ModuleName,
        ModtypeName,
        ClassName,
        InstVarName,
        MethodName,
        ValuePath,
        Constr,
        Typeconstr,
        Field,
        ModtypePath,
        ClassPath,
        ClasstypePath,
        ModulePath,
        ExtendedModulePath,
        ExtendedModuleName,

        //
        // Type expressions
        //
        Typexpr
    };

    Options();
    explicit Options(po::variables_map const& vm);
    Options(Options const& other);
    Options(Options &&other);
    virtual ~Options();

    Options& operator=(Options other);

    void swap(Options& other);

    std::string const& getInputFile() const;
    std::string const& getOutputFile() const;

    bool isStdInput() const;
    bool isStdOutput() const;

    Rules getSelectedRule() const;

    void parseOptions(po::variables_map const& vm);

private:
    std::unique_ptr<OptionsPriv> _p;
};

} /* namespace OCaml */

#endif /* OPTIONS_H_ */
