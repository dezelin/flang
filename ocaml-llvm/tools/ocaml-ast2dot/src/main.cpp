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

#include <boost/program_options.hpp>

#include <iostream>

#include "Options.h"
#include "Translator.h"

namespace po = boost::program_options;

int main(int argc, char **argv)
{
    try {
        po::options_description general("General options");
        general.add_options()
        ("help,h", "Help")
        ("input-file,i", po::value<std::string>(), "Input file")
        ("output-file,o", po::value<std::string>(), "Output Graphviz file")
            ;

        po::options_description lexical("Lexical rules");
        lexical.add_options()
        ("capitalized-ident", "capitalized-ident rule")
        ("lowercase-ident", "lowercase-ident rule")
        ("ident", "ident rule")
        ("label-name", "label-name rule")
        ("label", "label rule")
        ("optlabel", "optlabel rule")
        ("integer-literal", "integer-literal rule")
        ("float-literal", "float-literal rule")
        ("char-literal", "char-literal rule")
        ("string-literal", "string-literal rule")
            ;

        po::options_description names("Names rules");
        names.add_options()
        ("value-name", "value-name rule")
        ("operator-name", "operator-name rule")
        ("infix-op", "infix-op rule")
        ("constr-name", "constr-name rule")
        ("tag-name", "tag-name rule")
        ("typeconstr-name", "typeconstr-name rule")
        ("field-name", "field-name rule")
        ("module-name", "module-name rule")
        ("modtype-name", "modtype-name rule")
        ("class-name", "class-name rule")
        ("inst-var-name", "inst-var-name rule")
        ("method-name", "method-name rule")
        ("value-path", "value-path rule")
        ("constr", "constr rule")
        ("typeconstr", "typeconstr rule")
        ("field", "field rule")
        ("modtype-path", "modtype-path rule")
        ("class-path", "class-path rule")
        ("classtype-path", "classtype-path rule")
        ("module-path", "module-path rule")
        ("extended-module-path", "extended-module-path rule")
        ("extended-module-name", "extended-module-name rule")
            ;

        po::options_description all("Allowed options");
        all.add(general).add(lexical).add(names);

        po::positional_options_description p;
        p.add("input-file", -1);

        po::variables_map vm;
        po::store(po::command_line_parser(argc, argv)
            .options(all).positional(p).run(), vm);
        po::notify(vm);

        if (vm.count("help")) {
            std::cout << "Usage: ocaml-ast2dot [options] file" << std::endl;
            std::cout << all << std::endl;
            std::cout << std::endl;
            std::cout
                << "If no input file is given standard input will be used."
                << std::endl;
            std::cout
                << "If no output file is given standard output will be used."
                << std::endl << std::endl;
            std::cout
                << "Only one rule can be specified at the time."
                << std::endl;
            std::cout << std::endl;
            return 0;
        }

        OCaml::Options options(vm);
        OCaml::Translator translator(options);
        return translator.run();
    }
    catch (std::exception &e) {
        std::cerr << "error: " << e.what() << std::endl;
        return 1;
    }
    catch (...) {
        std::cerr << "Exception of unknown type" << std::endl;
        return 1;
    }

    return 0;
}
