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

#define BOOST_TEST_MODULE OCamlLexerTest
#define BOOST_TEST_MAIN
#define BOOST_TEST_DYN_LINK

//#define BOOST_SPIRIT_LEXERTL_DEBUG

#include "ocamllexer.h"

#include <boost/filesystem.hpp>
#include <boost/test/unit_test.hpp>

#include <iomanip>
#include <fstream>

using namespace boost::filesystem;
using namespace boost::spirit;

std::string read_from_file(std::string const &filePath)
{
    std::string ctx, line;
    std::ifstream file(filePath);
    if (file.is_open()) {
        while (getline(file, line)) {
            ctx += line + "\n";
        }

        file.close();
    }
    else {
        std::cout << "Can't open file " << filePath << std::endl;
    }

    return ctx;
}

BOOST_AUTO_TEST_SUITE(OCamlLexerTest)

BOOST_AUTO_TEST_CASE(LexerTest)
{
/*
    typedef std::string::iterator base_iterator_type;
    typedef lex::lexertl::actor_lexer<lex::lexertl::token<base_iterator_type>>
        lexer_type;
        
    std::cout << "First token: " << ocaml::lexer::Tokens::Blank << "\n";
    
    ocaml::lexer::OCamlLexer<lexer_type> ocamlLexer;
    std::string contentToLex = "=!!";
    base_iterator_type first = contentToLex.begin();
    bool r = lex::tokenize(first, contentToLex.end(), ocamlLexer);
    if (!r) {
        std::string rest(first, contentToLex.end());
        std::cerr << "Lexical analysis failed\n" << "stopped at: \""
        << rest << "\"\n";
    }
    
    BOOST_CHECK(r);
*/    

    path dataDir(OCAML_TEST_CASE_DATA_DIR);
    BOOST_CHECK(exists(dataDir));
    BOOST_CHECK(is_directory(dataDir));

    std::vector<path> files;
    copy(recursive_directory_iterator(dataDir), recursive_directory_iterator(),
        std::back_inserter(files));

    sort(files.begin(), files.end());
    auto new_end = std::remove_if(files.begin(), files.end(), [](path file) {
        return file.extension().string() != ".ml" && file.extension().string()
            != ".mli";
    });

    files.erase(new_end, files.end());

    typedef std::string::iterator base_iterator_type;

    // lexer type
    typedef lex::lexertl::actor_lexer<lex::lexertl::token<base_iterator_type>>
        lexer_type;

    std::cout << "Lexing files from OCaml distribution:\n";
    std::for_each(files.begin(), files.end(), [&](path file) {
        std::cout << ">> File: " << file << "\n";
        // OCaml lexer
        ocaml::lexer::OCamlLexer<lexer_type> ocamlLexer;
        std::string contentToLex = read_from_file(file.string());
        base_iterator_type first = contentToLex.begin();
        bool r = lex::tokenize(first, contentToLex.end(), ocamlLexer);
        if (!r) {
            std::string rest(first, contentToLex.end());
            std::cerr << file.string() << ":\n";
            std::cerr << "Lexical analysis failed\n" << "stopped at: \""
                << rest << "\"\n";
        }

        BOOST_CHECK(r);
    });
}

BOOST_AUTO_TEST_SUITE_END()
