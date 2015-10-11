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

#include "Translator.h"

namespace OCaml
{

class TranslatorPriv
{
public:
    explicit TranslatorPriv(Translator *q);
    explicit TranslatorPriv(Translator *q, const Options& options);
    TranslatorPriv(TranslatorPriv const& other);

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
    std::swap(*this, other);
}

Translator& Translator::operator =(Translator other)
{
    std::swap(*this, other);
    return *this;
}

void Translator::swap(Translator& other)
{
    std::swap(_p, other._p);
}

int Translator::run()
{
    return 0;
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

} /* namespace OCaml */
