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

#ifndef FLANG_OCAMLAST_H
#define FLANG_OCAMLAST_H

// Increase the maximum number of tuple items for Boost variant types
#define BOOST_MPL_CFG_NO_PREPROCESSED_HEADERS
#define BOOST_MPL_LIMIT_LIST_SIZE 40
#define BOOST_MPL_LIMIT_VECTOR_SIZE 40

#include "ocamlids.h"

#include <boost/config/warning_disable.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/spirit/include/support_extended_variant.hpp>
#include <boost/spirit/include/support_attributes.hpp>
#include <boost/optional.hpp>
#include <boost/variant/recursive_variant.hpp>

#include <list>

namespace ocaml
{
namespace ast
{

///////////////////////////////////////////////////////////////////////////
//  The AST
///////////////////////////////////////////////////////////////////////////

struct tagged
{
    int id; // Used to annotate the AST with the iterator position.
    // This id is used as a key to a map<int, Iterator>
    // (not really part of the AST.)
};

//
// Lexical
//

struct lowercase_ident
    : tagged
{
    lowercase_ident(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct capitalized_ident
    : tagged
{
    capitalized_ident(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct ident
    : tagged
{
    ident(std::string const &name = "")
        : name(name) { }

    std::string name;
};

struct label_name
    : lowercase_ident
{
    label_name(std::string const &name = "")
        : lowercase_ident(name) { }
};

struct label
    : label_name
{
    label(std::string const &name = "")
        : label_name(name) { }
};

struct optlabel
    : label_name
{
    optlabel(std::string const &name = "")
        : label_name(name) { }
};

struct integer_literal
    : tagged
{
    integer_literal(int i = 0)
        : val(i) { }

    int val;
};

struct float_literal
    : tagged
{
    float_literal(float f = 0.f)
        : val(f) { }

    float val;
};

struct char_literal
    : tagged
{
    char_literal(char c = '\0')
        : val(c) { }

    char val;
};

struct string_literal
    : tagged
{
    string_literal(std::string const &str = "")
        : val(str) { }

    std::string val;
};

//
// Names
//


//
//  BNF-like notation:
//  ========================================================================
//
//  value-name	::=	lowercase-ident
//           ∣	 ( operator-name )
//
//  operator-name	::=	prefix-symbol ∣  infix-op
//
//  infix-op	::=	infix-symbol
//          ∣  *  ∣  +  ∣  -  ∣  -.  ∣  =  ∣  !=  ∣  <  ∣  >  ∣  or  ∣  ||
//          ∣  &  ∣  &&  ∣  :=  ∣  mod  ∣  land  ∣  lor  ∣  lxor  ∣  lsl
//          ∣  lsr  ∣  asr
//
//  constr-name	::=	capitalized-ident
//
//  tag-name	::=	capitalized-ident
//
//  typeconstr-name	::=	lowercase-ident
//
//  field-name	::=	lowercase-ident
//
//  module-name	::=	capitalized-ident
//
//  modtype-name	::=	ident
//
//  class-name	::=	lowercase-ident
//
//  inst-var-name	::=	lowercase-ident
//
//  method-name	::=	lowercase-ident
//
//  value-path	::=	[ module-path . ]  value-name
//
//  constr	::=	[ module-path . ]  constr-name
//
//  typeconstr	::=	[ extended-module-path . ]  typeconstr-name
//
//  field	::=	[ module-path . ]  field-name
//
//  modtype-path	::=	[ extended-module-path . ]  modtype-name
//
//  class-path	::=	[ module-path . ]  class-name
//
//  classtype-path	::=	[ extended-module-path . ]  class-name
//
//  module-path	::=	module-name  { . module-name }
//
//  extended-module-path	::=	extended-module-name  { . extended-module-name }
//
//  extended-module-name	::=	module-name  { ( extended-module-path ) }
//

struct method_name
    : lowercase_ident
{
    method_name(std::string const &name = "")
        : lowercase_ident(name) { }
};

struct inst_var_name
    : lowercase_ident
{
    inst_var_name(std::string const &name = "")
        : lowercase_ident(name) { }
};

struct class_name
    : lowercase_ident
{
    class_name(std::string const &name = "")
        : lowercase_ident(name) { }
};

struct modtype_name
    : ident
{
    modtype_name(std::string const &name = "")
        : ident(name) { }
};

struct module_name
    : capitalized_ident
{
    module_name(std::string const &name = "")
        : capitalized_ident(name) { }
};

struct field_name
    : lowercase_ident
{
    field_name(std::string const &name = "")
        : lowercase_ident(name) { }
};

struct typeconstr_name
    : lowercase_ident
{
    typeconstr_name(std::string const &name = "")
        : lowercase_ident(name) { }
};

struct tag_name
    : capitalized_ident
{
    tag_name(std::string const &name = "")
        : capitalized_ident(name) { }
};

struct constr_name
    : capitalized_ident
{
    constr_name(std::string const &name = "")
        : capitalized_ident(name) { }
};

struct operation
    : tagged
{
    ocaml::lexer::Tokens operation;
};

struct infix_symbol
    : tagged
{
    infix_symbol(std::string const &symbol = "")
        : symbol(symbol) { }

    std::string symbol;
};

struct infix_op
    : tagged
      , boost::spirit::extended_variant<
        infix_symbol, operation
    >
{
    infix_op()
        : base_type() { }

    infix_op(infix_symbol const &val)
        : base_type(val) { }

    infix_op(operation const &val)
        : base_type(val) { }
};

struct prefix_symbol
    : tagged
{
    prefix_symbol(std::string const &symbol = "")
        : symbol(symbol) { }

    std::string symbol;
};

struct operator_name
    : tagged
      , boost::spirit::extended_variant<
        prefix_symbol, infix_op
    >
{
    operator_name()
        : base_type() { }

    operator_name(prefix_symbol const &val)
        : base_type(val) { }

    operator_name(infix_op const &val)
        : base_type(val) { }
};

struct value_name
    : tagged
      , boost::spirit::extended_variant<
        lowercase_ident, operator_name>
{
    value_name()
        : base_type() { }

    value_name(lowercase_ident const &val)
        : base_type(val) { }

    value_name(operator_name const &val)
        : base_type(val) { }
};

struct extended_module_path;

typedef std::list<extended_module_path> extended_module_path_list;

struct extended_module_name
    : tagged
{
    module_name name;
    boost::optional<extended_module_path_list> path_list;
};

typedef std::list<extended_module_name> extended_module_name_list;

struct extended_module_path
    : tagged
{
    extended_module_name name;
    boost::optional<extended_module_name_list> name_list;
};

typedef std::list<module_name> module_name_list;

struct module_path
    : tagged
{
    module_name name;
    boost::optional<module_name_list> name_list;
};

struct classtype_path
    : tagged
{
    boost::optional<extended_module_path> path;
    class_name name;
};

struct class_path
    : tagged
{
    boost::optional<module_path> path;
    class_name name;
};

struct modtype_path
    : tagged
{
    boost::optional<extended_module_path> path;
    modtype_name name;
};

struct field
    : tagged
{
    boost::optional<module_path> path;
    field_name name;
};

struct typeconstr
    : tagged
{
    boost::optional<extended_module_path> path;
    typeconstr_name name;
};

struct constr
    : tagged
{
    boost::optional<module_path> path;
    constr_name name;
};

struct value_path
    : tagged
{
    boost::optional<module_path> path;
    value_name name;
};

//
// Type expressions
//


//
//  BNF-like notation:
//  ========================================================================
//
//  typexpr	::=	' ident
//      	∣	 _
//       	∣	 ( typexpr )
//      	∣	 [[?]label-name:]  typexpr ->  typexpr
//      	∣	 typexpr  { * typexpr }+
//       	∣	 typeconstr
//      	∣	 typexpr  typeconstr
//      	∣	 ( typexpr  { , typexpr } )  typeconstr
//      	∣	 typexpr as '  ident
//      	∣	 polymorphic-variant-type
//      	∣	 < [..] >
//      	∣	 < method-type  { ; method-type }  [; ∣  ; ..] >
//      	∣	 # class-path
//      	∣	 typexpr #  class-path
//      	∣	 ( typexpr  { , typexpr } ) #  class-path
//
//  poly-typexpr	::=	typexpr
//      	∣	 { ' ident }+ .  typexpr
//
//  method-type	::=	method-name :  poly-typexpr
//
//  polymorphic-variant-type	::=	[ tag-spec-first  { | tag-spec } ]
//      	∣	 [> [ tag-spec ]  { | tag-spec } ]
//      	∣	 [< [|] tag-spec-full  { | tag-spec-full }  [ > { `tag-name }+ ] ]
//
//  tag-spec-first	::=	`tag-name  [ of typexpr ]
//      	∣	 [ typexpr ] |  tag-spec
//
//  tag-spec	::=	`tag-name  [ of typexpr ]
// 	        ∣	 typexpr
//
//  tag-spec-full	::=	`tag-name  [ of [&] typexpr  { & typexpr } ]
//      	∣	 typexpr
//

struct anon_type_variable;
struct function_typexpr;
struct tuple_typexpr;
struct constructed_typexpr;
struct constructed_nary_typexpr;
struct aliased_or_recursive_typexpr;
struct polymorphic_variant_type;
struct object_typexpr;
struct octothorpe_typexpr;
struct octothorpe_list_typexpr;

struct typexpr
    : tagged
      , boost::spirit::extended_variant<ident,
        boost::recursive_wrapper<anon_type_variable>,
        boost::recursive_wrapper<function_typexpr>,
        boost::recursive_wrapper<tuple_typexpr>,
        boost::recursive_wrapper<constructed_typexpr>,
        boost::recursive_wrapper<constructed_nary_typexpr>,
        boost::recursive_wrapper<aliased_or_recursive_typexpr>,
        boost::recursive_wrapper<polymorphic_variant_type>,
        boost::recursive_wrapper<object_typexpr>,
        boost::recursive_wrapper<octothorpe_typexpr>,
        boost::recursive_wrapper<octothorpe_list_typexpr>
    >
{
    typexpr()
        : base_type() { }

    typexpr(ident const &val)
        : base_type(val) { }

    typexpr(anon_type_variable const &val)
        : base_type(val) { }

    typexpr(function_typexpr const &val)
        : base_type(val) { }

    typexpr(tuple_typexpr const &val)
        : base_type(val) { }

    typexpr(constructed_typexpr const &val)
        : base_type(val) { }

    typexpr(constructed_nary_typexpr const &val)
        : base_type(val) { }

    typexpr(aliased_or_recursive_typexpr const &val)
        : base_type(val) { }

    typexpr(polymorphic_variant_type const &val)
        : base_type(val) { }

    typexpr(object_typexpr const &val)
        : base_type(val) { }

    typexpr(octothorpe_typexpr const &val)
        : base_type(val) { }

    typexpr(octothorpe_list_typexpr const &val)
        : base_type(val) { }
};

// _
struct anon_type_variable
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens anon_type_variable;
};

// [[?]label-name:]  typexpr ->  typexpr
struct function_typexpr
    : tagged
{
    boost::optional<label_name> label;
    boost::optional<optlabel> optlabel_;
    typexpr expr;
    typexpr retexpr;
};

typedef std::list<typexpr> typexpr_list;

//  typexpr  { * typexpr }+
struct tuple_typexpr
    : tagged
{
    typexpr expr;
    typexpr_list other;
};

// typexpr  typeconstr
struct constructed_typexpr
    : tagged
{
    typexpr expr;
    typeconstr constr;
};

// ( typexpr  { , typexpr } )  typeconstr
struct constructed_nary_typexpr
    : tagged
{
    typexpr expr;
    boost::optional<typexpr_list> other;
    typeconstr constr;
};

// typexpr as '  ident
struct aliased_or_recursive_typexpr
    : tagged
{
    typexpr expr;
    ident alias;
};

struct exact_variant_type;
struct opened_variant_type;
struct closed_variant_type;

struct polymorphic_variant_type
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<exact_variant_type>,
        boost::recursive_wrapper<opened_variant_type>,
        boost::recursive_wrapper<closed_variant_type>
    >
{
    polymorphic_variant_type()
        : base_type() { }

    polymorphic_variant_type(exact_variant_type const &val)
        : base_type(val) { }

    polymorphic_variant_type(opened_variant_type const &val)
        : base_type(val) { }

    polymorphic_variant_type(closed_variant_type const &val)
        : base_type(val) { }
};

struct tag_spec_of;
struct tag_spec_of_list;
struct tag_spec_or;

struct tag_spec_first
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<tag_spec_of>,
        boost::recursive_wrapper<tag_spec_or>
    >
{
    tag_spec_first()
        : base_type() { }

    tag_spec_first(tag_spec_of const &val)
        : base_type(val) { }

    tag_spec_first(tag_spec_or const &val)
        : base_type(val) { }
};

struct tag_spec
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<tag_spec_of>,
        typexpr
    >
{
    tag_spec()
        : base_type() { }

    tag_spec(tag_spec_of const &val)
        : base_type(val) { }

    tag_spec(typexpr const &val)
        : base_type(val) { }
};

struct tag_spec_full
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<tag_spec_of_list>,
        typexpr
    >
{
    tag_spec_full()
        : base_type() { }

    tag_spec_full(tag_spec_of_list const &val)
        : base_type(val) { }

    tag_spec_full(typexpr const &val)
        : base_type(val) { }
};

typedef std::list<tag_spec> tag_spec_list;

struct exact_variant_type
    : tagged
{
    tag_spec_first first;
    boost::optional<tag_spec_list> other;
};

struct opened_variant_type
    : tagged
{
    boost::optional<tag_spec> first;
    boost::optional<tag_spec_list> other;
};

typedef std::list<tag_spec_full> tag_spec_full_list;
typedef std::list<tag_name> tag_name_list;

struct closed_variant_type
    : tagged
{
    tag_spec_full first;
    boost::optional<tag_spec_full_list> other;
    boost::optional<tag_name_list>
        tags;
};

struct tag_spec_of
    : tagged
{
    tag_name name;
    boost::optional<typexpr> expr;
};

struct tag_spec_of_list
    : tagged
{
    tag_name name;
    boost::optional<typexpr_list> expr;
};

struct tag_spec_or
    : tagged
{
    boost::optional<typexpr> expr;
    tag_spec tag;
};


struct explicit_poly_typexpr;

struct poly_typexpr
    : tagged
      , boost::spirit::extended_variant<
        typexpr,
        boost::recursive_wrapper<explicit_poly_typexpr>
    >
{
    poly_typexpr()
        : base_type() { }

    poly_typexpr(typexpr const &val)
        : base_type(val) { }

    poly_typexpr(explicit_poly_typexpr const &val)
        : base_type(val) { }
};

struct method_type
    : tagged
{
    method_name name;
    poly_typexpr expr;
};

struct row_method_type
    : tagged
{
    method_type first;
    method_type last;
};

typedef std::list<method_type> method_type_list;
typedef std::list<row_method_type> row_method_type_list;

// < method-type  { ; method-type }  [; ∣  ; ..] >
struct object_typexpr
    : tagged
{
    method_type type;
    boost::optional<method_type_list> other;
    boost::optional<row_method_type_list> rows;
};

//  typexpr #  class-path
struct octothorpe_typexpr
    : tagged
{
    typexpr expr;
    class_path path;
};

struct octothorpe_list_typexpr
    : tagged
{
    typexpr expr;
    boost::optional<typexpr_list> other;
    class_path path;
};

typedef std::list<ident> ident_list;

struct explicit_poly_typexpr
    : tagged
{
    ident ident_;
    boost::optional<ident_list> other;
    typexpr expr;
};

//
// Constants
//


//
//  BNF-like notation:
//  ========================================================================
//
//  constant	::=	integer-literal
// 	        ∣	 float-literal
// 	        ∣	 char-literal
// 	        ∣	 string-literal
// 	        ∣	 constr
// 	        ∣	 false
// 	        ∣	 true
// 	        ∣	 ()
// 	        ∣	 begin end
// 	        ∣	 []
// 	        ∣	 [||]
// 	        ∣	 `tag-name
//

struct const_false
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens false_;
};

struct const_true
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens true_;
};

struct const_unit
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens opened;
    ocaml::lexer::Tokens closed;
};

struct const_empty_record
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens begin;
    ocaml::lexer::Tokens end;
};

struct const_empty_list
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens opened;
    ocaml::lexer::Tokens closed;
};

struct const_empty_array
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens opened;
    ocaml::lexer::Tokens closed;
};

struct constant
    : tagged
      , boost::spirit::extended_variant<
        integer_literal,
        float_literal,
        char_literal,
        string_literal,
        constr,
        const_false,
        const_true,
        const_unit,
        const_empty_record,
        const_empty_list,
        const_empty_array,
        tag_name>
{
    constant()
        : base_type() { }

    constant(integer_literal const &val)
        : base_type(val) { }

    constant(float_literal const &val)
        : base_type(val) { }

    constant(char_literal const &val)
        : base_type(val) { }

    constant(string_literal const &val)
        : base_type(val) { }

    constant(constr const &val)
        : base_type(val) { }

    constant(const_false const &val)
        : base_type(val) { }

    constant(const_true const &val)
        : base_type(val) { }

    constant(const_unit const &val)
        : base_type(val) { }

    constant(const_empty_record const &val)
        : base_type(val) { }

    constant(const_empty_list const &val)
        : base_type(val) { }

    constant(const_empty_array const &val)
        : base_type(val) { }

    constant(tag_name const &val)
        : base_type(val) { }
};

//
// Patterns
//


//
//  BNF-like notation:
//  ========================================================================
//
//  pattern	::=	value-name
// 	        ∣	 _
// 	        ∣	 constant
// 	        ∣	 pattern as  value-name
// 	        ∣	 ( pattern )
// 	        ∣	 ( pattern :  typexpr )
// 	        ∣	 pattern |  pattern
// 	        ∣	 constr  pattern
// 	        ∣	 `tag-name  pattern
// 	        ∣	 #typeconstr
// 	        ∣	 pattern  { , pattern }+
// 	        ∣	 { field =  pattern  { ; field =  pattern }  [ ; ] }
// 	        ∣	 [ pattern  { ; pattern }  [ ; ] ]
// 	        ∣	 pattern ::  pattern
// 	        ∣	 [| pattern  { ; pattern }  [ ; ] |]
//

struct any_value_pattern;
struct alias_pattern;
struct parenthized_pattern;
struct or_pattern;
struct variant_pattern;
struct variant_non_empty_list_pattern;
struct variant_list_pattern;
struct polymorphic_variant_pattern;
struct polymorphic_variant_abbrev_pattern;
struct tuple_pattern;
struct record_pattern;
struct array_pattern;

struct pattern
    : tagged
      , boost::spirit::extended_variant<
        value_name,
        boost::recursive_wrapper<any_value_pattern>,
        constant,
        boost::recursive_wrapper<alias_pattern>,
        boost::recursive_wrapper<parenthized_pattern>,
        boost::recursive_wrapper<or_pattern>,
        boost::recursive_wrapper<variant_pattern>,
        boost::recursive_wrapper<polymorphic_variant_pattern>,
        boost::recursive_wrapper<polymorphic_variant_abbrev_pattern>,
        boost::recursive_wrapper<tuple_pattern>,
        boost::recursive_wrapper<record_pattern>,
        boost::recursive_wrapper<variant_list_pattern>,
        boost::recursive_wrapper<variant_non_empty_list_pattern>,
        boost::recursive_wrapper<array_pattern>
    >
{
    pattern()
        : base_type() { }

    pattern(value_name const &val)
        : base_type(val) { }

    pattern(any_value_pattern const &val)
        : base_type(val) { }

    pattern(constant const &val)
        : base_type(val) { }

    pattern(alias_pattern const &val)
        : base_type(val) { }

    pattern(parenthized_pattern const &val)
        : base_type(val) { }

    pattern(or_pattern const &val)
        : base_type(val) { }

    pattern(variant_pattern const &val)
        : base_type(val) { }

    pattern(polymorphic_variant_pattern const &val)
        : base_type(val) { }

    pattern(polymorphic_variant_abbrev_pattern const &val)
        : base_type(val) { }

    pattern(tuple_pattern const &val)
        : base_type(val) { }

    pattern(record_pattern const &val)
        : base_type(val) { }

    pattern(variant_list_pattern const &val)
        : base_type(val) { }

    pattern(variant_non_empty_list_pattern const &val)
        : base_type(val) { }

    pattern(array_pattern const &val)
        : base_type(val) { }
};

// _
struct any_value_pattern
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens anyValue;

};

// pattern as  value-name
struct alias_pattern
    : tagged
{
    pattern pattrn;
    value_name name;
};

// ( pattern :  typexpr )
struct parenthized_pattern
    : tagged
{
    pattern pattrn;
    boost::optional<typexpr> type;
};

//  pattern |  pattern
struct or_pattern
    : tagged
{
    pattern left;
    pattern right;
};

// constr  pattern
struct variant_pattern
    : tagged
{
    constr constr_;
    pattern pattrn;
};

// `tag-name  pattern
struct polymorphic_variant_pattern
    : tagged
{
    tag_name tag;
    pattern pattrn;
};

// #typeconstr
struct polymorphic_variant_abbrev_pattern
    : tagged
{
    typeconstr constr;
};

typedef std::list<pattern> pattern_list;

// pattern  { , pattern }+
struct tuple_pattern
    : tagged
{
    pattern pattrn;
    pattern_list other;
};

struct record_field
    : tagged
{
    field name;
    pattern pattrn;
};

typedef std::list<record_field> record_field_list;

// { field =  pattern  { ; field =  pattern }  [ ; ] }
struct record_pattern
    : tagged
{
    record_field first;
    boost::optional<record_field_list> other;
};

// [ pattern  { ; pattern }  [ ; ] ]
struct variant_list_pattern
    : tagged
{
    pattern first;
    boost::optional<pattern_list> other;
};

//  pattern :: pattern
struct variant_non_empty_list_pattern
    : tagged
{
    pattern head;
    pattern tail;
};

// [| pattern  { ; pattern }  [ ; ] |]
struct array_pattern
    : tagged
{
    pattern first;
    boost::optional<pattern_list> other;
};

//
// Expressions
//

//
//  BNF-like notation:
//  ========================================================================
//
//    expr	::=	value-path
//          ∣	 constant
//          ∣	 ( expr )
//          ∣	 begin expr end
//          ∣	 ( expr :  typexpr )
//          ∣	 expr  {, expr}+
//          ∣	 constr  expr
//          ∣	 `tag-name  expr
//          ∣	 expr ::  expr
//          ∣	 [ expr  { ; expr }  [;] ]
//          ∣	 [| expr  { ; expr }  [;] |]
//          ∣	 { field =  expr  { ; field =  expr }  [;] }
//          ∣	 { expr with  field =  expr  { ; field =  expr }  [;] }
//          ∣	 expr  { argument }+
//          ∣	 prefix-symbol  expr
//          ∣	 - expr
//          ∣	 -. expr
//          ∣	 expr  infix-op  expr
//          ∣	 expr .  field
//          ∣	 expr .  field <-  expr
//          ∣	 expr .(  expr )
//          ∣	 expr .(  expr ) <-  expr
//          ∣	 expr .[  expr ]
//          ∣	 expr .[  expr ] <-  expr
//          ∣	 if expr then  expr  [ else expr ]
//          ∣	 while expr do  expr done
//          ∣	 for value-name =  expr  ( to ∣  downto ) expr do  expr done
//          ∣	 expr ;  expr
//          ∣	 match expr with  pattern-matching
//          ∣	 function pattern-matching
//          ∣	 fun multiple-matching
//          ∣	 try expr with  pattern-matching
//          ∣	 let [rec] let-binding  { and let-binding } in  expr
//          ∣	 new class-path
//          ∣	 object class-body end
//          ∣	 expr #  method-name
//          ∣	 inst-var-name
//          ∣	 inst-var-name <-  expr
//          ∣	 ( expr :>  typexpr )
//          ∣	 ( expr :  typexpr :>  typexpr )
//          ∣	 {< [ inst-var-name =  expr  { ; inst-var-name =  expr }  [;] ] >}
//
//    argument	::=	expr
//          ∣	 ~ label-name
//          ∣	 ~ label-name :  expr
//          ∣	 ? label-name
//          ∣	 ? label-name :  expr
//
//    pattern-matching	::=	[ | ] pattern  [when expr] ->  expr  { | pattern  [when expr] ->  expr }
//
//    multiple-matching	::=	{ parameter }+  [when expr] ->  expr
//
//    let-binding	::=	pattern =  expr
//          ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr
//
//    parameter	::=	pattern
//          ∣	 ~ label-name
//          ∣	 ~ ( label-name  [: typexpr] )
//          ∣	 ~ label-name :  pattern
//          ∣	 ? label-name
//          ∣	 ? ( label-name  [: typexpr]  [= expr] )
//          ∣	 ? label-name :  pattern
//          ∣	 ? label-name : (  pattern  [: typexpr]  [= expr] )
//

struct begin_end_parenthized_expr;
struct parenthized_expr;
struct parenthized_coercion_expr;
struct tuple_expr;
struct unary_variant_expr;
struct polymorphic_variant_expr;
struct non_empty_list_variant_expr;
struct list_variant_expr;
struct array_variant_expr;
struct record_expr;
struct with_record_expr;
struct function_application_expr;
struct prefix_symbol_expr;
struct integer_negation_expr;
struct float_negation_float_expr;
struct infix_op_expr;
struct record_access_field_expr;
struct record_mutate_field_expr;
struct array_index_access_expr;
struct array_index_mutate_expr;
struct string_index_access_expr;
struct string_index_mutate_expr;
struct if_expr;
struct while_expr;
struct for_expr;
struct sequence_expr;
struct match_expr;
struct function_expr;
struct fun_expr;
struct try_expr;
struct let_expr;
struct new_expr;
struct object_expr;
struct method_invocation_expr;
struct instance_var_mutate_expr;
struct object_duplication_expr;

struct expr
    : tagged
      , boost::spirit::extended_variant<
        value_path,
        constant,
        boost::recursive_wrapper<begin_end_parenthized_expr>,
        boost::recursive_wrapper<parenthized_expr>,
        boost::recursive_wrapper<parenthized_coercion_expr>,
        boost::recursive_wrapper<tuple_expr>,
        boost::recursive_wrapper<unary_variant_expr>,
        boost::recursive_wrapper<polymorphic_variant_expr>,
        boost::recursive_wrapper<non_empty_list_variant_expr>,
        boost::recursive_wrapper<list_variant_expr>,
        boost::recursive_wrapper<array_variant_expr>,
        boost::recursive_wrapper<record_expr>,
        boost::recursive_wrapper<with_record_expr>,
        boost::recursive_wrapper<function_application_expr>,
        boost::recursive_wrapper<prefix_symbol_expr>,
        boost::recursive_wrapper<integer_negation_expr>,
        boost::recursive_wrapper<float_negation_float_expr>,
        boost::recursive_wrapper<infix_op_expr>,
        boost::recursive_wrapper<record_access_field_expr>,
        boost::recursive_wrapper<record_mutate_field_expr>,
        boost::recursive_wrapper<array_index_access_expr>,
        boost::recursive_wrapper<array_index_mutate_expr>,
        boost::recursive_wrapper<string_index_access_expr>,
        boost::recursive_wrapper<string_index_mutate_expr>,
        boost::recursive_wrapper<if_expr>,
        boost::recursive_wrapper<while_expr>,
        boost::recursive_wrapper<for_expr>,
        boost::recursive_wrapper<sequence_expr>,
        boost::recursive_wrapper<match_expr>,
        boost::recursive_wrapper<function_expr>,
        boost::recursive_wrapper<fun_expr>,
        boost::recursive_wrapper<try_expr>,
        boost::recursive_wrapper<let_expr>,
        boost::recursive_wrapper<new_expr>,
        boost::recursive_wrapper<object_expr>,
        boost::recursive_wrapper<method_invocation_expr>,
        inst_var_name,
        boost::recursive_wrapper<instance_var_mutate_expr>,
        boost::recursive_wrapper<object_duplication_expr>
    >
{
    expr()
        : base_type() { }

    expr(value_path const &val)
        : base_type(val) { }

    expr(constant const &val)
        : base_type(val) { }

    expr(begin_end_parenthized_expr const &val)
        : base_type(val) { }

    expr(parenthized_expr const &val)
        : base_type(val) { }

    expr(parenthized_coercion_expr const &val)
        : base_type(val) { }

    expr(tuple_expr const &val)
        : base_type(val) { }

    expr(unary_variant_expr const &val)
        : base_type(val) { }

    expr(polymorphic_variant_expr const &val)
        : base_type(val) { }

    expr(non_empty_list_variant_expr const &val)
        : base_type(val) { }

    expr(list_variant_expr const &val)
        : base_type(val) { }

    expr(array_variant_expr const &val)
        : base_type(val) { }

    expr(record_expr const &val)
        : base_type(val) { }

    expr(with_record_expr const &val)
        : base_type(val) { }

    expr(function_application_expr const &val)
        : base_type(val) { }

    expr(prefix_symbol_expr const &val)
        : base_type(val) { }

    expr(integer_negation_expr const &val)
        : base_type(val) { }

    expr(float_negation_float_expr const &val)
        : base_type(val) { }

    expr(infix_op_expr const &val)
        : base_type(val) { }

    expr(record_access_field_expr const &val)
        : base_type(val) { }

    expr(record_mutate_field_expr const &val)
        : base_type(val) { }

    expr(array_index_access_expr const &val)
        : base_type(val) { }

    expr(array_index_mutate_expr const &val)
        : base_type(val) { }

    expr(string_index_access_expr const &val)
        : base_type(val) { }

    expr(string_index_mutate_expr const &val)
        : base_type(val) { }

    expr(if_expr const &val)
        : base_type(val) { }

    expr(while_expr const &val)
        : base_type(val) { }

    expr(for_expr const &val)
        : base_type(val) { }

    expr(sequence_expr const &val)
        : base_type(val) { }

    expr(match_expr const &val)
        : base_type(val) { }

    expr(function_expr const &val)
        : base_type(val) { }

    expr(fun_expr const &val)
        : base_type(val) { }

    expr(try_expr const &val)
        : base_type(val) { }

    expr(let_expr const &val)
        : base_type(val) { }

    expr(new_expr const &val)
        : base_type(val) { }

    expr(object_expr const &val)
        : base_type(val) { }

    expr(method_invocation_expr const &val)
        : base_type(val) { }

    expr(inst_var_name const &val)
        : base_type(val) { }

    expr(instance_var_mutate_expr const &val)
        : base_type(val) { }

    expr(object_duplication_expr const &val)
        : base_type(val) { }
};

struct argument_label;
struct argument_optlabel;

struct argument
    : tagged
      , boost::spirit::extended_variant<
        label,
        boost::recursive_wrapper<argument_label>,
        optlabel,
        boost::recursive_wrapper<argument_optlabel>
    >
{
    argument()
        : base_type() { }

    argument(label const &val)
        : base_type(val) { }

    argument(argument_label const &val)
        : base_type(val) { }

    argument(optlabel const &val)
        : base_type(val) { }

    argument(argument_optlabel const &val)
        : base_type(val) { }

};

struct argument_label
    : tagged
{
    label name;
    expr expr_;
};

struct argument_optlabel
    : tagged
{
    optlabel name;
    expr expr_;
};

struct pattern_match
    : tagged
{
    pattern pattrn;
    boost::optional<expr> when;
    expr returnExpr;
};

typedef std::list<pattern_match> pattern_match_list;

struct pattern_matching
    : tagged
{
    pattern_match first;
    boost::optional<pattern_match_list> other;
};

struct parameter_label_with_typexpr;
struct parameter_label_with_pattern;
struct parameter_optlabel_with_typexpr;
struct parameter_optlabel_with_pattern;
struct parameter_optlabel_with_pattern_typexpr_expr;

struct parameter
    : tagged
      , boost::spirit::extended_variant<
        pattern,
        label,
        boost::recursive_wrapper<parameter_label_with_typexpr>,
        boost::recursive_wrapper<parameter_label_with_pattern>,
        optlabel,
        boost::recursive_wrapper<parameter_optlabel_with_typexpr>,
        boost::recursive_wrapper<parameter_optlabel_with_pattern>,
        boost::recursive_wrapper<parameter_optlabel_with_pattern_typexpr_expr>
    >
{
    parameter()
        : base_type() { }

    parameter(pattern const &val)
        : base_type(val) { }

    parameter(label const &val)
        : base_type(val) { }

    parameter(parameter_label_with_typexpr const &val)
        : base_type(val) { }

    parameter(parameter_label_with_pattern const &val)
        : base_type(val) { }

    parameter(optlabel const &val)
        : base_type(val) { }

    parameter(parameter_optlabel_with_typexpr const &val)
        : base_type(val) { }

    parameter(parameter_optlabel_with_pattern const &val)
        : base_type(val) { }

    parameter(parameter_optlabel_with_pattern_typexpr_expr const &val)
        : base_type(val) { }
};

struct parameter_label_with_typexpr
    : tagged
{
    label name;
    boost::optional<typexpr> type;
};

struct parameter_label_with_pattern
    : tagged
{
    label name;
    pattern pattrn;
};

struct parameter_optlabel_with_typexpr
    : tagged
{
    optlabel name;
    boost::optional<typexpr> type;
    boost::optional<expr> expt;
};

struct parameter_optlabel_with_pattern
    : tagged
{
    optlabel name;
    pattern pattrn;
};

struct parameter_optlabel_with_pattern_typexpr_expr
    : tagged
{
    optlabel name;
    pattern pattrn;
    boost::optional<typexpr> type;
    boost::optional<expr> expt;
};

typedef std::list<parameter> parameter_list;

struct multiple_matching
    : tagged
{
    parameter_list parameters;
    boost::optional<expr> when;
    expr returnExpr;
};

struct let_binding_pattern;
struct let_binding_value;

struct let_binding
    : tagged
      , boost::spirit::extended_variant<
        boost::recursive_wrapper<let_binding_pattern>,
        boost::recursive_wrapper<let_binding_value>
    >
{
    let_binding()
        : base_type() { }

    let_binding(let_binding_pattern const &val)
        : base_type(val) { }

    let_binding(let_binding_value const &val)
        : base_type(val) { }
};

struct let_binding_pattern
    : tagged
{
    pattern pattrn;
    expr expr_;
};

struct let_binding_value
    : tagged
{
    value_name name;
    boost::optional<parameter_list> params;
    boost::optional<typexpr> type;
    boost::optional<typexpr> coercion;
    expr expr_;
};

struct begin_end_parenthized_expr
    : tagged
{
    // FIXME: tokens are not needed

    ocaml::lexer::Tokens begin;
    expr expr_;
    ocaml::lexer::Tokens end;
};

struct parenthized_expr
    : tagged
{
    expr expr_;
    typexpr type;
};

struct parenthized_coercion_expr
    : tagged
{
    expr expr_;
    boost::optional<typexpr> type;
    typexpr coercion;
};

typedef std::list<expr> expr_list;

struct tuple_expr
    : tagged
{
    expr first;
    expr_list other;
};

struct unary_variant_expr
    : tagged
{
    constr constr_;
    expr expr_;
};

struct polymorphic_variant_expr
    : tagged
{
    tag_name tag;
    expr expr_;
};

struct non_empty_list_variant_expr
    : tagged
{
    expr head;
    expr tail;
};

struct list_variant_expr
    : tagged
{
    expr first;
    boost::optional<expr_list> other;
};

struct array_variant_expr
    : tagged
{
    expr first;
    boost::optional<expr_list> other;
};

struct record_expr
    : tagged
{
    record_field first;
    boost::optional<record_field_list> other;
};

struct with_record_expr
    : tagged
{
    expr expr_;
    record_field with;
    boost::optional<record_field_list> withOther;
};

typedef std::list<argument> argument_list;

struct function_application_expr
    : tagged
{
    expr expr_;
    argument_list args;
};

struct prefix_symbol_expr
    : tagged
{
    prefix_symbol symbol;
    expr expr_;
};

struct integer_negation_expr
    : tagged
{
    expr expr_;
};

struct float_negation_float_expr
    : tagged
{
    expr expr_;
};

struct infix_op_expr
    : tagged
{
    expr left;
    infix_op operation;
    expr right;
};

struct record_access_field_expr
    : tagged
{
    expr expr_;
    field field_;
};

struct record_mutate_field_expr
    : tagged
{
    expr expr_;
    field field_;
    expr mutation;
};

struct array_index_access_expr
    : tagged
{
    expr _expr;
    expr index;
};

struct array_index_mutate_expr
    : tagged
{
    expr _expr;
    expr index;
    expr mutation;
};

struct string_index_access_expr
    : tagged
{
    expr _expr;
    expr index;
};

struct string_index_mutate_expr
    : tagged
{
    expr _expr;
    expr index;
    expr mutation;
};

struct if_expr
    : tagged
{
    expr if_;
    expr then;
    boost::optional<expr> else_;
};

struct while_expr
    : tagged
{
    expr while_;
    expr do_;
};

struct for_expr
    : tagged
{
    value_name name;
    expr from;
    expr to;
    expr do_;
};

struct sequence_expr
    : tagged
{
    expr first;
    expr second;
};

struct match_expr
    : tagged
{
    expr expr_;
    pattern_matching with;
};

struct function_expr
    : tagged
{
    pattern_matching function;
};

struct fun_expr
    : tagged
{
    multiple_matching fun;
};

struct try_expr
    : tagged
{
    expr try_;
    pattern_matching with;
};

typedef std::list<let_binding> let_binding_list;

struct let_expr
    : tagged
{
    let_binding let;
    boost::optional<let_binding_list> and_;
    expr in;
};

struct new_expr
    : tagged
{
    class_path new_;
};

struct class_body;

struct object_expr
    : tagged
{
    // FIXME: Declare class_body before this struct
    boost::recursive_wrapper<class_body> object;
};

struct method_invocation_expr
    : tagged
{
    expr expr_;
    method_name name;
};

struct instance_var_mutate_expr
    : tagged
{
    inst_var_name name;
    expr mutation;
};

// {< [ inst-var-name =  expr  { ; inst-var-name =  expr }  [;] ] >}
struct inst_var_name_expr
    : tagged
{
    inst_var_name name;
    expr expr_;
};

typedef std::list<inst_var_name_expr> inst_var_name_expr_list;

struct object_duplication_expr
    : tagged
{
    inst_var_name_expr first;
    inst_var_name_expr_list other;
};

//
// Type and exception definitions
//

//
//  BNF-like notation:
//  ========================================================================
//
//    type-definition	::=	type typedef  { and typedef }
//
//    typedef	::=	[type-params]  typeconstr-name  type-information
//
//    type-information	::=	[type-equation]  [type-representation]  { type-constraint }
//
//    type-equation	::=	= typexpr
//
//    type-representation	::=	= [|] constr-decl  { | constr-decl }
//          ∣	 = { field-decl  { ; field-decl }  [;] }
//
//    type-params	::=	type-param
//          ∣	 ( type-param  { , type-param } )
//
//    type-param	::=	[variance] '  ident
//
//    variance	::=	+
//          ∣	 -
//
//    constr-decl	::=	(constr-name ∣  ()) [ of typexpr  { * typexpr } ]
//
//    field-decl	::=	[mutable] field-name :  poly-typexpr
//
//    type-constraint	::=	constraint ' ident =  typexpr
//
//    exception-definition	::=	exception constr-name  [ of typexpr  { * typexpr } ]
//          ∣	 exception constr-name =  constr
//

struct type_constraint
    : tagged
{
    ident ident_;
    typexpr type;
};

struct field_decl
    : tagged
{
    field_name name;
    poly_typexpr type;
};

struct constr_decl
    : tagged
{
    boost::optional<constr_name> name;
    boost::optional<typexpr_list> types;
};

// + | -
struct variance
    : tagged
{
    ocaml::lexer::Tokens token;
};

struct type_param
    : tagged
{
    boost::optional<variance> variance;
    ident ident_;
};

typedef std::list<type_param> type_param_list;

struct type_params
    : tagged
      , boost::spirit::extended_variant<type_param,
        type_param_list>
{
    type_params()
        : base_type() { }

    type_params(type_param const &val)
        : base_type(val) { }

    type_params(type_param_list const &val)
        : base_type(val) { }
};

typedef std::list<constr_decl> constr_decl_list;
typedef std::list<field_decl> field_decl_list;

struct type_representation
    : tagged
      , boost::spirit::extended_variant<constr_decl_list, field_decl_list>
{
    type_representation()
        : base_type() { }

    type_representation(constr_decl_list const &val)
        : base_type(val) { }

    type_representation(field_decl_list const &val)
        : base_type(val) { }
};

struct type_equation
    : tagged
{
    typexpr type;
};

typedef std::list<type_constraint> type_constraint_list;

struct type_information
    : tagged
{
    boost::optional<type_equation> equation;
    boost::optional<type_representation> representation;
    boost::optional<type_constraint_list> constraints;
};

struct typedef_
    : tagged
{
    boost::optional<type_params> params;
    typeconstr_name name;
    type_information information;
};

typedef std::list<typedef_> typedef_list;

struct type_definition
    : tagged
{
    typedef_ typeDef;
    boost::optional<typedef_list> and_;
};

//
// Classes
//

//
//  BNF-like notation:
//  ========================================================================
//
//    class-type	::=	[[?]label-name:]  typexpr ->  class-type
//          ∣	   class-body-type
//
//    class-body-type	::=	object [( typexpr )]  {class-field-spec} end
//          ∣	  [[ typexpr  {, typexpr} ]]  classtype-path
//
//    class-field-spec	::=	inherit class-body-type
//          ∣	  val [mutable] [virtual] inst-var-name :  typexpr
//          ∣	  val virtual mutable inst-var-name :  typexpr
//          ∣	  method [private] [virtual] method-name :  poly-typexpr
//          ∣	  method virtual private method-name :  poly-typexpr
//          ∣	  constraint typexpr =  typexpr
//
//    class-expr	::=	class-path
//          ∣	  [ typexpr  {, typexpr} ]  class-path
//          ∣	  ( class-expr )
//          ∣	  ( class-expr :  class-type )
//          ∣	  class-expr  {argument}+
//          ∣	  fun {parameter}+ ->  class-expr
//          ∣	  let [rec] let-binding  {and let-binding} in  class-expr
//          ∣	  object class-body end
//
//    class-field	::=	inherit class-expr  [as lowercase-ident]
//          ∣	  val [mutable] inst-var-name  [: typexpr] =  expr
//          ∣	  val [mutable] virtual inst-var-name :  typexpr
//          ∣	  val virtual mutable inst-var-name :  typexpr
//          ∣	  method [private] method-name  {parameter}  [: typexpr] =  expr
//          ∣	  method [private] method-name :  poly-typexpr =  expr
//          ∣	  method [private] virtual method-name :  poly-typexpr
//          ∣	  method virtual private method-name :  poly-typexpr
//          ∣	  constraint typexpr =  typexpr
//          ∣	  initializer expr
//
//    class-body	::=	  [( pattern  [: typexpr] )]  { class-field }
//
//    class-definition	::=	class class-binding  { and class-binding }
//
//    class-binding	::=	[virtual] [[ type-parameters ]]  class-name  {parameter}  [: class-type]  =  class-expr
//
//    type-parameters	::=	' ident  { , ' ident }
//
//    class-specification	::=	class class-spec  { and class-spec }
//
//    class-spec	::=	[virtual] [[ type-parameters ]]  class-name :  class-type
//
//    classtype-definition	::=	class type classtype-def  { and classtype-def }
//
//    classtype-def	::=	[virtual] [[ type-parameters ]]  class-name =  class-body-type
//

// Placeholder
struct class_body { };

//
// Module types (module specifications)
//

//
//  BNF-like notation:
//  ========================================================================
//
//    module-type	::=	modtype-path
//          ∣	 sig { specification  [;;] } end
//          ∣	 functor ( module-name :  module-type ) ->  module-type
//          ∣	 module-type with  mod-constraint  { and mod-constraint }
//          ∣	 ( module-type )
//
//    mod-constraint	::=	type [type-params]  typeconstr  type-equation
//          ∣	 module module-path =  extended-module-path
//
//    specification	::=	val value-name :  typexpr
//          ∣	 external value-name :  typexpr =  external-declaration
//          ∣	 type-definition
//          ∣	 exception constr-decl
//          ∣	 class-specification
//          ∣	 classtype-definition
//          ∣	 module module-name :  module-type
//          ∣	 module module-name  { ( module-name :  module-type ) } :  module-type
//          ∣	 module type modtype-name
//          ∣	 module type modtype-name =  module-type
//          ∣	 open module-path
//          ∣	 include module-type
//


//
// Module expressions (module implementations)
//

//
//  BNF-like notation:
//  ========================================================================
//
//    module-expr	::=	module-path
//          ∣	 struct [ module-items ] end
//          ∣	 functor ( module-name :  module-type ) ->  module-expr
//          ∣	 module-expr (  module-expr )
//          ∣	 ( module-expr )
//          ∣	 ( module-expr :  module-type )
//
//    module-items	::=	[;;] ( definition ∣  expr )  { [;;] definition ∣  ;; expr }  [;;]
//
//    definition	::=	let [rec] let-binding  { and let-binding }
//          ∣	 external value-name :  typexpr =  external-declaration
//          ∣	 type-definition
//          ∣	 exception-definition
//          ∣	 class-definition
//          ∣	 classtype-definition
//          ∣	 module module-name  { ( module-name :  module-type ) }  [ : module-type ]  =  module-expr
//          ∣	 module type modtype-name =  module-type
//          ∣	 open module-path
//          ∣	 include module-expr
//


//
// Compilation units
//

//
//  BNF-like notation:
//  ========================================================================
//
//    unit-interface	::=	 { specification  [;;] }
//
//    unit-implementation	::=	 [ module-items ]
//

} // namespace ast
} // namespace ocaml

#endif //FLANG_OCAMLAST_H
