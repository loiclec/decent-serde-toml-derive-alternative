
extern crate proc_macro;
use proc_macro2::{self, Literal};

#[macro_use]
extern crate decent_synquote_alternative;

use decent_synquote_alternative::token_builder::TokenBuilderExtend;
use decent_synquote_alternative::parser as parser;
use decent_synquote_alternative::token_builder as token_builder;

use parser::{TokenParser, WhereClause, WhereClauseItem};
use token_builder::TokenBuilder;


#[proc_macro_derive(FromToml)]
pub fn derive_from_toml(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let mut parser = TokenParser::new(input);

    let mut tb = TokenBuilder::new();

    if let Some(s) = parser.eat_struct() {
        derive_from_toml_struct(s, &mut tb)
    } else if let Some(e) = parser.eat_enumeration() {
        derive_from_toml_enum(e, &mut tb)
    } else {
        extend_ts!(&mut tb,
            "compile_error ! ("
            Literal::string("decent-serde-toml-derive-alternative could not parse the structure")
            ") ;"
        )
    }
    let ts = tb.end();
    // eprintln!("{}", ts);
    ts.into()
}

fn derive_from_toml_struct(parsed_struct: parser::Struct, tb: &mut TokenBuilder) {
    let from_toml = ts!(":: decent_toml_rs_alternative :: FromToml");

    let mut where_clause_in_impl_from = parsed_struct.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
    for ty in &parsed_struct.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: from_toml.clone(),
        })
    }

    /*
    let mut field1 = None;
    let mut field2 = None;
    for (key, value) in o.iter() {
        match key {
            "field1" => {
                field1 = Some(<_>::from_toml(value)?);
            }
            "field2" => {
                field2 = Some(<_>::from_toml(value)?);
            }
            _ => { }
        }
    }
    let field1 = field1?;
    let field2 = field2?;

    Some ( Self {
        field1: field1,
        field2: field2,
    } )
    */
    extend_ts!(tb,
        "impl" parsed_struct.generics.removing_eq_type() from_toml "for" parsed_struct.ident parsed_struct.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
        "{
            fn from_toml ( fromtoml : :: std :: option :: Option < & :: decent_toml_rs_alternative :: TomlValue > ) -> Option < Self > {
                if let Some ( :: decent_toml_rs_alternative :: TomlValue :: Table ( o ) ) = fromtoml {"
                    join_ts!(&parsed_struct.struct_fields, f, 
                        "let mut" f.safe_ident() "= None ;"
                    )
                    "for ( key , value ) in o . iter ( ) {
                        match key . as_str ( ) {
                        "
                        join_ts!(&parsed_struct.struct_fields, f, {
                            let literal_key = Literal::string(&f.access().to_string());
                            ts!(
                            literal_key " => {"
                                f.safe_ident() " = Some ( value ) ;"
                            "
                            }"
                            )
                        })
                            "_ => { }"
                    "   }
                    }
                    "
                    join_ts!(&parsed_struct.struct_fields, f, 
                        "let" f.safe_ident() "= < _ > :: from_toml ( " f.safe_ident() " ) ? ; "
                    )

                    "Some ( Self {"
                        join_ts!(&parsed_struct.struct_fields, f, 
                            f.access() ":" f.safe_ident()
                        , separator: ",")
                    "} )"
                "} else {
                    None
                }
                "
            "}
        }"
    )
}

/*
{
    kind: "VariantA",
    payload: {
        ...
    }
}
*/

fn derive_from_toml_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let from_toml = ts!(":: decent_toml_rs_alternative :: FromToml");

    let mut where_clause_in_impl_from = parsed_enum.where_clause.clone().unwrap_or_else(|| WhereClause::default());

    for ty in &parsed_enum.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: from_toml.clone(),
        })
    }

    let kind_literal = Literal::string("kind");
    let payload_literal = Literal::string("payload");

    extend_ts!(tb,
        "impl" parsed_enum.generics.removing_eq_type() from_toml "for" parsed_enum.ident parsed_enum.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
        "{
            fn from_toml ( fromtoml : Option < & decent_toml_rs_alternative :: TomlValue > ) -> Option < Self > {
                if let Some ( :: decent_toml_rs_alternative :: TomlValue :: Table ( table ) ) = fromtoml {
                    if ! table . contains_key ( " kind_literal " ) { return None }
                    let kind = match & table [ " kind_literal " ] {
                        :: decent_toml_rs_alternative :: TomlValue :: String ( x ) => {
                            x . clone ( )
                        }
                        _ => {
                            return None
                        }
                    } ;
                    match kind . as_str ( ) {"
                        join_ts!(&parsed_enum.items, item, {
                            let variant_literal = Literal::string(&item.ident.to_string());
                            ts!(
                            variant_literal "=> {"
                                if let Some(fields) = item.get_struct_data().map(|d| d.1) {
                                    ts!(
                                    "if ! table . contains_key ( " payload_literal ") {
                                        return None
                                    }
                                    if let :: decent_toml_rs_alternative :: TomlValue :: Table ( payload ) = & table [ " payload_literal " ] {"
                                        join_ts!(fields, f, 
                                            "let mut" f.safe_ident() "= None ;"
                                        )
                                        "for ( key , value ) in payload . iter ( ) {
                                            match key . as_str ( ) {
                                            "
                                            join_ts!(fields, f, {
                                                let literal_key = Literal::string(&f.access().to_string());
                                                ts!(
                                                literal_key " => {" 
                                                    f.safe_ident() " = Some ( value ) ;" 
                                                "
                                                }"
                                                )
                                            })
                                                "_ => { }"
                                        "   }
                                        }
                                        "
                                        join_ts!(fields, f, 
                                            "let" f.safe_ident() "= < _ > :: from_toml ( " f.safe_ident() " ) ? ;"
                                        )
                                        "Some ( Self :: " item.ident " {"
                                            join_ts!(fields, f, 
                                                f.access() ":" f.safe_ident()
                                            , separator: ",")
                                        "} )"
                                    "} else {
                                        return None
                                    }"
                                    )
                                } else {
                                    ts!(
                                    "Some ( Self :: " item.ident " )"
                                    )
                                }
                            "}"
                            )
                        })
                        
                        "_ => { return None }"
                        // "Variant1" => {
                        //     if !o.contains_key("payload") {
                        //         return None
                        //     }
                        //     if let TomlValue::Table(payload) = o["payload"] {
                        //         let mut field1 = None;
                        //         let mut field2 = None;
                        //         for (key, value) in payload.iter() {
                        //             match key {
                        //                 "field1" => {
                        //                     field1 = Some(<_>::from_toml(value)?);
                        //                 }
                        //                 "field2" => {
                        //                     field2 = Some(<_>::from_toml(value)?);
                        //                 }
                        //                 _ => { }
                        //             }
                        //         }
                        //         let field1 = field1?;
                        //         let field2 = field2?;
            
                        //         Some ( Self::Variant1 {
                        //             field1: field1,
                        //             field2: field2,
                        //         } )
                        //     } else {
                        //         return None
                        //     }
                        // }
                        // "Variant2" => {
                        //     Some(Self::Variant2)
                        // }
                        // _ => {
                        //     return None
                        // }
                    "}
                } else {
                    return None
                }
            }
        }"
    )
}


#[proc_macro_derive(ToToml)]
pub fn derive_to_toml(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let mut parser = TokenParser::new(input);

    let mut tb = TokenBuilder::new();

    if let Some(s) = parser.eat_struct() {
        derive_to_toml_struct(s, &mut tb)
    } else if let Some(e) = parser.eat_enumeration() {
        derive_to_toml_enum(e, &mut tb)
    } else {
        extend_ts!(&mut tb,
            "compile_error ! ("
            Literal::string("decent-serde-toml-derive-alternative could not parse the structure")
            ") ;"
        )
    }
    let ts = tb.end();
    // eprintln!("{}", ts);
    ts.into()
}

fn derive_to_toml_struct(parsed_struct: parser::Struct, tb: &mut TokenBuilder) {
    let to_toml = ts!(":: decent_toml_rs_alternative :: ToToml");

    let mut where_clause_in_impl_from = parsed_struct.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
    for ty in &parsed_struct.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: to_toml.clone(),
        })
    }
    /*
        let mut table = :: decent_toml_rs_alternative :: table :: Table :: new () ;
        table . insert ( "field1" , self . field1 . to_toml ( ) ) ;
        table . insert ( "field2" , self . field2 . to_toml ( ) ) ;
        :: decent_toml_rs_alternative :: TomlValue :: Table ( table )
    */
    /*
        let mut table = HashMap::new();
        if let Some(value) = value.to_toml() {
            table.insert(key.clone(), value);
        }
        Some(TomlValue::Table(table))
    */
    extend_ts!(tb,
    "impl" parsed_struct.generics.removing_eq_type() to_toml "for" parsed_struct.ident parsed_struct.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
    "{
        fn to_toml ( & self ) -> Option < :: decent_toml_rs_alternative :: TomlValue > {
            let mut table = :: std :: collections :: HashMap :: < String , :: decent_toml_rs_alternative :: TomlValue > :: new ( ) ;
            "
            join_ts!(parsed_struct.struct_fields, f, 
                "
                if let Some ( " f.safe_ident() " ) = self . " f.access() " . to_toml ( ) {
                    table . insert ( " Literal::string(&f.access().to_string()) " . to_string ( ) , " f.safe_ident() ") ;
                }"
            )
            "
            Some ( :: decent_toml_rs_alternative :: TomlValue :: Table ( table ) )
        }
    }"
    )
}

fn derive_to_toml_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let to_toml = ts!(":: decent_toml_rs_alternative :: ToToml");

    let mut where_clause_in_impl_from = parsed_enum.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
    for ty in &parsed_enum.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: to_toml.clone(),
        })
    }
   
    extend_ts!(tb,
    "# [ allow ( non_shorthand_field_patterns ) ]"
    "impl" parsed_enum.generics.removing_eq_type() to_toml "for" parsed_enum.ident parsed_enum.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
    "{
        fn to_toml ( & self )  -> Option < :: decent_toml_rs_alternative :: TomlValue > {
            let mut table = :: std :: collections :: HashMap :: < String , :: decent_toml_rs_alternative :: TomlValue > :: new ( ) ;
            match self {"
            join_ts!(parsed_enum.items, item,
                item.pattern_match(&parsed_enum.ident, None) "=> {"
                    "table . insert ( " Literal::string("kind") " . to_string ( ) , :: decent_toml_rs_alternative :: TomlValue :: String ("  Literal::string(&item.ident.to_string()) " . to_string ( ) ) ) ;"
                    if let Some(fields) = item.get_struct_data().map(|d| d.1) {
                        ts!("
                            let mut payload = :: std :: collections :: HashMap :: < String , :: decent_toml_rs_alternative :: TomlValue > :: new ( ) ;
                            "
                            join_ts!(fields, f, 
                                "if let Some ( " f.safe_ident() " ) = " f.safe_ident() " . to_toml ( ) {
                                    payload . insert ( " Literal::string(&f.access().to_string()) " . to_string ( ) , " f.safe_ident() ") ;
                                }"
                            )
                            "table . insert ( " Literal::string("payload") " . to_string ( ) , :: decent_toml_rs_alternative :: TomlValue :: Table ( payload ) ) ; "
                        )
                    } else {
                        ts!()
                    }
                    "
                    Some ( :: decent_toml_rs_alternative :: TomlValue :: Table ( table ) )
                }"
            )
            "}
        }
    }")
}
