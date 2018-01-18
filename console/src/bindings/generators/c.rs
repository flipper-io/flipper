use std::fs::File;
use std::io::Write;
use std::rc::Rc;
use handlebars;
use serde_derive;
use failure::Error;

use handlebars::{
    Handlebars,
    Helper,
    RenderContext,
    RenderError,
};

use bindings::{
    Type,
    Parameter,
    Subprogram,
};

#[derive(Debug, Fail)]
enum GeneratorError {
    #[fail(display = "failed to find a required type name")]
    TypenameLookupError,
}

/// A serializable representation of a Parameter in C.
/// This is used by handlebars to populate the C template.
#[derive(Debug, Serialize)]
struct CParameter {
    name: String,
    typ: String,
}

/// A serializable representation of a Function in C.
/// This is used by handlebars to populate the C template.
#[derive(Debug, Serialize)]
struct CFunction {
    name: String,
    params: Vec<CParameter>,
    ret: String,
    ret_fmr: String,
}

/// A serializable representation of a Module in C.
/// This is used by handlebars to populate the C template.
#[derive(Debug, Serialize)]
struct CModule {
    name: String,
    description: String,
    funcs: Vec<CFunction>,
}

impl From<Parameter> for CParameter {
    fn from(param: Parameter) -> Self {
        CParameter {
            name: param.name,
            typ: param.typ.name(),
        }
    }
}

impl From<Subprogram> for CFunction {
    fn from(func: Subprogram) -> Self {
        let params: Vec<CParameter> = func.parameters.into_iter().map(|param| param.into()).collect();

        let ret = func.ret.name();
        let ret_size = func.ret.size();

        let fmr_type = match ret_size {
            0 => "fmr_void_t".to_owned(),
            1 => "fmr_uint8_t".to_owned(),
            2 => "fmr_uint16_t".to_owned(),
            4 => "fmr_uint32_t".to_owned(),
            8 => "fmr_uint64_t".to_owned(),
            _ => "fmr_void_t".to_owned(),
        };

        CFunction {
            name: func.name,
            params,
            ret,
            ret_fmr: fmr_type,
        }
    }
}

/// C does not allow trailing commas in struct definitions, so we need
/// a custom handlebars helper which takes the appropriate struct contents
/// and appends them to the template, omitting the last comma of each entry.
fn struct_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let values = h.param(0).unwrap().value().as_array().unwrap();
    let module_name = h.param(1).unwrap().value().as_str().unwrap();
    let mut funcs = String::new();

    if values.len() > 0 {
        funcs.push_str(module_name);
        funcs.push('_');
        funcs.push_str(values[0].as_object().unwrap().get("name").unwrap().as_str().unwrap());
    }

    for value in values.iter().skip(1) {
        funcs.push_str(",\n    ");
        funcs.push_str(module_name);
        funcs.push('_');
        funcs.push_str(value.as_object().unwrap().get("name").unwrap().as_str().unwrap());
    }

    rc.writer.write(funcs.into_bytes().as_ref())?;

    Ok(())
}

/// C does not allow trailing commas in parameter lists, so we need
/// a custom handlebars helper which takes the appropriate arguments
/// and appends them to the template, omitting the last comma of each entry.
fn param_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let param = h.param(0).unwrap().value();
    let mut params = String::new();

    let values = param.as_array().unwrap();
    if values.len() > 0 {
        params.push_str(values[0].as_object().unwrap().get("typ").unwrap().as_str().unwrap());
        params.push(' ');
        params.push_str(values[0].as_object().unwrap().get("name").unwrap().as_str().unwrap());
    }

    for value in values.iter().skip(1) {
        params.push_str(", ");
        params.push_str(value.as_object().unwrap().get("typ").unwrap().as_str().unwrap());
        params.push(' ');
        params.push_str(value.as_object().unwrap().get("name").unwrap().as_str().unwrap());
    }

    rc.writer.write(params.into_bytes().as_ref())?;

    Ok(())
}

/// C does not allow trailing commas in parameter lists, so we need
/// a custom handlebars helper. However, fmr parameters need to be wrapped
/// in a `fmr_infer( )` macro, so this helper does that as well.
fn fmr_expansion_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    let param = h.param(0).unwrap().value();
    let mut fmr_args = String::new();

    let values = param.as_array().unwrap();
    if values.len() > 0 {
        fmr_args.push_str("fmr_infer(");
        fmr_args.push_str(values[0].as_object().unwrap().get("name").unwrap().as_str().unwrap());
        fmr_args.push(')');
    }

    for value in values.iter().skip(1) {
        fmr_args.push_str(", ");
        fmr_args.push_str("fmr_infer(");
        fmr_args.push_str(value.as_object().unwrap().get("name").unwrap().as_str().unwrap());
        fmr_args.push(')');
    }

    rc.writer.write(fmr_args.into_bytes().as_ref())?;

    Ok(())
}

/// Configures handlebars, serializes the given `CModule`, and writes it
/// to the given output.
fn generate_module<W: Write>(module: &CModule, out: &mut W) -> Result<(), Error> {
    let mut reg = Handlebars::new();
    reg.register_helper("param_expansion", Box::new(param_helper));
    reg.register_helper("struct_expansion", Box::new(struct_helper));
    reg.register_helper("fmr_expansion", Box::new(fmr_expansion_helper));
    reg.register_template_file("c", "./src/bindings/generators/templates/c.hbs").unwrap();

    write!(out, "{}", reg.render("c", &module).unwrap());

    Ok(())
}

mod test {
    use super::*;

    #[test]
    fn test_into_cparameter() {

        // Base types
        let b0 = Rc::new(Type::Base { name: "int".to_owned(), size: 4 });
        let b1 = Rc::new(Type::Base { name: "unsigned char".to_owned(), size: 1 });
        let b2 = Rc::new(Type::Base { name: "short unsigned int".to_owned(), size: 2 });
        let b3 = Rc::new(Type::Base { name: "unsigned int".to_owned(), size: 4 });
        let b4 = Rc::new(Type::Base { name: "char".to_owned(), size: 1 });

        // Alias types
        let a0 = Rc::new(Type::Alias { name: "uint8_t".to_owned(), typ: b1.clone() });
        let a1 = Rc::new(Type::Alias { name: "uint16_t".to_owned(), typ: b2.clone() });
        let a2 = Rc::new(Type::Alias { name: "uint32_t".to_owned(), typ: b3.clone() });

        // Reference types
        let r0 = Rc::new(Type::Reference { typ: b4.clone() });

        // Parameters
        let p0 = Parameter { name: "first".to_owned(), typ: r0.clone() };
        let p1 = Parameter { name: "second".to_owned(), typ: a1.clone() };
        let p2 = Parameter { name: "third".to_owned(), typ: a2.clone() };

        let cp0: CParameter = p0.into();
        let cp1: CParameter = p1.into();
        let cp2: CParameter = p2.into();
        println!("Got CParameter: {:?}", cp0);
        println!("Got CParameter: {:?}", cp1);
        println!("Got CParameter: {:?}", cp2);
    }

    #[test]
    fn test_from_dwarf() {
        use bindings::dwarf::parse;

        let dwarf: &[u8] = include_bytes!("../test_resources/dwarf_parse_test");
        let result = parse(dwarf);
        assert!(result.is_ok());
        let subprograms = result.unwrap();
        let funcs: Vec<CFunction> = subprograms.into_iter().map(|s| s.into()).collect();

        let module = CModule {
            name: "user".to_owned(),
            description: "A user module".to_owned(),
            funcs,
        };

        let mut file = File::create("./binding.c").unwrap();
        generate_module(&module, &mut file);
    }
}