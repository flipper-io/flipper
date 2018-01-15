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

use ::bindings::meta::{
    Type,
    Parameter,
    Subprogram,
};

#[derive(Debug, Fail)]
enum GeneratorError {
    #[fail(display = "failed to find a required type name")]
    TypenameLookupError,
}

#[derive(Debug, Serialize)]
struct CParameter {
    name: String,
    typ: String,
}

#[derive(Debug, Serialize)]
struct CFunction {
    name: String,
    params: Vec<CParameter>,
    ret: String,
    ret_fmr: String,
}

#[derive(Debug, Serialize)]
struct CModule {
    name: String,
    description: String,
    funcs: Vec<CFunction>,
}

fn resolve_typename(typ: Option<&Rc<Type>>) -> Option<String> {

    if typ.is_none() { return Some("void".to_owned()); }
    let typ = typ.unwrap();

    match **typ {
        Type::Base { ref name, .. } => Some(name.clone()),
        Type::Alias { ref name, .. } => Some(name.clone()),
        Type::Reference { ref typ, .. } => {
            let string = resolve_typename(typ.as_ref());
            string.map(|s| {
                s.clone().push('*');
                s
            })
        }
        _ => None,
    }
}

fn resolve_typesize(typ: Option<&Rc<Type>>) -> Option<u64> {

    if typ.is_none() { return Some(0); }
    let typ = typ.unwrap();

    match **typ {
        Type::Base { size, .. } => Some(size),
        Type::Alias { ref typ, .. } => resolve_typesize(Some(typ)),
        Type::Reference { ref typ, .. } => resolve_typesize(typ.as_ref()),
        _ => None,
    }
}

fn into_cparameter(param: Parameter) -> Option<CParameter> {
    if param.typ.is_none() { return None; }

    resolve_typename(param.typ.as_ref())
        .map(|typ| CParameter {
            name: param.name,
            typ,
        })
}

fn into_cfunction(func: Subprogram) -> Option<CFunction> {
    let params: Vec<Option<CParameter>> = func.parameters.into_iter().map(|param| {
        into_cparameter(param)
    }).collect();

    if params.iter().any(|p| p.is_none()) { return None; }
    let params: Vec<_> = params.into_iter().map(|p| p.unwrap()).collect();

    let ret = resolve_typename(func.ret.as_ref());
    if ret.is_none() { return None; }
    let ret = ret.unwrap();

    let ret_size = resolve_typesize(func.ret.as_ref());
    if ret_size.is_none() { return None; }
    let ret_size = ret_size.unwrap();

    let fmr_type = match ret_size {
        0 => "fmr_void_t".to_owned(),
        1 => "fmr_uint8_t".to_owned(),
        2 => "fmr_uint16_t".to_owned(),
        4 => "fmr_uint32_t".to_owned(),
        8 => "fmr_uint64_t".to_owned(),
        _ => "fmr_void_t".to_owned(),
    };

    Some(CFunction {
        name: func.name,
        params,
        ret,
        ret_fmr: fmr_type,
    })
}

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

fn param_helper(h: &Helper, _: &Handlebars, rc: &mut RenderContext) -> Result<(), RenderError> {
    // Should be a list of CParameters.
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

fn generate_module<W: Write>(module: &CModule, out: &mut W) -> Result<(), Error> {
    let mut reg = Handlebars::new();
    reg.register_helper("param_expansion", Box::new(param_helper));
    reg.register_helper("struct_expansion", Box::new(struct_helper));
    reg.register_template_file("c", "/home/nick/Documents/flipper/console/src/bindings/generators/templates/c.hbs").unwrap();

    write!(out, "{}", reg.render("c", &module).unwrap());

    Ok(())
}

mod test {
    use super::*;

    #[test]
    fn test_into_cparameter() {

        // Base types
        let b0 = Rc::new(Type::Base { name: "int".to_owned(), offset: 59, size: 4 });
        let b1 = Rc::new(Type::Base { name: "unsigned char".to_owned(), offset: 84, size: 1 });
        let b2 = Rc::new(Type::Base { name: "short unsigned int".to_owned(), offset: 102, size: 2 });
        let b3 = Rc::new(Type::Base { name: "unsigned int".to_owned(), offset: 120, size: 4 });
        let b4 = Rc::new(Type::Base { name: "char".to_owned(), offset: 245, size: 1 });

        // Alias types
        let a0 = Rc::new(Type::Alias { name: "uint8_t".to_owned(), offset: 73, typ: b1.clone() });
        let a1 = Rc::new(Type::Alias { name: "uint16_t".to_owned(), offset: 91, typ: b2.clone() });
        let a2 = Rc::new(Type::Alias { name: "uint32_t".to_owned(), offset: 109, typ: b3.clone() });

        // Reference types
        let r0 = Rc::new(Type::Reference { offset: 239, typ: Some(b4.clone()) });

        // Parameters
        let p0 = Parameter { name: "first".to_owned(), typ: Some(r0.clone()) };
        let p1 = Parameter { name: "second".to_owned(), typ: Some(a1.clone()) };
        let p2 = Parameter { name: "third".to_owned(), typ: Some(a2.clone()) };

        let cp0 = into_cparameter(p0);
        let cp1 = into_cparameter(p1);
        let cp2 = into_cparameter(p2);
        println!("Got CParameter: {:?}", cp0);
        println!("Got CParameter: {:?}", cp1);
        println!("Got CParameter: {:?}", cp2);
    }

    #[test]
    fn test_render() {
        let data = CModule {
            name: "uart_blah".to_owned(),
            description: "The best user module to date".to_owned(),
            funcs: vec! [
                CFunction {
                    name: "func_one".to_owned(),
                    ret: "int".to_owned(),
                    params: vec![
                        CParameter {
                            name: "name".to_owned(),
                            typ: "char *".to_owned(),
                        }
                    ],
                    ret_fmr: "fmr_int32_t".to_owned(),
                },
                CFunction {
                    name: "func_two".to_owned(),
                    ret: "char *".to_owned(),
                    params: vec![
                        CParameter {
                            name: "arg".to_owned(),
                            typ: "uint8_t".to_owned(),
                        }
                    ],
                    ret_fmr: "fmr_ptr_t".to_owned(),
                },
            ],
        };

        let mut file = File::create("./binding.c").unwrap();
        generate_module(&data, &mut file);
    }
}