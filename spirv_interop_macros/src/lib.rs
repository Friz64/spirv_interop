use proc_macro2::{Literal, TokenStream};
use quote::quote;
use rspirv::dr::{Instruction, Module, Operand};
use spirv_headers::{Decoration, Op, Word};
use std::{env, fs, path::Path};
use syn::{
    parse::{self, Parse, ParseStream},
    parse_macro_input, Ident, LitStr, Token, Visibility,
};

struct Reflect {
    visibility: Visibility,
    name: Ident,
    shader_path: LitStr,
}

impl Parse for Reflect {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let visibility = input.parse()?;
        input.parse::<Token![mod]>()?;
        let name = input.parse()?;
        input.parse::<Token![:]>()?;
        let shader_path = input.parse()?;

        Ok(Reflect {
            visibility,
            name,
            shader_path,
        })
    }
}

fn inst(insts: &[Instruction], id: Word) -> &Instruction {
    insts
        .iter()
        .find(|inst| inst.result_id == Some(id))
        .expect("failed to find instruction")
}

fn name(module: &Module, id: Word) -> &String {
    module
        .debugs
        .iter()
        .find_map(|inst| match (inst.class.opcode, inst.operands.as_slice()) {
            (Op::Name, [Operand::IdRef(id_ref), Operand::LiteralString(name_str)])
                if id_ref == &id =>
            {
                Some(name_str)
            }
            _ => None,
        })
        .expect("failed to find name")
}

fn member_name(module: &Module, id: Word, member_idx: Word) -> &String {
    module
        .debugs
        .iter()
        .find_map(|inst| {
            if let Op::MemberName = inst.class.opcode {
                match (&inst.operands[0], &inst.operands[1], &inst.operands[2]) {
                    (
                        &Operand::IdRef(id_ref),
                        &Operand::LiteralInt32(member_idx_ref),
                        Operand::LiteralString(name_str),
                    ) if id_ref == id && member_idx_ref == member_idx => Some(name_str),
                    _ => None,
                }
            } else {
                None
            }
        })
        .expect("failed to find member name")
}

fn member_decorations(
    module: &Module,
    id: Word,
    member_idx: Word,
) -> Vec<(Decoration, Option<Operand>)> {
    module
        .annotations
        .iter()
        .filter_map(|inst| {
            if let Op::MemberDecorate = inst.class.opcode {
                match (&inst.operands[0], &inst.operands[1], &inst.operands[2]) {
                    (
                        &Operand::IdRef(id_ref),
                        &Operand::LiteralInt32(member_idx_ref),
                        &Operand::Decoration(decoration),
                    ) if id_ref == id && member_idx_ref == member_idx => {
                        Some((decoration, inst.operands.get(3).cloned()))
                    }
                    _ => None,
                }
            } else {
                None
            }
        })
        .collect()
}

fn int_convert(type_inst: &Instruction) -> Option<TokenStream> {
    match (type_inst.class.opcode, type_inst.operands.as_slice()) {
        (Op::TypeInt, [Operand::LiteralInt32(bits), Operand::LiteralInt32(signed)]) => {
            Some(match (bits, signed) {
                (64, 0) => quote! { u64 },
                (32, 0) => quote! { u32 },
                (16, 0) => quote! { u16 },
                (8, 0) => quote! { u8 },
                (64, 1) => quote! { i64 },
                (32, 1) => quote! { i32 },
                (16, 1) => quote! { i16 },
                (8, 1) => quote! { i8 },
                _ => panic!(),
            })
        }
        _ => None,
    }
}

fn float_convert(type_inst: &Instruction) -> Option<TokenStream> {
    match (type_inst.class.opcode, type_inst.operands.as_slice()) {
        (Op::TypeFloat, [Operand::LiteralInt32(bits)]) => Some(match bits {
            64 => quote! { f64 },
            32 => quote! { f32 },
            _ => panic!(),
        }),
        _ => None,
    }
}

fn vector_info(
    module: &Module,
    type_inst: &Instruction,
    decorations: &[(Decoration, Option<Operand>)],
) -> Option<(TokenStream, Word)> {
    match (type_inst.class.opcode, type_inst.operands.as_slice()) {
        (Op::TypeVector, [Operand::IdRef(inner_ref), Operand::LiteralInt32(size)]) => {
            let inner_ty = type_convert(module, *inner_ref, decorations);
            Some((inner_ty, *size))
        }
        _ => None,
    }
}

fn matrix_info(
    module: &Module,
    type_inst: &Instruction,
    decorations: &[(Decoration, Option<Operand>)],
) -> Option<(TokenStream, Word, Word)> {
    match (type_inst.class.opcode, type_inst.operands.as_slice()) {
        (Op::TypeMatrix, [Operand::IdRef(inner_ref), Operand::LiteralInt32(major_size)]) => {
            let (inner_ty, minor_size) = vector_info(
                module,
                inst(&module.types_global_values, *inner_ref),
                decorations,
            )
            .unwrap();

            Some((inner_ty, *major_size, minor_size))
        }
        _ => None,
    }
}

fn constant_info(inst: &Instruction) -> Option<(Word, Word)> {
    match (inst.class.opcode, inst.operands.as_slice()) {
        (Op::Constant, [Operand::LiteralInt32(size)]) => Some((inst.result_type.unwrap(), *size)),
        _ => None,
    }
}

fn type_convert(
    module: &Module,
    type_id: Word,
    decorations: &[(Decoration, Option<Operand>)],
) -> TokenStream {
    let type_inst = inst(&module.types_global_values, type_id);

    match type_inst.class.opcode {
        Op::TypeArray => {
            let (inner_ty_ref, size_ref) = match type_inst.operands.as_slice() {
                [Operand::IdRef(inner_ty_ref), Operand::IdRef(size_ref)] => {
                    (*inner_ty_ref, *size_ref)
                }
                _ => panic!(),
            };
            let inner_ty = type_convert(module, inner_ty_ref, decorations);
            let size = constant_info(inst(&module.types_global_values, size_ref)).unwrap();
            let size_literal = Literal::usize_unsuffixed(size.1 as usize);

            quote! {
                [#inner_ty; #size_literal]
            }
        }
        Op::TypeInt => int_convert(type_inst).unwrap(),
        Op::TypeFloat => float_convert(type_inst).unwrap(),
        Op::TypeVector => match vector_info(module, type_inst, decorations).unwrap() {
            (float_ty, 2) => quote! { ::spirv_interop::mint::Vector2<#float_ty> },
            (float_ty, 3) => quote! { ::spirv_interop::mint::Vector3<#float_ty> },
            (float_ty, 4) => quote! { ::spirv_interop::mint::Vector4<#float_ty> },
            _ => panic!(),
        },
        Op::TypeMatrix => {
            let row_major = decorations
                .iter()
                .any(|(dec, _)| dec == &Decoration::RowMajor);

            match matrix_info(module, type_inst, decorations).unwrap() {
                (float_ty, 2, 2) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix2<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix2<#float_ty> }
                    }
                }
                (float_ty, 2, 3) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix2x3<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix2x3<#float_ty> }
                    }
                }
                (float_ty, 2, 4) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix2x4<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix2x4<#float_ty> }
                    }
                }
                (float_ty, 3, 2) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix3x2<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix3x2<#float_ty> }
                    }
                }
                (float_ty, 3, 3) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix3<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix3<#float_ty> }
                    }
                }
                (float_ty, 3, 4) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix3x4<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix3x4<#float_ty> }
                    }
                }
                (float_ty, 4, 2) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix4x2<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix4x2<#float_ty> }
                    }
                }
                (float_ty, 4, 3) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix4x3<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix4x3<#float_ty> }
                    }
                }
                (float_ty, 4, 4) => {
                    if row_major {
                        quote! { ::spirv_interop::mint::RowMatrix4<#float_ty> }
                    } else {
                        quote! { ::spirv_interop::mint::ColumnMatrix4<#float_ty> }
                    }
                }
                _ => panic!(),
            }
        }
        _ => panic!("Unsupported type instruction: {}", type_inst.class.opname),
    }
}

fn array(name: TokenStream, ty: TokenStream, values: &[TokenStream]) -> TokenStream {
    let len = Literal::usize_unsuffixed(values.len());
    quote! {
        #name: [#ty; #len] = [#(#values),*]
    }
}

fn entry_points(module: &Module) -> TokenStream {
    let entry_points: Vec<_> = module
        .entry_points
        .iter()
        .map(|entry_point| match &entry_point.operands[2] {
            Operand::LiteralString(name) => quote!(#name),
            _ => panic!(),
        })
        .collect();

    let arr = array(quote!(ENTRY_POINTS), quote!(&str), &entry_points);
    quote! {
        pub const #arr;
    }
}

fn structs(module: &Module) -> TokenStream {
    let mut structs = vec![];
    'outer: for instruction in module.types_global_values.iter() {
        if let Op::TypeStruct = instruction.class.opcode {
            let id = instruction.result_id.unwrap();
            let name: Ident = syn::parse_str(name(module, id)).unwrap();

            let mut fields = vec![];
            for (i, operand) in instruction.operands.iter().enumerate() {
                match operand {
                    &Operand::IdRef(type_id) => {
                        let member_idx = i as Word;
                        let name: Ident =
                            syn::parse_str(member_name(module, id, member_idx)).unwrap();
                        let decorations = member_decorations(module, id, member_idx);

                        // skip over structs with built in fields
                        if decorations
                            .iter()
                            .any(|(dec, _)| dec == &Decoration::BuiltIn)
                        {
                            continue 'outer;
                        }

                        let ty = type_convert(module, type_id, &decorations);

                        fields.push(quote! {
                            #name: #ty
                        });
                    }
                    _ => panic!(),
                }
            }

            structs.push(quote! {
                #[repr(C)]
                #[derive(Debug)]
                pub struct #name {
                    #(#fields),*
                }
            });
        }
    }

    structs.into_iter().collect()
}

#[proc_macro]
pub fn reflect(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Reflect {
        visibility,
        name,
        shader_path,
    } = parse_macro_input!(tokens as Reflect);

    let shader_path = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join(shader_path.value());
    let shader_bytes = fs::read(shader_path).expect("Failed to read SPIR-V shader source");
    let shader_module = rspirv::dr::load_bytes(&shader_bytes).expect("Failed to parse SPIR-V");

    if false {
        panic!(
            "\n{}",
            rspirv::binary::Disassemble::disassemble(&shader_module)
        );
    }

    let entry_points = entry_points(&shader_module);
    let structs = structs(&shader_module);

    proc_macro::TokenStream::from(quote! {
        #visibility mod #name {
            #entry_points
            #structs
        }
    })
}
