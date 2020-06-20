use ast::{Type, TypeId};
use itertools::Itertools;
use utils::{NameTable, TypeTable};

pub fn type_to_string(name_table: &NameTable, type_table: &TypeTable, type_id: TypeId) -> String {
    match type_table.get_type(type_id) {
        Type::Unit => "()".to_string(),
        Type::Int => "int".to_string(),
        Type::Float => "float".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Char => "char".to_string(),
        Type::String => "string".to_string(),
        Type::Var(id) => format!("var({})", id),
        Type::Array(type_id) => format!("[{}]", type_to_string(name_table, type_table, *type_id)),
        Type::Record(fields) => {
            let fields_str = fields
                .iter()
                .map(|(name, type_id)| {
                    let type_str = type_to_string(name_table, type_table, *type_id);
                    let name_str = name_table.get_str(name);
                    format!("{}: {}", name_str, type_str)
                })
                .join(", ");
            format!("{{ {} }}", fields_str)
        }
        Type::Tuple(types) => {
            let elem_str = types
                .iter()
                .map(|type_id| type_to_string(name_table, type_table, *type_id))
                .join(", ");
            format!("({})", elem_str)
        }
        Type::Arrow(params_type, return_type) => {
            let params_str = type_to_string(name_table, type_table, *params_type);
            let return_str = type_to_string(name_table, type_table, *return_type);
            format!("{} => {}", params_str, return_str)
        }
    }
}
