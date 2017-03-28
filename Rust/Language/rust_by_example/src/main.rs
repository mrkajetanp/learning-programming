mod formatted_print;
mod primitives;
mod custom_types;
mod variable_bindings;
mod flow_control;

fn main() {
    formatted_print::formatted_print();
    primitives::primitives();
    custom_types::custom_types();
    variable_bindings::variable_bindings();
    flow_control::flow_control();
}
