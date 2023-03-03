var sourcesIndex = JSON.parse('{\
"diatom":["",[],["lib.rs"]],\
"diatom_cli":["",[["cli",[],["highlighter.rs","mod.rs","prompt.rs","validator.rs"]]],["main.rs"]],\
"diatom_core":["",[["file_manager",[],["mod.rs","util.rs"]],["frontend",[["lexer",[],["error.rs","mod.rs","token.rs"]],["parser",[],["ast.rs","error.rs","mod.rs","path_resolver.rs"]]],["mod.rs","util.rs"]],["gc",[],["key_pool.rs","mod.rs","pool.rs"]],["interpreter",[["ffi",[],["mod.rs","obj.rs","obj_mut.rs"]],["scanner",[],["capture_scanner.rs","const_scanner.rs","mod.rs"]]],["error.rs","mod.rs","register_table.rs","std_core.rs"]],["vm",[],["error.rs","mod.rs","op.rs"]]],["lib.rs"]],\
"diatom_std_core":["",[["files",[],["mod.rs"]],["math",[],["mod.rs"]]],["built_in.rs","float.rs","int.rs","lib.rs","list.rs","string.rs"]],\
"diatom_std_os":["",[],["lib.rs"]]\
}');
createSourceSidebar();
