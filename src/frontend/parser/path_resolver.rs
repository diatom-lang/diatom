use std::{fs, path::PathBuf};

use crate::file_manager::FileManager;

fn compose_path(path: PathBuf) -> (PathBuf, PathBuf) {
    let mut direct = path.clone();
    direct.set_extension("dm");
    let mut indirect = path;
    indirect.push("mod.dm");
    (direct, indirect)
}

fn join_search_path(search_path: impl Into<PathBuf>, import: &[String]) -> (PathBuf, PathBuf) {
    let mut path = search_path.into();
    import.iter().for_each(|import_path| path.push(import_path));
    compose_path(path)
}

fn try_read_path(path: PathBuf, file_manager: &mut FileManager) -> Option<(usize, PathBuf)> {
    if let Some(fid) = file_manager.look_up_fid(&path) {
        return Some((fid, path));
    }
    fs::read_to_string(&path)
        .map(|content| (file_manager.add_file(path.clone(), content), path))
        .ok()
}

pub fn try_get_mod(
    search_path: &PathBuf,
    import: &[String],
    file_manager: &mut FileManager,
) -> Option<(usize, PathBuf)> {
    let (direct, indirect) = join_search_path(search_path, import);
    try_read_path(direct, file_manager).or_else(|| try_read_path(indirect, file_manager))
}
