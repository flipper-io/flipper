use std::path::{Path, PathBuf};
use std::fs;

#[derive(Clone)]
pub struct Layout {
    root: PathBuf,
    module: Option<PathBuf>,
    apps: Vec<PathBuf>,
}

impl Layout {
    pub fn from_project_path(root_path: &Path) -> Layout {
        let mut module = None;
        let mut apps: Vec<u8> = vec![];

        // Check if the expected root module file exists.
        let module_file = root_path.join("src").join("module.c");
        if fs::metadata(&module_file).is_ok() {
            module = Some(module_file);
        }

        Layout {
            root: root_path.to_path_buf(),
            module: None,
            apps: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Manifest {
    name: String,
    version: String,
    author: String,
}

pub fn print_manifest() {

}
