source_if_needed <- function(output_path, source_path, import = TRUE) {
    if (folder_is_empty(output_path)) {
        source(source_path)
    }
    if (import) {
        import_all_objects_auto(output_path)
    }
}
