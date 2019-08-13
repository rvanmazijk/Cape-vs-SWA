rmarkdown::render(
  "draft-02/manuscript/02_materials-and-methods_rvm.md",
  "word_document",
  output_options = list(
    reference_docx = "style.docx"
  )
)
