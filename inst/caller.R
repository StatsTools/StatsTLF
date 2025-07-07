library(StatsTLF)

package_init <- StatsTLF::create_content_package(
 start_number = list(T = 1L, F = 1L, L = 1L),
 sep_subtitle = "newline",
 sep_population = "newline",
 language = "PT-BR"
)

package <- StatsTLF::run_caller_contents(package_init, )

StatsTLF::export_package(
 package = package,
 report_name = 'SAR', # Please, do not change this if going to use `build_submission()`.
 template_name = "template_PT-BR.docx",
 supp = FALSE,
 dataset = FALSE,
 add_toc = FALSE
)

rm(list = ls())
