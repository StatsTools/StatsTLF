#' Convert CDISC define.xml to HTML using XSLT
#'
#' @description
#' Reads a CDISC define.xml file and an XSL file, applies the XSL transformation,
#' and saves the resulting HTML file.
#'
#' @param xml_file Path to the CDISC define.xml file.
#' @param xsl_file Path to the XSL file to use for transformation.
#' @param output_file Path to the output HTML file. Defaults to "define.html".
#'
#' @return Invisibly returns the XML object of the transformed HTML.
#' @examples
#' \dontrun{
#' define_to_html(
#'   xml_file = "path/to/define.xml",
#'   xsl_file = "path/to/define2-1-0.xsl",
#'   output_file = "define.html"
#' )
#' }
#' @export
define_to_html <- function(xml_file, xsl_file, output_file = "define.html") {
  # Read XML and XSL files
  doc_xml <- xml2::read_xml(xml_file)
  doc_xsl <- xml2::read_xml(xsl_file)

  # Apply XSLT transformation
  html_output <- xslt::xml_xslt(doc_xml, doc_xsl)

  # Write HTML output
  xml2::write_xml(html_output, output_file, options = "format")

  invisible(html_output)
}
