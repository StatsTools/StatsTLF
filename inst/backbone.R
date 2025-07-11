#' Title: Create Table – XXXXXXXXXX
#'
#' Author: XXXXXXX
#' Date: XXXX-XX-XX
#'
#' Description:
#' XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#'
#' Parameters:
#'   - dataset: A tibble.
#'   - ...: Any other additional argument to be used (Description below).
#'      - :
#'
#' Output:
#'   - A flextable object containing the summary table.
#'
#' Program Flow:
#'   Step 01: Set up default parameter values.
#'   Step 02: Define user functions.
#'   Step 03: Input validation (bullet proof).
#'   Step 04: Extract relevant variables.
#'   Step 05: Process and summarize data.
#'   Step 06: Create output.
#'   Step 07: Return result.
#'
#' @return flextable object
#'
backbone <- StatsTLF::create_content_backbone(title = '', type = '', fun = function(dataset, ...) {

 # -----------------------------------------------------------------------------
 # -                   Step 01: Set up default parameter values.               -
 # -----------------------------------------------------------------------------

 arg <- list(...)

 flextable::set_flextable_defaults(
  font.size = 10,
  font.family = 'Times New Roman',
  hansi.family = 'Times New Roman')

 # -----------------------------------------------------------------------------
 # -                   Step 02: Define user functions.                         -
 # -----------------------------------------------------------------------------

 # -----------------------------------------------------------------------------
 # -                   Step 03: Input validation (bullet proof).               -
 # -----------------------------------------------------------------------------

 stopifnot("Validation error: The dataset does not conform to the defined metadata." = StatsTLF::validate_adam_dataset(dataset))

 # -----------------------------------------------------------------------------
 # -                   Step 04: Extract relevant variables.                    -
 # -----------------------------------------------------------------------------

 # -----------------------------------------------------------------------------
 # -                   Step 05: Process and summarize data.                    -
 # -----------------------------------------------------------------------------

 # -----------------------------------------------------------------------------
 # -                   Step 06: Create output.                                 -
 # -----------------------------------------------------------------------------

 # -----------------------------------------------------------------------------
 # -                   Step 07: Return result.                                 -
 # -----------------------------------------------------------------------------

 return()
})
