

#' @param df [data.table] `df` object from `prep_exp`

#' @return [data.table] `df` object with `cv_admin` set to 1 for select survey names
#' @export

#' @examples
recode_survey_as_admin <- function(df){
   
   survey_names <- c(
      "IND/HMIS"
      , "BRA/NATIONAL_IMMUNIZATION_PROGRAM_PNI"
   )
   message("Coding these surveys as admin data:")
   for(survey_name_i in survey_names){
      message(survey_name_i)
      df[grepl(survey_name_i, survey_name) , cv_admin := 1]
   }
   
   
   return(df)
}