copyrmd <- function(x){
  cwp_require_package("usethis", "to copy a file from GitHub")
  last_path = function(y){basename(y)}
  if(!file.exists(paste0(gsub(as.character(here::here()),"",as.character(getwd())), paste0("/", last_path(x)))))
    usethis::use_github_file(repo_spec =x,
                    save_as = paste0(gsub(as.character(here::here()),"",as.character(getwd())), paste0("/", last_path(x))),
                    ref = NULL,
                    ignore = FALSE,
                    open = FALSE,
                    host = NULL
    ) }
