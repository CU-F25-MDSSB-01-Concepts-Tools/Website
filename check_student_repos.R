library(ghclass) # For managing the GitHub organization
library(tidyverse)
library(glue)
orgname <- "CU-F25-MDSSB-01-Concepts-Tools"
names_omit <- c("Jan", "Armin", "Mustafa", "Matthew", "Aiman", "Poro")

clone_or_pull_student_repos <- function(
  prefix,
  local_student_repo_path = "../student_repos/"
) {
  localfiles <- dir(glue("{local_student_repo_path}{prefix}/"))
  localrepos <- sort(glue("{orgname}/{localfiles}"))
  remoterepos <- sort(
    org_repos(orgname, glue("{prefix}_")) |>
      discard(~ any(str_detect(., names_omit)))
  )
  to_clone <- setdiff(remoterepos, localrepos)
  local_repo_clone(
    repo = to_clone,
    local_path = glue("{local_student_repo_path}{prefix}/")
  )
  local_repo_pull(
    repo = paste0(local_student_repo_path, prefix, "/", localfiles)
  )
}

clone_or_pull_student_repos("Project_NYCFlights")
clone_or_pull_student_repos("Project_COVID19")
clone_or_pull_student_repos("Project_FuelWatch")
clone_or_pull_student_repos("Project_DataScienceProfiles")
clone_or_pull_student_repos("FinalProject")

## Delete one repo
#repo_delete("CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Armin")
