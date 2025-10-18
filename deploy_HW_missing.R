library(ghclass) # For managing the GitHub organization
library(tidyverse)
library(Microsoft365R) # For sending emails via outlook
library(readxl)
library(glue)
library(googlesheets4)

# Variables
orgname <- "CU-F25-MDSSB-01-Concepts-Tools"
jlorenz_outlook <- Microsoft365R::get_business_outlook()
jlorenz_od <- Microsoft365R::get_business_onedrive()
jlorenz_od$download_file(
  "GitHub username for Data Science Concepts_Tools.xlsx",
  overwrite = TRUE
)

students <- read_xlsx(
  "GitHub username for Data Science Concepts_Tools.xlsx"
) |>
  filter(year(`Start time`) == 2025) |>
  mutate(
    FirstName = word(Name, 1),
    GitHub = `Your GitHub username`,
    Status = "Student"
  )
instructors <- tribble(
  ~FirstName,
  ~GitHub,
  ~Email,
  "Jan",
  "janlorenz",
  "jlorenz@constructor.university",
  "Armin",
  "arminmueller81",
  "armmueller@constructor.university",
  "Mustafa",
  "mansari40",
  "mansari@constructor.university",
  "Matthew",
  "intox1ca7ed",
  "mtrifanov@constructor.university",
  "Aiman",
  "Aiman-DS",
  "Aiman-DS"
) |>
  mutate(Status = "Instructor")


for (proj in c("NYCFlights", "COVID19")) {
  gh <- bind_rows(students, instructors) |>
    mutate(HW_Project = paste0("Project_", proj, "_", FirstName))
  gh_missing <- gh |>
    filter(
      HW_Project %in%
        (setdiff(
          glue("{orgname}/{gh$HW_Project}"),
          org_repo_search(orgname, paste0("Project_", proj, "_"))
        ) |>
          str_remove(glue("{orgname}/")))
    )
  org_create_assignment(
    org = orgname,
    user = gh_missing$GitHub, #"org_members(orgname),
    repo = gh_missing$HW_Project,
    source_repo = paste0("janlorenz/Project_", proj, "_Template"),
    private = TRUE
  )

  # Write an email with jlorenz_outlook
  for (i in 1:nrow(gh_missing)) {
    # Email students to get started
    email <- glue(
      "Dear student, 

The Homework repository {proj} has been created for you. Find it here:
https://github.com/{orgname}/Project_{proj}_{gh_missing$FirstName[i]}

Start working on it and learn data science! Reach out for help if you get stuck with the.
Tip: Build a workgroup with your classmates and work together on it for an hour or so. 

Jan Lorenz"
    )
    jlorenz_outlook$create_email(
      to = gh_missing$Email[i],
      subject = glue("Data Science Concepts/Tools Homework Project {proj}"),
      body = email
    )$send()
  }
}

# org_remove(orgname, "poroman")
# repo_delete("CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Poro")
# org_create_assignment(
#   org = orgname,
#   user = "poroman", #"org_members(orgname),
#   repo = "Project_NYCFlights_Poro",
#   source_repo = "janlorenz/Project_NYCFlights_Template",
#   private = TRUE
# )

# repo_names <- c("CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Uyazi"
# ,"CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Kay-Lee"
# ,"CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Felix"
# ,"CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Mcolisi"
# ,"CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Ayse"
# ,"CU-F25-MDSSB-01-Concepts-Tools/Project_NYCFlights_Aya")
# # # Check your permissions on one of the repos
# # repo_names <- glue("{orgname}/{gh$HW_Project}")
# delete_script <- glue("gh repo delete {repo_names} --yes")
# writeLines(delete_script, "delete_repos.sh")
# system("bash delete_repos.sh")
