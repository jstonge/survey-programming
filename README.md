# survey-programming

The survey lives [here](https://qualtrics.uvm.edu/survey-builder/SV_29K2Bj7QejHFNoq/edit) (you need permission to access it). See [Project status](https://github.com/users/jstonge/projects/11/views/2).

## Steps that we could somehow automatized

 - Assuming columns that finish by `Other: - Text` or `Fill in the blank: - Text` always come in pair with columns finishing with `- Selected Choice`, we can update the `Selected` column with the `Other` column.

## Steps that need human beings

 - Recoding ordinal variables properly. We like to have more negative/costly attitudes as lower values and more positive/benefitials attitudes as higher values. For instance, in the following question "Do you feel you have institutional support?", we have from `No and I wish there were = 1` to `Yes = 3`.
 - Finding good labels for question names and text values.
 - Identify columns that are superfluous and/or sensitive to display publically.
 - Some columns might be problematic because of how we wrote the survey in `Qualtrics`:
    - Conditional on how many advisors a student has, we ask many times "What is your advisor's first main department?". This translated in `readr::read_csv` adding `...\\d{1,2}` at the end of the duplicate column names. We had to fix those manually.

We use `lookups.R` as file with named lists whereby old values are names and new values are the elements, e.g. `list(old=new)`. We can then feed that easily in the pipes and add some tests to catch any values that would've been modified along the way. 
