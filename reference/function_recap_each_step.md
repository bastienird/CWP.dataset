# Function to Record Step Details and Save Results

This function records the details of each processing step, including
explanations, function names, and options. It saves the results as RDS
files and text files in a specified directory.

## Usage

``` r
function_recap_each_step(
  step_name,
  rds_data,
  explanation = "No explanation provided to this step",
  functions = "No function used in this step",
  option_list = NULL,
  entity = NULL
)
```

## Arguments

- step_name:

  A character string specifying the name of the step.

- rds_data:

  A data frame containing the data to be saved as an RDS file.

- explanation:

  A character string providing an explanation of the step.

- functions:

  A character string listing the functions used in the step.

- option_list:

  A list of options used in the step.

- entity:

  A geoflow entity

## Value

None

## Author

Bastien Grasset

## Examples

``` r
if (FALSE) { # \dontrun{
function_recap_each_step("step1", data, "This step does X", "function1, function2", list(option1 = "value1"))
} # }
```
