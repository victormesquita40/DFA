## Test environments
* local OS X install, R 3.6.1
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release

## incoming checks issue 2020-06-22

cran-comments.md wasn't added to .Rbuildignore and it resulted in a note, now fixed.

## Response to Swetlana Herbrandt's comments 2020-06-22


>If there are references describing the (theoretical background of DFA)
>methods in your package, please add these in the Description field of
>your DESCRIPTION file in the form authors (year) <doi:...>
>authors (year) <arXiv:...> authors (year, ISBN:...)
>with no space after 'doi:', 'arXiv:' and angle brackets for auto-linking.

We are not entirely sure what this is refering to, but we added ```BugReports``` in case it helps. This package does not reference academic publications.

## Response to Martina Schmirl's comments 2020-06-30

>\dontrun{} should only be used if the example really cannot be executed
>(e.g. because of missing additional software, missing API keys, ...) by
>the user. That's why wrapping examples in \dontrun{} adds the comment
>("# Not run:") as a warning for the user.
>Does not seem necessary.
>Please unwrap the examples if they are executable in < 5 sec, or replace
>\dontrun{} with \donttest{}.

I replace ```\dontrun{}``` to ```\donttest{}``` in the examples.

>If there are references describing the methods in your package, please
>add these in the description field of your DESCRIPTION file in the form
>authors (year) <doi:...>
>authors (year) <arXiv:...>
>authors (year, ISBN:...)
>or if those are not available: <[https:...]https:...>
>with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
>auto-linking.
>(If you want to add a title as well please put it in quotes: "Title")

We are not entirely sure what this is refering to, but we added ```BugReports``` in case it helps. This package does not reference academic publications.
