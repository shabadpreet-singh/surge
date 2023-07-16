# Comparision of different Matching Algorithms in Two-Sided Matching

We consider the both-sided matching framework where one side has to be matched with
the other side, and both sides have preferences over the other side. In this setting, there
are three widely recognized matching algorithms: **Deferred Acceptance, Top-trading cycle**,
and **Serial Dictatorship**. In this study, we intend to compare the above matching algorithms
based on their expected **rank-utility** under the uniform distribution on the preference profiles.

More about rank utility is mentioned in my report.

You can find code for matching algorithms in the `Code for algorithms used` folder. Both TTC and Deferred Acceptance take both men's and women's preference profiles as two separate matrices with ith column representing the preference profile of the ith person and return a matrix of engagements with the first column as the index of men and the other as the index of women. The `Score_Function.R` file contains a function to calculate rank-utility at given preference profiles of both men and women and the final matching.
