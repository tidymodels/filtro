# filtro (development version)

# filtro 0.2.0

* Added five new scoring methods: correlation, random forest feature importance, information gain, area under the ROC curve, and cross tabulation filters. 

* Transitioned from S3 to S7 and refactored S7 for improved clarity, maintainability, and performance.  

* Added multiple new functions to support ranking of single score. `show_best_score_prop()`, `show_best_score_num()`, `show_best_score_cutoff()`, `show_best_score_dual()`, `rank_best_score_min()` and `rank_best_score_dense()` can be used to select and rank single score. 

* Added two new functions to support ranking of multiple scores. `show_best_desirability_prop()` and `show_best_desirability_num()` can be used for multiparameter optimization via desirability functions. 

* Added multiple utility functions to support handling of both single and multiple scores: `arrange_score()`, `fill_safe_value()`, `bind_scores()`, and `fill_safe_values()`. 

# filtro 0.1.0

* Initial CRAN submission.
