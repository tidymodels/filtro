# Create a base class
new_score_obj_v2 <- S7::new_class(
  "new_score_obj_v2",
  properties = list(
    #subclass = S7::class_character,
    outcome_type = S7::class_character,
    predictor_type = S7::class_character,
    case_weights = S7::class_logical,
    range = S7::class_numeric,
    inclusive = S7::class_logical,
    fallback_value = S7::class_numeric,
    score_type = S7::class_character,
    trans = S7::class_function,
    sorts = S7::class_function,
    direction = S7::class_character,
    deterministic = S7::class_logical,
    tuning = S7::class_logical,
    ties = S7::class_logical,
    calculating_fn = S7::class_function,
    label = S7::class_character
  )
)
class(new_score_obj_v2())
new_score_obj_v2@constructor
new_score_obj_v2()
a <- new_score_obj_v2()
a@score_type

# Create a base class with default
new_score_obj_v2 <- S7::new_class(
  "new_score_obj_v2",
  properties = list(
    # subclass = S7::new_property(
    #   S7::class_character,
    #   default = c("cat_num", "cat_cat", "num_num", "any")
    # ),
    outcome_type = S7::new_property(
      S7::class_character,
      default = c("numeric", "factor")
    ),
    predictor_type = S7::new_property(
      S7::class_character,
      default = c("numeric", "factor")
    ),
    case_weights = S7::class_logical,
    range = S7::class_numeric,
    inclusive = S7::class_logical,
    fallback_value = S7::class_numeric,
    score_type = S7::class_character,
    trans = S7::class_function,
    sorts = S7::class_function,
    direction = S7::class_character,
    deterministic = S7::class_logical,
    tuning = S7::class_logical,
    ties = S7::class_logical,
    calculating_fn = S7::class_function,
    label = S7::class_character
  )
)
class(new_score_obj_v2())
new_score_obj_v2@constructor
new_score_obj_v2()
a <- new_score_obj_v2()
a@score_type
