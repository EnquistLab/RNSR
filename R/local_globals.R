# The offline resolver is written against data.table, which refers to columns by bare
# name.  R CMD check cannot tell those from undefined globals, so they are declared here
# rather than silenced one call at a time.  Keep this list in step with the set path.
utils::globalVariables(c(
  ".", ".N", ".SD", ":=",
  "code", "conflict_type", "consulted_here", "fraction", "from_region",
  "has_int", "has_nat", "in_place", "is_cultivated",
  "n_here", "n_in", "n_nat", "n_src_sets", "n_sub_int", "n_sub_nat",
  "ovl", "ovl_srcs", "qid", "r_ord", "reason", "region_key", "relation",
  "scope", "source_name", "srcs", "srcs_any",
  "stated_int", "stated_nat", "stated_pre", "status",
  "sub_sets", "sub_srcs", "sub_status",
  "taxon_id", "to_region", "to_sys", "within_src"
))
