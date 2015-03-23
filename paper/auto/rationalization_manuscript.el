(TeX-add-style-hook
 "rationalization_manuscript"
 (lambda ()
   (TeX-run-style-hooks
    "theory_figure"
    "research_design_figure"
    "res_table"
    "predictor_table"
    "res_r_table")
   (LaTeX-add-labels
    "fig:pref"
    "fig:d_dist"
    "fig:pred"
    "fig:imp"
    "fig:post_pred"
    "fig:mean_diff"
    "fig:time"
    "fig:map"
    "fig:bal"
    "fig:d_dist_box"
    "fig:d_dist_log"
    "fig:pd"
    "fig:trace_1")))

