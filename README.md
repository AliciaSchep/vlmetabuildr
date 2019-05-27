![lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

# vlmetabuildr

The goal of vlmetabuildr is to provide functions that can be used to build up an api... it is not a package meant to be used to actually build visualizations themselves, just to help build the actual package one would use.


vl_bin_{enc}
vl_stack_{enc}
vl_sort_{enc}
vl_impute_{enc}
vl_aggregate_{enc}

vl_axis_{enc}
vl_scale_{enc}
vl_legend_{enc}

vl_add_selections

vl_add_single_selection
vl_add_multi_selection
vl_add_interval_selection
vl_bind_checkbox

vl_bind_radio
vl_bind_select
vl_bind_range
vl_bind_scale
vl_bind_html

vl_condition_{enc}

vl_and
vl_or
vl_not

vl_facet_row
vl_facet_col
vl_facet_wrap
vl_resolve_scale
vl_resolve_axis
vl_resolve_legend

vl_add_projection
vl_add_title



# customize order -- encode: field first, selection type first
# encode - sugar for type

vl_hconcat
vl_vconcat
vl_layer
vl_repeat

vl_SingleSelection
vl_MultiSelection
vl_IntervalSelection
vl_BinParams
vl_Axis
vl_Legend
vl_TimeUnit
vl_BindCheckbox
vl_BindRange
vl_BindScale
vl_BindRadioSelect
