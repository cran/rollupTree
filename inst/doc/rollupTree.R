## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6
)

## ----setup--------------------------------------------------------------------
library(rollupTree)

## ----example------------------------------------------------------------------
library(rollupTree)
wbs_table

## ----wbs_tree-plot------------------------------------------------------------
library(igraph)
wbs_tree <- graph_from_edgelist(
  as.matrix(wbs_table[which(!is.na(wbs_table$pid)), c("id", "pid")]),
  directed = TRUE
)

## -----------------------------------------------------------------------------
igraph::topo_sort(wbs_tree)

## -----------------------------------------------------------------------------
rollup(
  tree=wbs_tree,
  ds=wbs_table,
  update=function(d, t, s) update_df_prop_by_id(df=d, target=t, sources=s, prop="work"),
  validate_ds=function(t, d) validate_df_by_id(tree=t, df=d, prop="work")
)

## -----------------------------------------------------------------------------
rollup(
  tree=wbs_tree,
  ds=wbs_table,
  update=function(d, t, s) update_df_prop_by_id(df=d, target=t, sources=s, prop="work"),
  validate_ds=function(t, d) validate_df_by_id(tree=t, df=d, prop="work")
) |> rollup(
  tree=wbs_tree,
  ds=_,
  update=function(d, t, s) update_df_prop_by_id(df=d, target=t, sources=s, prop="budget"),
  validate_ds=function(t, d) validate_df_by_id(tree=t, df=d, prop="budget")
)

## -----------------------------------------------------------------------------
rollup(
  tree = wbs_tree,
  ds = wbs_table,
  update = function(d, t, s) {
    update_df_prop_by_id(
      df = d,
      target = t,
      sources = s,
      prop = "work"
    ) |>
      update_df_prop_by_id(target = t,
                           sources = s,
                           prop = "budget")
  },
  validate_ds = function(t, d) {
    validate_df_by_id(tree = t, df = d, prop = "work") &&
      validate_df_by_id(tree = t, df = d, prop = "budget")
  }
)

## -----------------------------------------------------------------------------
my_get <- function(d, i) c(
  w=df_get_by_id(df=d, id=i, prop="work"),
  b=df_get_by_id(df=d, id=i, prop="budget")
)
my_set <- function(d, i, v) {
  df_set_by_id(df=d, id=i, prop="work", val=v["w"]) |>
    df_set_by_id(id=i, prop="budget", val=v["b"])
}
my_update <- function(d, t, s) {
    update_prop(ds=d, target=t, sources=s, set=my_set, get=my_get)
}
my_validate <- function(t, d) {
  validate_ds(tree=t, ds=d,
               get_keys=function(d) df_get_ids(df=d),
               get_prop=my_get,
               op=function(v) my_check(v["w"]) && my_check(v["b"])
  )
}
my_check <- function(v)
  is.numeric(v) && !is.na(v) && (v > 0.0)

rollup(
  tree = wbs_tree,
  ds = wbs_table,
  update = my_update,
  validate_ds = my_validate
)

## -----------------------------------------------------------------------------
new_wbs_table <- wbs_table
new_wbs_table$work <- NULL
new_wbs_table$budget_unc <- ifelse(is.na(wbs_table$budget), NA, wbs_table$budget * 0.05)
new_wbs_table

## -----------------------------------------------------------------------------
combine_rss <- function(vl) {
  sqrt(Reduce(f = `+`, x = Map(
    f = function(v)
      v * v,
    vl
  )))
}
result <- rollup(
  tree = wbs_tree,
  ds = new_wbs_table,
  update = function(d, t, s)
    update_df_prop_by_id(
      df = d,
      target = t,
      sources = s,
      prop = "budget"
    ) |>
    update_df_prop_by_id(
      target = t,
      sources = s,
      prop = "budget_unc",
      combine = combine_rss
    ),
  validate_ds = function(t, d)
    validate_df_by_id(tree = t, df = d, prop = "budget_unc"),
)
result$budget_unc_pct <- result$budget_unc / result$budget * 100.
result

## -----------------------------------------------------------------------------
wbs_list <- lapply(split(wbs_table, wbs_table$id),
                   function(r) list(name = r$name, budget = r$budget)
)
str(wbs_list)

## -----------------------------------------------------------------------------
list_get <- function(d, i) d[[i]]$budget
list_set <- function(d, i, v) { d[[i]]$budget = v; d }
list_update <- function(d, t, s) { update_prop(d, t, s, list_set, list_get) }
list_validate <- function(t, d) validate_ds(t, d, get_keys = function(l) names(l), get = list_get)

## -----------------------------------------------------------------------------
list_result <- rollup(wbs_tree, wbs_list, list_update, list_validate)
str(list_result)


## -----------------------------------------------------------------------------
new_wbs_tree <- Reduce(
  f = function(g, k) set_vertex_attr(g, 'budget', k, df_get_by_id(wbs_table, k, 'budget')),
  x = names(V(wbs_tree)),
  init = wbs_tree
)
ib <- vertex_attr(new_wbs_tree, "budget")
names(ib) <- names(V(new_wbs_tree))
ib

## -----------------------------------------------------------------------------
tree_get <- function(d, k) vertex_attr(d, "budget", k)
tree_set <- function(d, k, v) set_vertex_attr(d, "budget", k, v)
tree_update <- function(d, t, s) update_prop(d, t, s, set = tree_set, get = tree_get)
tree_validate <- function(t, d) validate_ds(t, d, get_keys = function(d) names(V(d)), get = tree_get)

## -----------------------------------------------------------------------------
tree_result <- rollup(new_wbs_tree, new_wbs_tree, update = tree_update, validate_ds = tree_validate)
ob <- vertex_attr(tree_result, "budget")
names(ob) <- names(V(tree_result))
ob

