#ifndef TC_FLAGS_H /* { */
#define TC_FLAGS_H

extern bool flag_readwrite;
extern bool flag_use_maxnargs;
extern int flag_verbose;
extern bool flag_recycle;
extern bool flag_readable_output;
extern bool flag_gref_filter;
extern bool flag_gmodref_skip_locals;
extern int flag_may_be_uninit;
extern bool flag_reaching_defs;
extern bool flag_vtfix;
extern bool flag_compute_iref;

extern bool flag_collapse_inclto_cycle;
extern bool flag_collapse_ptsto_cycle;

extern enum wl_mode {
  WL_MODE_FIFO		= 0,
  WL_MODE_LIFO		= 1,
  WL_MODE_LEAF_FIRST	= 2
} flag_worklist_mode;

extern bool flag_instantiate_structunion;
extern bool flag_filter_relevant_aos;
extern bool flag_worklist_init_depth_first;

extern bool flag_exposed_deref_is_unsafe;

extern bool flag_range_filter_loop_backedge;
extern bool flag_range_filter_call_backedge;
extern bool flag_range_zero_special;
extern bool flag_range_must_have_ao;
extern bool flag_range_ty_can_change;
extern bool flag_range_do_sliding;
extern bool flag_range_handle_preds;
extern bool flag_range_skip_targeted_preds;
extern bool flag_range_exact_sizes;
extern bool flag_range_collect_stats;

extern bool flag_debug_range_collect_phase;

extern bool flag_strlit_readonly;
extern bool flag_range_handle_malloc;
extern bool flag_mbu_lowerthan_by_size;
extern bool flag_callsite_top;
extern bool flag_use_bblocks;

#endif /* } ifndef TC_FLAGS_H */
