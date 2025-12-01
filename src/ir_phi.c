#include "ir_phi.h"
#include "ir.h"
#include <stdlib.h>
#include <string.h>

// ----- CFG Construction -----
static void add_cfg_edge(IrBlock *from, IrBlock *to) {
    if (from->succ_count >= from->succ_cap) {
        from->succ_cap = from->succ_cap ? from->succ_cap * 2 : 4;
        from->succs = realloc(from->succs, from->succ_cap * sizeof(IrBlock *));
    }
    from->succs[from->succ_count++] = to;

    if (to->pred_count >= to->pred_cap) {
        to->pred_cap = to->pred_cap ? to->pred_cap * 2 : 4;
        to->preds    = realloc(to->preds, to->pred_cap * sizeof(IrBlock *));
    }
    to->preds[to->pred_count++] = from;
}

void build_cfg_edges(IrFunc *fn) {
    for (IrBlock *b = fn->blocks; b; b = b->next) {
        b->pred_count = 0;
        b->succ_count = 0;

        free(b->preds);
        free(b->succs);

        b->preds    = NULL;
        b->succs    = NULL;

        b->pred_cap = 0;
        b->succ_cap = 0;
    }

    for (IrBlock *b = fn->entry; b; b = b->next) {
        if (!b->last)
            continue;

        IrInst *terminator = b->last;

        if (terminator->op == IR_OP_BR) {
            IrBlock *target = find_block_by_label(fn, terminator->imm);
            if (target) {
                add_cfg_edge(b, target);
            }
        }

        else if (terminator->op == IR_OP_BRCOND) {
            IrBlock *true_target = find_block_by_label(fn, terminator->imm);
            if (true_target) {
                add_cfg_edge(b, true_target);
            }

            if (b->next) {
                add_cfg_edge(b, b->next);
            }
        } else if (terminator->op == IR_OP_RET) {
            // No-op
        } else {
            if (b->next) {
                add_cfg_edge(b, b->next);
            }
        }
    }
}

// ----- RPO Calculation -----

// DFS post-order traversal of the CFG to calcuate RPO for each block
static void rpo_dfs(IrBlock *block, IrBlock **post_order, int *idx) {
    block->visited = true;

    for (uint32_t i = 0; i < block->succ_count; i++) {
        IrBlock *succ = block->succs[i];
        if (!succ->visited) {
            rpo_dfs(succ, post_order, idx);
        }
    }

    post_order[*idx] = block;
    *idx             = *idx + 1;

    return;
}

// Calculate RPO for each block in the CFG
IrRpo calculate_rpo(IrFunc *fn) {
    // Pre-dfs cleanup
    for (IrBlock *block = fn->entry; block; block = block->next) {
        block->rpo_index = -1;
        block->visited   = false;
    }

    // DFS
    uint32_t block_count = fn->block_count;
    IrBlock **post_order = malloc(block_count * sizeof(IrBlock *));
    int idx              = 0;
    rpo_dfs(fn->entry, post_order, &idx);

    // Reverse post-order and assign RPO indices
    IrBlock **rpo_blocks = malloc(idx * sizeof(IrBlock *));
    for (int i = 0; i < idx; i++) {
        rpo_blocks[i]            = post_order[idx - i - 1];
        rpo_blocks[i]->rpo_index = i;
    }

    free(post_order);

    IrRpo out = {
        .blocks = rpo_blocks,
        .count  = idx,
    };
    return out;
}

// ----- Immediate Dominators -----
// Find nearest common domintor of two blocks
static IrBlock *intersect(IrBlock *b1, IrBlock *b2) {
    while (b1 != b2) {
        while (b1->rpo_index > b2->rpo_index) {
            b1 = b1->idom;
        }

        while (b2->rpo_index > b1->rpo_index) {
            b2 = b2->idom;
        }
    }
    return b1;
}

// Calculate immediate dominators for each block in the CFG using the
// Cooper-Harvey-Kennedy algorithm
void calculate_immediate_dominators(const IrRpo *rpo) {
    // Init
    for (uint32_t i = 1; i < rpo->count; i++) {
        rpo->blocks[i]->idom = NULL;
    }
    rpo->blocks[0]->idom = rpo->blocks[0];

    // Cooper-Harvey-Kennedy algorithm
    bool changed = true;
    while (changed) {
        changed = false;

        for (int i = 1; i < rpo->count; i++) {
            IrBlock *b = rpo->blocks[i];
            if (b->pred_count == 0) {
                fprintf(stderr, "idom pass: block %u has no preds\n", b->id);
                continue;
            }

            // Compute idom
            IrBlock *new_idom = NULL;
            for (uint32_t j = 0; j < b->pred_count; j++) {
                IrBlock *pred = b->preds[j];

                if (pred->idom == NULL) {
                    continue;
                }

                if (new_idom == NULL) {
                    new_idom = pred;
                } else {
                    new_idom = intersect(new_idom, pred);
                }
            }

            if (!new_idom) {
                continue;
            }

            if (b->idom != new_idom) {
                b->idom = new_idom;
                changed = true;
            }
        }
    }
}

// ----- Dominator tree construction -----
static void dom_add_child(IrBlock *parent, IrBlock *child) {
    if (parent->dom_children_count >= parent->dom_children_cap) {
        parent->dom_children_cap =
            parent->dom_children_cap ? parent->dom_children_cap * 2 : 4;
        parent->dom_children = realloc(
            parent->dom_children, parent->dom_children_cap * sizeof(IrBlock *));
    }
    parent->dom_children[parent->dom_children_count++] = child;
}

void build_dom_tree(const IrRpo *rpo) {
    // Init
    for (uint32_t i = 1; i < rpo->count; i++) {
        IrBlock *block            = rpo->blocks[i];
        block->dom_children       = NULL;
        block->dom_children_count = 0;
        block->dom_children_cap   = 0;
    }

    for (uint32_t i = 1; i < rpo->count; i++) {
        IrBlock *b = rpo->blocks[i];
        IrBlock *p = b->idom;

        if (!p) {
            fprintf(stderr, "idom pass: block %u has no idom\n", b->id);
            continue;
        }

        dom_add_child(p, b);
    }
}

// ----- Dominance frontier construction -----
static void dom_add_frontier(IrBlock *block, IrBlock *frontier) {
    for (int i = 0; i < block->dom_frontier_count; i++) {
        if (block->dom_frontier[i] == frontier)
            return;
    }

    if (block->dom_frontier_count >= block->dom_frontier_cap) {
        block->dom_frontier_cap =
            block->dom_frontier_cap ? block->dom_frontier_cap * 2 : 4;
        block->dom_frontier = realloc(
            block->dom_frontier, block->dom_frontier_cap * sizeof(IrBlock *));
    }
    block->dom_frontier[block->dom_frontier_count++] = frontier;
}

void calculate_dominance_frontiers(const IrRpo *rpo) {
    for (int i = 0; i < rpo->count; i++) {
        IrBlock *block            = rpo->blocks[i];
        block->dom_frontier_count = 0;
    }

    for (int i = 0; i < rpo->count; i++) {
        IrBlock *block = rpo->blocks[i];

        if (block->pred_count < 2) {
            continue;
        }

        IrBlock *idom_b = block->idom;

        for (int pi = 0; pi < block->pred_count; pi++) {
            IrBlock *pred   = block->preds[pi];
            IrBlock *runner = pred;

            while (runner && runner != idom_b) {
                dom_add_frontier(runner, block);
                runner = runner->idom;
            }
        }
    }
}

// ----- Variable analysis -----
VarAnalysis *analyze_variables(IrFunc *fn) {
    VarAnalysis *analysis = malloc(sizeof(VarAnalysis));
    analysis->vars        = malloc(16 * sizeof(VarInfo));
    analysis->count       = 0;
    analysis->cap         = 16;

    // First pass: collect variable definitions (ALLOCA)
    for (IrBlock *b = fn->entry; b; b = b->next) {
        for (IrInst *inst = b->first; inst; inst = inst->next) {
            if (inst->op != IR_OP_ALLOCA) {
                continue;
            }

            if (analysis->count >= analysis->cap) {
                analysis->cap *= 2;
                analysis->vars =
                    realloc(analysis->vars, analysis->cap * sizeof(VarInfo));
            }

            VarInfo *var     = &analysis->vars[analysis->count++];
            var->alloca_addr = inst->dst;
            var->type        = inst->type;
            var->def_sites   = malloc(8 * sizeof(IrBlock *));
            var->def_count   = 0;
            var->def_cap     = 8;
        }
    }

    for (IrBlock *b = fn->entry; b; b = b->next) {
        for (IrInst *inst = b->first; inst; inst = inst->next) {
            if (inst->op != IR_OP_STORE) {
                continue;
            }

            IrValue addr = inst->dst;
            for (int i = 0; i < analysis->count; i++) {
                if (analysis->vars[i].alloca_addr != addr) {
                    continue;
                }

                VarInfo *var       = &analysis->vars[i];

                bool already_added = false;
                for (int j = 0; j < var->def_count; j++) {
                    if (var->def_sites[j] != b) {
                        continue;
                    }

                    already_added = true;
                    break;
                }

                if (already_added) {
                    break;
                }

                if (var->def_count >= var->def_cap) {
                    var->def_cap *= 2;
                    var->def_sites = realloc(var->def_sites,
                                             var->def_cap * sizeof(IrBlock *));
                }
                var->def_sites[var->def_count++] = b;
            }
        }
    }

    return analysis;
}

// ----- PHI node generation -----
static void insert_phi_node(IrFunc *fn, IrBlock *b, VarInfo *var) {
    IrInst *phi = malloc(sizeof(IrInst));
    memset(phi, 0, sizeof(IrInst));

    phi->op         = IR_OP_PHI;
    phi->type       = var->type;
    phi->dst        = ir_func_new_value(fn, var->type);
    phi->imm        = var->alloca_addr;

    phi->phi.count  = b->pred_count;
    phi->phi.values = malloc(b->pred_count * sizeof(IrValue));
    phi->phi.labels = malloc(b->pred_count * sizeof(int));

    // Fill phi values
    for (int i = 0; i < b->pred_count; i++) {
        phi->phi.values[i] = (IrValue)~0u;
        phi->phi.labels[i] = b->preds[i]->label_value;
    }

    phi->next  = b->first;
    phi->prev  = NULL;
    phi->block = b;

    if (b->first) {
        b->first->prev = phi;
    } else {
        b->last = phi;
    }

    b->first = phi;
}

void phi_generation_pass(IrFunc *fn, VarAnalysis *analysis) {
    for (int v = 0; v < analysis->count; v++) {
        VarInfo *var       = &analysis->vars[v];

        IrBlock **worklist = malloc(fn->block_count * sizeof(IrBlock *));
        int worklist_count = 0;

        for (int i = 0; i < var->def_count; i++) {
            worklist[worklist_count++] = var->def_sites[i];
        }

        bool *has_phi = calloc(fn->block_count, sizeof(bool));

        int processed = 0;
        while (processed < worklist_count) {
            IrBlock *b = worklist[processed++];

            for (int i = 0; i < b->dom_frontier_count; i++) {
                IrBlock *frontier = b->dom_frontier[i];
                if (!has_phi[frontier->id]) {
                    insert_phi_node(fn, frontier, var);
                    has_phi[frontier->id]      = true;
                    worklist[worklist_count++] = frontier;
                }
            }
        }
        free(worklist);
        free(has_phi);
    }
}

// ----- SSA Renaming -----
// Stack manegement
static RenameState *create_rename_state(VarAnalysis *analysis) {
    RenameState *state  = malloc(sizeof(RenameState));
    state->var_count    = analysis->count;

    state->def_stacks   = malloc(state->var_count * sizeof(IrValue *));
    state->stack_depths = calloc(state->var_count, sizeof(int));
    state->stack_caps   = malloc(state->var_count * sizeof(int));

    for (int i = 0; i < state->var_count; i++) {
        state->def_stacks[i]   = malloc(8 * sizeof(IrValue));
        state->stack_depths[i] = 0;
        state->stack_caps[i]   = 8;
    }

    return state;
}

static void free_rename_state(RenameState *state) {
    for (int i = 0; i < state->var_count; i++) {
        free(state->def_stacks[i]);
    }
    free(state->def_stacks);
    free(state->stack_depths);
    free(state->stack_caps);
    free(state);
}

static void push_def(RenameState *state, int var_id, IrValue value) {
    int depth = state->stack_depths[var_id];

    if (depth >= state->stack_caps[var_id]) {
        state->stack_caps[var_id] *= 2;
        state->def_stacks[var_id] =
            realloc(state->def_stacks[var_id],
                    state->stack_caps[var_id] * sizeof(IrValue));
    }

    state->def_stacks[var_id][depth] = value;
    state->stack_depths[var_id]++;
}

static IrValue top_def(RenameState *state, int var_id) {
    int depth = state->stack_depths[var_id];

    if (depth == 0) {
        fprintf(stderr, "top_def: variable used before defined, %d\n", var_id);
        abort();
    }

    return state->def_stacks[var_id][depth - 1];
}

static void save_stack_depths(RenameState *state, int *saved_depths) {
    memcpy(saved_depths, state->stack_depths, state->var_count * sizeof(int));
}

static void restore_stack_depths(RenameState *state, int *saved_depths) {
    memcpy(state->stack_depths, saved_depths, state->var_count * sizeof(int));
}

// Helper functions
static int find_var_for_alloca(VarAnalysis *analysis, IrValue alloca_addr) {
    for (int i = 0; i < analysis->count; i++) {
        if (analysis->vars[i].alloca_addr == alloca_addr) {
            return i;
        }
    }

    return -1;
}

static void replace_value_uses(IrFunc *fn, IrValue old, IrValue new) {
    for (IrBlock *b = fn->entry; b; b = b->next) {
        for (IrInst *inst = b->first; inst; inst = inst->next) {
            if (inst->src0 == old)
                inst->src0 = new;

            if (inst->src1 == old)
                inst->src1 = new;

            if (inst->op == IR_OP_CALL) {
                for (int i = 0; i < inst->call_arg_count; i++) {
                    if (inst->call_args[i] == old)
                        inst->call_args[i] = new;
                }
            }
        }
    }
}

static void remove_inst(IrInst *inst) {
    if (inst->prev)
        inst->prev->next = inst->next;
    else
        inst->block->first = inst->next;

    if (inst->next)
        inst->next->prev = inst->prev;
    else
        inst->block->last = inst->prev;

    free(inst);
}

static int find_predecessor_index(IrBlock *successor, IrBlock *predecessor) {
    for (int i = 0; i < successor->pred_count; i++) {
        if (successor->preds[i] == predecessor) {
            return i;
        }
    }

    fprintf(stderr, "find_predecessor_index: predecessor not found\n");
    abort();
}

static void rename_block(IrFunc *fn, VarAnalysis *analysis, IrBlock *block,
                         RenameState *state) {
    int *saved_depths = malloc(state->var_count * sizeof(int));
    save_stack_depths(state, saved_depths);

    // Process phi nodes
    for (IrInst *inst = block->first; inst && inst->op == IR_OP_PHI;
         inst         = inst->next) {
        int var_id = find_var_for_alloca(analysis, inst->imm);

        if (var_id >= 0) {
            push_def(state, var_id, inst->dst);
        }
    }

    // Process instructions
    IrInst *inst = block->first;
    while (inst) {
        IrInst *next = inst->next;

        if (inst->op == IR_OP_PHI) {
            inst = next;
            continue;
        }

        if (inst->op == IR_OP_LOAD) {
            int var_id = find_var_for_alloca(analysis, inst->src0);

            if (var_id >= 0) {
                IrValue current_def = top_def(state, var_id);
                replace_value_uses(fn, inst->dst, current_def);
                remove_inst(inst);
            }
        }

        else if (inst->op == IR_OP_STORE) {
            int var_id = find_var_for_alloca(analysis, inst->dst);

            if (var_id >= 0) {
                IrValue stored_value = inst->src0;
                push_def(state, var_id, stored_value);
                remove_inst(inst);
            }
        }

        inst = next;
    }

    // Fil PHI ops in successors
    for (int s = 0; s < block->succ_count; s++) {
        IrBlock *succ = block->succs[s];

        int pred_idx  = find_predecessor_index(succ, block);

        if (pred_idx < 0) {
            fprintf(stderr, "rename_block: predecessor not found\n");
            abort();
        }

        for (IrInst *phi = succ->first; phi && phi->op == IR_OP_PHI;
             phi         = phi->next) {
            int var_id = find_var_for_alloca(analysis, phi->imm);

            if (var_id >= 0) {
                IrValue current_def       = top_def(state, var_id);
                phi->phi.values[pred_idx] = current_def;
            }
        }
    }

    // Process blocks dominated by this block
    for (int i = 0; i < block->dom_children_count; i++) {
        rename_block(fn, analysis, block->dom_children[i], state);
    }

    restore_stack_depths(state, saved_depths);
    free(saved_depths);
}

void ssa_renaming_pass(IrFunc *fn, VarAnalysis *analysis) {
    RenameState *state = create_rename_state(analysis);
    rename_block(fn, analysis, fn->entry, state);
    free_rename_state(state);
}

// ----- PHI node elimination -----
static IrInst *create_copy_inst(IrFunc *fn, IrValue dst, IrValue src,
                                TypeId type) {
    IrInst *copy = malloc(sizeof(IrInst));
    memset(copy, 0, sizeof(IrInst));

    copy->op   = IR_OP_MOV;
    copy->dst  = dst;
    copy->src0 = src;
    copy->src1 = (IrValue)~0u;
    copy->type = type;

    return copy;
}

static void insert_before(IrInst *before, IrInst *inst) {
    inst->block = before->block;
    inst->next  = before;
    inst->prev  = before->prev;

    if (before->prev) {
        before->prev->next = inst;
    } else {
        before->block->first = inst;
    }

    before->prev = inst;
}

void phi_elimination_pass(IrFunc *fn) {
    for (IrBlock *b = fn->entry; b; b = b->next) {
        IrInst **phis      = NULL;
        uint32_t phi_count = 0;
        uint32_t phi_cap   = 0;

        for (IrInst *inst = b->first; inst && inst->op == IR_OP_PHI;
             inst         = inst->next) {
            if (phi_count >= phi_cap) {
                phi_cap = phi_cap ? phi_cap * 2 : 4;
                phis    = realloc(phis, phi_cap * sizeof(IrInst *));
            }

            phis[phi_count++] = inst;
        }

        if (phi_count == 0) {
            continue;
        }

        for (uint32_t p = 0; p < b->pred_count; p++) {
            IrBlock *pred      = b->preds[p];

            IrInst *terminator = pred->last;
            if (!terminator)
                continue;

            IrValue *temps = malloc(phi_count * sizeof(IrValue));

            for (int i = 0; i < phi_count; i++) {
                IrInst *phi       = phis[i];
                IrValue src_value = phi->phi.values[p];
                IrInst *copy1 =
                    create_copy_inst(fn, phi->dst, src_value, phi->type);
                insert_before(terminator, copy1);
            }

            free(temps);
        }

        for (int i = 0; i < phi_count; i++) {
            IrInst *phi = phis[i];

            if (phi->prev) {
                phi->prev->next = phi->next;
            } else {
                phi->block->first = phi->next;
            }

            if (phi->next) {
                phi->next->prev = phi->prev;
            } else {
                phi->block->last = phi->prev;
            }

            free(phi->phi.values);
            free(phi->phi.labels);
            free(phi);
        }

        free(phis);
    }
}

// ----- ALLOCA elimination -----
static bool instruction_uses_value(IrInst *inst, IrValue value) {
    switch (inst->op) {
    case IR_OP_CALL:
        for (int i = 0; i < inst->call_arg_count; i++) {
            if (inst->call_args[i] == value)
                return true;
        }
        return false;

    case IR_OP_PHI:
        for (int i = 0; i < inst->phi.count; i++) {
            if (inst->phi.values[i] == value)
                return true;
        }
        return false;

    default:
        break;
    }

    if (inst->dst == value || inst->src0 == value || inst->src1 == value) {
        return true;
    }

    return false;
}

static bool alloca_is_used(IrFunc *fn, IrValue alloca_addr) {
    for (IrBlock *b = fn->entry; b; b = b->next) {
        for (IrInst *inst = b->first; inst; inst = inst->next) {
            if (inst->op == IR_OP_ALLOCA && inst->dst == alloca_addr) {
                continue; // Skip the alloca itself
            }

            if (instruction_uses_value(inst, alloca_addr)) {
                return true;
            }
        }
    }
    return false;
}

void dead_alloca_elimination_pass(IrFunc *fn) {
    for (IrBlock *b = fn->entry; b; b = b->next) {
        IrInst *inst = b->first;

        while (inst) {
            IrInst *next = inst->next;

            if (inst->op == IR_OP_ALLOCA) {
                if (!alloca_is_used(fn, inst->dst)) {
                    remove_inst(inst);
                }
            }

            inst = next;
        }
    }
}
