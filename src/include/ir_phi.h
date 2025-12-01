#pragma once

#include "ir.h"

// RPO Ordered IR Blocks
typedef struct IrRpo {
    IrBlock **blocks;
    uint32_t count;
} IrRpo;

// Variable analysis
typedef struct VarInfo {
    IrValue alloca_addr;
    TypeId type;
    IrBlock **def_sites;
    int def_count;
    int def_cap;
} VarInfo;

typedef struct VarAnalysis {
    VarInfo *vars;
    uint32_t count;
    uint32_t cap;
} VarAnalysis;

typedef struct RenameState {
    IrValue **def_stacks;
    int *stack_depths;
    int *stack_caps;
    int var_count;
} RenameState;

void build_cfg_edges(IrFunc *fn);
IrRpo calculate_rpo(IrFunc *fn);
void calculate_immediate_dominators(const IrRpo *rpo);
void build_dom_tree(const IrRpo *rpo);
void calculate_dominance_frontiers(const IrRpo *rpo);
VarAnalysis *analyze_variables(IrFunc *fn);
void phi_generation_pass(IrFunc *fn, VarAnalysis *analysis);
void ssa_renaming_pass(IrFunc *fn, VarAnalysis *analysis);
void phi_elimination_pass(IrFunc *fn);
void dead_alloca_elimination_pass(IrFunc *fn);
