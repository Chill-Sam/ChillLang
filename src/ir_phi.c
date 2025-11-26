#include "ir_phi.h"
#include <stdlib.h>
#include <string.h>

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

static void build_cfg_edges(IrFunc *fn) {
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

static void insert_before_terminator(IrBlock *b, IrInst *new_inst) {
    if (!b->last) {
        // Empty block
        new_inst->next  = NULL;
        new_inst->prev  = NULL;
        new_inst->block = b;
        b->first        = new_inst;
        b->last         = new_inst;
        return;
    }

    // Insert before last instruction
    IrInst *terminator = b->last;
    new_inst->next     = terminator;
    new_inst->prev     = terminator->prev;
    new_inst->block    = b;

    if (terminator->prev) {
        terminator->prev->next = new_inst;
    } else {
        b->first = new_inst;
    }
    terminator->prev = new_inst;
}

void remove_instruction(IrBlock *b, IrInst *inst) {
    if (inst->prev) {
        inst->prev->next = inst->next;
    } else {
        b->first = inst->next;
    }

    if (inst->next) {
        inst->next->prev = inst->prev;
    } else {
        b->last = inst->prev;
    }

    // Free PHI-specific data
    if (inst->op == IR_OP_PHI) {
        free(inst->phi.values);
        free(inst->phi.labels);
    }

    free(inst);
}

void phi_elimination_pass(IrFunc *fn) {
    build_cfg_edges(fn);

    for (IrBlock *b = fn->entry; b; b = b->next) {
        for (int i = 0; i < b->succ_count; i++) {
            IrBlock *succ = b->succs[i];

            for (IrInst *phi = succ->first->next; phi && phi->op == IR_OP_PHI;
                 phi         = phi->next) {
                for (int j = 0; j < phi->phi.count; j++) {
                    if (phi->phi.labels[j] == b->label_value) {
                        IrInst *mov = malloc(sizeof(IrInst));
                        mov->op     = IR_OP_MOV;
                        mov->type   = phi->type;
                        mov->dst    = phi->dst;
                        mov->src0   = phi->phi.values[j];

                        insert_before_terminator(b, mov);
                        break;
                    }
                }
            }
        }
    }

    for (IrBlock *b = fn->entry; b; b = b->next) {
        IrInst *inst = b->first->next;
        while (inst && inst->op == IR_OP_PHI) {
            IrInst *next = inst->next;
            remove_instruction(b, inst);
            inst = next;
        }
    }
}
