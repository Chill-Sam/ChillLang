#include "ir_opt.h"

// ----- Dead code elimination -----
void dead_code_elimination_pass(IrModule *m) {
    // Loop through all blocks and remove all instructions after a RET or BR
    for (uint32_t i = 0; i < m->funcs_count; i++) {
        IrFunc *fn = &m->funcs[i];
        for (IrBlock *b = fn->entry; b; b = b->next) {
            for (IrInst *inst = b->first; inst; inst = inst->next) {
                if (inst->op == IR_OP_RET || inst->op == IR_OP_BR) {
                    b->last = inst;
                    break;
                }
            }
        }
    }
}

// ----- Redundant BR instructions -----
void redundant_br_elimination_pass(IrModule *m) {
    for (uint32_t i = 0; i < m->funcs_count; i++) {
        IrFunc *fn = &m->funcs[i];
        for (IrBlock *b = fn->entry; b; b = b->next) {
            for (IrInst *inst = b->first; inst; inst = inst->next) {
                if (inst->op != IR_OP_BR)
                    continue;
                if (!inst->next)
                    continue;
                if (inst->next->op != IR_OP_LABEL)
                    continue;
                if (inst->imm != inst->next->imm)
                    continue;

                b->last = inst->prev;
            }
        }
    }
}
