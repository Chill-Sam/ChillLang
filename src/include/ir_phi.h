#pragma once

#include "ir.h"

void build_cfg_edges(IrModule *m);
void phi_elimination_pass(IrFunc *fn);
