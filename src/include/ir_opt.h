#pragma once

#include "ir.h"

void dead_code_elimination_pass(IrModule *m);
void redundant_br_elimination_pass(IrModule *m);
