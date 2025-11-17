#pragma once

#include "ast.h"
#include "ir.h"

IrModule *lower_to_ir(AstNode *translation_unit);
