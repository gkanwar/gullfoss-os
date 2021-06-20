#include "process.h"

#include "assert.h"

static ProcessManager* inst;

ProcessManager::ProcessManager() { assert_make_inst(inst, this); }
ProcessManager& ProcessManager::get() { return assert_get_inst(inst); }
