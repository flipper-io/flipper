.arm

.section ".start"

.extern __init

_start: b __init
