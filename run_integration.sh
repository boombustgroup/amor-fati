#!/usr/bin/env bash
# Run Phase 5 integration tests with all mechanisms enabled
set -euo pipefail
HH_MODE=individual \
OPEN_ECON=true \
NBP_QE=true \
NBP_FX_INTERVENTION=true \
IO_MODE=enabled \
IO_SCALE=0.5 \
sbt "testOnly sfc.engine.IntegrationFullSpec"
