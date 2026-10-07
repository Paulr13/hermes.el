#!/bin/sh
# Live-gateway e2e runner: bounds a batch-emacs harness with `timeout` and
# strips the cron env markers (HERMES_CRON_SESSION / HERMES_EXEC_ASK /
# HERMES_AGENT) — with them set the agent auto-denies approvals per
# approvals.cron_mode and no srq prompt ever reaches the client.
#
# Usage (from the repo root or anywhere):
#   ./test/live/run.sh live-e2e               # terminal tool + subagent round-trip
#   ./test/live/run.sh live-e2e-prompts 400   # approval + clarify round-trips
#   ./test/live/run.sh live-e2e-multisession  # side-by-side two-session streaming
#   ./test/live/run.sh live-e2e-tool-stream   # no mid-run tool output (tool.progress dead)
#
# Env overrides:
#   LIVE_E2E_PYTHON   gateway python (default HERMES_DEV_PYTHON, then
#                     ~/.hermes/venv-current/bin/python — the one that can
#                     `import tui_gateway`)
#   LIVE_E2E_LOG      result/log file (default under temporary-file-directory)

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
name=${1:?usage: test/live/run.sh <live-e2e|live-e2e-prompts|live-e2e-multisession> [timeout_s]}
tmo=${2:-560}

harness="$ROOT/test/live/$name.el"
[ -f "$harness" ] || { echo "no such harness: $harness" >&2; exit 2; }

PY=${LIVE_E2E_PYTHON:-${HERMES_DEV_PYTHON:-$HOME/.hermes/venv-current/bin/python}}
[ -x "$PY" ] || { echo "gateway python not executable: $PY (set LIVE_E2E_PYTHON)" >&2; exit 2; }

cd "$ROOT"
exec env -u HERMES_CRON_SESSION -u HERMES_EXEC_ASK -u HERMES_AGENT \
    LIVE_E2E_PYTHON="$PY" \
    timeout "$tmo" emacs --batch -Q -L "$ROOT" -l "test/live/$name.el"
