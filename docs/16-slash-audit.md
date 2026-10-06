# Slash-command pipeline audit — input → completion → dispatch (2026-10)

Audited against a **live gateway** (scratch probes, commands.catalog + real
slash.exec round-trips), not the README. Wire contract from
`tui_gateway/contracts/tools_commands.py` (`SlashExecResult`) and
`tui_gateway/methods_tools.py`.

## What slash.exec can return (wire contract)

1. **Plain worker result** — `output` (+ optional `warning`). Plugin/script
   command output.
2. **Dispatch directive** — the gateway reroutes the command and returns a
   directive instead of doing the work itself:
   - `type: "send"` (+ `message`, optional `notice`/`display`) — UI must
     SUBMIT `message` as a user turn. `/retry /steer /undo /compress /queue
     /plan /goal /loop /moa /learn /init /q` and every skill slash resolve
     to this via `_PENDING_INPUT_COMMANDS` (server.py).
   - `type: "skill"` — same shape, for skill prompts.
   - `type: "alias"` (+ `target`) — re-run `/target`.
   - `type: "prefill"` — put `message` in the input box (not submitted).
3. **4018 gate** — JSON-RPC error `"skill command: use command.dispatch for
   /x"` when a skill slug must go through the dispatch path.

## What the client did before the fix

- Rendered only `output` (silent for directives; `warning` dropped).
- 4018 surfaced as a raw error; the user had to retype the command as a
  plain message to reach the skill.
- Prefill rendered nothing.
- Completion annotations showed the unresolved i18n key
  `slash.shared.usage_suffix` (64 of 279 catalog pairs carry only that
  placeholder — real live catalog, 2026-10).

## Fixes (hermes-input.el)

- `hermes-input--slash-result` — central handler for slash.exec responses:
  worker output renders; `warning` appends `⚠ <warning>`; directives act
  (send/skill → prompt.submit via the normal send path; alias → re-run the
  target with original args; prefill → renders the text as pending display
  guidance this tick — no prompt-insertion helper exists yet); 4018 → one
  automatic `command.dispatch` retry with the original slug+args.
- `hermes-input--slash-max-depth` (= 2) bounds directive chains (send /
  alias hop / 4018 retry); a chain at the cap renders the directive/error
  instead of looping. Guards gateway alias loops.
- `hermes-input--usage-suffix-placeholder` — the junk key is filtered from
  completion annotations and doc buffers (`hermes-input--pair-desc`).
- `hermes-input--send-1` now takes a DEPTH parameter (0 = user send; >0 =
  chained hop) and passes it to `hermes-input--slash-result`.

## Verification

- Unit: 12 new tests (directive handling incl. args pass-through, cap
  rendering, 4018 retry shapes, placeholder filter, warning-only output).
  Gate: **484/484 green, 0 unexpected**.
- Live (scratch/slash-fix-probe.el, real gateway, session 2009059d /
  fe3d6dc0): `/usage` worker output renders; `/queue hello from emacs`
  returned a send directive whose notice rendered and whose `message`
  drove a real `prompt.submit` (the queued turn committed — 2 committed
  turns at exit); skill `/yuanbao` hit the 4018 gate, client auto-retried
  `command.dispatch`, gateway answered with the skill directive, and the
  client submitted it (`prompt.submit`) — chain PASS, 0 failures.

## Known remaining gaps (deliberate, for later items)

- Prefill is display-only (no prompt-insertion helper in hermes-comint).
- Bench wipes ephemeral system messages on refresh (pre-existing comint
  behavior; committed turns keep).
- `/session` (singular) does not exist in the catalog — falls through to
  slash.exec and errors; use `/sessions` (client-intercepted) instead.
- The catalog `skills` key is a hash of `/<slug> → {usage, origin}`; the
  pair list is the completion source (skills appear there as real pairs).
