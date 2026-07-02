# Clinician Scenarios for the MYCIN Rulebase

A curated set of vignettes for driving the expanded 15-rule MYCIN rulebase
(`examples/mycin.lisp`) through the Claude driver
(`src/llm/claude/driver.py`) and the HTTP bridge. Each scenario is written
the way a clinician might present a case at the bedside, and each is
annotated with:

- **Facts to be extracted** — what Claude should turn the vignette into
- **Rules expected to fire** — the descriptive rule names from the rulebase
- **Expected differential** — the organism hypotheses and belief behavior,
  contrasted between certainty factors (CF) and simplified
  Dempster-Shafer (DS)

Together the seven cases exercise every rule in the current base and,
critically, produce cases where *multiple rules conclude the same organism*
— which is where belief combination becomes visible.

## How to Run

Start the bridge under the belief system you want to see:

```bash
# Certainty factors (default)
sbcl --load lisa.asd \
     --eval '(asdf:load-system :lisa)' \
     --eval '(in-package :lisa-user)' \
     --eval '(load "examples/mycin.lisp")' \
     --eval '(asdf:load-system :lisa-bridge)' \
     --eval '(lisa-bridge:start)'

# Dempster-Shafer
LISA_BELIEF_SYSTEM=ds sbcl --load lisa.asd ...  (same, just the env var)
```

Then start the driver in another shell:

```bash
export ANTHROPIC_API_KEY=sk-...
python src/llm/claude/driver.py
# transcripts land in ./sessions/session-YYYYmmdd-HHMMSS.md by default
```

You can switch belief systems per session at the `Clinician:` prompt by
asking Claude to reset with a specific system (Claude will pass
`{"belief_system": "ds"}` to `reset_session`), or from the shell:

```bash
curl -sX POST http://localhost:8090/reset \
     -H 'content-type: application/json' \
     -d '{"belief_system":"ds"}'
```

Transcript flags: `--no-transcript`, `--transcript-verbosity {minimal,normal,full}`,
`--transcript-dir PATH`. See `driver.py --help`.

---

## Scenario 1 — PAIP culture-1 baseline

> "I have a 27-year-old female burn patient. She's obviously
> immunocompromised. Blood culture: gram-negative rods, aerobic, three days
> old."

**Facts to extract**:
`burn=serious` (patient-1), `compromised-host=t` (patient-1),
`culture-site=blood`, `culture-age=3`, `gram=neg` (organism-1),
`morphology=rod`, `aerobicity=aerobic`.

**Rules that fire**:
- `gram-neg-rod-in-burn-patient-suggests-pseudomonas` (0.4)
- `gram-neg-rod-in-compromised-host-suggests-pseudomonas` (0.6)
- `aerobic-gram-neg-rod-suggests-enterobacteriaceae` (0.8)
- `aerobic-gram-neg-rod-in-compromised-host-suggests-klebsiella` (0.5)

**Expected differential**:
- **Enterobacteriaceae** — single rule, strongest single hypothesis.
  - CF: `0.8`, DS: `bel 0.8, pl 1.0, ignorance 0.2`
- **Pseudomonas** — two rules conclude it, so belief combines.
  - CF: `~0.76` (0.4 ⊕ 0.6 = 0.4 + 0.6 − 0.24)
  - DS: `bel ~0.76`, `pl 1.0`, `ignorance ~0.24`
- **Klebsiella** — single rule with a moderate belief; enters the
  differential but with wide ignorance.
  - CF: `0.5`, DS: `bel 0.5, pl 1.0, ignorance 0.5`

This is the canonical case for showing "multiple rules → belief
combination" (on pseudomonas), and simultaneously for showing how a
single-rule hypothesis with a moderate belief factor produces a wide
ignorance interval under DS — a nuance CF collapses to a single number.
It's also the anchor case in the README.

---

## Scenario 2 — Hospital-acquired immunocompromised gram-negative

> "62-year-old male, been inpatient for two weeks with a central line.
> Immunocompromised — chemo. New fevers. Blood culture: gram-negative rods,
> aerobic."

**Facts to extract**:
`compromised-host=t`, `hospital-acquired=t`, `culture-site=blood`,
`gram=neg`, `morphology=rod`, `aerobicity=aerobic`.

**Rules that fire**:
- `gram-neg-rod-in-compromised-host-suggests-pseudomonas` (0.6)
- `aerobic-gram-neg-rod-suggests-enterobacteriaceae` (0.8)
- `hospital-acquired-gram-neg-rod-in-compromised-host-suggests-klebsiella` (0.6)
- `hospital-acquired-aerobic-gram-neg-rod-suggests-pseudomonas` (0.7)
- `aerobic-gram-neg-rod-in-compromised-host-suggests-klebsiella` (0.5)

**Expected differential**:
- **Pseudomonas** — two rules → combined belief. CF ~0.88; DS bel ~0.88, low
  ignorance.
- **Klebsiella** — two rules → combined belief. CF ~0.80; DS bel ~0.80.
- **Enterobacteriaceae** — single rule. CF 0.8; DS bel 0.8, ignorance 0.2.

Good three-way differential. Exercises the hospital-acquired branch of the
new rules and produces two organisms with multi-rule support.

---

## Scenario 3 — Respiratory strep in an immunocompromised patient

> "45-year-old woman, HIV-positive with a low CD4 count, presenting with
> pneumonia. Sputum and blood cultures both showing gram-positive cocci in
> chains."

**Facts to extract**:
`compromised-host=t`, `culture-site=blood`,
`infection-site=respiratory` (patient-1), `gram=pos`, `morphology=coccus`,
`growth-conformation=chains`.

**Rules that fire**:
- `gram-pos-cocci-in-chains-suggests-streptococcus` (0.7)
- `respiratory-gram-pos-cocci-in-chains-suggests-strep-pneumoniae` (0.75)
- `gram-pos-cocci-in-chains-in-blood-compromised-suggests-enterococcus` (0.7)

**Expected differential**:
Three competing gram-positive hypotheses, each from a single rule — so no
belief combination on any one organism, but a great case for showing
partial-matches driving discriminating questions ("do we have a respiratory
source?" completes the *strep-pneumoniae* rule).

- **Streptococcus** — CF 0.7, DS bel 0.7 / ignorance 0.3
- **Streptococcus pneumoniae** — CF 0.75, DS bel 0.75 / ignorance 0.25
- **Enterococcus** — CF 0.7, DS bel 0.7 / ignorance 0.3

---

## Scenario 4 — Tropical traveler with gram-negative rod

> "Patient just back from two weeks in Southeast Asia. Bloody diarrhea, now
> febrile. Blood culture: gram-negative rods, aerobic."

**Facts to extract**:
`recent-travel=tropical` (patient-1), `culture-site=blood`, `gram=neg`,
`morphology=rod`, `aerobicity=aerobic`.

**Rules that fire**:
- `aerobic-gram-neg-rod-suggests-enterobacteriaceae` (0.8)
- `gram-neg-rod-with-tropical-travel-suggests-salmonella` (0.65)

**Expected differential**:
- **Enterobacteriaceae** — CF 0.8, DS bel 0.8 / ignorance 0.2
- **Salmonella** — CF 0.65, DS bel 0.65 / ignorance 0.35

Enterobacteriaceae is the correct broader hypothesis (Salmonella is one of
them); worth having Claude explain the taxonomic relationship after
narrating the beliefs.

---

## Scenario 5 — Sepsis with low WBC

> "Elderly patient, septic, WBC is 2.1 — quite low. Blood culture:
> gram-negative rods."

**Facts to extract**:
`white-blood-count=low` (patient-1), `culture-site=blood`, `gram=neg`,
`morphology=rod`. (Aerobicity not yet available — this is an early-in-the-case
scenario.)

**Rules that fire**:
- `gram-neg-rod-in-blood-with-low-wbc-suggests-salmonella` (0.55)

**Expected partial matches**:
Multiple rules are one fact away from firing — this is a good scenario for
demonstrating `get_partial_matches` and Claude asking "do you have
aerobicity results?" as the discriminating question.

**Expected differential (with only the facts above)**:
- **Salmonella** — CF 0.55, DS bel 0.55 / ignorance 0.45 (wide — a good
  DS-narration teaching moment)

If aerobicity=aerobic is added, `aerobic-gram-neg-rod-suggests-enterobacteriaceae`
also fires and the differential broadens.

---

## Scenario 6 — Abdominal anaerobe

> "Post-op appendectomy patient with an abdominal source. Culture from the
> collection shows gram-negative rods, anaerobic."

**Facts to extract**:
`infection-site=abdominal` (patient-1), `gram=neg`, `morphology=rod`,
`aerobicity=anaerobic`. (Note: `culture-site=blood` is NOT asserted here —
the culture is from the abdominal collection, not blood.)

**Rules that fire**:
- `anaerobic-gram-neg-rod-in-abdomen-suggests-bacteroides` (0.8)

If the clinician also mentions a positive blood culture (adding
`culture-site=blood`), the classic PAIP rule fires too:
- `anaerobic-gram-neg-rod-in-blood-suggests-bacteroides` (0.9)

**Expected differential (both rules firing)**:
- **Bacteroides** — two rules → combined belief.
  - CF: `0.9 ⊕ 0.8 = 0.98`
  - DS: `bel 0.98, pl 1.0, ignorance 0.02` — tight interval, sharp
    conclusion.

Excellent case for showing DS combination *narrowing* the ignorance interval
as independent evidence accumulates.

---

## Scenario 7 — Ambiguous gram stain

> "Same 27-year-old burn patient as before, but the microbiologist is
> hedging: **probably gram-negative** rods on the slide, but they say
> **possibly gram-positive** — the stain wasn't great. Blood culture,
> anaerobic organism."

**Facts to extract** (note the confidence values driven by clinician hedging):
`burn=serious`, `compromised-host=t`, `culture-site=blood`,
`gram=neg` with `confidence=0.8` ("probably"),
`gram=pos` with `confidence=0.6` ("possibly"), (both on organism-1),
`morphology=rod`, `aerobicity=anaerobic`.

**Rules that fire**:
- `anaerobic-gram-neg-rod-in-blood-suggests-bacteroides` (0.9)
- `gram-neg-rod-in-burn-patient-suggests-pseudomonas` (0.4)
- `gram-neg-rod-in-compromised-host-suggests-pseudomonas` (0.6)
- Possibly gram-positive branches (staphylococcus, streptococcus) if
  `growth-conformation` is later asserted.

**Expected differential**:
Both belief systems propagate the input uncertainty into the conclusions,
but they do it differently:

- **CF**: the low input CF on the gram-pos fact weakens any rule requiring
  it via `weaken-belief` (multiplication).
- **DS**: the input becomes a `ds-belief` with `bel = confidence, pl = 1.0`.
  Downstream rules produce wider ignorance intervals — you can see the
  uncertainty flowing through the network.

This is the interesting DS case: **DS makes input uncertainty visible in
the output interval, where CF collapses it to a single number**.

---

## Rule Coverage Matrix

| Rule | Scenarios that exercise it |
|---|---|
| gram-neg-rod-in-burn-patient-suggests-pseudomonas | 1, 7 |
| gram-pos-cocci-in-clumps-suggests-staphylococcus | (add clumps to 7) |
| anaerobic-gram-neg-rod-in-blood-suggests-bacteroides | 6*, 7 |
| gram-neg-rod-in-compromised-host-suggests-pseudomonas | 1, 2, 7 |
| aerobic-gram-neg-rod-suggests-enterobacteriaceae | 1, 2, 4 |
| gram-pos-cocci-in-chains-suggests-streptococcus | 3 |
| hospital-acquired-gram-pos-cocci-in-clumps-suggests-staph-aureus | (variant of 2) |
| hospital-acquired-gram-neg-rod-in-compromised-host-suggests-klebsiella | 2 |
| hospital-acquired-aerobic-gram-neg-rod-suggests-pseudomonas | 2 |
| aerobic-gram-neg-rod-in-compromised-host-suggests-klebsiella | 1, 2 |
| respiratory-gram-pos-cocci-in-chains-suggests-strep-pneumoniae | 3 |
| gram-neg-rod-with-tropical-travel-suggests-salmonella | 4 |
| gram-pos-cocci-in-chains-in-blood-compromised-suggests-enterococcus | 3 |
| gram-neg-rod-in-blood-with-low-wbc-suggests-salmonella | 5 |
| anaerobic-gram-neg-rod-in-abdomen-suggests-bacteroides | 6 |

*Scenario 6 fires this rule only if a blood culture is also asserted.

---

## Notes for Investigators

- Scenarios that fire **only one rule per organism** don't exercise belief
  combination. If you want to see combination, either combine scenarios
  (e.g., add hospital-acquired to Scenario 1) or use Scenarios 1, 2, or 6.
- The abdominal-plus-blood variant of Scenario 6 is currently the tightest
  DS interval the base can produce (two high-belief rules on the same
  organism).
- Scenario 7 is the most instructive case for contrasting CF and DS — run
  it under both belief systems and compare the transcripts.
