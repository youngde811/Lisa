#!/usr/bin/env bash

# Test the Lisa bridge by asserting the culture-1 scenario from mycin.lisp
# and verifying inference results.
#
# Prerequisites:
#   1. Lisa loaded with mycin.lisp (classes and rules, no assertions)
#   2. (lisa:reset) evaluated
#   3. (lisa-bridge:start) running on port 8090

BASE_URL="${LISA_BRIDGE_URL:-http://localhost:8090}"

echo "=== Culture-1 Scenario ==="
echo ""
echo "--- Resetting session ---"

curl -s -X POST "$BASE_URL/reset" | python3 -m json.tool

echo ""

echo "--- Asserting facts ---"
echo ""

echo "1. compromised-host (patient-1, t)"

curl -s -X POST "$BASE_URL/assert-fact" \
  -d '{"fact_type":"compromised-host","entity":"patient-1","value":"t","entity_class":"patient"}' \
  | python3 -m json.tool

echo ""

echo "2. burn (patient-1, serious)"

curl -s -X POST "$BASE_URL/assert-fact" \
  -d '{"fact_type":"burn","entity":"patient-1","value":"serious","entity_class":"patient"}' \
  | python3 -m json.tool

echo ""

echo "3. culture-site (blood)"

curl -s -X POST "$BASE_URL/assert-fact" \
  -d '{"fact_type":"culture-site","value":"blood"}' \
  | python3 -m json.tool

echo ""

echo "4. culture-age (3)"

curl -s -X POST "$BASE_URL/assert-fact" \
  -d '{"fact_type":"culture-age","value":"3"}' \
  | python3 -m json.tool

echo ""

echo "5. gram (organism-1, neg)"

curl -s -X POST "$BASE_URL/assert-fact" \
  -d '{"fact_type":"gram","entity":"organism-1","value":"neg"}' \
  | python3 -m json.tool

echo ""

echo "6. morphology (organism-1, rod)"

curl -s -X POST "$BASE_URL/assert-fact" \
  -d '{"fact_type":"morphology","entity":"organism-1","value":"rod"}' \
  | python3 -m json.tool

echo ""

echo "7. aerobicity (organism-1, aerobic)"

curl -s -X POST "$BASE_URL/assert-fact" \
  -d '{"fact_type":"aerobicity","entity":"organism-1","value":"aerobic"}' \
  | python3 -m json.tool

echo ""

echo "--- Running inference ---"

curl -s -X POST "$BASE_URL/run-inference" | python3 -m json.tool

echo ""

echo "--- Conclusions ---"

curl -s "$BASE_URL/conclusions" | python3 -m json.tool

echo ""

exit 0
