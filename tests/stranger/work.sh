# Shared by container.sh and mac.sh (sourced; $here is tests/stranger).
#   prepare_work BUNDLE RUN_DIR OUT   OUT (new) gets lean_mpas.bundle, run/ (a copy of RUN_DIR, links followed) and
#                                     record; RUN_DIR's expected.json stays outside OUT
#   write_prompt OUT WORK MACHINE ACCESS   OUT.prompt.md: prompt.md with <WORK>, <MACHINE>, <ACCESS> substituted
prepare_work() {
  [ -f "$1" ] || { echo "no bundle $1" >&2; exit 1; }
  [ -f "$2/namelist.atmosphere" ] || { echo "$2 is not a run directory" >&2; exit 1; }
  [ ! -e "$3" ] || { echo "$3 exists: every round needs a fresh folder" >&2; exit 1; }
  mkdir -p "$3"
  cp "$1" "$3/lean_mpas.bundle"
  cp -RL "$2" "$3/run"
  cp "$here/record" "$3/record"
  chmod +x "$3/record"
}

write_prompt() {
  python3 - "$here/prompt.md" "$1.prompt.md" "$2" "$3" "$4" <<'EOF'
import sys
source, out, work, machine, access = sys.argv[1:]
text = open(source).read().replace('<WORK>', work).replace('<MACHINE>', machine).replace('<ACCESS>', access)
open(out, 'w').write(text)
EOF
}
