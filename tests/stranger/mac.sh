#!/bin/sh
# A stranger round on this macOS machine itself (a tester new to lean_mpas, a person or an AI agent, follows only the
# README):
#   sh tests/stranger/mac.sh BUNDLE RUN_DIR OUT
# OUT (a new folder outside the folder that holds this clone) becomes the work root and the HOME of every command. Beside OUT:
#   OUT.shell      OUT.shell 'COMMAND' runs sh -c COMMAND in OUT with only HOME=OUT, PATH=/opt/homebrew/bin:/usr/bin:/bin
#                  and LANG set
#   OUT.prompt.md  the prompt with <WORK> = OUT, <MACHINE> this machine, <ACCESS> OUT.shell
# Then: give the tester OUT.prompt.md and nothing else; afterwards python3 tests/stranger/judge.py OUT
# <RUN_DIR's parent>/expected.json; delete OUT, OUT.shell and OUT.prompt.md.
set -eu
[ $# -eq 3 ] || { echo 'usage: mac.sh BUNDLE RUN_DIR OUT' >&2; exit 2; }
here=$(cd "$(dirname "$0")" && pwd -P)
. "$here/work.sh"
parent=$(cd "$here/../../.." && pwd -P)
case $(cd "$(dirname "$3")" && pwd -P)/ in
  "$parent"/*) echo "mac.sh: $3 is inside $parent: use a folder outside it" >&2; exit 1 ;;
esac
prepare_work "$1" "$2" "$3"
out=$(cd "$3" && pwd -P)
cat > "$out.shell" <<EOF
#!/bin/sh
# Runs one command with sh in $out, with a clean environment. Variables and cd do not carry over between calls.
cd "$out" && exec env -i HOME="$out" PATH=/opt/homebrew/bin:/usr/bin:/bin LANG=en_US.UTF-8 sh -c "\$1"
EOF
chmod +x "$out.shell"
write_prompt "$out" "$out" "this machine (macOS on Apple silicon, Homebrew in /opt/homebrew; Homebrew is shared with \
the machine's owner: install what is missing, never upgrade or uninstall a package)" "run every command through \
$out.shell 'COMMAND', which runs sh -c COMMAND starting in $out each time, with a clean environment (HOME is $out); cd \
and variables do not carry over between calls."
echo "MAC ROUND ready; work root $out"
echo "prompt: $out.prompt.md"
echo "judge:  python3 $here/judge.py $out $(cd "$2/.." && pwd -P)/expected.json"
echo "after:  rm -rf $out $out.shell $out.prompt.md"
