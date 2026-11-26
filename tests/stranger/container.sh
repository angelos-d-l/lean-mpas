#!/bin/sh
# A stranger round in a fresh Linux container (a tester new to lean_mpas, a person or an AI agent, follows only the
# README):
#   sh tests/stranger/container.sh BUNDLE RUN_DIR OUT
# OUT (a new folder on the host, outside the folder that holds this clone) becomes the work root, mounted at /work in a new ubuntu:24.04
# container (root, --cpus 2, --memory 8g, network on for apt) that waits; nothing is installed in it. Beside OUT:
#   OUT.shell      the only way into the container: OUT.shell 'COMMAND' runs sh -c COMMAND in /work, as root
#   OUT.prompt.md  the prompt with <WORK> = /work, <MACHINE> the container, <ACCESS> OUT.shell
# Then: give the tester OUT.prompt.md and nothing else; afterwards python3 tests/stranger/judge.py OUT
# <RUN_DIR's parent>/expected.json; docker rm -f <name>. The image is pulled on the first run (about 30 MB).
set -eu
[ $# -eq 3 ] || { echo 'usage: container.sh BUNDLE RUN_DIR OUT' >&2; exit 2; }
here=$(cd "$(dirname "$0")" && pwd -P)
. "$here/work.sh"
prepare_work "$1" "$2" "$3"
out=$(cd "$3" && pwd -P)
name=lean-stranger-$(basename "$out")
docker run -d --name "$name" --cpus 2 --memory 8g -v "$out:/work" -w /work ubuntu:24.04 sleep infinity > /dev/null
cat > "$out.shell" <<EOF
#!/bin/sh
# Runs one command in the container $name, with sh, in /work. Variables and cd do not carry over between calls.
exec docker exec -i -w /work $name sh -c "\$1"
EOF
chmod +x "$out.shell"
write_prompt "$out" /work "a fresh Ubuntu 24.04 Linux container (you are root; 2 CPUs, 8 GB of memory, network \
for apt; nothing installed beyond the base image)" "run every command in the container through $out.shell 'COMMAND', \
which runs sh -c COMMAND inside the container, starting in /work each time; cd and variables do not carry over between \
calls."
echo "CONTAINER $name ready; work root $out (/work inside)"
echo "prompt: $out.prompt.md"
echo "judge:  python3 $here/judge.py $out $(cd "$2/.." && pwd -P)/expected.json"
echo "after:  docker rm -f $name"
