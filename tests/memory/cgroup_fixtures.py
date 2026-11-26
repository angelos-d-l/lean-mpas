"""Fixture roots of /proc and cgroup files for the host-availability readers (src/run/lean_memory.f90 available_linux and
tools/lean/memory.py available_linux): make(root) writes them and returns {name: (expected bytes, expected source)}."""
from pathlib import Path

GIB = 2 ** 30
MEMINFO = 'MemTotal:       65000000 kB\nMemFree:         1000000 kB\nMemAvailable:   20971520 kB\n'   # 20 GiB available


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def make(root: Path) -> dict:
    root = Path(root)
    cases = {}
    v2_mount = '30 23 0:26 / /sys/fs/cgroup rw,nosuid shared:4 - cgroup2 cgroup2 rw,nsdelegate\n'
    # v2 without a limit anywhere: MemAvailable
    r = root / 'v2_nolimit'
    _write(r / 'proc/meminfo', MEMINFO)
    _write(r / 'proc/self/mountinfo', v2_mount)
    _write(r / 'proc/self/cgroup', '0::/user.slice/job\n')
    _write(r / 'sys/fs/cgroup/user.slice/job/memory.max', 'max\n')
    _write(r / 'sys/fs/cgroup/user.slice/job/memory.current', '1000\n')
    _write(r / 'sys/fs/cgroup/user.slice/memory.max', 'max\n')
    cases['v2_nolimit'] = (20 * GIB, 'MemAvailable')
    # v2 with a limit on the leaf: 8 GiB less 3 GiB used
    r = root / 'v2_leaf'
    _write(r / 'proc/meminfo', MEMINFO)
    _write(r / 'proc/self/mountinfo', v2_mount)
    _write(r / 'proc/self/cgroup', '0::/docker/abc\n')
    _write(r / 'sys/fs/cgroup/docker/abc/memory.max', str(8 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/docker/abc/memory.current', str(3 * GIB) + '\n')
    cases['v2_leaf'] = (5 * GIB, 'cgroup /docker/abc')
    # v2 with a smaller ancestor limit: the parent leaves 2 GiB
    r = root / 'v2_ancestor'
    _write(r / 'proc/meminfo', MEMINFO)
    _write(r / 'proc/self/mountinfo', v2_mount)
    _write(r / 'proc/self/cgroup', '0::/batch/job/step\n')
    _write(r / 'sys/fs/cgroup/batch/job/step/memory.max', str(16 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/batch/job/step/memory.current', str(1 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/batch/job/memory.max', str(6 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/batch/job/memory.current', str(4 * GIB) + '\n')
    cases['v2_ancestor'] = (2 * GIB, 'cgroup /batch/job')
    # v2 with usage above the limit: 0
    r = root / 'v2_over'
    _write(r / 'proc/meminfo', MEMINFO)
    _write(r / 'proc/self/mountinfo', v2_mount)
    _write(r / 'proc/self/cgroup', '0::/full\n')
    _write(r / 'sys/fs/cgroup/full/memory.max', str(1 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/full/memory.current', str(2 * GIB) + '\n')
    cases['v2_over'] = (0, 'cgroup /full')
    # v1: the memory controller's own mount, the 2^62-or-more sentinel above a 4 GiB limit
    r = root / 'v1'
    _write(r / 'proc/meminfo', MEMINFO)
    _write(r / 'proc/self/mountinfo',
           '25 24 0:22 / /sys/fs/cgroup/cpu rw - cgroup cgroup rw,cpu,cpuacct\n'
           '26 24 0:23 / /sys/fs/cgroup/memory rw - cgroup cgroup rw,memory\n')
    _write(r / 'proc/self/cgroup', '5:cpu,cpuacct:/slurm/job1\n4:memory:/slurm/job1\n')
    _write(r / 'sys/fs/cgroup/memory/slurm/job1/memory.limit_in_bytes', str(4 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/memory/slurm/job1/memory.usage_in_bytes', str(1 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/memory/slurm/memory.limit_in_bytes', '9223372036854771712\n')
    _write(r / 'sys/fs/cgroup/memory/slurm/memory.usage_in_bytes', str(1 * GIB) + '\n')
    _write(r / 'sys/fs/cgroup/memory/memory.limit_in_bytes', '9223372036854771712\n')
    cases['v1'] = (3 * GIB, 'cgroup /slurm/job1')
    # no MemAvailable
    r = root / 'no_meminfo'
    _write(r / 'proc/meminfo', 'MemTotal: 1 kB\n')
    cases['no_meminfo'] = (None, None)
    return cases
