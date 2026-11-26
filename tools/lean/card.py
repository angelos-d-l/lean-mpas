"""The card lean_mpas uses: CUDA device 0 after CUDA_VISIBLE_DEVICES (OpenACC's default), found through the driver
library, then its nvidia-smi line by UUID (nvidia-smi numbers the cards in its own order, which can differ from
CUDA's). And the card build's stack frame.

    device_zero(driver=None, smi=None) -> Card | None     None without a driver library or a card
    smi_lines(runner=None) -> list[dict]                   every card nvidia-smi lists
    find_cuobjdump(nvfortran) -> str | None                cuobjdump next to the CUDA nvfortran uses, else on PATH
    largest_frame(text) -> int                             the largest STACK of `cuobjdump -res-usage`
"""
from __future__ import annotations

import ctypes
import ctypes.util
import os
import re
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Optional

SMI_FIELDS = 'uuid,name,memory.total,memory.free,memory.used,driver_version'
# CUdevice_attribute values of cuda.h
CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT = 16
CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR = 39
CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR = 75
CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR = 76


@dataclass(frozen=True)
class Card:
    uuid: str
    name: str
    cc: int                  # 89 for compute capability 8.9
    sms: int
    threads_per_sm: int
    total_mib: Optional[int] = None
    free_mib: Optional[int] = None
    used_mib: Optional[int] = None
    driver: Optional[str] = None
    driver_cuda: Optional[str] = None

    @property
    def threads(self) -> int:
        return self.sms * self.threads_per_sm


class Driver:
    """libcuda through ctypes: the driver API sees the devices after CUDA_VISIBLE_DEVICES, as the program does."""

    def __init__(self, lib=None):
        name = ctypes.util.find_library('cuda') or 'libcuda.so.1'
        self.lib = lib or ctypes.CDLL(name)
        if self.lib.cuInit(0) != 0:
            raise OSError('cuInit failed')

    def count(self) -> int:
        n = ctypes.c_int()
        return n.value if self.lib.cuDeviceGetCount(ctypes.byref(n)) == 0 else 0

    def _device(self, index: int):
        device = ctypes.c_int()
        if self.lib.cuDeviceGet(ctypes.byref(device), index) != 0:
            raise OSError(f'cuDeviceGet({index}) failed')
        return device

    def attribute(self, index: int, attribute: int) -> int:
        value = ctypes.c_int()
        self.lib.cuDeviceGetAttribute(ctypes.byref(value), attribute, self._device(index))
        return value.value

    def uuid(self, index: int) -> str:
        raw = (ctypes.c_ubyte * 16)()
        self.lib.cuDeviceGetUuid(raw, self._device(index))
        h = bytes(raw).hex()
        return f'GPU-{h[:8]}-{h[8:12]}-{h[12:16]}-{h[16:20]}-{h[20:]}'

    def name(self, index: int) -> str:
        buffer = ctypes.create_string_buffer(256)
        self.lib.cuDeviceGetName(buffer, 256, self._device(index))
        return buffer.value.decode(errors='replace')

    def cuda_version(self) -> str:
        version = ctypes.c_int()
        self.lib.cuDriverGetVersion(ctypes.byref(version))
        return f'{version.value // 1000}.{version.value % 1000 // 10}'


def smi_lines(runner: Optional[Callable[[list], str]] = None) -> list[dict]:
    """[{uuid, name, total, free, used, driver}] from nvidia-smi (MiB); [] without it."""
    if runner is None:
        smi = shutil.which('nvidia-smi')
        if smi is None:
            return []

        def runner(command):
            done = subprocess.run(command, capture_output=True, text=True, timeout=60)
            return done.stdout if done.returncode == 0 else ''
        command = [smi, f'--query-gpu={SMI_FIELDS}', '--format=csv,noheader,nounits']
    else:
        command = ['nvidia-smi', f'--query-gpu={SMI_FIELDS}', '--format=csv,noheader,nounits']
    out = []
    for line in runner(command).splitlines():
        parts = [part.strip() for part in line.split(',')]
        if len(parts) != 6:
            continue
        number = lambda text: int(text) if text.isdigit() else None   # noqa: E731
        out.append({'uuid': parts[0], 'name': parts[1], 'total': number(parts[2]), 'free': number(parts[3]),
                    'used': number(parts[4]), 'driver': parts[5]})
    return out


def device_zero(driver=None, smi: Optional[list] = None) -> Optional[Card]:
    """Device 0 of the driver library (after CUDA_VISIBLE_DEVICES) with its nvidia-smi line found by UUID."""
    try:
        driver = driver or Driver()
        if driver.count() < 1:
            return None
        uuid = driver.uuid(0)
        major = driver.attribute(0, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR)
        minor = driver.attribute(0, CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MINOR)
        card = dict(uuid=uuid, name=driver.name(0), cc=major * 10 + minor,
                    sms=driver.attribute(0, CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT),
                    threads_per_sm=driver.attribute(0, CU_DEVICE_ATTRIBUTE_MAX_THREADS_PER_MULTIPROCESSOR),
                    driver_cuda=driver.cuda_version())
    except (OSError, AttributeError):
        return None
    line = next((entry for entry in (smi_lines() if smi is None else smi) if entry['uuid'] == uuid), None)
    if line:
        card.update(total_mib=line['total'], free_mib=line['free'], used_mib=line['used'], driver=line['driver'])
    return Card(**card)


def find_cuobjdump(nvfortran: Optional[str]) -> Optional[str]:
    """cuobjdump of the CUDA in nvfortran's NVHPC bundle (<nvhpc>/<arch>/<year>/cuda/<version>/bin), the newest version
    there; else the first on PATH."""
    if nvfortran:
        parents = Path(nvfortran).resolve().parents
        cuda = parents[2] / 'cuda' if len(parents) > 2 else None
        if cuda is not None and cuda.is_dir():
            versions = sorted((p for p in cuda.iterdir() if re.fullmatch(r'\d+\.\d+', p.name)),
                              key=lambda p: tuple(int(x) for x in p.name.split('.')), reverse=True)
            for version in versions:
                tool = version / 'bin' / 'cuobjdump'
                if os.access(tool, os.X_OK):
                    return str(tool)
    return shutil.which('cuobjdump')


def largest_frame(text: str) -> int:
    """The largest per-thread stack frame (STACK:<bytes>) of `cuobjdump -res-usage` output; 0 when none is listed."""
    return max((int(found) for found in re.findall(r'\bSTACK:(\d+)', text)), default=0)
