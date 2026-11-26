You are setting up a weather model you have never seen. On <MACHINE>, in <WORK>, there are lean_mpas.bundle (a git
bundle: clone it with `git clone <WORK>/lean_mpas.bundle <WORK>/lean_mpas`) and run/ (an MPAS-Atmosphere 8.4.2 run
directory: a 2,337-cell regional mesh, its init and boundary files, MPAS's namelist, streams and stream-list files and
MPAS's physics tables). Read lean_mpas/README.md and nothing else of the repository: no other file, no linked page, no
source code. If the README does not tell you what to do, note it. Goal: build lean_mpas for this machine's CPU as the
README says and run the forecast that run/ describes, unchanged in length and output, with lean_mpas in place of MPAS's
atmosphere_model. Start the forecast with `<WORK>/record <your command>`. You may install packages with the system's
package manager. This machine is shared: use at most 2 CPU cores. Keep every command you run, its exit status and the
end of its output in <WORK>/stranger.log. When you finish or give up, write <WORK>/report.md: did the forecast finish (yes
or no); each place where the README was missing, wrong or unclear, with the README line and what you did, marked
blocked, guessed or minor.

Format of report.md: its first line is `Finished: yes` or `Finished: no`; each item after it carries its mark in square
brackets: `[blocked]`, `[guessed]` or `[minor]`.

How to run commands: <ACCESS>
