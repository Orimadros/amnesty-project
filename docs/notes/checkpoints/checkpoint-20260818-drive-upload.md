# Checkpoint 2026-08-18 — Google Drive archive upload (paused, needs resume)

Handoff for a fresh session:
"read docs/notes/checkpoints/checkpoint-20260818-drive-upload.md and continue."

This checkpoint is about the DRIVE ARCHIVE UPLOAD only. For the empirics/
migration state (all exhibits reproduced), see
`docs/notes/checkpoints/checkpoint-20260810-overnight.md` — that work is done and
committed/pushed on branch `integration/car-mapbiomas`.

## What this is

We built a frozen-release folder tree on the user's Google Drive
(kjenner23@gmail.com) and started uploading the ~156 GB of data + the Docker
image into it via rclone. The upload was ~70% done when the background job was
killed by session teardown (the Bash background runner does not survive process
exit for multi-hour jobs). It must be RESUMED — rclone copy is idempotent, so
resuming just uploads what's missing.

## Drive layout (already created via web)

`gdrive:Amazon Land Amnesty - Replication/release_2026-08/` with subfolders
00_docker_image, 01_raw_inputs, 02_intermediate, 03_final_outputs,
04_documentation, 05_recovered_legacy. Top-level also has a Google Doc
`00_READ_ME_FIRST` explaining the layout + the "frozen release, never edit in
place" rule.

## Upload status (checked 2026-08-18)

| dest folder | source | status | on Drive |
|---|---|---|---|
| 04_documentation | docs/ | DONE | 46 files / 20 MB |
| 03_final_outputs | output/ + data/clean + data/validation_snapshots | DONE | 32 files / 402 MB |
| 05_recovered_legacy | data/legacy_dropbox (excl. fetch2/ zips) | DONE | 2,263 files / 10.7 GB |
| 01_raw_inputs | data/input | DONE | 40,343 files / 45 GB |
| **00_docker_image** | `docker save amazon-amnesty:dev` + sha256 | **PARTIAL** | 2 files / 2.4 GB (tar should be ~9 GB) — REDO |
| **02_intermediate** | data/intermediate | **PARTIAL** | 40,865 of 59,956 files / 33 GB of 90 GB — RESUME (~19k files, ~57 GB left) |

## rclone setup (persists across sessions)

- Binary: `~/bin/rclone` (v1.75.0, installed no-sudo). Add to PATH:
  `export PATH="$HOME/bin:$PATH"`.
- Remote: `gdrive:` (full-drive scope), config at
  `~/.config/rclone/rclone.conf`. The refresh token persists; access token
  auto-refreshes. Verified working 2026-08-18 (`rclone lsd` returns the folders).
- NOTE: rclone warns it uses its SHARED Google client_id, being retired during
  2026. Works now; for a long-lived archive, make an own client_id
  (https://rclone.org/drive/#making-your-own-client-id).

## HOW TO RESUME (run in a REAL Terminal, not a chat background job)

The multi-hour upload must run somewhere that survives — a real Terminal with
`caffeinate` (prevents sleep) and `nohup` (survives terminal close). rclone copy
skips already-uploaded files, so this only sends what's missing.

```bash
export PATH="$HOME/bin:$PATH"
ROOT="/Users/kristopherjenner/Local projects/Scheinkman/Amnesty/amnesty-project"
DEST="gdrive:Amazon Land Amnesty - Replication/release_2026-08"
OPTS="--transfers 8 --checkers 16 --drive-chunk-size 128M --fast-list --stats 2m --stats-one-line"

# 1) finish the intermediate tree (~57 GB, ~19k files remaining) — resumes automatically
caffeinate -is nohup rclone copy "$ROOT/data/intermediate" "$DEST/02_intermediate" $OPTS \
  --log-file ~/amnesty_upload_intermediate.log --log-level INFO &

# 2) redo the Docker image (delete the partial first, then save + upload + verify)
rclone delete "$DEST/00_docker_image"
TAR=~/amazon-amnesty-dev.tar
docker save amazon-amnesty:dev -o "$TAR"
shasum -a 256 "$TAR" > ~/amazon-amnesty-dev.tar.sha256
caffeinate -is rclone copy "$TAR" "$DEST/00_docker_image" $OPTS
rclone copy ~/amazon-amnesty-dev.tar.sha256 "$DEST/00_docker_image" $OPTS
rm -f "$TAR"
```

Verify when done:
```bash
rclone size "$DEST/02_intermediate" --fast-list   # expect ~90 GB / 59,956 files
rclone size "$DEST/00_docker_image"               # expect ~9 GB
du -sh "$ROOT/data/intermediate"                  # source reference: 90 GB
```

## Notes / decisions carried forward

- We deliberately EXCLUDED `data/legacy_dropbox/fetch2/**` (the raw download
  zips, ~12 GB) — they duplicate already-extracted content. Add them only if the
  user wants the raw archives too.
- Local disk headroom is tight (~79 GB free). The docker `docker save` tar (~9 GB)
  fits; delete it after upload. rclone copy of the data does NOT duplicate
  locally (reads source, streams up).
- Source of truth for CODE is Git (GitHub Orimadros/amnesty-project, branch
  integration/car-mapbiomas). Drive holds DATA + the Docker image only.

## Also still pending (unrelated to upload)

- **Log out of the "Easy Access" app** (third-party Drive client, bundle
  com.wwall.drivemate). It has NO Drive access (only Lumin PDF does — that's a
  legit unrelated app), so no urgency. To disconnect: open Easy Access → click
  "Sign Out Active Account" (bottom-right), then quit/uninstall. Claude can do
  this only if macOS Screen Recording permission is granted to the Claude
  desktop app.

## RESUMED 2026-08-18 17:05 EDT

Resumed via a detached runner so it survives session teardown:

- Script: `~/amnesty_resume_upload.sh` (does step 1 then step 2, then verifies)
- Launched with: `nohup caffeinate -is ~/amnesty_resume_upload.sh > ~/amnesty_resume_upload.out 2>&1 &`
- Logs: `~/amnesty_upload_intermediate.log`, `~/amnesty_upload_docker.log`,
  console/verify output in `~/amnesty_resume_upload.out`

Change from the plan above: the partial Docker tar on Drive is NOT deleted
first — the fresh `rclone copy` replaces it (size differs), which avoids a
destructive step for the same result.

Starting point confirmed at resume: 02_intermediate 40,865 files / 31.1 GiB;
00_docker_image 2 files / 2.195 GiB (truncated tar).

Check progress:
```bash
grep -i ETA ~/amnesty_upload_intermediate.log | tail -3
cat ~/amnesty_resume_upload.out
```

### Restarted 2026-08-18 18:56 EDT at --transfers 24

Run 1 (`--transfers 8 --checkers 16`) sustained only ~1.6 MiB/s; killed at 25%
(10.5 GiB moved, nothing lost — `rclone copy` resumes). Relaunched the same
script with `--transfers 24 --checkers 32`, which sustains ~4.5 MiB/s (~2.8x).
Run 1's log preserved at `~/amnesty_upload_intermediate.log.run1`.

Lesson for any future Drive push from this repo: on trees of many ~1 MB files,
per-file round-trip overhead dominates and `--transfers` is the lever that
matters, not `--drive-chunk-size`.

## COMPLETE 2026-08-19 07:37 EDT

Upload finished. Final verified state of
`gdrive:Amazon Land Amnesty - Replication/release_2026-08`:

| folder | objects | size |
|---|---|---|
| 00_docker_image | 2 | 2.195 GiB |
| 01_raw_inputs | 40,343 | 42.131 GiB |
| 02_intermediate | 59,956 | 89.326 GiB |
| 03_final_outputs | 32 | 383.9 MiB |
| 04_documentation | 46 | 19.06 MiB |
| 05_recovered_legacy | 2,263 | 9.945 GiB |
| **total** | **102,642** | **~144 GiB** |

`02_intermediate` object count matches the source exactly (59,956 files);
17,148 files transferred in the final run with **0 errors**.

### Correction: the Docker image was never partial

The "00_docker_image PARTIAL — tar should be ~9 GB, REDO" line above was a
MISDIAGNOSIS. `docker images` reports two columns:

```text
DISK USAGE   8.99GB   <- uncompressed layers inside the Docker VM
CONTENT SIZE 2.36GB   <- what `docker save` actually writes
```

The checkpoint read DISK USAGE. `docker save` emits 2,356,610,560 bytes, which
is exactly what was already on Drive from 2026-08-11. Nothing was truncated and
nothing needed redoing.

Verified on 2026-08-19 by re-running `docker save` and comparing both ways:

```text
Drive-side MD5   ef1a29ce36e0ade3f183bfeed51a2732  == freshly saved tar
sha256 sidecar   c2eedd38506e35d1d7b0544940de774d64f85a075500a455f4ced34e16ae3fb5  == freshly saved tar
```

Useful side finding: `docker save amazon-amnesty:dev` is **bit-reproducible**
on this host — two independent saves produced byte-identical tars. Good for the
30-year reproducibility contract.

The sidecar was rewritten to name the tar by bare filename rather than the
absolute host path, so `shasum -c amazon-amnesty-dev.tar.sha256` now verifies
from inside the download directory.

### Still open (unchanged, non-urgent)

- rclone uses its shared Google client_id, retiring during 2026. For an archive
  meant to outlive that, mint an own client_id.
- `data/legacy_dropbox/fetch2/**` (~12 GB of raw download zips) deliberately
  excluded; add only if the raw archives are wanted.
- Log out of the "Easy Access" app (needs Screen Recording permission).
