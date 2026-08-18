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
