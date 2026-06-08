# AGENTS.md

Project briefing for Codex (and any other agentic coding tool that reads AGENTS.md). Read this before doing anything in the repo.

## Project

`amnesty-project` (image tag: `amazon-amnesty:dev`) — applied microeconometrics project on Brazilian Amazon land amnesty. Spatial analysis using R, with `sf`, `geobr`, `tidyverse`, etc. R version 4.5.3 from `rocker/geospatial` base image. Reproducibility is a hard requirement: results must be regenerable bit-identically by anyone with the archived Docker image, today and 30 years from now.

## Repository structure

```text
amnesty-project/
  Dockerfile
  .dockerignore
  Makefile               host-side aliases (Docker plumbing)
  analysis.mk            (if present) pipeline rules, run inside container
  renv.lock              authoritative R package versions
  .Rprofile              activates renv ON HOST ONLY (suppressed in container)
  r-src/                 R scripts live here
    map_vnp_prices.R
    vnp_geocoding.R
    get_land_price_moments.R
    ...
  data/                  raw and processed data
  output/                figures, tables, intermediate outputs
```

## The non-negotiable rule

**All R code runs inside the Docker container. Never run R scripts directly on the host.** The container has the pinned GDAL/GEOS/PROJ/sf stack that the analysis depends on. Host R has different versions and will produce subtly different spatial results.

The single exception: package management. To add a new R package, run `install.packages()` and `renv::snapshot()` on the host (see "Adding a new R package" below). Then rebuild the image.

## How to run things

The Makefile defines the entry points. Always go through it.

```text
make docker-build  Build/rebuild the image. Run after Dockerfile or renv.lock changes.
make docker-boot   Drop into a bash shell inside the container. Daily entry point (human use only).
make docker-run    Non-TTY entry point for agent drivers (Codex, Claude Code, etc.). Use CMD="..." to pass a command.
make all           Run the full analysis pipeline (uses analysis.mk if present).
```

**For humans** — to run a specific script interactively:

```bash
make docker-boot
# now inside the container at /amnesty-project:
Rscript r-src/some_script.R
exit
```

**For agent drivers (Codex, Claude Code, etc.)** — `make docker-boot` requires a real TTY and will fail in a non-interactive executor with `cannot attach stdin to a TTY-enabled container`. Always use `docker-run` instead:

```bash
make docker-run CMD="Rscript r-src/some_script.R"
```

Each invocation is a fresh ephemeral container that runs the one command and exits. Outputs land on the host filesystem via the bind mount, exactly as with `docker-boot`. Do not use `make docker-boot` or bare `docker run -it` — they will error with "stdin is not a terminal". Do not invent shortcuts that bypass the container.

## When you need to rebuild the image

You must run `make docker-build` after any of these changes:

```text
Dockerfile edited
renv.lock changed (new package added, version bumped)
.dockerignore changed in a way that affects what gets COPYed in
A package needs to be installed in the image rather than ad hoc
```

You do NOT need to rebuild after:

```text
Editing R scripts
Editing data files
Editing Makefile or analysis.mk targets
```

Those are visible to the container live via the bind mount.

## Adding a new R package (the dance)

If a script needs a package that isn't in `renv.lock`, you'll see:

```
Error in library(somepackage) : there is no package called 'somepackage'
```

The fix has three steps, in order:

1. **On the host**, update `renv.lock`:

   ```bash
   Rscript -e "install.packages('somepackage'); renv::snapshot()"
   ```

   This installs to the host's renv-managed library and records the new package + dependencies in `renv.lock`.

2. **Rebuild the image** so `renv::restore()` picks up the new entry during build:

   ```bash
   make docker-build
   ```

3. **Verify inside the container**:

   ```bash
   make docker-boot
   Rscript -e "library(somepackage); cat('ok\n')"
   ```

Never `install.packages()` *inside* a running container. It writes to the writable layer, vanishes on `exit`, and is not recorded in `renv.lock` — i.e., it breaks reproducibility.

## Important environment quirks

### `.Rprofile` and renv inside the container

The repo has an `.Rprofile` that sources `renv/activate.R`. This is correct on the host (it's how renv works) but would break things in the container, because the container already has packages installed at the system R library. The Dockerfile suppresses it via:

```dockerfile
ENV RENV_CONFIG_AUTOLOAD_ENABLED=FALSE
ENV R_PROFILE_USER=/dev/null
```

So inside the container, R behaves as if there's no `.Rprofile`. This is intentional. Do not "fix" it. Do not edit `.Rprofile` to be conditional on environment.

If a script ever fails inside the container with "package not found" for something that *is* in `renv.lock`, the suspicion is that `R_PROFILE_USER` got unset somehow — check `echo $R_PROFILE_USER` first, before assuming a real package issue.

### Bind mount semantics

The host project folder is bind-mounted at `/amnesty-project` in the container. This means:

```text
Edits in VS Code on host           -> visible immediately in container
Outputs written by container       -> appear immediately on host
Files NOT YET SAVED on host        -> NOT visible in container
Files saved in unexpected location -> NOT visible at expected path
```

If a script can't find a file: first check whether it's saved on the host, and whether it's at the path the script expects. Use `ls` inside the container to confirm what the bind mount actually shows.

### Script paths

Scripts live in `r-src/`, not the project root. When invoking from inside the container at `/amnesty-project`, use:

```bash
Rscript r-src/script_name.R
```

not `Rscript script_name.R`.

### Working directory inside R

Scripts should use `here::here()` or relative paths from the project root. The container's working directory is `/amnesty-project` (set by `WORKDIR` and `-w`), so `here::here("data", "raw", "file.csv")` works. Don't hardcode `/Users/...` paths or assume any host-specific location.

## Reproducibility safeguards already in place (don't break them)

The Dockerfile pins:

```text
Base image       rocker/geospatial:4.5.3 by sha256 digest
R packages       via renv::restore() against renv.lock
CRAN snapshot    posit packagemanager dated 2026-05-03
PROJ network     OFF (no runtime grid downloads)
Threading        OMP/OPENBLAS/MKL/VECLIB all = 1 (deterministic FP)
sf engine        s2 explicitly enabled
```

If you change any of these, you change the reproducibility contract. Don't do it casually. If a change is genuinely needed (e.g., bumping the CRAN snapshot date for a security fix), call it out explicitly and update the README.

## When something doesn't work

In rough order of likelihood:

```text
1. Did you save the file on the host?
2. Are you running from inside the container (make docker-boot first)?
3. Are you using the right path (r-src/script.R, not script.R)?
4. Is the package in renv.lock? (If no -> the dance above.)
5. Did you rebuild after editing renv.lock or the Dockerfile?
6. Is the bind mount working? (ls in container should match ls on host.)
7. Cache issue with docker build? Try: docker build --no-cache -t amazon-amnesty:dev .
```

If genuinely stuck, exit the container, run `docker rmi amazon-amnesty:dev`, then `make docker-build` to rebuild from scratch. Containers are cheap. The image takes 10-20 minutes to fully rebuild from a cold cache.

## What to do when asked to run code

Default flow when the user asks to run a script or pipeline:

```text
1. Check that the script exists at the path you'll use.
2. Check whether any packages it needs are missing from renv.lock.
   (grep for library() and require() calls; cross-check against renv.lock.)
3. If packages are missing, do the package-adding dance first.
4. Run via: make docker-run CMD="Rscript r-src/whatever.R"
5. Report any errors verbatim with the failing command.
```

Don't silently install packages, don't bypass the container, don't switch the workflow to host-side R "just to test something."

## What NOT to do

```text
Never edit data/raw/ — treat it as immutable input.
Never run R scripts directly on host (except the install/snapshot dance).
Never install.packages() inside a running container.
Never delete .Rprofile, renv/, or modify renv/activate.R.
Never commit renv/library/ (it's host-specific binaries; should be gitignored).
Never commit large output files unless explicitly asked.
Never modify the Dockerfile's pinned versions without explicit approval.
```

## TL;DR for a typical session

**Human:**
```bash
make docker-boot
# inside:
Rscript r-src/whatever.R
exit
```

**Agent driver (no TTY available):**
```bash
make docker-run CMD="Rscript r-src/whatever.R"
```

If it errors with a missing package: do the host-side install + snapshot dance, `make docker-build`, then re-run.
