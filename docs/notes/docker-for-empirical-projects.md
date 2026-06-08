# Docker for Empirical Projects

This document has two parts.

**Part I — Concepts and motivation.** Why Docker, what it is, how it differs from `renv`/`venv`/`uv`, what `COPY` and bind mounts each do, how Git and Docker relate, and how the archival story fits together.

**Part II — Setup and daily workflow reference.** A concrete checklist: what to install, what to put in the project, how a normal work session looks, what to do at the end of the project.

Read Part I once. Refer to Part II whenever you sit down to work.

---

# Part I — Concepts and motivation

## Why Docker matters for empirical work

Suppose you are running a spatial Difference-in-Differences project.

Your project might look like this:

```text
spatial-did/
  scripts/
    01_clean_spatial_data.R
    02_construct_matches.R
    03_build_panel.R
    04_estimate_did.R
    05_make_tables_figures.R
  data_raw/
    census_tracts_2022.gpkg
    treatment_locations.csv
  data_processed/
  output/
  renv.lock
  Makefile
  README.md
```

The empirical question might be cleanly econometric:

```text
Did treatment in some locations affect outcomes in nearby census tracts?
```

But the computational pipeline depends on many fragile spatial details:

```text
coordinate reference systems
spatial joins
nearest-neighbor matching
buffers
distance calculations
geometry validity fixes
intersection operations
raster/vector transformations
```

And those operations depend on software outside plain R or Python packages.

In R you may use:

```r
library(sf)
library(fixest)
library(did)
library(data.table)
```

But `sf` is not just an R package floating in isolation. It talks to lower-level geospatial libraries:

```text
GDAL
GEOS
PROJ
udunits
```

This is where reproducibility becomes harder.

An R lockfile (`renv.lock`) can say:

```text
Use sf version 1.0-x.
Use fixest version 0.x.
Use did version 2.x.
```

But it does not say:

```text
Use this exact Linux system.
Use this exact GDAL binary.
Use this exact GEOS binary.
Use this exact PROJ data setup.
Use this exact system-level library stack.
```

So six months later, on a coauthor's machine, or in a journal replication package, your code might fail because the machine underneath is different.

Sometimes the difference is merely annoying — a package installation fails. Sometimes it affects the actual spatial pipeline:

```text
A geometry validity check behaves differently.
A coordinate transformation gives slightly different output.
A spatial join throws a different warning.
A matching step fails because a geometry operation changed behavior.
```

Docker helps because it lets you define and preserve the computational environment around the project. The goal is not "can my laptop run this today?" — it is "could someone else run this on a clean machine and get the same results?"

A good mental model:

```text
renv / venv / uv = reproduce language-level packages
Docker          = reproduce the computational environment where those packages live
```

For many applied econometrics projects, `renv` or `uv` is enough. Docker becomes more attractive whenever the lower-level computational stack matters. Other situations where Docker helps:

```text
Projects using Homebrew-installed command-line tools
  Example: wget, gdal, imagemagick, pandoc, ffmpeg, jq, poppler.

Optimization projects using external solvers
  Example: Gurobi, CPLEX, Ipopt, GLPK, CBC.

Structural estimation or simulation projects
  Example: compiled C++/Fortran code, OpenMP settings, BLAS/LAPACK differences.

Machine-learning or prediction projects
  Example: CUDA versions, PyTorch/TensorFlow versions, system drivers.

Database-backed empirical projects
  Example: PostgreSQL/PostGIS, DuckDB versions, command-line database clients.

Text-as-data or document-processing projects
  Example: OCR tools, PDF utilities, LaTeX, pandoc, tokenizer dependencies.

Large replication packages
  Example: projects that need to run on a journal server, coauthor machine, or cloud VM.
```

The common feature is that your results or pipeline depend on software outside the narrow language-level package environment.

---

## The core idea: Dockerfile, image, container

The simplest way to understand Docker is as a tuple:

```text
Dockerfile = recipe
Image      = computing environment built from the recipe
Container  = running process using that environment
```

These are three different objects. Confusing them makes Docker much harder to understand.

### What is a Dockerfile?

A Dockerfile is a text file in your project folder, usually called `Dockerfile`. It is committed to git and lives alongside your code.

Example:

```dockerfile
FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    make \
    pandoc \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY renv.lock /app/renv.lock

RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
RUN R -e "renv::restore()"

CMD ["make", "all"]
```

This file says:

```text
Start from an image that already has a particular R version.
Install system libraries, including GDAL, GEOS, and PROJ.
Set /app as the working directory.
Copy the R package lockfile.
Restore the R packages.
When the container runs, run make all.
```

The Dockerfile is not the environment itself — it is the recipe for building the environment. A recipe for bread is not bread.

### What is an image?

An image is the built result of the Dockerfile — the cooked result.

Physically, an image is a collection of files and metadata: *a full Linux filesystem*, stored by Docker on your machine.

It does not sit inside your project directory the way `venv/` does. Docker stores it in Docker's own image storage, so it is not tracked in git. On Linux, Docker data lives somewhere like `/var/lib/docker/`. On Mac, Docker Desktop usually stores images inside a hidden Linux virtual-machine disk.

After building the example Dockerfile above, the image contains:

```text
Linux filesystem files
R installation
GDAL binaries
GEOS libraries
PROJ libraries
R packages restored by renv
metadata saying the default command is `make all`
```

A simplified picture:

```text
Image: spatial-did:dev

Layer 1: base R Linux environment
  /usr/bin/R
  /bin/bash
  /usr/lib/...

Layer 2: geospatial system libraries
  /usr/bin/gdalinfo
  /usr/lib/libgdal.so
  /usr/lib/libgeos.so
  /usr/lib/libproj.so

Layer 3: R package environment
  /usr/local/lib/R/site-library/sf/
  /usr/local/lib/R/site-library/fixest/
  /usr/local/lib/R/site-library/did/

Layer 4: metadata
  default command = make all
```

If your laptop has GDAL 3.9 installed globally, but your Docker image has GDAL 3.6 inside it, then a process running inside a container from that image uses GDAL 3.6 — because the process sees the image's filesystem, not your ordinary host filesystem.

From your host machine's perspective, `/usr/local/bin/gdalinfo` might refer to your Mac/Homebrew GDAL. From inside the container, `/usr/bin/gdalinfo` refers to the GDAL file inside the image. The same path-like idea can point to different physical files depending on the process's filesystem view.

Analogy:

```text
Apartment A has a kitchen.
Apartment B has a kitchen.

Inside Apartment A, "go to the kitchen" means Apartment A's kitchen.
Inside Apartment B, "go to the kitchen" means Apartment B's kitchen.
```

Docker gives the process its own apartment.

### How images are stored: layers

When Docker builds an image, it stores it as a series of **diffs** — each instruction in your Dockerfile that modifies the filesystem produces one diff, called a layer.

Concretely, for a Dockerfile starting `FROM gdal:ubuntu-small-3.8.4` and then installing some Python packages:

- **Layer 1** (from `FROM`): the complete Ubuntu + GDAL filesystem — every file in `/usr`, `/lib`, `/bin`, etc. This is the base, so it is not a diff against anything; it's the whole thing.
- **Layer 2** (from `RUN apt-get install python3-pip ...`): a diff that records only what changed — new files added to `/usr/lib/python3/`, modifications to some files in `/var/cache/apt/`.
- **Layer 3** (from `COPY requirements.txt .`): a diff containing just that one file appearing at `/project/requirements.txt`.
- **Layer 4** (from `RUN pip install ...`): a diff containing all the new files that pip dropped into `/usr/local/lib/python3.x/site-packages/` — geopandas, shapely, etc.

Each layer is a content-addressed blob stored in Docker's internal storage (e.g. `/var/lib/docker/overlay2/` on Linux). Layers are read-only and never modified after creation.

When you ask "what does the filesystem of this image look like?", Docker answers by stacking all the layers in order using a **union filesystem** (specifically, OverlayFS on modern Linux). OverlayFS presents multiple directories as if they were one merged directory. You see a single coherent filesystem, but underneath it's just those four read-only diffs sitting next to each other on disk.

This layered structure is also why rebuilds are fast. If you change only `requirements.txt`, Docker reuses Layers 1 and 2 from cache and rebuilds only from Layer 3 onward.

### What is a container?

An image is inert — a frozen filesystem on disk. A **container** is what you get when Docker takes that frozen filesystem and actually boots it as a running mini-Linux.

When you run:

```bash
docker run spatial-did:dev
```

Docker starts an ordinary process on your machine, but gives that process a special isolated view of the world.

A process is just a running program. When you run `Rscript scripts/04_estimate_did.R`, the operating system starts a process. That process has a process ID, memory, open files, environment variables, a current working directory, access to CPU, and access to files it is allowed to read. Chrome is a process. RStudio is a process.

Docker also starts processes. The difference is that Docker starts them with isolation.

A containerized process gets:

```text
its own filesystem view
its own process namespace
possibly its own network namespace
possibly resource limits
selected host folders mounted into it
```

The key formula:

```text
container = running process + image filesystem + isolation settings
```

So when you run from your project root:

```bash
docker run --rm \
  -v "$PWD":/app \
  -w /app \
  spatial-did:dev \
  make all
```

Docker does something like:

```text
Start a process from the image spatial-did:dev.
Show the image's Linux filesystem to that process.
Mount my current project folder into the container at /app.
Use /app as the working directory.
Run make all inside the container.
Delete the stopped container after it exits.
```

Inside the container, `/app` is your project folder. But `/usr/bin/R`, `/usr/bin/gdalinfo`, and `/usr/lib/libgdal.so` come from the Docker image. Your code and data live in your normal folder, while R/GDAL/GEOS/PROJ come from the image. That is the practical magic.

### What actually happens when you `docker run`

The image layers are read-only — nothing can ever write to them. But a running container clearly needs to be able to write files (temp files, logs, whatever). So Docker adds one more layer on top of the stack: a **writable layer**, also called the container layer. It starts completely empty.

OverlayFS now presents the union of: all the read-only image layers + this one writable layer on top. From inside the container, it looks like one normal filesystem. But the mechanics of what happens on a write are specific:

- **Reading a file:** OverlayFS looks from the top down. It checks the writable layer first, then layer 4, then layer 3, then layer 2, then layer 1. The first hit wins. Since the writable layer starts empty, all initial reads fall through to the image layers.
- **Writing a new file:** The new file is created in the writable layer only. The image layers are untouched.
- **Modifying an existing file** (say, a file that lives in Layer 1): OverlayFS performs a **copy-on-write**. It copies the original file from Layer 1 up into the writable layer, then modifies that copy. Now the writable layer has a modified version, the image's Layer 1 still has the original, and OverlayFS shows you the writable layer's version because it's on top.
- **Deleting a file:** OverlayFS writes a special "whiteout" marker into the writable layer. The original file still exists in the image layer below, but OverlayFS hides it from view.

When the container exits and `--rm` removes it, Docker deletes exactly one thing: that writable layer. The image layers are untouched — they live on in Docker's storage exactly as before, ready to be the base for the next container.

This is why `pip install seaborn` inside a running container vanishes: the install wrote files into the writable layer, that layer got deleted on exit, and the image layers never knew it happened.

Which raises the obvious problem: how do you get your code in and your results out?

---

## The bind mount: punching a hole between universes

When you start a container with `-v "$PWD":/app`, Docker takes the folder you're currently in on the host and makes it appear at the path `/app` inside the container.

This isn't a copy. It's a **bind mount** — a live two-way window. It's the same bytes on disk, viewed from two places:

- Your Mac sees them at `/Users/you/spatial-did/scripts/04_estimate_did.R`
- The container sees the exact same bytes at `/app/scripts/04_estimate_did.R`

When you save the file in VS Code on your Mac, the container sees the new contents instantly. When your R script inside the container writes `output/figures/event_study.png`, the file appears at `/Users/you/spatial-did/output/figures/event_study.png` on your Mac instantly. There's no syncing step — there is literally one copy of the file on disk, visible from both worlds.

The workflow that follows: **edit on your host with your normal tools (VS Code, RStudio, whatever), and run inside the container.** Code and outputs live on your host filesystem the entire time.

### The full lifecycle of one work session

Concretely, here's what happens between sitting down to work and going home:

**You sit down.** Your project folder on your Mac contains the code you wrote yesterday and the outputs from yesterday's runs. The Docker image `spatial-did:dev` is sitting in Docker's storage, untouched since you built it. No containers are running.

**You start a container.** Docker creates a fresh container from the image, bind-mounts your project folder at `/app`, and gives you a bash prompt inside it (basically SSHs you into the container). You're now inside the pocket universe. The container's filesystem is the image's frozen Ubuntu, plus your live project folder bind-mounted at `/app`.

**You work.** The two things you do when working — *editing* and *running* — happen in separate places.

- **Editing** happens on your Mac, in VS Code, like always. You open `scripts/04_estimate_did.R`, change something, hit save. The bind mount makes it so your Mac and the container share the project folder, so updates are visible to both (not synced — it's the *same* folder).
- **Running** happens inside the container, in the terminal where you're "SSH'd in". You type `Rscript scripts/04_estimate_did.R` there. The script reads `data_raw/census_tracts_2022.gpkg` (your real file on your Mac, visible through the mount), uses the container's GDAL 3.8.4 and GEOS, and writes `output/tables/table1.tex` (which appears immediately on your Mac).

You do this for three hours. Maybe you launch Jupyter or an R session. Maybe you run twelve scripts. All output files land in your real project folder on your Mac.

**You're done. You type `exit`.** The bash process ends, which ends the container. Because of the `--rm` flag, Docker also deletes the container's writable layer.

What gets deleted: the temporary writable layer of the container — anything you wrote *outside* `/app` inside the container. If you ran `install.packages("seaborn-equivalent")` ad hoc inside the container, that installation lived in the writable layer and is now gone. If you wrote a file to `/tmp/scratch.txt` inside the container, gone.

What survives: **everything in your project folder**. Your code edits, your output CSVs, your figures, your logs — all of it is on your Mac's real filesystem. The mount was a window, not a container; closing the container closes the window but doesn't take the files with it. The image itself is also untouched.

**You shut down your laptop.** Nothing special. Tomorrow you start another container and pick up where you left off.

### The one rule that follows from all this

Because the writable layer gets thrown away but your project folder persists:

```text
Anything you want to keep -> write into /app (your bind-mounted project).
Anything in the reproducible environment -> put in the Dockerfile.
```

If you need a new R package: add it to your project's `renv.lock` (via `renv::snapshot()` on the host), then rebuild the image. Don't `install.packages()` ad hoc inside a running container — it'll vanish on `exit`, and worse, it won't be reproducible because it's not recorded anywhere.

This rule is what makes the whole thing reproducible. The image is defined entirely by the Dockerfile (in git). The project state is entirely in your project folder (in git, except for large data). Nothing important lives only inside a running container.

---

## Two ways to use a container: session vs. ephemeral

Everything above described one specific way of using a container: boot once at the start of a work session, work inside it for hours, exit at the end. Call this the **session model** — the container is your workshop for the afternoon.

There is another way, and it is actually closer to Docker's design philosophy. Call it the **ephemeral model**: each command spins up its own fresh container, the command runs, the container is destroyed. No login. No session. Each invocation is a brand-new universe that lives only as long as one command takes to run.

```text
Session model
  one container per work session
  many commands inside one container
  pay container startup once

Ephemeral model
  one container per command
  many containers per work session
  pay container startup every time
```

### Why both exist

The session model is intuitive because it looks like SSH-ing into a remote machine — log in, do stuff, log out. But Docker is not a VM. The natural Docker unit is **one command, one container, throw it away**. A container is closer to "a run of a program" than "a machine you log into." The session model is a comfort layer on top of Docker, mostly for humans.

The trade-off is ergonomics versus hermeticity.

The session model gives you ergonomics. You don't pay container startup cost on every command (a few seconds matters when you run twenty things in an afternoon). You can `cd` around, set environment variables, run a quick `R -e "head(df)"` between scripts. It is just nicer to "live inside" the container while iterating.

The ephemeral model gives you hermeticity. Every run is independent. Nothing carries over from a previous invocation — no stray `Sys.setenv()`, no leftover `/tmp` file, no environment variable you forgot you set. Each container is identical, defined entirely by the image. This is exactly the property your final replication package needs.

For day-to-day iteration, the session model wins on ergonomics. For automated pipelines, CI runs, or anything you want to be exactly reproducible, the ephemeral model wins on hermeticity.

### You're already using both

Look at the Makefile we sketched earlier:

```makefile
docker-boot:
	docker run --rm -it -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) bash

all:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk all
```

The two `docker run` invocations differ in exactly the way you would now expect:

- `docker-boot` uses `-it` (interactive + TTY) and runs `bash`. That is the session model: open a shell, work inside.
- `all` has no `-it` and passes `make -f analysis.mk all` directly as the command. That is the ephemeral model: spin up, run the pipeline, destroy.

`make all` is also how a replicator in 2046 will run your project. They will not "log in and type `make all`" — they will just `docker run ... make all` and the universe boots from scratch. The reproducibility story relies on the ephemeral model.

So you have been using both all along. Daily iteration is session-style; the canonical pipeline run is ephemeral-style.

### A third pattern: ephemeral, one command at a time

Sometimes you want the ephemeral model for a single command, not the whole pipeline. The most common reason is that something else is driving Docker for you — an automation script, a CI runner, an AI coding assistant — and that driver does not have a real TTY, so it cannot use the session model. But it also does not want to run the entire pipeline; it just wants to run one script, see the output, then run another.

Concretely, if you try to `docker run -it ... bash` from a non-TTY caller, Docker errors out with:

```text
cannot attach stdin to a TTY-enabled container because stdin is not a terminal
```

The fix is to keep the container ephemeral (drop `-it`, drop interactive `bash`) and pass each command in via `bash -c`. Add a Makefile target like:

```makefile
docker-run:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) bash -c "$(CMD)"
```

(The name is arbitrary — `docker-run`, `docker-exec`, `docker-cmd`, whatever reads well. The mechanics are what matters: no `-it`, command passed via `bash -c "$(CMD)"`.)

Then any caller can do:

```bash
make docker-run CMD="Rscript scripts/04_estimate_did.R"
make docker-run CMD="ls output/"
make docker-run CMD="R -e 'sf::sf_extSoftVersion()'"
```

Each invocation is a fresh container that runs one command and exits. The driver pays the startup cost per call, but in exchange every run is independent — no session state can drift between calls.

### The three targets, side by side

```text
make docker-boot   human, interactive       session model        -it bash
make docker-run    automation / AI driver   ephemeral, one cmd   bash -c "$(CMD)"
make all           full pipeline            ephemeral, all cmds  make -f analysis.mk all
```

You will mostly use the first when you are at the keyboard. The second exists for drivers without a TTY. The third is what `make all` always was, and what a replicator uses.

### The intuition to take away

**Docker is more naturally about ephemeral runs than long-lived sessions.** The session model is a comfort layer for humans iterating. The pipeline run, the CI run, the replicator run — all ephemeral.

If you instinctively reach for "boot the container, then do stuff inside," that is fine for your own iteration. But the more principled framing is "every command is its own container," and that is the framing your final replication package will end up using anyway.

---

## `COPY` versus the bind mount

Once you understand the bind mount, a natural question follows: if the bind mount is what gives the container access to my project, why does the Dockerfile usually contain a line like:

```dockerfile
COPY . /app
```

It looks redundant. Both `COPY` and the bind mount put your project files inside the container. Why have both?

The answer is that they happen at different times and serve different audiences.

### What `COPY` does

`COPY` runs at `docker build` time. It takes a snapshot of your project folder *as it exists on the day you build the image* and freezes those files into the image itself. From that moment on, the copied files are part of the image, exactly like the GDAL binary or the R packages installed via `renv::restore()`.

If you build the image on May 3, 2026, the image contains your code as of May 3, 2026 — permanently, even if you later change laptops, delete the project folder, or wipe the disk.

### What the bind mount does (in this comparison)

The bind mount runs at `docker run` time. It exposes your *current, live* project folder on the host inside the container at the same path the `COPY` wrote to. The bind mount wins: while the container runs, you see live host files, not the frozen copy. The frozen copy is hidden underneath, like a rug thrown over a floor.

### Why have both

Because they serve different audiences.

**You during development.** You want the bind mount. You're editing code constantly and need edits to be visible inside the container instantly. A frozen copy from last Tuesday is useless. So you always run with `-v "$PWD":/app`, the bind mount overlays the frozen copy, and you work with live files. The `COPY` is invisible during this phase.

**A replicator in 2046.** They don't have your project folder. They have one thing: the image, loaded from your archived `.tar`. They run:

```bash
docker run --rm leo/spatial-did:2026-final make all
```

No `-v` flag, because they have nothing to mount. The container boots, `make all` looks for `scripts/04_estimate_did.R` inside `/app`, and finds it — because `COPY . /app` froze it into the image years ago. The pipeline runs, produces results.

If you had skipped the `COPY`, the container would boot into an empty `/app` and the replicator would get "file not found." They'd then need to also obtain your code from somewhere, mount it manually, and hope they got the right version paired with the right image.

### The mental model

```text
COPY       = safety net for the future. Guarantees the image alone,
             with no external files, contains everything to run.

Bind mount = convenience for the present. Lets you bypass the frozen
             copy while you're actively editing.
```

You don't notice `COPY` exists during daily work because the bind mount always overlays it. It's invisible until the day someone tries to run the image without a mount — and on that day, it's the difference between "the analysis runs" and "file not found."

This is also why a `.dockerignore` file matters. Since `COPY .` would otherwise grab everything in your repo — `.git/`, large raw datasets, intermediate outputs, `.Rhistory` files, `.DS_Store`, and so on — you want to exclude what shouldn't be frozen into the image. A `.dockerignore` works exactly like a `.gitignore` and keeps the image lean.

---

## Git versus Docker: same code, different artifacts

A related confusion: if `COPY .` puts your project into the image, is Docker doing GitHub's job? Once a replicator has the image, do they ever need GitHub?

The answer requires separating two different roles for the code.

### The roles do not overlap

```text
Git / GitHub
  Version control during development.
  Branches, commits, history, collaboration, pull requests.
  Tracks how your code evolves.

Docker
  The computing environment.
  OS, system libraries, language versions, system dependencies.
  Tracks how your machine looks.
```

Docker is not replacing Git.

What confuses the issue is that for archival purposes, the convention in computational research is to ship a *single frozen package* containing code + environment + (where possible) data, all matched to each other. A replicator should not have to "go to GitHub and grab whatever is on main, then build this Docker image, and hope the versions line up."

The `COPY .` line is what bundles a *frozen copy of the code at build time* into the image, so the archived `.tar` is self-contained. It is not replacing Git. Your code still lives in Git, gets committed, gets pushed, has history. The `COPY` only takes a photograph of the code at one moment and tucks it into the image for archival.

### Three audiences, three lifecycles

The reason both Git and the archived image exist is that different people need different things at different times.

```text
You, while writing the paper (2026-2028)
  Use Git every day. Branches, commits, recovery from mistakes,
  collaboration with coauthors. Docker is irrelevant to this phase
  except as the engine your code runs in.

Other researchers reading your paper soon after publication (2028-2035)
  Use GitHub. They might fork your code, reuse a function, build on
  your method, or check what changed between v1 and v2. The image
  is overkill if all they want is to read your spatial join function.

The replicator in 2046
  Uses the archived image. They want to push a button and reproduce
  Table 3. They go to Zenodo, get the image, run it. GitHub is
  irrelevant and may well be defunct.
```

Abandoning Git would mean giving up version control during the actual work, which is unthinkable. Skipping the archived image would mean asking a future replicator to correctly pair an archived image with the right Git commit, which is one more way for things to go wrong over decades.

### The cleaner analogy

```text
Git                   = manuscript with track changes
Archived Docker image = the printed book
```

Track changes is invaluable while you're writing. You see the history, you can revert, multiple authors can edit. Once the book is published, nobody reads the track-changes version. They read the printed book. But you don't conclude that track changes was useless. It did its job during the writing phase. The printed book does its job at the publication phase.

The replicator in 2046 reads the book. You and your coauthors used the manuscript to write the book.

```text
Git    = evolution
Image  = one frozen moment that needs to stay runnable forever
```

They are complementary, not redundant.

---

## Docker compared with renv, venv, and uv

A virtual environment is mostly a project-specific package directory plus path configuration.

For Python, a virtual environment looks like:

```text
project/
  .venv/
    bin/python
    lib/python3.12/site-packages/
```

When you activate it with `source .venv/bin/activate`, your shell changes variables like `PATH`, so when you type `python` it uses `project/.venv/bin/python` rather than the system Python.

In R, `renv` creates a project-specific R package library and a lockfile. Useful, but it mostly controls language-level packages.

Docker controls a lower layer too. Think of the stack:

```text
Your empirical code
R/Python packages
System libraries: GDAL, GEOS, PROJ
Operating system files
Kernel / hardware
```

`renv`, `venv`, and `uv` control:

```text
R/Python packages
```

Docker controls:

```text
R/Python packages
system libraries
Linux filesystem layout
OS-level tools
default commands
environment variables
```

Docker does not include a whole separate kernel like a full virtual machine. Containers share the host kernel, or on Mac, they run inside Docker Desktop's lightweight Linux VM.

A helpful analogy:

```text
renv / venv = bring your own toolbox
Docker      = bring the whole workshop
VM          = bring the whole building
```

For pure applied econometrics, `renv` may be enough. For spatial empirical work, Docker becomes more useful because the workshop matters: GDAL, GEOS, PROJ, system libraries, command-line tools, and Linux behavior can all matter. The same logic applies whenever the surrounding environment matters more than just R or Python packages.

---

## Tags and digests

Docker images can be referenced by tags or digests.

A tag looks like:

```text
leo/spatial-did:2026-04-30
```

A digest looks like:

```text
leo/spatial-did@sha256:abc123...
```

The tag is a human-readable label. The digest is a content fingerprint.

A tag can move. Today:

```text
leo/spatial-did:dev -> image A
```

Tomorrow, after you rebuild and push:

```text
leo/spatial-did:dev -> image B
```

The tag stayed the same, but the image changed.

A digest is different. It identifies exact image content, assuming the registry still has that content. A digest is not reversible — given `sha256:abc123...`, Docker cannot reconstruct the image from the digest. Instead, Docker asks a registry: "do you have the image whose fingerprint is `sha256:abc123...`?" If yes, Docker downloads the bytes and verifies that `SHA(downloaded bytes) == sha256:abc123...`. If the registry no longer has the image, the digest alone is not enough.

Analogy:

```text
Digest          = exact library catalog number
Docker Hub      = the library
Saved image tar = your own copy of the book
```

The catalog number tells you exactly what you want. It does not recreate the book if every copy disappears. So for a final replication package, record the digest, but do not rely only on it.

---

## Docker Hub versus the `.tar` archive

Docker Hub is like GitHub for Docker images.

```text
GitHub repo            Docker Hub repository
  Dockerfile             built image layers
  analysis scripts       image tags
  renv.lock              metadata
  README                 image digests
```

An image name like `leo/spatial-did:dev` decomposes as:

```text
leo            Docker Hub username or organization
spatial-did    Docker Hub image repository
dev            tag
```

Using Docker Hub from early in the project makes your environment portable while you work — switching laptops, sharing with a coauthor, running on a server, or wiring up CI all become a single `docker pull`.

But Docker Hub is not the final long-term archive. Companies disappear, free tiers change, repositories get deleted. For a 30-year horizon, you also need a preservation copy.

The preservation copy is a `.tar` file produced by `docker save`. The two solve different problems:

```text
Docker Hub
  Convenient remote distribution while the project is active.
  Useful for coauthors, servers, CI, and easy pulls.

Docker image .tar
  Preservation copy.
  Useful for long-term replication even if registries change.
```

The `.tar` contains the actual image layers as files:

```text
/usr/bin/R
/usr/bin/gdalinfo
/usr/lib/libgdal.so
/usr/lib/libgeos.so
/usr/lib/libproj.so
/usr/local/lib/R/site-library/sf/
```

This is what matters for 2046. If GDAL 3.6 is no longer distributed in normal package repositories, the saved image still contains the old GDAL files. A future researcher loads the `.tar` into Docker, runs the container, and the analysis works. They do not need old GDAL installed on their machine, do not need 2026 CRAN, do not need old Ubuntu package repositories. They need:

```text
Docker or a compatible container runtime
the saved image tar
the project code
the project data
```

This is why the `.tar` is the archival object.

A reasonable strategy:

```text
During the project: push to Docker Hub for convenience.
At final release:   also save as .tar for preservation.
```

---

## Final mental model

For empirical projects, especially spatial ones:

```text
renv.lock
  Records R package versions.

Dockerfile
  Records how to build the machine.

Docker image
  Is the machine.

Container
  Is the machine running one command.

Docker Hub
  Is remote storage for the machine while you work and share.

Digest
  Is the exact fingerprint of the machine in Docker Hub.

.tar from docker save
  Is your preserved physical copy of the machine.

checksums.txt
  Lets future users verify that the archived files did not change.
```

The shortest version:

```text
Dockerfile = recipe
Image      = built environment
Container  = running environment
Docker Hub = convenient remote image storage
.tar       = long-term preservation copy
```

For empirical projects, Docker is not about making regressions more sophisticated. It is about making the computational environment explicit, portable, and preservable.

---

# Part II — Setup and daily workflow reference

This is the practical section. Read once, then refer back to it when you actually sit down to work.

## One-time: install Docker

Install Docker Desktop from docker.com. Free for academic/personal use. Open it after installation so the Docker daemon is running.

Verify:

```bash
docker --version
docker run hello-world
```

If `hello-world` prints a success message, Docker is working.

## One-time: project setup

In your project root, you need three files: `Dockerfile`, `.dockerignore`, and `renv.lock`. Plus a `Makefile` for convenience.

### Dockerfile

The Dockerfile defines the environment. Example skeleton (adjust the base image, system libraries, and CMD to your project):

```dockerfile
# syntax=docker/dockerfile:1.7

FROM rocker/geospatial:4.5.3@sha256:5d50d6d3bd70f5b48e60bf4ecc5cabb2cb854bf072117f975ba78d5d5165bfa3

LABEL org.opencontainers.image.title="amazon-amnesty" \
      org.opencontainers.image.description="Reproducible spatial environment for the amnesty project replication package"

ARG CRAN_SNAPSHOT=https://packagemanager.posit.co/cran/2026-05-03

ENV TZ=Etc/UTC \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PROJ_NETWORK=OFF \
    GDAL_DATA=/usr/share/gdal \
    PROJ_LIB=/usr/share/proj \
    OMP_NUM_THREADS=1 \
    OPENBLAS_NUM_THREADS=1 \
    MKL_NUM_THREADS=1 \
    VECLIB_MAXIMUM_THREADS=1 \
    RSPM=${CRAN_SNAPSHOT}

WORKDIR /amnesty-project

# Pin CRAN snapshot and explicit sf engine behavior.
RUN printf "options(repos = c(CRAN = '%s'))\noptions(sf_use_s2 = TRUE)\n" "${CRAN_SNAPSHOT}" > /usr/local/lib/R/etc/Rprofile.site

# Restore exact package versions from renv.lock.
COPY renv.lock /amnesty-project/renv.lock
RUN R -q -e "install.packages('renv'); renv::restore()"

# Freeze the project into the image for archival.
# During development, the bind mount overrides this with live edits.
COPY . /amnesty-project

CMD ["bash"]
```

Key choices:

- `FROM ... @sha256:...` pins the base image to an exact cryptographic hash, so even if someone re-pushes a new image with the same `4.5.3` tag in 2030, you get bit-identical bytes.
- `ARG CRAN_SNAPSHOT` pins R packages to a specific date snapshot of CRAN, so `renv::restore()` finds the same versions in 2046.
- `PROJ_NETWORK=OFF` prevents PROJ from silently downloading grid shift files at runtime, which would make results depend on a network resource.
- `OMP_NUM_THREADS=1` etc. force single-threaded BLAS/OpenMP. Multi-threaded numerical libraries can produce tiny floating-point differences depending on thread count and scheduling.
- `options(sf_use_s2 = TRUE)` pins sf's geometry engine choice (planar vs spherical) so the default cannot drift between sf versions.
- `COPY . /amnesty-project` freezes the project into the image for archival. Bind-mounting at runtime overrides this with live files for development.
- `CMD ["bash"]` makes the default a shell, suitable for development. Replicators override with `Rscript ...` or `make all`.

### .dockerignore

Create a `.dockerignore` file (same syntax as `.gitignore`) so that `COPY . /app` doesn't bake your `.git` history, raw data, intermediate outputs, and editor cruft into the image:

```text
.git/
.Rhistory
.RData
.Rproj.user/
output/
data/raw/large_files/
*.log
.DS_Store
```

Adjust to your repo's actual large/derivative paths.

### renv.lock

Generate it once from your current R session. From your project root, in R:

```r
install.packages("renv")
renv::init()
```

`renv::init()` scans your scripts, detects packages, installs them into a project-local library, and writes `renv.lock`. When asked, choose the option to initialize from your current library.

Commit `renv.lock` to git. Add `renv/library/` to `.gitignore` (it's machine-specific and large).

From here on, every time you `install.packages("something")` for the project, follow it with `renv::snapshot()` to update `renv.lock`.

### Makefile

A Makefile hides the verbose Docker commands behind short aliases. Create one in your project root (indentation must be **tabs**, not spaces):

```makefile
IMAGE := amazon-amnesty:dev

docker-build:
	docker build -t $(IMAGE) .

docker-boot:
	-docker run --rm -it -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) bash

docker-run:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) bash -c "$(CMD)"

all:
	docker run --rm -v "$(PWD)":/amnesty-project -w /amnesty-project $(IMAGE) make -f analysis.mk all
```

What each target does:

```text
make docker-build  Rebuild the image. Run when you change renv.lock or the Dockerfile.
make docker-boot   Drop into a bash prompt inside the container. Daily entry point for human work (session model).
make docker-run    Run a single command in an ephemeral container. For drivers without a TTY: CI, automation,
                   AI coding assistants. Pass the command as CMD="...".
make all           Run the full analysis pipeline ephemerally. For clean-machine tests and replicators.
```

The split is the philosophy from Part I made concrete: `docker-boot` is the session model for humans; `docker-run` and `all` are ephemeral, the latter for the whole pipeline and the former for one command at a time.

### First build

From the project root:

```bash
make docker-build
```

The trailing `.` is the build context — it tells Docker which folder to read for `COPY` instructions. Without it Docker errors out.

The first build takes 10–20 minutes because everything is downloaded fresh. Subsequent builds use Docker's layer cache and are seconds, unless `renv.lock` or earlier Dockerfile lines change.

Verify:

```bash
docker images
```

You should see `amazon-amnesty:dev` listed.

Sanity check that GEOS/GDAL/PROJ are visible from R inside the container:

```bash
make shell
# inside the container:
R -e "library(sf); sf::sf_extSoftVersion()"
exit
```

If version numbers print, you're set.

## Daily workflow

The loop is: open shell, work, exit.

### Sit down to work

```bash
cd amnesty-project
make shell
```

You're now at a bash prompt inside the container, in `/amnesty-project`. Your project folder is bind-mounted, so all files are visible and live.

Open VS Code or RStudio on your host as usual for editing. The container is for running.

### Work

Edit on the host, run in the container:

```bash
# inside the container:
Rscript code/analysis/run_vnp_price_map.R
```

Or open an interactive R session:

```bash
# inside the container:
R
```

Outputs land in `output/` on your host filesystem, instantly visible.

### Add a new R package

This is the slightly annoying case. Two steps, in order:

**Step 1.** On the host, in R:

```r
install.packages("newpackage")
renv::snapshot()
```

This updates `renv.lock`.

**Step 2.** Rebuild the image so the package is baked in:

```bash
exit               # leave the running container
make build         # ~30 sec to a few minutes (cached layers reused)
make shell         # back in, with the new package now installed
```

The reason for this dance: anything you `install.packages()` *inside* a running container vanishes on `exit`. Only what's in the Dockerfile + `renv.lock` survives. Annoying for the first week, but exactly what enforces reproducibility.

### End of day

```bash
exit
```

The container is destroyed. Your code, outputs, and `renv.lock` are all on your host disk. Shut down your laptop normally.

### Running scripts from outside the container

If something else is driving Docker for you — a CI runner, a build script, an AI coding assistant — it will not have a real TTY, so `make docker-boot` will fail with `cannot attach stdin to a TTY-enabled container`. Use the ephemeral one-command pattern instead:

```bash
make docker-run CMD="Rscript code/analysis/run_vnp_price_map.R"
```

Each invocation is a fresh container that runs the one command and exits. Outputs land on the host filesystem just as before, via the bind mount. See "Two ways to use a container" in Part I for the philosophy.

### When things go sideways

Containers are cheap. If a session gets weird:

```bash
exit
make shell    # fresh container, no carried-over state
```

If the image gets weird:

```bash
docker rmi amazon-amnesty:dev
make build
```

Rebuild from scratch.

## Periodic: push to Docker Hub

Optional but useful as off-machine backup and for moving between machines.

One-time setup:

```bash
docker login                                                    # enter Docker Hub credentials
docker tag amazon-amnesty:dev yourdockerhub/amazon-amnesty:dev
docker push yourdockerhub/amazon-amnesty:dev
```

Repeat the `tag` + `push` whenever you've made meaningful environment changes (added important packages, changed system libraries).

To pull on another machine:

```bash
docker pull yourdockerhub/amazon-amnesty:dev
docker run --rm -it -v "$PWD":/amnesty-project -w /amnesty-project yourdockerhub/amazon-amnesty:dev bash
```

## Periodic: clean-machine test

A useful discipline is to occasionally run the full pipeline in Docker as a clean-machine check, especially:

```text
after adding packages
after changing spatial operations
before sharing results with a coauthor
before submission
```

```bash
make all
```

If `make all` works in a fresh container, your replication story is in good shape.

## Final release: archival

When the project is finished (paper accepted, replication package due):

### 1. Tag the code in Git

```bash
git tag v1.0-final
git push --tags
```

### 2. Build and push the final image

```bash
docker build -t yourdockerhub/amazon-amnesty:v1.0-final .
docker push yourdockerhub/amazon-amnesty:v1.0-final
```

### 3. Record the digest

```bash
docker inspect --format='{{index .RepoDigests 0}}' yourdockerhub/amazon-amnesty:v1.0-final
```

Save the output (something like `yourdockerhub/amazon-amnesty@sha256:abc123...`) for your README.

### 4. Save environment manifest

For a human-readable record of what's actually inside the image:

```bash
docker run --rm yourdockerhub/amazon-amnesty:v1.0-final gdalinfo --version > environment_manifest.txt
docker run --rm yourdockerhub/amazon-amnesty:v1.0-final R -e "sessionInfo()" >> environment_manifest.txt
```

### 5. Save the archival tarball

```bash
docker save yourdockerhub/amazon-amnesty:v1.0-final | gzip > amazon-amnesty-v1.0-final.tar.gz
```

This is the single self-contained archival artifact. Probably 1–3 GB for a geospatial stack.

### 6. Compute checksums

```bash
shasum -a 256 amazon-amnesty-v1.0-final.tar.gz > checksums.txt
shasum -a 256 data_raw/* >> checksums.txt
```

### 7. Deposit on Zenodo

A complete archive contains:

```text
source-code.zip                                     (or pulled from GitHub release)
raw-data.zip                                        (with checksums)
amazon-amnesty-v1.0-final.tar.gz                    (the image)
environment_manifest.txt                            (what's inside)
checksums.txt                                       (verify integrity)
README.md                                           (replication instructions)
```

Good archive destinations:

```text
Zenodo
OSF
Dataverse
journal replication archive
institutional archive
```

## What to write in the README

Two replication modes for whoever shows up.

### Convenient mode: Docker Hub

```bash
docker pull yourdockerhub/amazon-amnesty@sha256:abc123...
docker run --rm -v "$PWD":/amnesty-project -w /amnesty-project \
  yourdockerhub/amazon-amnesty@sha256:abc123... make all
```

Use the digest, not the tag, because the digest pins exact content.

### Archival mode: saved image tar

```bash
docker load -i amazon-amnesty-v1.0-final.tar.gz
docker run --rm -v "$PWD":/amnesty-project -w /amnesty-project \
  yourdockerhub/amazon-amnesty:v1.0-final make all
```

Then say:

```text
The Docker Hub image is provided for convenience.
The archived .tar image is the authoritative long-term preservation copy.
```

## How someone replicates the project in 2046

Suppose a researcher in 2046 downloads:

```text
amnesty-project.zip
amazon-amnesty-v1.0-final.tar.gz
checksums.txt
```

They unzip the project:

```bash
unzip amnesty-project.zip
cd amnesty-project
```

They verify the image archive:

```bash
shasum -a 256 ../amazon-amnesty-v1.0-final.tar.gz
```

They compare the result to `checksums.txt`.

They load the image:

```bash
docker load -i ../amazon-amnesty-v1.0-final.tar.gz
```

They check the environment:

```bash
docker run --rm yourdockerhub/amazon-amnesty:v1.0-final gdalinfo --version
```

They run the full project:

```bash
docker run --rm \
  -v "$PWD":/amnesty-project \
  -w /amnesty-project \
  yourdockerhub/amazon-amnesty:v1.0-final \
  make all
```

Inside the container:

```text
/amnesty-project      the mounted project folder from the 2046 machine
/usr/bin/R            R from your archived Docker image
/usr/bin/gdalinfo     GDAL from your archived Docker image
/usr/lib/libgdal.so   GDAL library from your archived Docker image
```

The 2046 machine does not need to have your GDAL installed globally. The container uses the GDAL inside the image. That is the point.

## Cheat sheet

```text
DAILY (human, session model)
  cd project && make shell      # start work
  exit                          # end work

AUTOMATION / AI DRIVER (no TTY, ephemeral one-shot)
  make docker-run CMD="Rscript code/analysis/some_script.R"

ADDED A PACKAGE
  R> install.packages("...")
  R> renv::snapshot()            # on host
  exit && make build && make shell

PUSH TO DOCKER HUB
  docker tag amazon-amnesty:dev you/amazon-amnesty:dev
  docker push you/amazon-amnesty:dev

PULL ON ANOTHER MACHINE
  docker pull you/amazon-amnesty:dev
  docker run --rm -it -v "$PWD":/amnesty-project -w /amnesty-project you/amazon-amnesty:dev bash

CLEAN-MACHINE TEST
  make all

FINAL ARCHIVAL
  git tag v1.0-final && git push --tags
  docker build -t you/amazon-amnesty:v1.0-final .
  docker push you/amazon-amnesty:v1.0-final
  docker save you/amazon-amnesty:v1.0-final | gzip > amazon-amnesty-v1.0-final.tar.gz
  shasum -a 256 amazon-amnesty-v1.0-final.tar.gz > checksums.txt
  # deposit .tar.gz, code zip, data, checksums to Zenodo

LOAD AN ARCHIVED IMAGE
  docker load -i amazon-amnesty-v1.0-final.tar.gz
  docker run --rm -v "$PWD":/amnesty-project -w /amnesty-project you/amazon-amnesty:v1.0-final make all
```
