# The paper vs. our pipeline — group definitions, validation targets, and a corrected bug diagnosis

Date: 2026-07-29
Source: `amnesty_wp.pdf` (Scheinkman & Tremacoldi-Rossi, draft dated 8 Sept 2025, 47pp),
supplied by the user and read this session.

This supersedes parts of `docs/notes/2_empirics_scoping.md`. Where they disagree, this
file is right — it is based on the paper's own statement of the design.

## 1. The design, from the paper

The 2009 Land-Grabbing Amnesty. Eligibility has **two** conditions, conditional on
occupying a *target area* (undesignated federal land):

```
eligible  =  1{in target area} × 1{occupation started <= 2004} × 1{area <= 1,500 ha}
```

Three groups, and **two separate regressions**, both against the same control:

| group | definition | role |
|---|---|---|
| **eligible** | target area, occupied ≤2004, ≤1,500 ha | treated (direct effect) |
| **ineligible** | target area, but occupied >2004 OR >1,500 ha (or both) | treated (expectation effect) |
| **never-eligible** | squatters in **indigenous reservations and conservation units** | control for both |

Model: `y_it = β·x_i·1{t ≥ 2009} + υ_i + υ_t + ε_it`, parcel and year fixed effects,
SEs clustered at state level, **data period 2005-2014**.

## 2. CRITICAL — our scaffold's group names do NOT mean what the paper's mean

`code/01_build/01_car/0_build_car_layers_from_raw.R` defines:

```r
ineligible_flag = control_overlap_share > 0.01      # overlaps reservations/conservation
eligible_flag   = !ineligible_flag & target_overlap_share > 0.01
```

That is a pure spatial test with **no occupation-year and no area condition**. So:

| our file | our count | what it ACTUALLY is in the paper's language |
|---|---|---|
| `car_eligible_cleaned.shp` | 164,223 | **eligible + ineligible combined** (everything in target areas) |
| `car_ineligible_cleaned.shp` | 13,025 | **never-eligible** (the control group — reservations/conservation) |

**The word "ineligible" means opposite things in the two systems.** Ours means "in
protected land, never eligible"; the paper's means "in target land but failing the
2004/1,500 ha test". Anything that reads our `car_ineligible_cleaned.shp` expecting the
paper's ineligible group gets the control group instead — a silent wrong answer, not an
error. Same hazard as the `car_eligible_cleaned.shp` filename collision noted in the
scoping doc.

**Consequence for the plan:** the paper's eligible/ineligible split does not exist in our
data yet. It has to be created by splitting our 164,223 target-area parcels on the two
eligibility conditions.

## 3. Progress on that split (measured this session)

Applying the **area** condition to our 164,223 target-area parcels:

```
area <=  1500 ha : 159,464   mean area   148.8 ha
area >   1500 ha :   4,759   mean area 6,874.8 ha
area missing     :       0
```

The paper says there are "roughly 15,000 ineligible properties". The area rule alone
yields 4,759, so the remaining ~10,000 must come from the **occupied-after-2004**
condition. That is consistent, and it tells us the occupation-year condition is doing
most of the work in defining the ineligible group.

Direction also checks out against the paper's "considerably more eligible properties,
which are smaller than ineligible": 159,464 small parcels averaging 149 ha versus 4,759
averaging 6,875 ha.

**The occupation-year condition is the missing piece**, and it is exactly what the
`active2004` machinery in `2_empirics` computes (a parcel counts as occupied by 2004 if
it shows deforestation in the 2004 raster). So that step cannot be skipped — it is what
separates eligible from ineligible.

## 4. CORRECTED bug diagnosis (supersedes the scoping doc)

The scoping doc noted that `control` (line 2727) and `spillover` (line 2748) both read
`CAR_notEligible_defo_*`, and guessed that **`spillover`** was the mis-wired one. **That
guess was backwards.** With the paper's design in hand:

- `spillover` in the code = the paper's **ineligible** group. Reading
  `CAR_notEligible_defo_*` ("notEligible" = ineligible) is **correct**.
- `control` should be the paper's **never-eligible** group — squatters in reservations
  and conservation units. It should read `CAR_inReservas_defo_*`, which is written at
  line 2718 and read nowhere. **`control` is the mis-wired one.**

So `did2 = rbind(control, spillover)` currently stacks the ineligible group against
itself. The one-line fix is to point the `control` loop at `CAR_inReservas_defo_`.
Not applied — flagged for the authors, since it affects published estimates.

## 5. Validation targets from the paper

We now have a baseline, which we previously did not (no `.dta` exists locally and the
Dropbox `output/` folder holds none). Numbers to reproduce:

**Eligible**
- deforested area: **5.1 M ha (2008) → 5.27 M ha (2014)**
- pre-2009 deforestation rate: **58.4%**; mean pre-2009 deforested area **69.03 ha**
- effect: **β̂ ≈ −1.4 p.p.**, ≈2% reduction; ~54,000 ha forest kept

**Ineligible**
- count: **~15,000 properties**
- deforested area: **4.1 M ha (2008) → 4.66 M ha (2014)**
- pre-2009 deforestation rate: **11.4%**; mean pre-2009 deforested area **204.3 ha**
- effect: **β̂ = +4.2 p.p.**, >30% increase; ~227,000 ha cut
- mean rate rises to **21.4%**

**Never-eligible (control):** deforested area rises **11%** over the period.

**Net:** 227,000 − 54,000 = **173,000 ha** net forest removed; ×500 t CO₂/ha =
**86 Mt CO₂**.

These give us a genuine acceptance test for the rebuilt chain — the CAR migration used
`muni_year_intersections.csv` the same way.

## 6. Drafting issues spotted in the PDF

Placeholders and unfinished text:
- p.1 abstract reads **"Write abstract."**; **"Keywords:"** is empty.
- p.2 "properties of less than**...**if deforestation occurred before 2009" — the hectare
  threshold is missing.
- p.2 **"(cite)"** twice.
- p.1 acknowledgements thank **"Preach?"** — a placeholder name.
- p.2 contains an untranslated note to a co-author left in the body text:
  *"Pedro: Estamos usando estas 4 classes ou merging (1) (2) e (3)? ... Addressed!"*
- p.2 ends with an orphan sentence: *"In many settings, Students not qualifying for debt
  forgiveness may borrow more..."* (stray capital, no closing punctuation).
- §3.3 body is the placeholder **"One paragraph detailing each subsection briefly."**
- The CO₂-per-hectare claim cites **"( ?)"** — a broken reference.

Numbering:
- **§4.1 is missing** — the paper goes 4 → 4.2 → 4.3.
- Results jump from **Table 1 to Table 3**; Table 2 is never referenced.

Internal inconsistencies worth the authors' attention:
- The eligible effect is given as **"1 percentage point"** in the text but **1.4** in the
  arithmetic below it; likewise **"approximately 2%"** vs **"the 2.4% decrease"** later.
- Eligible deforested area is described as rising **"about 6% or around 200 thousand
  hectares"**, but the Table 1 figures quoted (5.1 M → 5.27 M) are **+170,000 ha, +3.3%**.
- Ineligible is described as increasing **"over 15%"**, but 4.1 M → 4.66 M is **+13.7%**.
- **"an effect one order of magnitude larger"** — 4.2 vs 1.4 p.p. is about 3×.
- **"Between 2014 and 2009"** — reversed; and **"a 11 percent-increase"** → "an 11 percent
  increase".

Consistent on checking (no action needed): footnote 7 (58.4→62.5 = 7%; 9.4 vs 7 = 34%),
the net-effect subtraction (227−54 = 173), and the carbon figure (173,000 × 500 t = 86 Mt).

Note: apparent run-together words in the extracted text (`asnever-eligible`, `parceli`,
`SUTV A`) are PDF text-extraction artifacts, not document typos — ignore them.

## 7. What this changes about next steps

1. The eligible/ineligible split must be **built**, not reused. Our
   `car_eligible_cleaned.shp` is the raw pool for it.
2. The area condition is trivial and already measured. The **occupation-year (≤2004)
   condition requires the deforestation raster work** — so the heavy step 5 is on the
   critical path after all; it cannot be sidestepped.
3. The never-eligible control group maps to our existing `car_ineligible_cleaned.shp`
   (13,025 parcels) — that part IS reusable, just misleadingly named.
4. We can now validate against the paper's numbers rather than needing the missing
   `amazon_working/` files. If our rebuild lands near §5's figures, the magic files can be
   dropped entirely.
