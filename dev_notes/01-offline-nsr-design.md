# 05. Design: an offline NSR (native status) for the BIEN service stack

Draft 2026-09-18, for BM's review before implementation. Sibling of `04-versioned-divisions-design.md`;
the political-division reference built there is the backbone here. Lives in `RGNRS/dev_notes`
only until an `RNSR` clone exists, then moves with the code.

## Why this, and what already exists

NSR is the last BIEN service with no offline implementation:

| Service | Offline | Where |
|---|---|---|
| TNRS | yes | `RTNRS`: `TNRS_local()`, `local_build.R`, `local_backbone.R`, `tnrs_cache_dir()` |
| GNRS | yes | `RGNRS` branch `versioned-divisions`: `GNRS_local()`, history/cshapes components |
| GVS | yes | `RGVS` branch `local-implementation`: `GVS_local()`, raster index, centroids, CShapes |
| **NSR** | **no** | `NSR` 0.1.0 is API-only: `NSR()`, `NSR_simple()`, `NSR_sources()`, metadata helpers |

The service's database is built by PHP under `ojalaquellueva/nsr/db` (the separate `nsr_db` repo is
retired), importing 12 checklists. There is no published build of the compiled database, and the
repository documents no licence for the source data, so, as with GADM and CShapes, **sources are
downloaded on the user's machine at build time and nothing derived is shipped**.

## What the service does, and must be reproduced

Output (probed live, `Pinus ponderosa` / United States / California), 18 columns:

    family, genus, species, country, state_province, county_parish, poldiv_full, poldiv_type,
    native_status_country, native_status_state_province, native_status_county_parish,
    native_status, native_status_reason, native_status_sources, isIntroduced,
    isCultivatedNSR, is_cultivated_taxon, user_id

Status codes: **N** native, **Ne** native and endemic, **I** introduced, **Ie** introduced inferred
from endemism elsewhere, **P** present without status, **A** absent from the checklists, **UNK** no
data. A status is returned per political level, plus an overall `native_status` taken at the lowest
division supplied.

Propagation rules, from the service README:
- **Taxonomic:** native propagates *up* (a native variety makes the species, genus and family native
  there); introduced propagates *down* (an introduced species makes its varieties introduced). With
  no opinion for the taxon, higher ranks are consulted. **We reproduce the first and decline the
  last - see below.**
- **Political:** native propagates *up* (native in a county implies native in its state and country);
  introduced propagates *down* (introduced in a country implies introduced in its states).
- **Absence:** absent from a checklist gives `A`; absent while endemic elsewhere gives `Ie`.
- `is_comprehensive` per source decides whether absence is informative at all: only a checklist that
  claims to list everything in its area can turn "not listed" into evidence.

## Architecture

Mirrors the other two packages, so the three share one cache and one idiom.

- Component `nsr` in the shared cache (`GNRS.cache_dir` / `R_user_dir("GNRS")`), tables prefixed
  `nsr-`, provenance RDS per component, `nanoparquet` `.gz.parquet` files.
- `NSR_local_build(sources = c("powo", "vascan", "flbr"), dir, ...)`, one importer per
  source behind a common interface (fetch -> standardise -> resolve names -> resolve divisions ->
  append), so adding the remaining eight later is additive.
- `NSR_local(occurrence_dataframe, dir, ...)` with the service's input columns
  (`species`/`genus`/`family`, `country`, `state_province`, `county_parish`) and its 18 output
  columns, so existing code swaps one call for the other.
- **Names** resolve through `TNRS_local(sources = "wcvp")`, the resolver every other layer of this
  project already uses; checklist names and query names go through the same call, so matching is
  backbone-consistent by construction rather than by string equality.
- **Political divisions** resolve through `GNRS_local()`, which gives the country/state/county ids
  and, new in the versioned-divisions work, **historical divisions**: a record labelled "USSR" or
  "Zaire" resolves to the entity and its current successors, so native status can be looked up in
  the successor whose polygon contains it. The live NSR cannot do this.

### Cache tables

| File | One row per | Columns |
|---|---|---|
| `nsr-sources` | source | source_name, full name, url, date_accessed, is_comprehensive, licence, citation, poldiv_level, divisions covered |
| `nsr-checklist` | taxon x division x source | taxon_id, rank, poldiv_id, poldiv_level, status (`native`/`introduced`/`present`), source_id |
| `nsr-taxa` | taxon | taxon_id (WCVP accepted id), family, genus, species, rank, is_cultivated_taxon |
| `nsr-regions` | region | region_key (`system:code`), system, code, name, level, GNRS ids and GADM gid where applicable |
| `nsr-region-links` | pair of regions | from_region, to_region, relation (`within`/`contains`/`overlaps`), fraction |
| `nsr-endemism` | taxon | taxon_id, endemic_poldiv_id, source (for the `Ie` rule) |

Checklist rows key on resolved ids, never on strings, so a source's own spellings and division names
are a build-time problem only.

## Sources, first release

All four are public and redistributable-in-principle, and cover the places where WGSRPD level 3 (the
resolution WCVP gives us) is too coarse to say anything about sub-country status.

| Source | What it gives | Format, size | Licence |
|---|---|---|---|
| **wcvp** | global native/introduced by WGSRPD level 3 | WCVP v15 **already in hand** (`data/wcvp_v15/wcvp_distribution.csv`), no download | CC BY 4.0 |
| ~~usda~~ | USA, by state | **not obtainable**: the per-state export is no longer served; the GBIF copy is names only and the API gives region-level status | CC0 |
| **vascan** | Canada, by province; native/introduced/ephemeral | DwC-A, `data.canadensys.net/ipt/archive.do?r=vascan`, ~10 MB, updated 2026-08-04 | CC0 |
| **flbr** | Brazil, by state; native/naturalised/cultivated | DwC-A, `ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil`, ~50-100 MB | CC BY 4.0 |

**Division names are the default input; coordinates are opt-in (BM, 2026-09-22):**
`NSR_local()` answers from `country`/`state_province` unless `use_coordinates = TRUE`.
Two reasons. Placing every record by point costs a raster lookup per call, which is the
single most expensive thing the query path can do. And resolving coordinates to political
divisions is GVS's job: a pipeline that needs it already runs GVS, and duplicating that
step here would mean two implementations of the same lookup drifting apart. The
coordinate path stays available because it asks a finer question - it consults each
source in its own geography with no crosswalk - but it is a deliberate choice rather than
a default.

**Coordinates and names are compared, not pooled (BM, 2026-09-22):** the first
implementation unioned the region keys from both, so a record whose point and whose
division names disagreed was answered from evidence for two places and described neither.
Coordinates are not taken as authoritative instead: a transposed longitude and a mistyped
province are equally easy mistakes, and the existing `NSR_from_coordinates()` example
already renames `country` to `country_declared` precisely so declared and inferred
divisions can be compared. The two are therefore compared in the geography they share,
GADM, at each level separately - a right country with a wrong state is caught as readily
as a wrong country - and a level only one of them speaks to is not a disagreement. Where
they agree the wider key set answers, as before. Where they do not, `place_conflict` is
`TRUE`, `native_status` is `UNK`, and `native_status_coordinates` and
`native_status_names` carry the two answers. There is no service behaviour to follow
here: `NSR()` takes names only and `NSR_from_coordinates()` takes coordinates only, so
the API never meets the case.

**Extinct records are kept and filtered at query time (BM, 2026-09-22):** WCVP's
`extinct` is a bare 0/1 with no date or year - checked against the v15 distribution table,
whose only columns are `plant_locality_id, plant_name_id, continent_code_l1, continent,
region_code_l2, region, area_code_l3, area, introduced, extinct, location_doubtful`. It
therefore means "considered no longer present as of this release" and cannot be compared
against an occurrence's own date. Whether it should count is a property of the question,
so the build keeps the row with an `is_extinct` flag and `NSR_local(exclude_extinct = TRUE)`
decides per call: the default answers about the present day, `FALSE` models a past
distribution. 2,701 of 1,970,252 rows are affected, over 2,393 taxa; 906 of those taxa have
no surviving native record at all.

Independently of that switch, the endemism rules always read the taxon's whole native
range, extinct records included. A region a taxon has been lost from is still a region it
was native to, so `Ie` must not fire against it - the answer for a natively extirpated
record is `A` (gone), never `I`/`Ie` (arrived). Filtering at build time, as the garden
pipeline's script 02 does, would have destroyed the evidence needed for that distinction.

**The source is named `wcvp`, not `powo` (BM, 2026-09-22):** it is the WCVP archive that is
read, and it is what `TNRS_local(sources = "wcvp")` calls the same data, so the stack uses one
name for one dataset. `"powo"` is accepted as a synonym in `sources =` and in `files =`. The
cost is that `native_status_sources` no longer matches the live service's label for this source,
which is a known diff when validating `NSR_local()` against `NSR()`.

POWO is the service's global backbone and is the same Kew dataset as WCVP, at the same resolution;
we already apply its native filter in the garden pipeline (script 02 keeps `introduced == 0 &
extinct == 0 & location_doubtful == 0`). The other three add state-level status in the USA, Canada
and Brazil. Brazil matters most: WGSRPD divides it into five regions, so level 3 cannot distinguish
a species native to Amazonia from one introduced to São Paulo.

**usda is dropped from the release (BM, 2026-09-19): the USDA no longer serves the file.**
NSR's own importer expects `usda_plants_native_status.csv`, an export of PLANTS' native status
by state. Checked 2026-09-18: the GBIF-hosted copy of the PLANTS database
(`hosted-datasets.gbif.org/datasets/usda.zip`, CC0, 2.1 MB) is a COLDP archive of names only -
`NameUsage.tsv` and `VernacularName.tsv`, no distribution file; the `csvdownload` endpoint the
old site used now returns the site shell; and the current API
(`plantsservices.sc.egov.usda.gov/api/PlantProfile?symbol=...`) returns native status only by
region - CAN, L48, AK, HI, PR - not by state, which is the resolution that would have added
anything over POWO. No public bulk route to per-state status was found.

**Consequence, to state in the write-up:** in the USA the build has POWO alone, whose introduced
ranges are thin there - WCVP v15 holds no California record for *Quercus robur* at all, though it
is naturalised across the state. Five of the fifteen disagreements with the live service in the
API validation cited `usda`. US records will therefore skew towards `A` (absent) rather than `I`
where a species is introduced but unlisted, and `Ie` only rescues those whose native range is
confined elsewhere. Weakley's southeastern flora would cover part of the same ground if a route to
it appears.

Deferred to a later release: conosur, fwi, ipane, mab, mexico, newguinea, tropicos, weakley. Each
needs its own acquisition route (several are web pages or an API with a key), and two (`mab`
endemic genera, `ipane` introduced-only) are small special-purpose lists rather than checklists.

## Regions are polygons, not names (BM, 2026-09-18)

**Each source keeps its own geography, and queries are resolved against it spatially.**
WCVP records distributions against WGSRPD level-3 areas, which are biogeographic, not political;
VASCAN and Flora do Brasil record provinces and states, which are GADM units. Crosswalking either
onto the other by *name* loses data and invents detail:

- A first implementation matched level-3 unit names to GNRS divisions. Result: 79 of 370 units
  matched a state, 194 fell back to country, and **97 (26%) matched nothing at all** - Borneo,
  Sulawesi, Maluku, New Guinea, Lesser Sunda Is., Santa Cruz Is. Every POWO opinion for those areas
  would have been dropped.
- The reverse fails too: "Brazil Southeast" is one level-3 unit covering four states, so a name
  match can say nothing about Sao Paulo, although the containment is exact and knowable.

Instead:

| Source | Region system | Key |
|---|---|---|
| powo | WGSRPD level 3 (TDWG polygons, `rWCVPdata::wgsrpd3`) | `wgsrpd3:BZL` |
| vascan, flbr | GADM level 1, via the GNRS backbone's `gid_1` | `gadm1:BRA.25_1` |
| (later sources) | whichever they publish against | `<system>:<code>` |

**A query with coordinates is resolved natively in both systems**, by raster lookup: the WGSRPD
level-3 raster (built for the garden pipeline) and the GADM unit index (built for GVS), both at 30
arc-seconds with an exact fallback in boundary cells. No crosswalk is involved, which is the case
that matters for occurrence records.

**A query with only division names** resolves through GNRS to GADM units, then to WGSRPD areas
through a **spatial link table** computed once at build time (`nsr-region-links`: from, to, relation,
fraction of the smaller unit's area inside the larger). The link table is derived by cross-tabulating
the two rasters, so it needs no new downloads and costs one pass over the cells.

**Inheritance (revised 2026-09-18, BM): a place is judged by the polygons it lies IN.**
Evidence attaches to the polygon it was recorded for, and is not transferred to other polygons:

- an opinion about a polygon **containing** the place applies to it (POWO's finest statement about
  Guadeloupe is "native in the Leeward Islands"), and among those any native opinion wins;
- polygons **inside** the place describe only parts of it, so they are not its status. They answer
  only when they all agree; otherwise the answer is `P`, the reason says the status varies, and
  `n_subpolygons_native` / `n_subpolygons_introduced` give the split;
- polygons that merely **overlap** describe neither the place nor its parts, and answer `UNK`;
- `native_status_scope` records which of these produced the answer.

Give coordinates and the question does not arise: the record is judged on the ground it sits on,
which is how the garden pipeline will use it.

**Endemism is the deliberate exception** (BM): `Ne` and `Ie` are claims about the taxon's whole
range, not about one polygon, so they draw on evidence from elsewhere. A taxon confined to
California, found in Michigan, is introduced there whatever Michigan's checklists say.

The earlier design let a sub-polygon set the whole place's status, which made *Polycarpon
tetraphyllum* native in the USA on one `powo:native` area against eleven `powo:introduced` ones.
That is the failure this rule removes.
while "introduced in Brazil Southeast" answers it as `I`. Nothing is invented and nothing is lost.

## Resolution semantics

For each row, after name and division resolution:

1. Gather every checklist opinion for the taxon in the division (or a containing/contained one,
   per the political rule), from all built sources. Opinions recorded against a taxon's own
   infraspecifics are already part of it, having been rolled up at build time; opinions recorded
   against a *higher* rank are not consulted (see "Higher ranks are not consulted", below).
2. Reduce to one status per political level. **Precedence (BM, 2026-09-18): if any source says
   native, the taxon is native there.** Native is the hardest claim for a checklist to make by
   accident, whereas "introduced" and "absent" are often an artefact of a list's scope or age. Where
   sources disagree, the answer is native and the disagreement is recorded rather than hidden, in
   `native_status_conflict` (whether opinions differed) and `native_status_opinions` (the per-source
   verdicts, e.g. `powo:native; usda:introduced`). This mirrors `TNRS_local()`'s `Source_conflict`.
   Failing a native opinion, prefer the finer political level, then explicit `introduced` over
   `present`, then a comprehensive source over a non-comprehensive one.
3. Absence only yields `A`/`Ie` where a comprehensive source covers the division **and** the taxon
   is evaluable, i.e. some source holds native-status information about it (see below).
4. `native_status` is the status at the lowest division supplied, and `isIntroduced` is 1 for `I`
   and `Ie`.

New columns beyond the service, marked as extensions so output stays a superset:
`poldiv_is_historical`, `resolved_in_successor` (which current division answered a historical one),
and `native_status_basis` (the source and division that decided it).

## Absence: when "not listed" is evidence

**Decisions (BM, 2026-09-18): POWO counts as comprehensive for the species it covers; and a taxon
about which no source holds any native-status information cannot be called Absent or Present at
all.**

The live service does not make the second distinction, and it matters. Probed 2026-09-18:

| Query | Live NSR | Comment |
|---|---|---|
| *Sphagnum palustre*, California | **A**, "Absent from all checklists for region", sources powo, usda | a moss; POWO covers vascular plants only |
| *Marchantia polymorpha*, California | **A**, same | a liverwort |
| *Quercus robur*, California | I, sources powo, usda, weakley | correct |
| *Pinus ponderosa*, Brazil | I, source flbr, `isCultivatedNSR` 1 | correct |
| *Zea mays*, Mexico | N, sources mexico, powo | correct |

A bryophyte in California is reported Absent, when the truth is that no consulted checklist has an
opinion about bryophytes. Absence of evidence is returned as evidence of absence, and downstream
that becomes `isIntroduced` through the `Ie` rule.

**The rule (BM's formulation): go by which taxa the sources hold native-status information about,
not by what the sources claim to cover.** A taxon is *evaluable* if it has at least one
native-status record in any built source, anywhere in the world. Absence is interpretable only for
evaluable taxa; for the rest the answer is `UNK`, with `native_status_reason` "no source holds
native-status information for this taxon".

Why this rather than a declared taxonomic scope per source:
- **It does not need to know what a moss is.** *Sphagnum palustre* has no row in WCVP's
  distributions, so it is not evaluable, and no `A` is emitted. The server never reasons about
  taxonomic groups.
- **It survives new and user-supplied sources.** A curated scope field would have to be written, and
  kept right, for every source anyone adds. Coverage here is a property of the data, computed at
  build time as the distinct taxa in `nsr-checklist`.
- **It protects newly described species.** A species accepted in WCVP but with no distribution
  records yet is not evaluable, so it returns `UNK` instead of being flagged Absent (and then
  introduced) everywhere it is found. A declared-scope rule would get this wrong, since the species
  is squarely inside POWO's stated scope.

Implementation is one lookup: `evaluable(taxon_id)` is true when the taxon appears anywhere in
`nsr-checklist`. No scope column, no taxonomic-group inference, and the semantics stay the same as
sources are added: each new checklist can only widen the evaluable set.

This is a deliberate divergence from the service, and one to report upstream: the same query that
returns `A` there returns `UNK` here.

## Validation

1. **Against the live service**, on a stratified sample of BIEN and GBIF records (target ~50,000
   rows spanning all four source regions and a tail of elsewhere): agreement on `native_status`,
   a confusion matrix of codes, and disagreements broken down by source, division level and region.
   The four-source build cannot match the service everywhere, so the honest target is: agreement
   where only these four sources apply, and a measured, explained gap elsewhere.
2. **Against BIEN's stored `is_introduced`** on the 284M-record BIEN pull, the same check we ran for
   geovalidity and centroids.
3. **Historical divisions**: records whose country no longer exists resolve and receive a status
   through the successor, which the live service cannot do; report how many records that recovers.
4. **Garden pipeline**: the occurrence layer currently keeps a record only if it falls in the
   species' native WGSRPD level-3 range (POWO). Re-run with NSR and report how many cells change,
   which is the concrete answer to "does the finer native status matter for the paper?"

## Built, 2026-09-18 (milestones 1-3)

Branch `local-implementation`: `R/local_cache.R`, `R/local_build.R`, `R/local_import.R`,
`R/local_regions.R`, `R/NSR_local.R`, `tests/testthat/test-local-nsr.R` (24 expectations, all
passing). Built against the shared GNRS/GVS cache in 24 minutes:

| Source | Records | Taxa | Geography |
|---|---|---|---|
| powo | 1,986,877 | 443,168 | 375 WGSRPD level-3 areas |
| flbr | 154,169 | 38,326 | 27 Brazilian states |
| vascan | 25,459 | 6,086 | 13 Canadian provinces + Greenland |

Plus 415 regions and **13,742 spatial links** (3,330 within, 3,330 contains, 6,684 overlaps).
POWO needs no download (the WCVP archive is already in the TNRS cache) and no name resolution
(WCVP is the backbone, so `accepted_plant_name_id` is the taxon id). The other two resolve through
`TNRS_local()`: VASCAN matches 99.5% of names; FLBR 75%, the residue being the fungi and algae that
a vascular-plant backbone cannot match - which is exactly the evaluability rule's case.

### Reference cases

| Query | Local NSR | Live service |
|---|---|---|
| *Pinus ponderosa*, US/California | N (powo) | N |
| *Araucaria angustifolia*, Brazil/Sao Paulo | N (flbr + powo) | - |
| *Araucaria angustifolia*, Brazil/Amazonas | A | - |
| *Araucaria angustifolia*, by coordinates in Sao Paulo | N, resolved in both geographies | n/a |
| *Acer saccharum*, Canada/New Brunswick | N (vascan + powo) | - |
| *Zea mays*, Mexico | N (powo) | N |
| *Welwitschia mirabilis*, Namibia | N (powo) | N |
| *Sphagnum palustre* / *Marchantia polymorpha*, US/California | **UNK** | **A** |
| *Quercus robur*, US/California | **A** | **I** (powo, usda, weakley) |

The bryophyte divergence is the intended one. The *Quercus robur* divergence is not: **WCVP holds no
California record for it at all** (22 introduced records elsewhere, none in CAL), so the service's
answer came from USDA and Weakley. POWO's introduced ranges are thinnest exactly where USDA would
cover, which is an argument for keeping `usda` in the first release rather than dropping it (see
Open questions).

### Bugs worth remembering

- **Pooling administrative levels in the link table.** Aggregating cells to both level 1 and level 0
  before computing fractions counted every cell twice, so California-the-state and CAL-the-WGSRPD-area
  each looked half-inside the other and their relation degraded from `same` to `overlaps` - which
  correctly demoted every native opinion to mere presence. Each level now has its own denominators.
- **A state query inheriting its country's answer.** Keys for both levels sat in one pool, so
  Amazonas inherited Brazil's "native". The finest named place now answers alone; the country key
  only fills the country column.
- **VASCAN's two status columns are the reverse of the obvious guess**: `occurrenceStatus` is
  present/excluded/doubtful/irregular/absent, `establishmentMeans` is native/introduced.
- **The GNRS backbone spells some divisions bilingually** ("New Brunswick/Nouveau-Brunswick") and
  Newfoundland's HASC is `CA.NF`, not `CA.NL`.

### Validation against the live service (milestone 4)

`scratchpad/nsr_validate.R`: a stratified sample of species x division (POWO natives, POWO
introduced, Brazilian states, Canadian provinces, and a tail of random species in random
countries), sent to both. The service drops roughly a third of API batches whatever their size,
so 150 of 300 rows were compared.

**84% same decision** (78% identical code), against a service with four times as many sources:

| local \ service | absent | introduced | native | present | unknown |
|---|---|---|---|---|---|
| absent | 6 | 4 | 0 | 0 | 0 |
| introduced | 5 | 49 | 1 | 1 | 1 |
| native | 9 | 1 | 71 | 0 | 1 |
| present | 1 | 0 | 0 | 0 | 0 |

By stratum: powo 86.7% (n=120), vascan 86.7% (n=15), flbr 60% (n=5), no-source tail 60% (n=10).

Disagreement classes, all explained:
- **local native, service absent (9).** Four cite no source at all (*Monanthotaxis schweinfurthii*
  in DR Congo, *Galium spurium* in Syria, *Phleum* and *Trifolium spadiceum* in Russia); we cite
  POWO v15. The service's POWO import is older, so these look like ours being more current.
- **local introduced, service absent (5).** US and Australian cases where POWO records the species
  as introduced in an area inside the country and we inherit that upward; the service answers
  absent. Ours is the more informative reading of the same data.
- **local absent, service introduced (4)** and **one each way on native/introduced**: sources we
  lack (`usda`, `weakley`, `mab`, `fwi`, `conosur`) or POWO version differences.

Two rule changes came out of reading the disagreements, both now in the code:
- **Inheritance is symmetric.** Any native opinion about a related region makes it native (BM's
  precedence); failing that, any introduced opinion makes it introduced. Inferred answers say so in
  `native_status_reason`, and `native_status_opinions` records the relation.
- **Links under 1% of a region are ignored**, so raster disagreement along a coastline carries
  nothing. The `within` threshold is 0.95, not 0.99: these fractions come from 30 arc-second
  rasters of two independently drawn coastlines, so coextensive regions score 0.987-0.990.

Also added: **endemism** (`Ne`, `isEndemic`), read off the checklist - every region any source calls
the taxon native lies inside the queried place. Verified on single-area natives, and correctly
declining for *Araucaria angustifolia* (also Argentina, Paraguay) and *Welwitschia mirabilis* (also
Angola). Note it is a statement about the checklists, not a conservation claim: *Oryza sativa* in
China returns `Ne`, because POWO gives rice one native area. **Genus queries** are answered
directly (WCVP records distributions for 14,126 genera); what is refused is resolving a bare genus
onto some species of that genus.

Bugs fixed this round, worth remembering: WCVP carries the same name at two ranks (a *variety* row
also called "Pinus ponderosa"), and only one holds the distributions, so names index to the
species-rank id that has opinions; and accepted infraspecific taxa (60,278 of them) keep their own
distributions, so they are rolled up to their species through `parent_plant_name_id`.

### Status model completed: `Ie` (2026-09-18)

Absence becomes introduction only where the taxon could not have been native: its whole native
range lies elsewhere **and is confined**, to one region or to one country. A widely native species
merely unrecorded here stays `A`, because absence alone is not evidence of introduction.

| Query | Answer | Why |
|---|---|---|
| *Lepechinia calycina* (endemic to California) in Michigan | `Ie`, isIntroduced 1 | it could not be native there |
| *Lepechinia calycina* in California | `Ne`, isEndemic 1 | native and confined to it |
| *Homonoia* (widely native) in Michigan | `A` | absent, but could have been native |
| *Washingtonia* (California, Arizona, NW Mexico) in Michigan | `A` | native range not confined |

Final validation: **85.3% same decision** (79.3% identical code) on 150 compared rows; by stratum
powo 86.7%, vascan 86.7%, no-source tail 80%, flbr 60% (n=5). `Ie` moved four records from absent
to introduced.

The nine remaining "we native, service absent" rows were checked individually and are not artefacts
of loose inheritance: *Monanthotaxis schweinfurthii* in DR Congo is a **stated** `powo:native` while
the service returns absent citing nothing; *Galium spurium* in Syria is native within WGSRPD's
Lebanon-Syria unit, POWO's finest statement about Syria; *Trifolium spadiceum* is native in a
Russian sub-area. Over 1,500 country-level queries, inferred answers come through `within` and
`contains`, not weak overlaps.

**One consequence of the any-native precedence worth stating in the write-up:** at country level a
single native area carries the whole country. *Polycarpon tetraphyllum* in the USA answers `N` from
one `powo:native(contains)` against eleven `powo:introduced(contains)`. The counts are visible in
`native_status_opinions`, and the alternative (majority, or requiring a stated opinion) would lose
the cases the rule exists for, but users filtering on `native_status` at country level should know
it.

### Validation under the containment rule

**82.0% same decision** (76.7% identical code), against 85.3% under the old inheritance. The whole
drop is the deliberate change: 9 rows where we now answer `P` (the status varies among the polygons
inside the queried place) while the service commits to one, and 2 where only partly overlapping
polygons have an opinion and we answer `UNK`. Excluding those eleven, agreement is ~89%. Nothing
accidental changed: the same `usda`/`weakley` gaps and POWO-version differences remain.

By stratum: powo 82.5%, vascan 86.7%, no-source tail 80%, flbr 60% (n=5).

This is a case where agreement with the service is the wrong target. The service propagates a
sub-region's status upward; we decline to, and say so. For records with coordinates - every record
in the garden pipeline - the two approaches coincide, because the point lies inside exactly one
polygon per system.

### Higher ranks are not consulted (BM, 2026-09-24)

The service's taxonomic rule has two halves and they are not the same claim.

Upward is sound and **is** implemented, at build time in `nsr_species_of()`: a distribution recorded
against an accepted infraspecific taxon is rolled up to its species, because a subspecies of *X*
occurring natively in a region means *X* occurs natively in that region. The subspecies is an *X*.
Roughly 60k WCVP distribution rows hang off infraspecifics and reach the species this way.

Downward from a higher rank is **not** implemented, and deliberately. "With no opinion for the
taxon, higher ranks are consulted" means answering about a species from its genus's range, and
knowing that a genus is native to a region tells us nothing about any particular species in it:
that is inferring a property of a part from a property of the whole - the ecological fallacy, in
its division form. A genus native to Quebec means *some* species of it is native to Quebec, and the
species in hand is as likely to be the introduced one. Native is the strongest claim the resolver
can make, and making it at genus level would manufacture it at scale for exactly the taxa about
which the sources are silent.

So a species with no opinion of its own is `UNK` (or `A` where a comprehensive source covers the
place), never native-by-genus. `taxon_evaluable` says which it is. A genus *query* is still answered
directly, because WCVP records distributions against genera and those rows are kept keyed on the
genus - that is answering the question asked, not inferring downward from it.

**This is a known and permanent divergence from the service**, and belongs with the `wcvp`/`powo`
source-name difference in any validation against `NSR()`: where the live service returns a
genus-derived status for an unlisted species, we return `UNK` or `A`.

## Milestones

1. Cache scaffolding, `nsr-sources`, and the POWO importer from WCVP (no download); `NSR_local()`
   returning service-shaped output for country-level queries. Validates against the live API on
   country-level rows.
2. USDA, VASCAN and Flora do Brasil importers, with the state-level resolution and the political
   propagation rules.
3. Taxonomic roll-up of infraspecifics, endemism (`Ie`) and conflict handling. (`is_cultivated_taxon`
   was dropped - see open question 3; consulting higher ranks was declined - see "Higher ranks are
   not consulted".)
4. Validation 1-3; write-up.
5. Garden re-run (validation 4) and, separately, the remaining eight sources.

## Open questions for BM

1. **Conflict precedence.** Is the proposal above (finer division, explicit over present,
   comprehensive over not, all opinions recorded) how the service behaves? If BM has the service's
   SQL to hand it is faster than inferring it from probes.
2. **POWO as comprehensive.** Treating WCVP's absence as evidence of non-nativeness globally is what
   makes `A`/`Ie` possible at all outside the three national checklists. Reasonable, or too strong?
3. Settled (BM, 2026-09-19): **`is_cultivated_taxon` is dropped.** It was already dead in
   production - filled from table `cultspp`, loaded from `cultspp_staging` only
   `if (exists_table(...))`, with no importer in the public repo populating it; probes return
   `is_cultivated_taxon = 0` for both *Zea mays* and *Triticum aestivum*. The candidate
   replacements were examined 2026-09-19 and none is worth carrying.

   **Why dropped rather than substituted (BM):** a use flag is not much use without a polygon
   attached to it. Everything else NSR reports is an assertion about a taxon *in a place*, and
   the containment semantics exist precisely so that polygon-level evidence is not spread to
   other polygons. "Grown somewhere by someone" is not evidence that the record in hand was
   planted, so a taxon-level flag would either sit unused or be read as if it were
   place-specific. `isCultivatedNSR` already answers the version of the question that has a
   geography, and is unaffected by this: it comes from checklists carrying a cultivated status
   (FLBR flagged *Pinus ponderosa* in Brazil), reproduced from the same source field.

   What was examined, so it need not be examined again:

   - **Kew's World Checklist of Useful Plant Species** (Diazgranados et al. 2020, 40,292 species,
     CC BY 4.0) is published only as an 11.3 MB PDF plus its EML (KNB, doi:10.5063/F1CV4G34) -
     no data table. (The DOI in the earlier draft of this note, 10.34885/172, was wrong; that is
     the State of the World's Plants and Fungi report.) **The PDF was parsed successfully on
     2026-09-19** - it encodes rank in the font, so the hierarchy is recoverable exactly - and the
     result is `garden_variety_traits/data/nsr_sources/wcups_2020.csv`, produced by
     `R_scripts/48_parse_wcups_pdf.py`. It reproduces every published control total (40,292
     records; 3 kingdoms / 6 phyla / 14 classes / 101 orders / 433 families / 6,737 genera;
     40,239 LSIDs; all ten use-category counts; the top five families and genera; 70 species with
     all ten uses; 91 families and 2,790 genera with a single species). So availability is no
     longer the obstacle - but the data are still ten use categories per taxon with **no
     geography**, so the reason for dropping the flag is unchanged.
   - **GRIN Taxonomy** (CC0, COLDP, `hosted-datasets.gbif.org/datasets/grin.zip`, 10.7 MB) does
     carry economic uses in `TaxonProperty.tsv`: 17,111 taxa with a use, 9,836 in
     cultivation-like classes (ornamental 9,155, plus food, forage, materials, fuel). Machine-
     readable and usable - but taxon-level with **no geography at all**, which is the objection
     above. Note too that economic use is not cultivation: the largest class after ornamental is
     folklore medicine (4,812), largely wild-harvested.
   - **Wiersema & Leon's World Economic Plants** has the same taxon-level-only shape.

   GRIN's region-level `cultivated` distribution status was checked as a way to give the flag a
   geography, and is not fit for it: 1,437 taxa worldwide, 5,260 rows, 229 ISO3 countries and
   nothing finer. Coverage is not the heavily cultivated combinations - *Triticum aestivum*,
   *Oryza sativa*, *Glycine max* and *Malus domestica* have no distribution rows at all, and
   *Zea mays*, *Manihot esculenta*, *Sorghum bicolor*, *Vitis vinifera* and *Theobroma cacao*
   carry native ranges with zero cultivated entries, while *Hordeum vulgare* has 91. The reason
   is that GRIN's distribution field documents germplasm provenance, not cultivation extent, so a
   cultigen with no natural range often gets nothing; country totals (China 528, US 348, India
   299, Europe thin) track collecting effort rather than area under cultivation. The GBIF COLDP
   export also flattens GRIN's sub-country geography onto ISO3 - visible as 71 identical
   `iso:RUS` rows for a single taxon, one per Russian region - so even where it has content the
   resolution is country-level.

   In the code: `NSR_local()` returns `is_cultivated_taxon = NA_integer_` rather than production's
   `0`, so the column keeps its place in the service-shaped output without a never-populated
   field being read as a real negative. `NSR_local_by_region()` omits it.

4. Settled (BM, 2026-09-18): for the garden paper NSR is a **robustness check** on the existing
   WCVP native-range filter, not a gate on the occurrence layer, to be revisited if validation 4
   shows it makes a material difference.
5. Settled: sources downloaded 2026-09-18 to `garden_variety_traits/data/nsr_sources/` (gitignored):
   usda.zip (2.1 MB), VASCAN DwC-A, Flora do Brasil DwC-A. POWO comes from the WCVP v15 files
   already in the repo's data directory.
6. **GRIN as a fourth source?** Open. Examining GRIN for the cultivated flag turned up
   something more useful than the flag: `Distribution.tsv` holds 579,184 native/introduced rows
   for 65,196 taxa across 229 countries, CC0, independent of POWO. It is country-level only (the
   COLDP export flattens GRIN's sub-country geography onto ISO3), so it does not recover the
   per-state US resolution that `usda` would have given, and the documented POWO-alone US skew
   towards `A` stands either way. `iso:` maps directly onto the `gadm0:` keys the link table
   already carries. Awaiting BM's call; not built.

7. Settled: BM cloned `EnquistLab/RNSR` on 2026-09-19; work is on branch
   `local-implementation` and this note lives in the clone.
