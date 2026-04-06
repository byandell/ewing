# Predator-PreyDatasets

**Prompt:** find a well-documented predator prey dataset, ideally with age and location information for individuals

There are a few world-renowned ecological datasets that have
well-documented predator-prey with
**individual-level age and high-resolution spatial location data**.
The best datasets come from long-term tracking projects that utilize
GPS/VHF radio collars and capture-mark-recapture techniques.

**To start immediately:**
Go to
[Movebank](https://www.movebank.org/)
and use their map viewer to search for active, published Wolf & Elk or Lion & Zebra datasets.
It will allow you to download flat `.csv` files that map
`timestamp`, `location_lat`, `location_long`, `individual_id`, and `age_class`.

Here are the best, most well-documented open datasets:

## Table of Contents

- [1. The Yellowstone Wolf Project & Elk Data](#1-the-yellowstone-wolf-project--elk-data)
- [2. Isle Royale Wolf and Moose Study](#2-isle-royale-wolf-and-moose-study)
- [3. The Serengeti Lion Project (and Snapshot Serengeti)](#3-the-serengeti-lion-project-and-snapshot-serengeti)
- [4. The Kluane Boreal Forest Project (Snowshoe Hare & Canada Lynx)](#4-the-kluane-boreal-forest-project-snowshoe-hare--canada-lynx)
- [5. Movebank (The Universal Resource)](#5-movebank-the-universal-resource)

## 1. The Yellowstone Wolf Project & Elk Data

Perhaps the most famous modern dataset for individual-level tracking. Biologists have been tracking wolves and elk in Yellowstone since wolves were reintroduced in 1995.

- **The Data:**
They collar wolves to track their daily movements (location)
and maintain massive pedigree models tracking every individual's birth year/age
throughout the packs. They also record "kill sites", which provide the exact geographic location, species, and age (estimated via teeth/bones) of the prey (primarily elk).
- **Where to find it:**
You can find massive Yellowstone spatial datasets hosted on
[Movebank](https://www.movebank.org/)
(a free online database of animal tracking data).
You can also search the
[Dryad Data Repository](https://datadryad.org/)
for datasets like
_"Data from: Wolf (Canis lupus) and elk (Cervus elaphus) dynamics in Yellowstone National Park"_,
which often include supplementary CSV files with individual life histories.

## 2. Isle Royale Wolf and Moose Study

This is the longest-running continuous study of a predator-prey system in the world (over 60 years) on an isolated island in Lake Superior.

- **The Data:**
While historically population-level, modern iterations of the dataset track individual radio-collared wolves (location and age/lineage). The moose data often comes from intensive winter aerial surveys and recovered carcasses—where the geographic location of the kill is recorded, and the age of the individual moose is determined via bone/tooth analysis.
- **Where to find it:** The project maintains its own data portal at
[isleroyalewolf.org](https://isleroyalewolf.org),
and datasets can frequently be found via the
[Environmental Data Initiative (EDI)](https://edirepository.org/).

## 3. The Serengeti Lion Project (and Snapshot Serengeti)

The University of Minnesota’s Lion Center has followed individual lions in the Serengeti since the 1960s, maintaining records for over 5,000 individuals across 12 generations.

- **What it contains:**
Individual life histories (age, pride affiliation) and location data. To bridge the "prey" gap, the Snapshot Serengeti dataset provides millions of camera trap images geolocated across a grid, which can be cross-referenced with lion pride territories.
- **Data Access:**
  - Lions: Individual demographic data is often available through the
[Lion Research Center](https://lioncenter.umn.edu/our-work/lion-research).
  - Prey: The
[Snapshot Serengeti dataset on Dryad](https://datadryad.org/dataset/doi:10.5061/dryad.5pt92)
contains timestamps and locations for all prey species in the study area.

## 4. The Kluane Boreal Forest Project (Snowshoe Hare & Canada Lynx)

A famously comprehensive 40+ year terrestrial ecology project in the Yukon, Canada.

- **The Data:**
Rather than just GPS tracking, this project relies heavily on intensive trapping grids.
It provides capture-mark-recapture data, meaning you get individual IDs,
the exact location (trap station) where they were caught, their weight,
and their age class (juvenile vs. adult) over time for both the predator (lynx) and prey (hare).
- **Where to find it:**
Data from this project is frequently published openly in the
[Dryad](https://datadryad.org/)
or
[Zenodo](https://zenodo.org/)
repositories under the names of the lead researchers (e.g., Charles Krebs, Stan Boutin).

## 5. Movebank (The Universal Resource)

The absolute best resource in the world to pull raw CSVs of individual locations is
[Movebank.org](https://www.movebank.org/).

- It is a publicly accessible database where field researchers upload animal tracking data.
- The tabular data output gives exact timestamps, Lat/Lon coordinates, and attached metadata columns typically including the Individual_ID, Sex, and Life_Stage or Age.
- **How to use it:**
Search the data repository for predator species
(e.g., Panthera leo / African Lion, Canis lupus / Gray Wolf)
and prey species
(e.g., Equus quagga / Zebra, Rangifer tarandus / Caribou).
- **Finding Interactions:**
Search Movebank for "Predator" or specific pairings
(e.g., "Great White Shark and Seal" or "Cougar and Mule Deer").
- **Age Information:**
Most Movebank studies include metadata for each "Individual"
that includes their approximate age (juvenile vs. adult) and sex.
