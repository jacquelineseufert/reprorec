<h1 align="center">🔖 reprorec — Reproducibility Receipts for Open Data</h1>

<p align="center">
  <a href="https://github.com/jacquelineseufert/reprorec/stargazers">
    <img src="https://img.shields.io/github/stars/jacquelineseufert/reprorec" alt="GitHub stars"/>
  </a>
  <a href="./LICENSE">
    <img src="https://img.shields.io/badge/license-MIT-lightgrey.svg" alt="MIT License"/>
  </a>
</p>

---

## ✨ Why this exists

Open data is easy to *access*, but hard to *reproduce*.  
Eurostat, like many statistical agencies, provides **APIs, DOIs, and open standards**, but there is no guarantee that the file you download today is **bit-for-bit identical** to the one someone cited two years ago.  

This project provides a lightweight solution: **reproducibility receipts** that capture
- the file’s **SHA-256 hash**
- key HTTP metadata
- local file info
- a signed JSON receipt

These receipts let you (or anyone else) later **verify the exact bytes** you worked with.

---

## 📖 Theory: Reproducibility in Eurostat’s Dissemination (Laaboudi et al. 2024)

Laaboudi et al. (2024), *Open data dissemination at Eurostat: state of the art*, identify reproducibility as a **key challenge**.  

- Eurostat has made major strides in **openness, APIs, metadata, and DOIs**.  
- But: **the exact bytes are not versioned**.  
- A DOI identifies a dataset concept, not a specific file.  
- Result: a DOI cited in 2022 might not point to the same file in 2024.  

### ❌ The Problem
- Eurostat **overwrites previous vintages** when corrections or updates are made.  
- DOIs ensure *discoverability*, but not *bit-level reproducibility*.  
- Researchers cannot always prove which file they actually used.  

### 📊 Options Discussed

| Option | Managed by | Advantages | Limitations |
|--------|------------|------------|-------------|
| **Full dataset versioning (snapshots)** | Eurostat | Strongest reproducibility; every update preserved | Unsustainable storage and maintenance costs |
| **Persistent identifiers (DOIs)** | Eurostat | Good for citation and discovery | Identifies dataset concept, not bytes |
| **Publisher-anchored hashes (e.g. blockchain, time-stamping)** | Eurostat | Lightweight; official fingerprints published | Only verification, no preservation — if the user didn’t save the file, reproducibility is lost |
| **User-side hashing (logs, receipts)** | Users | Easy to implement; no publisher burden | Not authoritative unless logs are shared |

### 🏛 Their Proposal
Laaboudi et al. suggest Eurostat publish **hashes** for each update (possibly anchored on blockchain or time-stamping services).  

This avoids massive storage costs, but:  
- If you didn’t save the file, the hash is **worthless**.  
- Reproducibility depends on users’ local practices.  
- Blockchain anchoring adds **complexity without solving preservation**.  

> **Key message**: Eurostat’s approach is an improvement, but **not sufficient**. It shifts the burden to users and only enables verification, not guaranteed reproducibility.

---

## ✅ How reprorec fixes this

`reprorec` provides a **user-side reproducibility layer** that complements (and improves on) Eurostat’s system:

| Feature | Laaboudi proposal (Eurostat) | reprorec |
|---------|-------------------------------|----------|
| Version storage | None | Timestamped snapshots saved locally |
| Verification | Requires Eurostat hash registry | Built-in SHA-256 receipts |
| Preservation | Users must rely on Eurostat + blockchain anchoring | Users have authoritative receipts + optional signatures |
| Transparency | Centralized | User-led, shareable, verifiable |
| Effort | Depends on Eurostat infrastructure | Works today with R and minimal setup |

**Verdict**: `reprorec` guarantees **byte-level reproducibility** now, without waiting for Eurostat to build new systems.

---

## 🛠️ Features

- `rr_fetch()` → download file + generate JSON receipt  
- `rr_verify()` → verify file locally or against remote source  
- `rr_snapshot()` → save timestamped copies + ledger entries  
- `rr_diff_schema_latest()` → detect column changes between vintages  
- `rr_diff_rows_latest()` → detect added/removed rows and changed values  
- `rr_revision_triangle()` → build revision triangles for time series  
- `rr_sign()` + `rr_log_publish()` → optional GPG signatures + public logs  

---

## 🚀 Getting started

Install dependencies once:

```r
install.packages(c("httr2","jsonlite","digest","fs","readr","cli","dplyr","tidyr","DT","shiny"))
source("reprorec.R")
x <- rr_fetch("https://ec.europa.eu/eurostat/api/dissemination/files/data/nama_10_gdp.tsv.gz")

rr_verify(x$receipt, mode = "local")
shiny::runApp("app.R")


