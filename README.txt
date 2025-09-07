## 📊 Reproducibility in Eurostat’s Dissemination (Laaboudi et al. 2024)

Laaboudi et al. (2024), *Open data dissemination at Eurostat: state of the art*, identify **reproducibility** as a key challenge in statistical dissemination.  
While Eurostat has improved openness, APIs, metadata standards, and DOIs, the **exact bytes of datasets are not versioned**.  
This means researchers cannot always guarantee that the dataset retrieved in 2022 is identical to the one retrieved under the same DOI in 2024.

---

### 🚨 The Problem
- Eurostat’s system **overwrites previous vintages** of datasets when corrections or methodological updates are made.  
- **DOIs ensure discoverability**, but they identify a dataset concept, not the exact file content.  
- **Citing only a DOI is insufficient for reproducibility**, since the underlying bytes may have changed.  

---

### 🔄 Options Discussed

| Option                                | Who manages versions | ✅ Pros                                                | ⚠️ Cons                                                                 |
|--------------------------------------|----------------------|------------------------------------------------------|------------------------------------------------------------------------|
| **Full dataset versioning** (snapshots) | Eurostat             | Strongest reproducibility; every update preserved    | Very high storage & maintenance cost; limited reuse of old versions    |
| **Persistent identifiers (DOIs)**     | Eurostat             | Good for citation, metrics, discovery                | Identifies dataset *concept*, not file bytes → does not ensure reproducibility |
| **Publisher-anchored hashes** (e.g. blockchain registry) | Eurostat publishes hashes | Lightweight; users can verify files against Eurostat’s hashes | Requires trustworthy infrastructure for hash publication |
| **User-side hashing** (receipts, logs) | Users                | No publisher burden; easy to implement               | Not authoritative; proofs only exist locally unless shared             |

---

### 💡 Their Proposal
Eurostat is considering **publishing dataset hashes** at each update (possibly anchored on a blockchain).  

This avoids central storage of all vintages while still allowing reproducibility checks:

1. Eurostat publishes the **official hash** for each dataset update.  
2. Users **store their local files**.  
3. Anyone can later **verify** a file against the published hash.  

---

### 📌 Key Message
Eurostat acknowledges that **reproducibility requires more than open access and DOIs**.  
To close the gap, they are exploring **hash publication mechanisms** (e.g. blockchain anchoring) to guarantee **byte-level reproducibility** without the unsustainable cost of storing every vintage.
