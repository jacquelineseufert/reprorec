## Reproducibility in Eurostat’s Dissemination (Laaboudi et al. 2024)

Laaboudi et al. (2024), *Open data dissemination at Eurostat: state of the art*, identify reproducibility as a key challenge in statistical dissemination.  
While Eurostat has improved openness, APIs, metadata standards, and DOIs, the exact bytes of datasets are not versioned.  
This means researchers cannot always guarantee that the dataset retrieved in 2022 is identical to the one retrieved under the same DOI in 2024.

---

### The Problem
- Eurostat’s dissemination system overwrites previous vintages of datasets when corrections or methodological updates are made.  
- Persistent identifiers such as DOIs ensure dataset discoverability, but they refer to the dataset concept rather than the exact file content.  
- As a result, citing only a DOI is insufficient for reproducibility, since the underlying bytes may have changed.  

---

### Options Discussed

| Option                                | Managed by | Advantages                                         | Limitations                                                    |
|---------------------------------------|------------|----------------------------------------------------|----------------------------------------------------------------|
| **Full dataset versioning (snapshots)** | Eurostat   | Strongest reproducibility; every update preserved  | Very high storage and maintenance costs; limited reuse of older versions |
| **Persistent identifiers (DOIs)**      | Eurostat   | Good for citation, metrics, and discovery          | Identifies the dataset concept, not the file bytes             |
| **Publisher-anchored hashes** (e.g. blockchain) | Eurostat | Lightweight; allows verification of user files     | Requires trustworthy infrastructure for secure hash publication |
| **User-side hashing** (logs, receipts) | Users      | No publisher burden; easy to implement             | Not authoritative; proofs remain local unless shared           |

---

### Proposed Solution
Eurostat is considering publishing dataset hashes with each update (potentially anchored on a blockchain).  

This approach avoids storing all vintages centrally while still ensuring reproducibility:

1. Eurostat publishes the official hash for each dataset update.  
2. Users keep their local file copies.  
3. Anyone can later verify a file against the published hash.  

---

### Key Message
Reproducibility requires more than open access and DOIs.  
Eurostat is therefore exploring mechanisms such as hash publication to provide **byte-level reproducibility guarantees** without the unsustainable cost of archiving every dataset vintage.
