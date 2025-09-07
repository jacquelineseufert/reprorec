## Reproducibility in Eurostat’s dissemination (Laaboudi et al. 2024)

Laaboudi et al. (2024), Open data dissemination at Eurostat state of the art, identify reproducibility as a key challenge in statistical dissemination. They note that while Eurostat has improved openness, APIs, metadata standards, and DOIs, the exact bytes of datasets are not versioned. This creates a reproducibility gap researchers cannot always guarantee that the dataset they used in 2022 is identical to the one retrieved under the same DOI in 2024.

### The problem
- Eurostat’s dissemination system overwrites previous vintages of datasets when corrections or methodological updates are made.  
- Persistent identifiers like DOIs ensure dataset findability and citation, but not byte identity.  
- As a result, citing only a DOI is insufficient for reproducibility, because the underlying file content may have changed.  

### Options discussed
 Option  Who manages versions  Pros  Cons 
-------------------------------------------
 Full dataset versioning (server-side snapshots)  Eurostat  Strongest reproducibility; every update preserved  Very high storage & maintenance cost; low reuse of old versions 
 Persistent identifiers (DOIs)  Eurostat  Good for citation, metrics, discovery  Identifies a dataset concept, not the bytes; does not solve reproducibility 
 Publisher-anchored hashes (e.g. blockchain registry)  Eurostat publishes hashes of each dissemination  Lightweight; user keeps the file, can verify against Eurostat’s hash  Requires infrastructure for secure, trustworthy hash publication 
 User-side hashing (receipts, local logs)  Users  No publisher burden; easy to implement  Not authoritative; proofs only exist locally unless shared 

### Their proposal
- Eurostat is considering publishing dataset hashes at each update (possibly anchored on a blockchain).  
- This avoids storing all vintages centrally while still allowing users to prove that the file they downloaded matches what Eurostat disseminated on a given date.  
- In practice  
  1. Eurostat publishes the official hash for each update.  
  2. Users keep their local files.  
  3. Anyone can later verify the local file against Eurostat’s published hash.  

### Key message from Laaboudi et al.
 Eurostat recognises that reproducibility requires more than open access and DOIs. To close the gap, they are exploring hash publication mechanisms (e.g. blockchain anchoring) to provide byte-level reproducibility guarantees without the unsustainable cost of storing every vintage.
