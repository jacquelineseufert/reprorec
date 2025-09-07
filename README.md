<h2> 🔄 𝐑𝐞𝐩𝐫𝐨𝐝𝐮𝐜𝐢𝐛𝐢𝐥𝐢𝐭𝐲 𝐢𝐧 𝐄𝐮𝐫𝐨𝐬𝐭𝐚𝐭’𝐬 𝐃𝐢𝐬𝐬𝐞𝐦𝐢𝐧𝐚𝐭𝐢𝐨𝐧 <img src="https://raw.githubusercontent.com/ABSphreak/ABSphreak/master/gifs/Hi.gif" width="30px"></h2>

<img align='right' src='https://media.giphy.com/media/26xBukhZqQp3vOukw/giphy.gif' width='200"'>

[![Eurostat Badge](https://img.shields.io/badge/Eurostat-Data-blue?style=flat-square&logo=database&logoColor=white)](https://ec.europa.eu/eurostat) 
[![Paper Badge](https://img.shields.io/badge/Laaboudi%20et%20al.-2024-green?style=flat-square&logo=readthedocs&logoColor=white)](https://doi.org/10.xxxx)  
[![Repro Badge](https://img.shields.io/badge/Reproducibility-Matters-orange?style=flat-square&logo=checkmarx&logoColor=white)](#)  

---

Laaboudi et al. (2024), *Open data dissemination at Eurostat: state of the art*, identify **reproducibility** as a key challenge in statistical dissemination.  

⚠️ While Eurostat has improved openness, APIs, metadata standards, and DOIs, the **exact bytes of datasets are not versioned**.  
This means researchers cannot always guarantee that the dataset retrieved in 2022 is identical to the one retrieved under the same DOI in 2024.

---

## 🚨 𝐓𝐡𝐞 𝐏𝐫𝐨𝐛𝐥𝐞𝐦
- Eurostat’s system **overwrites previous vintages** of datasets when corrections or updates are made.  
- **DOIs ensure discoverability**, but identify a dataset *concept*, not the file itself.  
- **Citing only a DOI is insufficient** for reproducibility, since the underlying bytes may have changed.  

---

## 📊 𝐎𝐩𝐭𝐢𝐨𝐧𝐬 𝐃𝐢𝐬𝐜𝐮𝐬𝐬𝐞𝐝

| 🔧 Option | 👥 Who manages | ✅ Pros | ⚠️ Cons |
|-----------|----------------|--------|---------|
| **Full dataset versioning** (snapshots) | Eurostat | Strongest reproducibility; every update preserved | High storage & maintenance cost |
| **Persistent identifiers (DOIs)** | Eurostat | Good for citation & discovery | Identifies *concept*, not bytes |
| **Publisher-anchored hashes** (blockchain, registries) | Eurostat | Lightweight; verifiable against published hash | Needs secure, trustworthy infra |
| **User-side hashing** (logs, receipts) | Users | No burden on Eurostat; simple | Not authoritative; proofs stay local |

---

## 💡 𝐏𝐫𝐨𝐩𝐨𝐬𝐞𝐝 𝐒𝐨𝐥𝐮𝐭𝐢𝐨𝐧
Eurostat is considering publishing **dataset hashes** with each update (possibly blockchain-anchored).  

🔑 Workflow:  
1. Eurostat publishes an official **hash** for each dataset update.  
2. Users keep their **local copies**.  
3. Anyone can later **verify** their file against Eurostat’s published hash.  

---

## 📌 𝐊𝐞𝐲 𝐌𝐞𝐬𝐬𝐚𝐠𝐞
> Reproducibility requires more than open access and DOIs.  
> Eurostat is exploring **hash publication mechanisms** to guarantee **byte-level reproducibility** without the unsustainable costs of storing every dataset vintage.
