# File map: poc

Mapping between the OSF deposit *A Safe Bet: proof-of-concept evaluation* (https://doi.org/10.17605/OSF.IO/X6BFD), the repositories and the manuscript in preparation for *Addiction* (file checked: `Paper2_POC_Manuscript_Addiction_v68.docx`, 26 August 2026). The manuscript cites deposit items by number ("POC deposit, 02.2"), not as appendices; the column below gives that item number. Programme-level files live in the `common` repository and are listed here because the manuscript cites them. Repository paths are relative to the repository root; paths starting `common/` are in the `common` repository.

| OSF file name | Repository path and new file name | Manuscript appendix number |
|---|---|---|
| `01_1_Analysis_Preregistration.docx` | `poc/protocol/asb_poc_01_1_analysis_preregistration.docx` | 01.1 (POC deposit) |
| `01_1_Analysis_Preregistration.pdf` | `poc/protocol/asb_poc_01_1_analysis_preregistration.pdf` | 01.1 (POC deposit) |
| `02_1_Framework_Design.docx` | `poc/protocol/asb_poc_02_1_framework_design.docx` | 02.1 (POC deposit) |
| `02_1_Framework_Design.pdf` | `poc/protocol/asb_poc_02_1_framework_design.pdf` | 02.1 (POC deposit) |
| `02_2_Implementation_Record.docx` | `poc/protocol/asb_poc_02_2_implementation_record.docx` | 02.2 (POC deposit) |
| `02_2_Implementation_Record.pdf` | `poc/protocol/asb_poc_02_2_implementation_record.pdf` | 02.2 (POC deposit) |
| `02_3_Glossary.docx` | `poc/protocol/asb_poc_02_3_glossary.docx` | 02.3 (POC deposit) |
| `02_3_Glossary.pdf` | `poc/protocol/asb_poc_02_3_glossary.pdf` | 02.3 (POC deposit) |
| `02_4_Tool_and_UX_Inventory.docx` | `poc/protocol/asb_poc_02_4_tool_and_ux_inventory.docx` | 02.4 (POC deposit) |
| `02_4_Tool_and_UX_Inventory.pdf` | `poc/protocol/asb_poc_02_4_tool_and_ux_inventory.pdf` | 02.4 (POC deposit) |
| `02_5_Player_Communications.docx` | `poc/protocol/asb_poc_02_5_player_communications.docx` | 02.5 (POC deposit) |
| `02_5_Player_Communications.pdf` | `poc/protocol/asb_poc_02_5_player_communications.pdf` | 02.5 (POC deposit) |
| `03_2_Data_Lifecycle_and_Provenance.docx` | `poc/protocol/asb_poc_03_2_data_lifecycle_and_provenance.docx` | 03.2 (POC deposit) |
| `03_1_Data_Dictionary.xlsx` | `common/data-documentation/asb_data_dictionary.xlsx` (the 10 September 2026 `data_dictionary.xlsx` from the previous upload set; the OSF file itself is not carried, see note 3) | 03.1 (POC deposit) |
| `03_2_Data_Lifecycle_and_Provenance.docx` | `common/data-documentation/asb_data_lifecycle_and_provenance.docx` (the 10 September 2026 `data_lifecycle_and_provenance_v2.docx` from the previous upload set) | 03.2 (POC deposit) |
| `06_1_Ethics_Application_ETH2122-0810.pdf` | `common/governance/asb_ethics_eth2122_0810_v1.pdf` | 06.1 (POC deposit) |
| `06_2_Ethics_Decision_ETH2122-0810.pdf` | `common/governance/asb_ethics_eth2122_0810_v1_decision.pdf` | 06.2 (POC deposit) |
| `06_3_Ethics_Amendment_ETH2324-0128.pdf` | `common/governance/asb_ethics_eth2324_0128_v2.pdf` | 06.3 (POC deposit) |
| `06_4_Ethics_Decision_ETH2324-0128.pdf` | `common/governance/asb_ethics_eth2324_0128_v2_decision.pdf` | 06.4 (POC deposit) |
| `06_5_Ethics_Amendment_ETH2425-0364.pdf` | `common/governance/asb_ethics_eth2425_0364_v3.pdf` | 06.5 (POC deposit) |
| `06_6_Ethics_Decision_ETH2425-0364.pdf` | `common/governance/asb_ethics_eth2425_0364_v3_decision.pdf` | 06.6 (POC deposit) |
| `06_7_Data_Management_Plan.pdf` | `common/governance/asb_dmp_poc_v2.pdf` | 06.7 (POC deposit) |
| `05_1_UK_Implementation_Architecture.xlsx` (framework deposit) | `framework/materials/05_implementation_architectures/asb_framework_05_1_uk_implementation_architecture.xlsx` (in the `framework` repository) | 05.1 (framework deposit) |

## Notes

1. Item numbers cited in the v68 running text: 01.1, 02.1, 02.2, 03.1, 03.2 (POC deposit) and 05.1 (framework deposit). Items 02.3, 02.4, 02.5, 04, 05.1 to 05.4 and 06.1 to 06.7 appear in the data availability statement only.
2. The ethics files were copied from `Paper_2_POC/05_Ethics_and_Governance/`. The OSF copies in `Paper_2_POC/OSF/06_Ethics_and_Governance/` are byte-identical (MD5 checked), so the OSF names above map exactly.
3. `asb_data_dictionary.xlsx` and `asb_data_lifecycle_and_provenance.docx` are the 10 September 2026 working versions taken from the previous upload set, not the OSF files. Their differences from the OSF files were not checked here. The OSF `03_1_Data_Dictionary.xlsx` is not in this upload set; the OSF `03_2_Data_Lifecycle_and_Provenance.docx` is carried unchanged as `poc/protocol/asb_poc_03_2_data_lifecycle_and_provenance.docx`. See the upload manifest.
4. Not mapped because they are not in this upload set: 04 analysis pipeline (R scripts, excluded), 05.1 to 05.4 analysis documentation (not in the brief), `03_2_Data_Lifecycle_and_Provenance.pdf` (not in the brief).
5. `asb_dmp_poc_v3.pdf` in `common/governance/` is not on OSF and is not cited by the manuscript; it postdates the deposit.
